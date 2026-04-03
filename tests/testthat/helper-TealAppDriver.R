# Setup timeout options for shinytest2 if none are set in options nor on environment variables
withr::local_options(
  list(
    shinytest2.timeout = getOption(
      "shinytest2.timeout",
      default = Sys.getenv("SHINYTEST2_TIMEOUT", unset = 30 * 1000)
    ),
    shinytest2.load_timeout = getOption(
      "shinytest2.load_timeout",
      default = Sys.getenv("SHINYTEST2_LOAD_TIMEOUT", unset = 60 * 1000)
    ),
    shinytest2.duration = getOption(
      "shinytest2.duration",
      default = Sys.getenv("SHINYTEST2_DURATION", unset = 0.5 * 1000)
    )
  ),
  .local_envir = testthat::test_env()
)

# Initialization function to create a new TealAppDriver object
#
# By manipulating the server function as below, we can hint {shinytest2} to load
# this package and its "Depends".
# Related to https://github.com/rstudio/shinytest2/issues/381
init_teal_app_driver <- function(...) {
  testthat::with_mocked_bindings(
    {
      TealAppDriver <- getFromNamespace("TealAppDriver", "teal") # nolint: object_name.
      TealAppDriver$new(...)
    },
    shinyApp = function(ui, server, ...) {
      functionBody(server) <- bquote({
        # Hint to shinytest2 that this package should be available (via {globals})
        .hint_to_load_package <- tm_g_ci # Hint to shinytest2 when looking for packages in globals
        .(functionBody(server))
      })

      shiny::shinyApp(ui, server, ...)
    },
    # The relevant shinyApp call in `TealAppDriver` is being called without prefix,
    # hence why the package bindings that is changed is in {teal} and not {shiny}
    .package = "teal"
  )
}

# Escape an HTML element `id` for use inside a JavaScript double-quoted string literal.
.teal_picks_js_id_literal <- function(id) {
  id <- gsub("\\", "\\\\", id, fixed = TRUE)
  id <- gsub("\"", "\\\"", id, fixed = TRUE)
  id <- gsub("\r", "\\r", id, fixed = TRUE)
  id <- gsub("\n", "\\n", id, fixed = TRUE)
  paste0("\"", id, "\"")
}

# JavaScript array literal of quoted strings for picker values (may be empty).
.teal_picks_js_string_array_literal <- function(val) {
  val <- as.character(val)
  if (length(val) == 0L) {
    return("[]")
  }
  parts <- vapply(val, function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    x <- gsub("\"", "\\\"", x, fixed = TRUE)
    paste0("\"", x, "\"")
  }, character(1))
  paste0("[", paste(parts, collapse = ","), "]")
}

# JavaScript double-quoted string literal (e.g. Shiny.setInputValue name or string value).
.teal_picks_js_quoted_string <- function(s) {
  .teal_picks_js_id_literal(s)
}

# Value argument for Shiny.setInputValue: `[]`, a JSON string, or a JSON string array.
.teal_picks_shiny_setinput_value_literal <- function(val) {
  val <- as.character(val)
  if (length(val) == 0L) {
    return("[]")
  }
  if (length(val) == 1L) {
    return(.teal_picks_js_quoted_string(val[[1]]))
  }
  paste0("[", paste(vapply(val, .teal_picks_js_quoted_string, character(1)), collapse = ","), "]")
}

# Sync native <select> + bootstrap-select widget, then let Shiny read change events.
.teal_picks_apply_select_value_in_browser <- function(app_driver, select_id, val) {
  checkmate::assert_string(select_id)
  val <- as.character(val)
  id_lit <- .teal_picks_js_id_literal(select_id)
  arr_lit <- .teal_picks_js_string_array_literal(val)
  app_driver$run_js(sprintf(
    paste0(
      "(() => {\n",
      "  const sel = document.getElementById(%s);\n",
      "  if (!sel) return false;\n",
      "  const arr = %s;\n",
      "  if (sel.multiple) {\n",
      "    const wanted = new Set(arr);\n",
      "    for (const o of sel.options) o.selected = wanted.has(o.value);\n",
      "  } else {\n",
      "    sel.value = arr.length ? arr[0] : '';\n",
      "  }\n",
      "  if (window.jQuery && jQuery(sel).data('selectpicker')) {\n",
      "    if (sel.multiple) {\n",
      "      jQuery(sel).selectpicker('val', arr);\n",
      "    } else {\n",
      "      jQuery(sel).selectpicker('val', arr.length ? arr[0] : '');\n",
      "    }\n",
      "    jQuery(sel).selectpicker('refresh');\n",
      "  }\n",
      "  sel.dispatchEvent(new Event('input', { bubbles: true }));\n",
      "  sel.dispatchEvent(new Event('change', { bubbles: true }));\n",
      "  return true;\n",
      "})()"
    ),
    id_lit,
    arr_lit
  ))
  invisible(app_driver)
}

# Push values through Shiny and force teal.picks picker commit (selected_open pulse).
.teal_picks_shiny_set_picker_and_commit <- function(app_driver, sel_id, open_id, val) {
  checkmate::assert_string(sel_id)
  checkmate::assert_string(open_id)
  val <- as.character(val)
  name_sel <- .teal_picks_js_quoted_string(sel_id)
  name_open <- .teal_picks_js_quoted_string(open_id)
  val_js <- .teal_picks_shiny_setinput_value_literal(val)
  app_driver$run_js(paste(
    sprintf("Shiny.setInputValue(%s, %s, {priority: 'event'});", name_sel, val_js),
    sprintf("Shiny.setInputValue(%s, true, {priority: 'event'});", name_open),
    sprintf("Shiny.setInputValue(%s, false, {priority: 'event'});", name_open),
    sep = "\n"
  ))
  invisible(app_driver)
}

# Click the teal.picks summary badge (toggles the dropdown open/closed).
# Use getElementById + click() instead of AppDriver$click(CSS): Chromote querySelectorAll
# can return DOM error -32000 for some ids/selectors in CI even with attribute selectors.
.teal_picks_click_summary_badge <- function(app_driver, pick_id) {
  checkmate::assert_string(pick_id)
  badge_ns <- app_driver$namespaces()$module(paste0(pick_id, "-inputs-summary_badge"))
  id_lit <- .teal_picks_js_id_literal(badge_ns)
  app_driver$wait_for_js(sprintf("document.getElementById(%s) !== null", id_lit))
  app_driver$run_js(sprintf(
    "(() => { const el = document.getElementById(%s); el.click(); })()",
    id_lit
  ))
  app_driver$wait_for_idle()
  invisible(app_driver)
}

# Open a teal.picks badge dropdown. Required before set_input on nested pickers: badge-dropdown
# script.js calls Shiny.bindAll(container) only when the panel is shown.
open_teal_picks_dropdown <- function(app_driver, pick_id) {
  .teal_picks_click_summary_badge(app_driver, pick_id)
}

# Close the teal.picks badge if it is open (same control toggles).
close_teal_picks_dropdown <- function(app_driver, pick_id) {
  .teal_picks_click_summary_badge(app_driver, pick_id)
}

# Normalize JS read: NULL / empty -> NULL; one level -> scalar char; several -> char vector.
.teal_picks_normalize_slot_read <- function(raw) {
  if (is.null(raw)) {
    return(NULL)
  }
  v <- as.character(unlist(raw, use.names = FALSE))
  if (length(v) == 0L) {
    return(NULL)
  }
  if (length(v) == 1L) {
    return(v[[1]])
  }
  v
}

# Read the Shiny value for a categorical teal.picks slot (variables, values, datasets, ...).
# While the badge has never been opened, picker inputs are not bound (see teal.picks
# badge-dropdown script.js). `get_active_module_input` can list every choice after
# bootstrap-select binds; read the native <select> instead (true committed option(s)).
get_teal_picks_slot <- function(app_driver, pick_id, slot = "variables") {
  checkmate::assert_string(pick_id)
  checkmate::assert_string(slot)
  open_teal_picks_dropdown(app_driver, pick_id)
  close_teal_picks_dropdown(app_driver, pick_id)
  sel_id <- app_driver$namespaces()$module(paste0(pick_id, "-", slot, "-selected"))
  id_lit <- .teal_picks_js_id_literal(sel_id)
  app_driver$wait_for_js(sprintf("document.getElementById(%s) !== null", id_lit))
  raw <- app_driver$get_js(sprintf(
    paste0(
      "(() => {\n",
      "  const sel = document.getElementById(%s);\n",
      "  if (!sel) return null;\n",
      "  if (sel.multiple) return Array.from(sel.selectedOptions).map(o => o.value);\n",
      "  if (!sel.value) return [];\n",
      "  return [sel.value];\n",
      "})()"
    ),
    id_lit
  ))
  .teal_picks_normalize_slot_read(raw)
}

# Set a categorical teal.picks slot. `set_input` alone often does not refresh bootstrap-select
# or trigger teal.picks' commit observer reliably; sync the DOM widget then pulse
# `*_selected_open` via Shiny.setInputValue.
# Use value = NULL for an empty multi-select (character(0) is sent to Shiny).
# @param wait (`logical(1)`) if `TRUE` (default), call `wait_for_idle()` after committing the picker.
set_teal_picks_slot <- function(app_driver, pick_id, slot, value, wait = TRUE) {
  checkmate::assert_string(pick_id)
  checkmate::assert_string(slot)
  checkmate::assert_flag(wait)
  open_teal_picks_dropdown(app_driver, pick_id)
  sel_id <- app_driver$namespaces()$module(paste0(pick_id, "-", slot, "-selected"))
  open_id <- app_driver$namespaces()$module(paste0(pick_id, "-", slot, "-selected_open"))
  val <- if (is.null(value)) character(0) else as.character(value)
  .teal_picks_apply_select_value_in_browser(app_driver, sel_id, val)
  .teal_picks_shiny_set_picker_and_commit(app_driver, sel_id, open_id, val)
  if (isTRUE(wait)) {
    app_driver$wait_for_idle()
  }
  close_teal_picks_dropdown(app_driver, pick_id)
  invisible(app_driver)
}

ns_des_input <- function(id, dataname, type) {
  sprintf("%s-dataset_%s_singleextract-%s", id, dataname, type)
}
