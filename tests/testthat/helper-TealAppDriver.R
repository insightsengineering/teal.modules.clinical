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

# Escape `id` for a CSS attribute selector (IDs may contain `:` / `.` which break `#id` selectors).
.teal_picks_badge_css <- function(id) {
  id <- gsub("\\", "\\\\", id, fixed = TRUE)
  id <- gsub("\"", "\\\"", id, fixed = TRUE)
  paste0("[id=\"", id, "\"]")
}

# Open a teal.picks badge dropdown so nested pickerInput values are bound in the session.
open_teal_picks_dropdown <- function(app_driver, pick_id) {
  checkmate::assert_string(pick_id)
  badge_ns <- app_driver$namespaces()$module(paste0(pick_id, "-inputs-summary_badge"))
  app_driver$click(.teal_picks_badge_css(badge_ns))
  app_driver$wait_for_idle()
  invisible(app_driver)
}

# Read the Shiny value for a categorical teal.picks slot (variables, values, datasets, ...).
get_teal_picks_slot <- function(app_driver, pick_id, slot = "variables") {
  checkmate::assert_string(pick_id)
  checkmate::assert_string(slot)
  open_teal_picks_dropdown(app_driver, pick_id)
  raw <- app_driver$get_active_module_input(paste0(pick_id, "-", slot, "-selected"))
  if (is.null(raw)) {
    return(NULL)
  }
  as.vector(raw)
}

# Set a categorical teal.picks slot; picker commits when *_selected_open becomes FALSE.
# Use value = NULL for an empty multi-select (character(0) is sent to Shiny).
# @param wait (`logical(1)`) if `TRUE` (default), call `wait_for_idle()` after committing the picker.
set_teal_picks_slot <- function(app_driver, pick_id, slot, value, wait = TRUE) {
  checkmate::assert_string(pick_id)
  checkmate::assert_string(slot)
  checkmate::assert_flag(wait)
  open_teal_picks_dropdown(app_driver, pick_id)
  sel_id <- app_driver$namespaces()$module(paste0(pick_id, "-", slot, "-selected"))
  open_id <- app_driver$namespaces()$module(paste0(pick_id, "-", slot, "-selected_open"))
  val <- if (is.null(value)) character(0) else value
  app_driver$set_input(sel_id, val)
  app_driver$set_input(open_id, FALSE)
  if (isTRUE(wait)) {
    app_driver$wait_for_idle()
  }
  invisible(app_driver)
}

ns_des_input <- function(id, dataname, type) {
  sprintf("%s-dataset_%s_singleextract-%s", id, dataname, type)
}
