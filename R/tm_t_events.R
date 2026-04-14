#' Template: Events by Term
#'
#' Creates a valid expression to generate a table of events by term.
#'
#' @inheritParams template_arguments
#' @param sort_freq_col (`character`)\cr column to sort by frequency on if `sort_criteria` is set to `freq_desc`.
#' @param incl_overall_sum (`flag`)\cr  whether two rows which summarize the overall number of adverse events
#'   should be included at the top of the table.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_events()]
#'
#' @keywords internal
template_events <- function(dataname,
                            parentname,
                            arm_var,
                            hlt,
                            llt,
                            label_hlt = NULL,
                            label_llt = NULL,
                            add_total = TRUE,
                            total_label = default_total_label(),
                            na_level = tern::default_na_str(),
                            event_type = "event",
                            sort_criteria = c("freq_desc", "alpha"),
                            sort_freq_col = total_label,
                            prune_freq = 0,
                            prune_diff = 0,
                            drop_arm_levels = TRUE,
                            incl_overall_sum = TRUE,
                            basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_character(arm_var, min.len = 1, max.len = 2)
  checkmate::assert_string(hlt, null.ok = TRUE)
  checkmate::assert_string(llt, null.ok = TRUE)
  checkmate::assert_string(label_hlt, null.ok = TRUE)
  checkmate::assert_string(label_llt, null.ok = TRUE)
  checkmate::assert_character(c(llt, hlt))
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_string(event_type)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_scalar(prune_freq)
  checkmate::assert_scalar(prune_diff)

  sort_criteria <- match.arg(sort_criteria)

  y <- list()

  # Start data steps.
  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df,
      env = list(df = as.name(dataname))
    )
  )

  data_list <- add_expr(
    data_list,
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = arm_var[[1]],
      drop_arm_levels = drop_arm_levels
    )
  )
  if (length(arm_var) == 2) {
    data_list <- add_expr(
      data_list,
      prepare_arm_levels(
        dataname = "anl",
        parentname = parentname,
        arm_var = arm_var[[2]],
        drop_arm_levels = drop_arm_levels
      )
    )
  }

  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- tern::df_explicit_na(parentname, na_level = na_lvl),
      env = list(parentname = as.name(parentname), na_lvl = na_level)
    )
  )

  if (sort_criteria == "alpha") {
    if (!is.null(hlt)) {
      data_list <- add_expr(
        data_list,
        substitute(
          expr = anl[[hlt]] <- as.character(anl[[hlt]]),
          env = list(hlt = hlt)
        )
      )
    }

    if (!is.null(llt)) {
      data_list <- add_expr(
        data_list,
        substitute(
          expr = anl[[llt]] <- as.character(anl[[llt]]),
          env = list(llt = llt)
        )
      )
    }
  }

  term_vars <- c(hlt, llt)

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- anl %>%
        tern::df_explicit_na(omit_columns = setdiff(names(anl), term_vars)),
      env = list(
        term_vars = term_vars
      )
    )
  )
  y$data <- bracket_expr(data_list)

  # Start layout steps.
  layout_list <- list()

  basic_title <- if (is.null(hlt) && !is.null(llt)) {
    paste0("Event Summary by Term : ", label_llt)
  } else if (!is.null(hlt) && is.null(llt)) {
    paste0("Event Summary by Term : ", label_hlt)
  } else if (!is.null(hlt) && !is.null(llt)) {
    paste0("Event Summary by Term : ", label_hlt, " and ", label_llt)
  } else {
    "Event Summary by Term"
  }

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(show_colcounts = TRUE, title = basic_title)
    )
  )

  layout_list <- add_expr(layout_list, parsed_basic_table_args)
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_cols_by(var = arm_var),
      env = list(arm_var = arm_var[[1]])
    )
  )
  if (length(arm_var) == 2) {
    layout_list <- add_expr(
      layout_list,
      if (drop_arm_levels) {
        substitute(
          expr = rtables::split_cols_by(nested_col, split_fun = rtables::drop_split_levels),
          env = list(nested_col = arm_var[[2]])
        )
      } else {
        substitute(
          expr = rtables::split_cols_by(nested_col),
          env = list(nested_col = arm_var[[2]])
        )
      }
    )
  }

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_overall_col(label = total_label),
        env = list(total_label = total_label)
      )
    )
  }

  unique_label <- paste0("Total number of patients with at least one ", event_type)
  nonunique_label <- paste0("Overall total number of ", event_type, "s")

  if (incl_overall_sum) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        tern::summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = unique_label,
            nonunique = nonunique_label
          ),
          na_str = na_str
        ),
        env = list(unique_label = unique_label, nonunique_label = nonunique_label, na_str = na_level)
      )
    )
  }


  one_term <- is.null(hlt) || is.null(llt)

  if (one_term) {
    term_var <- ifelse(is.null(hlt), llt, hlt)

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::count_occurrences(vars = term_var, .indent_mods = -1L) %>%
          tern::append_varlabels(dataname, term_var),
        env = list(
          term_var = term_var,
          dataname = as.name(dataname)
        )
      )
    )
  } else {
    # Case when both hlt and llt are used.

    y$layout_prep <- quote(split_fun <- rtables::drop_split_levels)

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::split_rows_by(
          hlt,
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::col_labels(dataname[hlt])
        ) %>%
          tern::summarize_num_patients(
            var = "USUBJID",
            .stats = c("unique", "nonunique"),
            .labels = c(
              unique = unique_label,
              nonunique = nonunique_label
            ),
            na_str = na_str
          ) %>%
          tern::count_occurrences(vars = llt, .indent_mods = c(count_fraction = 1L)) %>%
          tern::append_varlabels(dataname, llt, indent = 1L),
        env = list(
          dataname = as.name(dataname),
          hlt = hlt,
          llt = llt,
          unique_label = unique_label,
          nonunique_label = nonunique_label,
          na_str = na_level
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- substitute(
    expr = table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent),
    env = list(parent = as.name(parentname))
  )

  # Start pruning table.
  prune_list <- list()
  prune_list <- add_expr(
    prune_list,
    quote(
      pruned_result <- rtables::prune_table(table)
    )
  )

  if (prune_freq > 0 || prune_diff > 0) {
    # Do not use "All Patients" column for pruning conditions.
    prune_list <- add_expr(
      prune_list,
      substitute(
        expr = col_indices <- 1:(ncol(table) - add_total),
        env = list(add_total = add_total)
      )
    )

    if (prune_freq > 0 && prune_diff == 0) {
      prune_list <- add_expr(
        prune_list,
        substitute(
          expr = row_condition <- has_fraction_in_any_col(atleast = prune_freq, col_indices = col_indices),
          env = list(prune_freq = prune_freq)
        )
      )
    } else if (prune_freq == 0 && prune_diff > 0) {
      prune_list <- add_expr(
        prune_list,
        substitute(
          expr = row_condition <- has_fractions_difference(atleast = prune_diff, col_indices = col_indices),
          env = list(prune_diff = prune_diff)
        )
      )
    } else if (prune_freq > 0 && prune_diff > 0) {
      prune_list <- add_expr(
        prune_list,
        substitute(
          expr = row_condition <- has_fraction_in_any_col(atleast = prune_freq, col_indices = col_indices) &
            has_fractions_difference(atleast = prune_diff, col_indices = col_indices),
          env = list(prune_freq = prune_freq, prune_diff = prune_diff)
        )
      )
    }

    # Apply pruning conditions.
    prune_list <- add_expr(
      prune_list,
      substitute(
        expr = pruned_result <- pruned_result %>% rtables::prune_table(keep_rows(row_condition))
      )
    )
  }

  y$prune <- bracket_expr(prune_list)

  # Start sorting pruned table.
  sort_list <- list()

  if (sort_criteria == "alpha") {
    if (prune_freq == 0 && prune_diff == 0) {
      # This is just a dummy step to get the right variable result.
      # No additional sorting is needed because during the data pre-processing step,
      # llt and/or hlt are converted to factors with alphabetically sorted levels.
      # So the order in y$table table is already alphabetically sorted.
      sort_list <- add_expr(
        sort_list,
        quote({
          pruned_and_sorted_result <- pruned_result
        })
      )
    } else {
      sort_list <- add_expr(
        sort_list,
        quote(
          criteria_fun <- function(tr) {
            inherits(tr, "ContentRow")
          }
        )
      )

      sort_list <- add_expr(
        sort_list,
        quote({
          pruned_and_sorted_result <- rtables::trim_rows(pruned_result, criteria = criteria_fun)
        })
      )
    }
  } else {
    # Sort by decreasing frequency.
    if (add_total) {
      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = idx_split_col <- which(sapply(rtables::col_paths(table), tail, 1) == sort_freq_col),
          env = list(sort_freq_col = sort_freq_col)
        )
      )
    }

    # When the "All Patients" column is present we only use that for scoring.
    scorefun_hlt <- if (add_total) {
      quote(rtables::cont_n_onecol(idx_split_col))
    } else {
      quote(rtables::cont_n_allcols)
    }
    scorefun_llt <- if (add_total) {
      quote(tern::score_occurrences_cols(col_indices = idx_split_col))
    } else {
      quote(tern::score_occurrences)
    }

    if (one_term) {
      term_var <- ifelse(is.null(hlt), llt, hlt)

      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = {
            pruned_and_sorted_result <- pruned_result %>%
              rtables::sort_at_path(path = c(term_var), scorefun = scorefun_llt)
          },
          env = list(
            term_var = term_var,
            scorefun_llt = scorefun_llt
          )
        )
      )
    } else {
      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = {
            pruned_and_sorted_result <- pruned_result %>%
              rtables::sort_at_path(path = c(hlt), scorefun = scorefun_hlt) %>%
              rtables::sort_at_path(path = c(hlt, "*", llt), scorefun = scorefun_llt)
          },
          env = list(
            llt = llt,
            hlt = hlt,
            scorefun_hlt = scorefun_hlt,
            scorefun_llt = scorefun_llt
          )
        )
      )

      if (prune_freq > 0 || prune_diff > 0) {
        sort_list <- add_expr(
          sort_list,
          quote(
            criteria_fun <- function(tr) {
              inherits(tr, "ContentRow")
            }
          )
        )

        sort_list <- add_expr(
          sort_list,
          quote(
            pruned_and_sorted_result <- rtables::trim_rows(pruned_and_sorted_result, criteria = criteria_fun)
          )
        )
      }
    }
  }
  y$sort <- bracket_expr(sort_list)

  y
}

#' teal Module: Events by Term
#'
#' This module produces a table of events by term.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_events
#' @param arm_var ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr specification of variable names that can be used as
#'   `arm_var`. It defines the grouping variable(s) in the results table.
#'   If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param hlt ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr specification of the analysis variable used as the
#'   high level term for event grouping.
#' @param llt ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr specification of the analysis variable used as the
#'   low level term for event grouping.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`TableTree` as created from `rtables::build_table`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_events(
#'    ..., # arguments for module
#'    decorators = list(
#'      table = teal_transform_module(...) # applied only to `table` output
#'    )
#' )
#' ```
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.clinical")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADAE <- tmc_ex_adae
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADAE <- data[["ADAE"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_events(
#'       label = "Adverse Event Table",
#'       dataname = "ADAE",
#'       arm_var = variables(
#'         choices = c("ARM", "ARMCD"),
#'         selected = "ARM"
#'       ),
#'       llt = variables(
#'         choices = c("AETERM", "AEDECOD"),
#'         selected = "AEDECOD"
#'       ),
#'       hlt = variables(
#'         choices = c("AEBODSYS", "AESOC"),
#'         selected = "AEBODSYS"
#'       ),
#'       add_total = TRUE,
#'       event_type = "adverse event"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_events <- function(label,
                        dataname,
                        parentname = "ADSL",
                        arm_var = variables(
                          choices = c("ARM", "ARMCD"),
                          selected = "ARM"
                        ),
                        hlt = variables(
                          choices = c("AEBODSYS", "AESOC"),
                          selected = "AEBODSYS"
                        ),
                        llt = variables(
                          choices = c("AETERM", "AEDECOD"),
                          selected = "AEDECOD"
                        ),
                        add_total = TRUE,
                        total_label = default_total_label(),
                        na_level = tern::default_na_str(),
                        event_type = "event",
                        sort_criteria = c("freq_desc", "alpha"),
                        sort_freq_col = total_label,
                        prune_freq = 0,
                        prune_diff = 0,
                        drop_arm_levels = TRUE,
                        incl_overall_sum = TRUE,
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.widgets::basic_table_args(),
                        transformators = list(),
                        decorators = list()) {
  message("Initializing tm_t_events")
  arm_var <- deprecate_pick_variables_arg(arm_var, "arm_var")
  hlt <- deprecate_pick_variables_arg(hlt, "hlt")
  llt <- deprecate_pick_variables_arg(llt, "llt")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(event_type)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_string(sort_freq_col)
  checkmate::assert_scalar(prune_freq)
  checkmate::assert_scalar(prune_diff)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_flag(incl_overall_sum)
  sort_criteria <- match.arg(sort_criteria)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")


  arm_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), arm_var)
  hlt <- teal.picks::picks(teal.picks::datasets(dataname, dataname), hlt)
  llt <- teal.picks::picks(teal.picks::datasets(dataname, dataname), llt)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_events_byterm,
    server = srv_t_events_byterm,
    ui_args = args[names(args) %in% names(formals(ui_t_events_byterm))],
    server_args = args[names(args) %in% names(formals(srv_t_events_byterm))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}

#' @keywords internal
ui_t_events_byterm <- function(id,
                               arm_var,
                               hlt,
                               llt,
                               add_total,
                               drop_arm_levels,
                               sort_criteria,
                               prune_freq,
                               prune_diff,
                               pre_output,
                               post_output,
                               decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Treatment Variable"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      ),
      tags$div(
        tags$label("Event High Level Term"),
        teal.picks::picks_ui(ns("hlt"), hlt)
      ),
      tags$div(
        tags$label("Event Low Level Term"),
        teal.picks::picks_ui(ns("llt"), llt)
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = add_total),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table")),
      bslib::accordion_panel(
        "Additional table settings",
        open = TRUE,
        checkboxInput(
          ns("drop_arm_levels"),
          label = "Drop columns not in filtered analysis dataset",
          value = drop_arm_levels
        ),
        selectInput(
          inputId = ns("sort_criteria"),
          label = "Sort Criteria",
          choices = c(
            "Decreasing frequency" = "freq_desc",
            "Alphabetically" = "alpha"
          ),
          selected = sort_criteria,
          multiple = FALSE
        ),
        helpText(tags$strong("Pruning Options:")),
        numericInput(
          inputId = ns("prune_freq"),
          label = "Minimum Incidence Rate(%) in any of the treatment groups",
          value = prune_freq,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        ),
        numericInput(
          inputId = ns("prune_diff"),
          label = "Minimum Difference Rate(%) between any of the treatment groups",
          value = prune_diff,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_t_events_byterm <- function(id,
                                data,
                                dataname,
                                parentname,
                                event_type,
                                arm_var,
                                hlt,
                                llt,
                                drop_arm_levels,
                                incl_overall_sum,
                                label,
                                total_label,
                                na_level,
                                sort_freq_col,
                                basic_table_args,
                                decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(arm_var = arm_var, hlt = hlt, llt = llt),
      data = data
    )

    anl_selectors <- selectors
    adsl_selectors <- selectors["arm_var"]

    data_with_card <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj
    })
    merged_anl <- merge_srv(
      "merge_anl",
      data = data_with_card, selectors = anl_selectors, output_name = "ANL"
    )
    merged_adsl_anl <- merge_srv(
      "merge_adsl_anl",
      data = merged_anl$data, selectors = adsl_selectors, output_name = "ANL_ADSL"
    )
    anl_q <- merged_adsl_anl$data

    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- anl_selectors$arm_var()$variables$selected
      input_level_term <- c(
        anl_selectors$hlt()$variables$selected,
        anl_selectors$llt()$variables$selected
      )

      validate(shiny::need(
        length(input_arm_var) >= 1L,
        "Please select a treatment variable."
      ))

      validate(
        if (length(input_arm_var) >= 1) {
          need(is.factor(adsl_filtered[[input_arm_var[[1]]]]), "Treatment variable is not a factor.")
        },
        if (length(input_arm_var) == 2) {
          need(
            is.factor(adsl_filtered[[input_arm_var[[2]]]]) & all(!adsl_filtered[[input_arm_var[[2]]]] %in% c(
              "", NA
            )),
            "Please check nested treatment variable which needs to be a factor without NA or empty strings."
          )
        }
      )

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_level_term),
        arm_var = input_arm_var[[1]]
      )
    })

    # The R-code corresponding to the analysis.
    table_q <- reactive({
      validate_checks()
      ANL <- anl_q()[["ANL"]]

      input_hlt <- anl_selectors$hlt()$variables$selected
      input_llt <- anl_selectors$llt()$variables$selected
      label_hlt <- if (length(input_hlt) != 0) attributes(ANL[[input_hlt]])$label else NULL
      label_llt <- if (length(input_llt) != 0) attributes(ANL[[input_llt]])$label else NULL

      my_calls <- template_events(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = anl_selectors$arm_var()$variables$selected,
        hlt = if (length(input_hlt) != 0) input_hlt else NULL,
        llt = if (length(input_llt) != 0) input_llt else NULL,
        label_hlt = label_hlt,
        label_llt = label_llt,
        add_total = input$add_total,
        total_label = total_label,
        na_level = na_level,
        event_type = event_type,
        sort_criteria = input$sort_criteria,
        sort_freq_col = sort_freq_col,
        prune_freq = input$prune_freq / 100,
        prune_diff = input$prune_diff / 100,
        drop_arm_levels = input$drop_arm_levels,
        incl_overall_sum = incl_overall_sum,
        basic_table_args = basic_table_args
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    table_renamed_q <- reactive({
      req(table_q())
      within(table_q(), {
        table <- pruned_and_sorted_result
      })
    })
    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = table_renamed_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    # Outputs to render.
    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
