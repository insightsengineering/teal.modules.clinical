#' Template: Summarize Variables by Row Groups Module
#'
#' Creates a valid expression to generate a table to summarize variables by row groups.
#'
#' @inheritParams template_arguments
#' @param parallel_vars (`logical`)\cr whether summarized variables should be arranged in columns. Can only be set to
#' `TRUE` if all chosen analysis variables are numeric.
#' @param row_groups (`logical`)\cr whether summarized variables should be arranged in row groups.
#' @param drop_zero_levels (`logical`)\cr whether rows with zero counts in all columns should be removed from the table.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_summary_by()]
#'
#' @keywords internal
template_summary_by <- function(parentname,
                                dataname,
                                arm_var,
                                id_var,
                                sum_vars,
                                by_vars,
                                var_labels = character(),
                                add_total = TRUE,
                                total_label = default_total_label(),
                                parallel_vars = FALSE,
                                row_groups = FALSE,
                                na.rm = FALSE, # nolint: object_name.
                                na_level = tern::default_na_str(),
                                numeric_stats = c(
                                  "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range"
                                ),
                                denominator = c("N", "n", "omit"),
                                drop_arm_levels = TRUE,
                                drop_zero_levels = TRUE,
                                basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(parentname)
  checkmate::assert_string(dataname)
  checkmate::assert_string(id_var)
  checkmate::assert_character(sum_vars)
  checkmate::assert_character(by_vars)
  checkmate::assert_character(var_labels)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(parallel_vars)
  checkmate::assert_flag(row_groups)
  checkmate::assert_flag(na.rm)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_character(numeric_stats)
  checkmate::assert_flag(drop_zero_levels)
  checkmate::assert_character(arm_var, min.len = 1, max.len = 2)

  denominator <- match.arg(denominator)

  y <- list()

  # Data processing
  data_list <- list()
  if (!na.rm) {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = anl <- tern::df_explicit_na(df,
          omit_columns = setdiff(
            names(df),
            c(by_vars, sum_vars)
          ),
          na_level = na_str
        ),
        env = list(
          df = as.name(dataname),
          by_vars = by_vars,
          sum_vars = sum_vars,
          na_str = na_level
        )
      )
    )
  } else {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = anl <- df,
        env = list(df = as.name(dataname))
      )
    )
  }

  prepare_arm_levels_call <- lapply(arm_var, function(x) {
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = x,
      drop_arm_levels = drop_arm_levels
    )
  })
  data_list <- Reduce(add_expr, prepare_arm_levels_call, init = data_list)
  if (!na.rm) {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = parentname <- tern::df_explicit_na(parentname, na_level = na_level),
        env = list(parentname = as.name(parentname), na_level = na_level)
      )
    )
  }

  y$data <- bracket_expr(data_list)

  # Build layout
  if (row_groups) {
    y$layout_cfun <- quote(
      cfun_unique <- function(x, labelstr = "", .N_col) { # nolint: object_name.
        y <- length(unique(x))
        rcell(
          c(y, y / .N_col),
          label = labelstr
        )
      }
    )
  }

  table_title <- paste("Summary Table for", paste(sum_vars, collapse = ", "), "by", paste(by_vars, collapse = ", "))

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        show_colcounts = TRUE,
        title = table_title,
        main_footer = sprintf("N represents the number of unique subject IDs such that the variable has NA (%s) values.", na_level)
      )
    )
  )

  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  split_cols_call <- lapply(arm_var, function(x) {
    if (drop_arm_levels) {
      substitute(
        expr = rtables::split_cols_by(x, split_fun = rtables::drop_split_levels),
        env = list(x = x)
      )
    } else {
      substitute(
        expr = rtables::split_cols_by(x),
        env = list(x = x)
      )
    }
  })
  layout_list <- Reduce(add_expr, split_cols_call, init = layout_list)

  if (add_total && !parallel_vars) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_overall_col(total_label),
        env = list(total_label = total_label)
      )
    )
  }

  env_vars <- list(
    sum_vars = sum_vars,
    sum_var_labels = var_labels[sum_vars],
    na.rm = na.rm,
    na_level = na_level,
    denom = ifelse(denominator == "n", "n", "N_col"),
    stats = c(
      numeric_stats,
      ifelse(denominator == "omit", "count", "count_fraction")
    )
  )

  for (by_var in by_vars) {
    split_label <- substitute(
      expr = teal.data::col_labels(dataname, fill = FALSE)[[by_var]],
      env = list(
        dataname = as.name(dataname),
        by_var = by_var
      )
    )

    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::split_rows_by(
          by_var,
          split_label = split_label,
          split_fun = rtables::drop_split_levels,
          label_pos = "topleft"
        ),
        env = list(
          by_var = by_var,
          split_label = split_label
        )
      )
    )

    if (row_groups) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          expr = rtables::summarize_row_groups(var = id_var, cfun = cfun_unique, na_str = na_str),
          env = list(
            id_var = id_var,
            na_str = na_level
          )
        )
      )
    }
  }

  if (parallel_vars) {
    layout_list <- add_expr(
      layout_list,
      if (length(var_labels) > 0) {
        substitute(
          expr = split_cols_by_multivar(vars = sum_vars, varlabels = sum_var_labels),
          env = list(sum_vars = sum_vars, sum_var_labels = var_labels[sum_vars])
        )
      } else {
        substitute(
          expr = split_cols_by_multivar(vars = sum_vars),
          env = list(sum_vars = sum_vars)
        )
      }
    )
  }

  if (row_groups) {
    layout_list <- layout_list
  } else {
    layout_list <- add_expr(
      layout_list,
      if (parallel_vars) {
        if (length(var_labels) > 0) {
          substitute(
            expr = summarize_colvars(
              na.rm = na.rm,
              denom = denom,
              .stats = stats,
              na_str = na_level
            ),
            env = env_vars
          )
        } else {
          substitute(
            expr = summarize_colvars(
              vars = sum_vars,
              na.rm = na.rm,
              denom = denom,
              .stats = stats,
              na_str = na_level
            ),
            env = env_vars
          )
        }
      } else {
        if (length(var_labels > 0)) {
          substitute(
            expr = tern::analyze_vars(
              vars = sum_vars,
              var_labels = sum_var_labels,
              na.rm = na.rm,
              na_str = na_level,
              denom = denom,
              .stats = stats
            ),
            env = env_vars
          )
        } else {
          substitute(
            expr = tern::analyze_vars(
              vars = sum_vars,
              na.rm = na.rm,
              na_str = na_level,
              denom = denom,
              .stats = stats
            ),
            env = env_vars
          )
        }
      }
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  if (drop_zero_levels) {
    y$table <- substitute(
      expr = {
        all_zero <- function(tr) {
          if (!inherits(tr, "TableRow") || inherits(tr, "LabelRow")) {
            return(FALSE)
          }
          rvs <- unlist(unname(rtables::row_values(tr)))
          isTRUE(all(rvs == 0))
        }
        table <- rtables::build_table(
          lyt = lyt,
          df = anl,
          alt_counts_df = parent
        ) %>% rtables::trim_rows(criteria = all_zero)
      },
      env = list(parent = as.name(parentname))
    )
  } else {
    y$table <- substitute(
      expr = {
        table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      },
      env = list(parent = as.name(parentname))
    )
  }

  y
}

#' teal Module: Summarize Variables by Row Groups
#'
#' This module produces a table to summarize variables by row groups.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_summary_by
#' @param arm_var ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table.
#'   If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`TableTree` - output of `rtables::build_table()`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_summary_by(
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
#'   ADLB <- tmc_ex_adlb
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_summary_by(
#'       label = "Summary by Row Groups Table",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADSL, c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       add_total = TRUE,
#'       by_vars = choices_selected(
#'         choices = variable_choices(ADLB, c("PARAM", "AVISIT")),
#'         selected = c("AVISIT")
#'       ),
#'       summarize_vars = choices_selected(
#'         choices = variable_choices(ADLB, c("AVAL", "CHG")),
#'         selected = c("AVAL")
#'       ),
#'       useNA = "ifany",
#'       paramcd = choices_selected(
#'         choices = value_choices(ADLB, "PARAMCD", "PARAM"),
#'         selected = "ALT"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_summary_by <- function(label,
                            dataname,
                            parentname = ifelse(
                              inherits(arm_var, "data_extract_spec"),
                              teal.transform::datanames_input(arm_var),
                              "ADSL"
                            ),
                            arm_var,
                            by_vars,
                            summarize_vars,
                            id_var = teal.transform::choices_selected(
                              teal.transform::variable_choices(dataname, subset = "USUBJID"),
                              selected = "USUBJID", fixed = TRUE
                            ),
                            paramcd = NULL,
                            add_total = TRUE,
                            total_label = default_total_label(),
                            parallel_vars = FALSE,
                            row_groups = FALSE,
                            useNA = c("ifany", "no"), # nolint: object_name.
                            na_level = tern::default_na_str(),
                            numeric_stats = c("n", "mean_sd", "median", "range"),
                            denominator = teal.transform::choices_selected(c("n", "N", "omit"), "omit", fixed = TRUE),
                            drop_arm_levels = TRUE,
                            drop_zero_levels = TRUE,
                            pre_output = NULL,
                            post_output = NULL,
                            basic_table_args = teal.widgets::basic_table_args(),
                            transformators = list(),
                            decorators = list()) {
  message("Initializing tm_t_summary_by")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  useNA <- match.arg(useNA) # nolint: object_name.
  checkmate::assert_string(na_level)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(by_vars, "choices_selected")
  checkmate::assert_class(summarize_vars, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(denominator, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_zero_levels)
  checkmate::assert_subset(denominator$choices, choices = c("n", "N", "omit"))
  checkmate::assert_flag(parallel_vars)
  checkmate::assert_flag(row_groups)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_character(numeric_stats, min.len = 1)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  numeric_stats_choices <- c("n", "mean_sd", "mean_ci", "geom_mean", "median", "median_ci", "quantiles", "range")
  numeric_stats <- match.arg(numeric_stats, numeric_stats_choices, several.ok = TRUE)

  assert_decorators(decorators, "table")

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname, multiple = TRUE, ordered = TRUE),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    paramcd = `if`(
      is.null(paramcd),
      NULL,
      cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE)
    ),
    by_vars = cs_to_des_select(by_vars, dataname = dataname, multiple = TRUE, ordered = TRUE),
    summarize_vars = cs_to_des_select(summarize_vars, dataname = dataname, multiple = TRUE, ordered = TRUE)
  )

  module(
    label = label,
    ui = ui_summary_by,
    ui_args = c(data_extract_list, args),
    server = srv_summary_by,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        total_label = total_label,
        na_level = na_level,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_summary_by <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$id_var,
    a$paramcd,
    a$by_vars,
    a$summarize_vars
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(a[c("arm_var", "id_var", "paramcd", "by_vars", "summarize_vars")]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Column Variable(s)",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      `if`(
        is.null(a$paramcd),
        NULL,
        teal.transform::data_extract_ui(
          id = ns("paramcd"),
          label = "Select Endpoint",
          data_extract_spec = a$paramcd,
          is_single_dataset = is_single_dataset_value
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("by_vars"),
        label = "Row By Variable",
        data_extract_spec = a$by_vars,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("summarize_vars"),
        label = "Summarize Variables",
        data_extract_spec = a$summarize_vars,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("parallel_vars"), "Show summarize variables in parallel", value = a$parallel_vars),
      checkboxInput(ns("row_groups"), "Summarize number of subjects in row groups", value = a$row_groups),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          checkboxInput(ns("drop_zero_levels"), "Drop rows with 0 count", value = a$drop_zero_levels),
          radioButtons(
            ns("useNA"),
            label = "Display NA counts",
            choices = c("ifany", "no"),
            selected = a$useNA
          ),
          teal.widgets::optionalSelectInput(
            inputId = ns("denominator"),
            label = "Denominator choice",
            choices = a$denominator$choices,
            selected = a$denominator$selected,
            fixed = a$denominator$fixed
          ),
          checkboxGroupInput(
            ns("numeric_stats"),
            label = "Choose the statistics to display for numeric variables",
            choices = c(
              "n" = "n",
              "Mean (SD)" = "mean_sd",
              "Mean 95% CI" = "mean_ci",
              "Geometric Mean" = "geom_mean",
              "Median" = "median",
              "Median 95% CI" = "median_ci",
              "25% and 75%-ile" = "quantiles",
              "Min - Max" = "range"
            ),
            selected = a$numeric_stats
          ),
          if (a$dataname == a$parentname) {
            shinyjs::hidden(
              checkboxInput(
                ns("drop_arm_levels"),
                label = "it's a BUG if you see this",
                value = TRUE
              )
            )
          } else {
            checkboxInput(
              ns("drop_arm_levels"),
              label = sprintf("Drop columns not in filtered %s", a$dataname),
              value = a$drop_arm_levels
            )
          }
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          teal.transform::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          )
        )
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_summary_by <- function(id,
                           data,
                           dataname,
                           parentname,
                           arm_var,
                           id_var,
                           paramcd,
                           by_vars,
                           summarize_vars,
                           add_total,
                           total_label,
                           na_level,
                           drop_arm_levels,
                           drop_zero_levels,
                           label,
                           basic_table_args,
                           decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    vars <- list(arm_var = arm_var, id_var = id_var, summarize_vars = summarize_vars, by_vars = by_vars)

    if (!is.null(paramcd)) {
      vars[["paramcd"]] <- paramcd
    }

    validation_rules <- list(
      arm_var = ~ if (length(.) != 1 && length(.) != 2) {
        "Please select 1 or 2 column variables"
      },
      id_var = shinyvalidate::sv_required("Please select a subject identifier."),
      summarize_vars = shinyvalidate::sv_required("Please select a summarize variable.")
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = vars,
      datasets = data,
      select_validation_rule = validation_rules,
      filter_validation_rule = list(paramcd = shinyvalidate::sv_required(message = "Please select a filter."))
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("numeric_stats", shinyvalidate::sv_required("Please select at least one statistic to display."))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      id = "adsl_merge",
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_id_var <- names(merged$anl_input_r()$columns_source$id_var)
      input_by_vars <- names(merged$anl_input_r()$columns_source$by_vars)
      input_summarize_vars <- names(merged$anl_input_r()$columns_source$summarize_vars)
      input_paramcd <- `if`(is.null(paramcd), NULL, unlist(paramcd$filter)["vars_selected"])

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_by_vars, input_summarize_vars, input_id_var),
        arm_var = input_arm_var[[1]]
      )

      if (input$parallel_vars) {
        validate(shiny::need(
          all(vapply(anl_filtered[input_summarize_vars], is.numeric, logical(1))),
          "Summarize variables must all be numeric to display in parallel columns."
        ))
      }
    })

    # Generate r code for the analysis.
    all_q <- reactive({
      validate_checks()
      summarize_vars <- as.vector(merged$anl_input_r()$columns_source$summarize_vars)
      var_labels <- teal.data::col_labels(merged$anl_q()[[dataname]][, summarize_vars, drop = FALSE])

      my_calls <- template_summary_by(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = as.vector(merged$anl_input_r()$columns_source$arm_var),
        sum_vars = summarize_vars,
        by_vars = as.vector(merged$anl_input_r()$columns_source$by_vars),
        var_labels = var_labels,
        id_var = as.vector(merged$anl_input_r()$columns_source$id_var),
        na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE),
        na_level = na_level,
        numeric_stats = input$numeric_stats,
        denominator = input$denominator,
        add_total = input$add_total,
        total_label = total_label,
        parallel_vars = input$parallel_vars,
        row_groups = input$row_groups,
        drop_arm_levels = input$drop_arm_levels,
        drop_zero_levels = input$drop_zero_levels,
        basic_table_args = basic_table_args
      )

      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Decoration of table output.
    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
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
