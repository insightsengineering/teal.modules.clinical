#' Template: Summarize Variables by Row Groups Module
#'
#' @inheritParams template_arguments
#' @param parallel_vars (`logical`) used to display `summarize_vars` as parallel columns
#'  (`FALSE` on default). Can be used only if all chosen analysis variables are numeric.
#' @param row_groups (`logical`) used to display `summarize_vars` as row groups
#'  (`FALSE` on default).
#' @param numeric_stats (`character`)\cr
#'  selected statistics for numeric summarize variables to be displayed. Possible values are `n`, `mean_sd`, `mean_ci`,
#'  `median`, `median_ci`, `quantiles`, `range`. All are selected by default.
#' @param drop_zero_levels (`logical`) used to remove rows with zero counts from the result table.
#'
#' @seealso [tm_t_summary_by()]
#' @keywords internal
#'
template_summary_by <- function(parentname,
                                dataname,
                                arm_var,
                                id_var,
                                sum_vars,
                                by_vars,
                                var_labels = character(),
                                add_total = TRUE,
                                total_label = "All Patients",
                                parallel_vars = FALSE,
                                row_groups = FALSE,
                                na.rm = FALSE, # nolint
                                na_level = "<Missing>",
                                numeric_stats = c(
                                  "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range"
                                ),
                                denominator = c("N", "n", "omit"),
                                drop_arm_levels = TRUE,
                                drop_zero_levels = TRUE,
                                basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(parentname),
    assertthat::is.string(dataname),
    assertthat::is.string(arm_var),
    assertthat::is.string(id_var),
    is.character(sum_vars),
    is.character(by_vars),
    is.character(var_labels),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label),
    assertthat::is.flag(parallel_vars),
    assertthat::is.flag(row_groups),
    assertthat::is.flag(na.rm),
    assertthat::is.string(na_level),
    assertthat::is.flag(drop_arm_levels),
    is.character(numeric_stats),
    assertthat::is.flag(drop_zero_levels)
  )
  denominator <- match.arg(denominator)

  y <- list()

  # Data processing
  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        df_explicit_na(omit_columns = setdiff(names(df), c(by_vars, sum_vars)), na_level = na_level),
      env = list(
        df = as.name(dataname),
        by_vars = by_vars,
        sum_vars = sum_vars,
        na_level = na_level
      )
    )
  )

  data_list <- add_expr(
    data_list,
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = arm_var,
      drop_arm_levels = drop_arm_levels
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- df_explicit_na(parentname, na_level = na_level),
      env = list(parentname = as.name(parentname), na_level = na_level)
    )
  )

  y$data <- bracket_expr(data_list)

  # Build layout
  y$layout_prep <- quote(split_fun <- drop_split_levels)
  if (row_groups) {
    y$layout_cfun <- quote(
      cfun_unique <- function(x, labelstr = "", .N_col) { # nolint
        y <- length(unique(x))
        rcell(
          c(y, y / .N_col),
          label = labelstr
        )
      }
    )
  }

  layout_list <- list()

  table_title <- paste("Summary Table for", paste(sum_vars, collapse = ", "), "by", paste(by_vars, collapse = ", "))

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(title = table_title)
    )
  )

  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = rtables::split_cols_by(
          arm_var,
          split_fun = add_overall_level(total_label, first = FALSE)
        ),
        env = list(
          arm_var = arm_var,
          total_label = total_label
        )
      )
    } else {
      substitute(
        expr = rtables::split_cols_by(arm_var),
        env = list(arm_var = arm_var)
      )
    }
  )

  layout_list <- add_expr(
    layout_list,
    quote(rtables::add_colcounts())
  )

  if (denominator == "omit") {
    env_vars <- list(
      sum_vars = sum_vars,
      sum_var_labels = var_labels[sum_vars],
      na.rm = na.rm,
      na_level = na_level,
      denom = ifelse(denominator == "n", "n", "N_col"),
      stats = c(numeric_stats, "count")
    )
  } else {
    env_vars <- list(
      sum_vars = sum_vars,
      sum_var_labels = var_labels[sum_vars],
      na.rm = na.rm,
      na_level = na_level,
      denom = ifelse(denominator == "n", "n", "N_col"),
      stats = c(numeric_stats, "count_fraction")
    )
  }

  for (by_var in by_vars) {
    split_label <- substitute(
      expr = formatters::var_labels(dataname, fill = FALSE)[[by_var]],
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
          split_fun = split_fun,
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
          expr = rtables::summarize_row_groups(var = id_var, cfun = cfun_unique),
          env = list(
            id_var = id_var
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
              .stats = stats
            ),
            env = env_vars
          )
        } else {
          substitute(
            expr = summarize_colvars(
              vars = sum_vars,
              na.rm = na.rm,
              denom = denom,
              .stats = stats
            ),
            env = env_vars
          )
        }
      } else {
        if (length(var_labels > 0)) {
          substitute(
            expr = summarize_vars(
              vars = sum_vars,
              var_labels = sum_var_labels,
              na.rm = na.rm,
              na_level = na_level,
              denom = denom,
              .stats = stats
            ),
            env = env_vars
          )
        } else {
          substitute(
            expr = summarize_vars(
              vars = sum_vars,
              na.rm = na.rm,
              na_level = na_level,
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
          rvs <- unlist(unname(row_values(tr)))
          isTRUE(all(rvs == 0))
        }
        result <- rtables::build_table(
          lyt = lyt,
          df = anl,
          alt_counts_df = parent
        ) %>% rtables::trim_rows(criteria = all_zero)
        result
      },
      env = list(parent = as.name(parentname))
    )
  } else {
    y$table <- substitute(
      expr = {
        result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
        result
      },
      env = list(parent = as.name(parentname))
    )
  }

  y
}

#' Teal Module: Summarize Variables by Row Groups Module
#'
#' @param drop_arm_levels (`logical`)\cr drop the unused `arm_var` levels.
#'   When `TRUE`, `arm_var` levels are set to those used in the `dataname` dataset. When `FALSE`,
#'   `arm_var` levels are set to those used in the `parentname` dataset.
#'   If `dataname` dataset and `parentname` dataset are the same (i.e. `ADSL`), then `drop_arm_levels` will always be
#'   TRUE regardless of the user choice when `tm_t_summary_by` is called.
#' @param numeric_stats (`character`)\cr
#'   selected statistics for numeric summarize variables to be displayed. Possible values are `n`, `mean_sd`, `mean_ci`,
#'   `median`, `median_ci`, `range`, `geom_mean`. By default,  `n`, `mean_sd`, `median`, `range` are selected.
#' @param drop_zero_levels (`logical`) used to remove rows with zero counts from the result table.
#' @inheritParams module_arguments
#' @inheritParams template_summary_by
#'
#' @export
#' @examples
#' adsl <- tmc_ex_adsl
#' adlb <- tmc_ex_adlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADLB", adlb)
#'   ),
#'   modules = modules(
#'     tm_t_summary_by(
#'       label = "Summary by Row Groups Table",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       add_total = TRUE,
#'       by_vars = choices_selected(
#'         choices = variable_choices(adlb, c("PARAM", "AVISIT")),
#'         selected = c("AVISIT")
#'       ),
#'       summarize_vars = choices_selected(
#'         choices = variable_choices(adlb, c("AVAL", "CHG")),
#'         selected = c("AVAL")
#'       ),
#'       useNA = "ifany",
#'       paramcd = choices_selected(
#'         choices = value_choices(adlb, "PARAMCD", "PARAM"),
#'         selected = "ALT"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
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
                            total_label = "All Patients",
                            parallel_vars = FALSE,
                            row_groups = FALSE,
                            useNA = c("ifany", "no"), # nolint
                            na_level = "<Missing>",
                            numeric_stats = c("n", "mean_sd", "median", "range"),
                            denominator = teal.transform::choices_selected(c("n", "N", "omit"), "omit", fixed = TRUE),
                            drop_arm_levels = TRUE,
                            drop_zero_levels = TRUE,
                            pre_output = NULL,
                            post_output = NULL,
                            basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_summary_by")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  useNA <- match.arg(useNA) # nolint
  checkmate::assert_string(na_level)
  checkmate::assert_class(id_var, "choices_selected")
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

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
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
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_summary_by <- function(id, ...) {
  ns <- shiny::NS(id)
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
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "id_var", "paramcd", "by_vars", "summarize_vars")]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
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
      shiny::checkboxInput(ns("parallel_vars"), "Show summarize variables in parallel", value = a$parallel_vars),
      shiny::checkboxInput(ns("row_groups"), "Summarize number of subjects in row groups", value = a$row_groups),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional table settings",
          shiny::checkboxInput(ns("drop_zero_levels"), "Drop rows with 0 count", value = a$drop_zero_levels),
          shiny::radioButtons(
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
          shiny::checkboxGroupInput(
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
              shiny::checkboxInput(
                ns("drop_arm_levels"),
                label = "it's a BUG if you see this",
                value = TRUE
              )
            )
          } else {
            shiny::checkboxInput(
              ns("drop_arm_levels"),
              label = sprintf("Drop columns not in filtered %s", a$dataname),
              value = a$drop_arm_levels
            )
          }
        )
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional Variables Info",
          teal.transform::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          )
        )
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


#' @noRd
srv_summary_by <- function(id,
                           data,
                           reporter,
                           filter_panel_api,
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
                           basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    vars <- list(arm_var = arm_var, id_var = id_var, summarize_vars = summarize_vars, by_vars = by_vars)

    if (!is.null(paramcd)) {
      vars[["paramcd"]] <- paramcd
    }

    validation_rules <- list(
      arm_var = shinyvalidate::sv_required("Please select a treatment variable."),
      id_var = shinyvalidate::sv_required("Please select a subject identifier."),
      summarize_vars = shinyvalidate::sv_required("Please select a summarize variable.")
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = vars,
      datasets = data,
      select_validation_rule = validation_rules,
      filter_validation_rule = list(paramcd = shinyvalidate::sv_required(message = "Please select a filter."))
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("numeric_stats", shinyvalidate::sv_required("Please select at least one statistic to display."))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data,
      join_keys = get_join_keys(data),
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      id = "adsl_merge",
      datasets = data,
      join_keys = get_join_keys(data),
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_id_var <- names(merged$anl_input_r()$columns_source$id_var)
      input_by_vars <- names(merged$anl_input_r()$columns_source$by_vars)
      input_summarize_vars <- names(merged$anl_input_r()$columns_source$summarize_var)
      input_paramcd <- `if`(is.null(paramcd), NULL, unlist(paramcd$filter)["vars_selected"])

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_by_vars, input_summarize_vars, input_id_var),
        arm_var = input_arm_var
      )

      if (input$parallel_vars) {
        shiny::validate(shiny::need(
          all(vapply(anl_filtered[input_summarize_vars], is.numeric, logical(1))),
          "Summarize variables must all be numeric to display in parallel columns."
        ))
      }
    })

    # The R-code corresponding to the analysis.
    all_q <- shiny::reactive({
      validate_checks()
      summarize_vars <- as.vector(merged$anl_input_r()$columns_source$summarize_vars)
      var_labels <- formatters::var_labels(merged$anl_q()[[dataname]][, summarize_vars, drop = FALSE])

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

      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
    })

    # Outputs to render.
    table_r <- shiny::reactive(all_q()[["result"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(all_q())))
    )

    # Render R code.
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Summarize Variables by Row Groups Table")
        card$append_text("Summarize Variables by Row Groups Table", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(all_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
