#' Template: Summary of Variables
#'
#' @param show_labels (`character`)\cr
#'   defines whether the labels for `sum_vars` should display. For details see [rtables::analyze()].
#' @param numeric_stats (`character`)\cr
#'   selected statistics for numeric summarize variables to be displayed. Possible values are `n`, `mean_sd`, `mean_ci`,
#'   `median`, `median_ci`, `quantiles`, `range` and `geom_mean`. All are selected by default.
#' @inheritParams template_arguments
#'
#' @seealso [tm_t_summary()]
#' @keywords internal
#'
template_summary <- function(dataname,
                             parentname,
                             arm_var,
                             sum_vars,
                             show_labels = c("default", "visible", "hidden"),
                             add_total = TRUE,
                             total_label = "All Patients",
                             var_labels = character(),
                             na.rm = FALSE, # nolint
                             na_level = "<Missing>",
                             numeric_stats = c(
                               "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean"
                             ),
                             denominator = c("N", "n", "omit"),
                             drop_arm_levels = TRUE,
                             basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    is.character(arm_var),
    is.character(sum_vars),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label),
    is.character(var_labels),
    assertthat::is.flag(na.rm),
    assertthat::is.string(na_level),
    assertthat::is.flag(drop_arm_levels)
  )
  checkmate::assert_character(numeric_stats, min.len = 1)
  checkmate::assert_subset(
    numeric_stats,
    c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean")
  )
  denominator <- match.arg(denominator)
  show_labels <- match.arg(show_labels)

  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        df_explicit_na(omit_columns = setdiff(names(df), c(sum_vars)), na_level = na_level),
      env = list(
        df = as.name(dataname),
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
      parentname <- df_explicit_na(parentname, na_level = ""),
      env = list(parentname = as.name(parentname))
    )
  )

  y$data <- bracket_expr(data_list)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        main_footer =
          "n represents the number of unique subject IDs such that the variable has non-NA values."
      )
    )
  )

  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_cols_by(arm_var),
      env = list(arm_var = arm_var[[1]])
    )
  )
  if (length(arm_var) == 2) {
    layout_list <- add_expr(
      layout_list,
      if (drop_arm_levels) {
        substitute(
          expr = rtables::split_cols_by(nested_col, split_fun = drop_split_levels),
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
        expr = rtables::add_overall_col(total_label),
        env = list(total_label = total_label)
      )
    )
  }
  layout_list <- add_expr(
    layout_list,
    quote(rtables::add_colcounts())
  )

  env_sum_vars <- list(
    sum_vars = sum_vars,
    sum_var_labels = var_labels[sum_vars],
    show_labels = show_labels,
    na.rm = na.rm,
    na_level = na_level,
    denom = ifelse(denominator == "n", "n", "N_col"),
    stats = c(
      numeric_stats,
      ifelse(denominator == "omit", "count", "count_fraction")
    )
  )

  layout_list <- add_expr(
    layout_list,
    if (length(var_labels) > 0) {
      substitute(
        expr = summarize_vars(
          vars = sum_vars,
          var_labels = sum_var_labels,
          show_labels = show_labels,
          na.rm = na.rm,
          na_level = na_level,
          denom = denom,
          .stats = stats
        ),
        env = env_sum_vars
      )
    } else {
      substitute(
        expr = summarize_vars(
          vars = sum_vars,
          show_labels = show_labels,
          na.rm = na.rm,
          na_level = na_level,
          denom = denom,
          .stats = stats
        ),
        env = env_sum_vars
      )
    }
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      result
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' Teal Module: Summary of Variables
#'
#' @param arm_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table. If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param drop_arm_levels (`logical`)\cr drop the unused `arm_var` levels.
#'   When `TRUE`, `arm_var` levels are set to those used in the `dataname` dataset. When `FALSE`,
#'   `arm_var` levels are set to those used in the `parentname` dataset.
#'   If `dataname` dataset and `parentname` dataset are the same (i.e. `ADSL`), then `drop_arm_levels` will always be
#'   TRUE regardless of the user choice when `tm_t_summary` is called.
#' @param numeric_stats (`character`)\cr
#'   selected statistics for numeric summarize variables to be displayed. Possible values are `n`, `mean_sd`, `mean_ci`,
#'   `median`, `median_ci`, `quantiles`, `range` and `geom_mean`. By default,  `n`, `mean_sd`, `median`, `range` are
#'   selected.
#' @inheritParams module_arguments
#'
#' @export
#' @examples
#' # Preparation of the test case
#' adsl <- tmc_ex_adsl
#' adsl$EOSDY[1] <- NA_integer_
#'
#' # Include `EOSDY` and `DCSREAS` variables below because they contain missing data.
#' stopifnot(
#'   any(is.na(adsl$EOSDY)),
#'   any(is.na(adsl$DCSREAS))
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl)
#'   ),
#'   modules = modules(
#'     tm_t_summary(
#'       label = "Demographic Table",
#'       dataname = "ADSL",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       add_total = TRUE,
#'       summarize_vars = choices_selected(
#'         c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE"),
#'         c("SEX", "RACE")
#'       ),
#'       useNA = "ifany"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_summary <- function(label,
                         dataname,
                         parentname = ifelse(
                           inherits(arm_var, "data_extract_spec"),
                           teal.transform::datanames_input(arm_var),
                           "ADSL"
                         ),
                         arm_var,
                         summarize_vars,
                         add_total = TRUE,
                         total_label = "All Patients",
                         useNA = c("ifany", "no"), # nolint
                         na_level = "<Missing>",
                         numeric_stats = c(
                           "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean"
                         ),
                         denominator = c("N", "n", "omit"),
                         drop_arm_levels = TRUE,
                         pre_output = NULL,
                         post_output = NULL,
                         basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_summary")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_character(numeric_stats, min.len = 1)
  useNA <- match.arg(useNA) # nolint
  denominator <- match.arg(denominator)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  checkmate::assertFlag(add_total)
  checkmate::assert_string(total_label)

  numeric_stats <- match.arg(numeric_stats, several.ok = TRUE)

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname, multiple = TRUE, ordered = TRUE),
    summarize_vars = cs_to_des_select(summarize_vars, dataname = dataname, multiple = TRUE, ordered = TRUE)
  )

  module(
    label = label,
    server = srv_summary,
    ui = ui_summary,
    ui_args = c(data_extract_list, args),
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
    datanames = dataname
  )
}

#' @noRd
ui_summary <- function(id, ...) {
  ns <- shiny::NS(id)
  a <- list(...)

  is_single_dataset_value <- teal.transform::is_single_dataset(a$arm_var, a$summarize_vars)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "summarize_vars")]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Column Variable(s)",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.transform::data_extract_ui(
        id = ns("summarize_vars"),
        label = "Summarize Variables",
        data_extract_spec = a$summarize_vars,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional table settings",
          shiny::radioButtons(
            ns("useNA"),
            label = "Display NA counts",
            choices = c("ifany", "no"),
            selected = a$useNA
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
          shiny::radioButtons(
            ns("denominator"),
            label = "Denominator choice",
            choices = c("N", "n", "omit"),
            selected = a$denominator
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
srv_summary <- function(id,
                        data,
                        reporter,
                        filter_panel_api,
                        dataname,
                        parentname,
                        arm_var,
                        summarize_vars,
                        add_total,
                        total_label,
                        na_level,
                        drop_arm_levels,
                        label,
                        basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")
  shiny::moduleServer(id, function(input, output, session) {
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(arm_var = arm_var, summarize_vars = summarize_vars),
      datasets = data,
      select_validation_rule = list(
        summarize_vars = shinyvalidate::sv_required("Please select a summarize variable"),
        arm_var = ~ if (length(.) != 1 && length(.) != 2) {
          "Please select 1 or 2 column variables"
        }
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("numeric_stats", shinyvalidate::sv_required("Please select at least one statistic to display."))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      id = "anl_merge",
      datasets = data,
      selector_list = selector_list,
      join_keys = get_join_keys(data),
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      id = "adsl_merge",
      datasets = data,
      data_extract = list(arm_var = arm_var),
      join_keys = get_join_keys(data),
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

    shiny::observeEvent(merged$anl_input_r()$columns_source$summarize_vars, {
      choices_classes <- sapply(
        names(merged$anl_input_r()$columns_source$summarize_vars),
        function(x) {
          summarize_var_data <- data[[summarize_vars$dataname]]()[[x]]
          inherits(summarize_var_data, "numeric") |
            inherits(summarize_var_data, "integer")
        }
      )

      if (any(choices_classes)) {
        shinyjs::show("numeric_stats")
      } else {
        shinyjs::hide("numeric_stats")
      }
    })

    # validate inputs
    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]
      anl <- merged$anl_q()[["ANL"]]

      # we take names of the columns source as they match names of the input data in merge_datasets
      # if we use $arm_var they might be renamed to <selector id>.arm_var
      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_summarize_vars <- names(merged$anl_input_r()$columns_source$summarize_vars)

      shiny::validate(
        shiny::need(
          length(unique(anl$USUBJID)) == nrow(anl),
          paste0(
            "Please choose an analysis dataset where each row represents a different subject, ",
            "i.e. USUBJID is different in each row"
          )
        ),
        shiny::need(
          !any(vapply(anl_filtered[, input_summarize_vars], inherits, c("Date", "POSIXt"),
            FUN.VALUE = logical(1)
          )),
          "Date and POSIXt variables are not supported, please select other variables"
        ),
        if (length(input_arm_var) == 2) {
          shiny::need(
            is.factor(adsl_filtered[[input_arm_var[[2]]]]) & all(!adsl_filtered[[input_arm_var[[2]]]] %in% c("", NA)),
            "Please check nested treatment variable which needs to be a factor without NA or empty strings."
          )
        }
      )

      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_summarize_vars),
        arm_var = input_arm_var[[1]]
      )
    })

    # generate r code for the analysis
    all_q <- shiny::reactive({
      validate_checks()

      summarize_vars <- merged$anl_input_r()$columns_source$summarize_vars
      var_labels <- formatters::var_labels(data[[dataname]]()[, summarize_vars, drop = FALSE])
      my_calls <- template_summary(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = merged$anl_input_r()$columns_source$arm_var,
        sum_vars = summarize_vars,
        show_labels = "visible",
        add_total = input$add_total,
        total_label = total_label,
        var_labels = var_labels,
        na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE), # nolint
        na_level = na_level,
        numeric_stats = input$numeric_stats,
        denominator = input$denominator,
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = basic_table_args
      )

      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
    })

    # Outputs to render.
    table_r <- shiny::reactive(all_q()[["result"]])
    teal.widgets::table_with_settings_srv(id = "table", table_r = table_r)

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
        card$set_name("Summary Table")
        card$append_text("Summary Table", "header2")
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
