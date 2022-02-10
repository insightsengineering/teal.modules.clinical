#' Template: Summary of Variables
#'
#' @param show_labels (`character`)\cr
#'   defines whether the labels for `sum_vars` should display. For details see [rtables::analyze()].
#' @param numeric_stats (`character`)\cr
#'   selected statistics for numeric summarize variables to be displayed. Possible values are `n`, `mean_sd`, `mean_ci`,
#'   `median`, `median_ci`, `quantiles` and `range`. All are selected by default.
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
                             var_labels = character(),
                             na.rm = FALSE, # nolint
                             na_level = "<Missing>",
                             numeric_stats = c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range"),
                             denominator = c("N", "n", "omit"),
                             drop_arm_levels = TRUE,
                             basic_table_args = teal.devel::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    is.character(arm_var),
    is.character(sum_vars),
    assertthat::is.flag(add_total),
    is.character(var_labels),
    assertthat::is.flag(na.rm),
    assertthat::is.string(na_level),
    assertthat::is.flag(drop_arm_levels)
  )
  checkmate::assert_character(numeric_stats, min.len = 1)
  checkmate::assert_subset(numeric_stats, c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range"))
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

  parsed_basic_table_args <- teal.devel::parse_basic_table_args(
    teal.devel::resolve_basic_table_args(
      user_table = basic_table_args
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
      quote(rtables::add_overall_col("All Patients"))
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
#' @param arm_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table. If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param drop_arm_levels (`logical`)\cr drop the unused `arm_var` levels.
#'   When `TRUE`, `arm_var` levels are set to those used in the `dataname` dataset. When `FALSE`,
#'   `arm_var` levels are set to those used in the `parentname` dataset.
#'   If `dataname` dataset and `parentname` dataset are the same (i.e. ADSL), then `drop_arm_levels` will always be
#'   TRUE regardless of the user choice when `tm_t_summary` is called.
#' @param numeric_stats (`character`)\cr
#'   selected statistics for numeric summarize variables to be displayed. Possible values are `n`, `mean_sd`, `mean_ci`,
#'   `median`, `median_ci`, `quantiles` and `range`. By default,  `n`, `mean_sd`, `median`, `range` are selected.
#' @inheritParams module_arguments
#'
#' @export
#' @examples
#' # Preparation of the test case.
#' library(dplyr)
#' library(scda)
#' library(tern)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#'
#' # Include `EOSDY` and `DCSREAS` variables below because they contain missing data.
#' stopifnot(
#'   any(is.na(adsl$EOSDY)),
#'   any(is.na(adsl$DCSREAS))
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     code = 'ADSL <- synthetic_cdisc_data("latest")$adsl',
#'     check = TRUE
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_summary <- function(label,
                         dataname,
                         parentname = ifelse(
                           inherits(arm_var, "data_extract_spec"),
                           teal.devel::datanames_input(arm_var),
                           "ADSL"
                         ),
                         arm_var,
                         summarize_vars,
                         add_total = TRUE,
                         useNA = c("ifany", "no"), # nolint
                         na_level = "<Missing>",
                         numeric_stats = c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range"),
                         denominator = c("N", "n", "omit"),
                         drop_arm_levels = TRUE,
                         pre_output = NULL,
                         post_output = NULL,
                         basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_summary")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_character(numeric_stats, min.len = 1)
  checkmate::assert_subset(numeric_stats, c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range"))
  useNA <- match.arg(useNA) # nolint
  numeric_stats <- match.arg(numeric_stats)
  denominator <- match.arg(denominator)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

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
        na_level = na_level,
        basic_table_args = basic_table_args
      )
    ),
    filters = dataname
  )
}

#' @noRd
ui_summary <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  is_single_dataset_value <- teal.devel::is_single_dataset(a$arm_var, a$summarize_vars)

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(teal.devel::table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(a[c("arm_var", "summarize_vars")]),
      teal.devel::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.devel::data_extract_ui(
        id = ns("summarize_vars"),
        label = "Summarize Variables",
        data_extract_spec = a$summarize_vars,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional table settings",
          radioButtons(
            ns("useNA"),
            label = "Display NA counts",
            choices = c("ifany", "no"),
            selected = a$useNA
          ),
          checkboxGroupInput(
            ns("numeric_stats"),
            label = "Choose the statistics to display for numeric variables",
            choices = c(
              "n" = "n",
              "Mean (SD)" = "mean_sd",
              "Mean 95% CI" = "mean_ci",
              "Median" = "median",
              "Median 95% CI" = "median_ci",
              "25% and 75%-ile" = "quantiles",
              "Min - Max" = "range"
            ),
            selected = a$numeric_stats
          ),
          radioButtons(
            ns("denominator"),
            label = "Denominator choice",
            choices = c("N", "n", "omit"),
            selected = a$denominator
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
      )
    ),
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_summary <- function(id,
                        datasets,
                        dataname,
                        parentname,
                        arm_var,
                        summarize_vars,
                        add_total,
                        na_level,
                        drop_arm_levels,
                        label,
                        basic_table_args) {
  stopifnot(is_cdisc_data(datasets))
  moduleServer(id, function(input, output, session) {
    teal.devel::init_chunks()

    anl_selectors <- teal.devel::data_extract_multiple_srv(
      list(arm_var = arm_var, summarize_vars = summarize_vars),
      datasets = datasets
    )

    anl_merged <- teal.devel::data_merge_srv(
      selector_list = anl_selectors,
      datasets = datasets,
      merge_function = "dplyr::inner_join"
    )

    adsl_merged <- teal.devel::data_merge_module(
      datasets = datasets,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    # validate inputs
    validate_checks <- reactive({
      adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
      anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

      anl_m <- anl_merged()
      input_arm_var <- anl_merged()$columns_source$arm_var
      input_summarize_vars <- anl_merged()$columns_source$summarize_vars

      validate(
        need(input_arm_var, "Please select a treatment variable"),
        need(input_summarize_vars, "Please select a summarize variable"),
        need(length(input_arm_var) <= 2, "Please limit treatment variables within two"),
        if (length(input_arm_var) == 2) {
          need(
            is.factor(adsl_filtered[[input_arm_var[[2]]]]) & all(!adsl_filtered[[input_arm_var[[2]]]] %in% c(
              "", NA
            )),
            "Please check nested treatment variable which needs to be a factor without NA or empty strings."
          )
        },
        need(!is.null(input$numeric_stats), "Please select at least one statistic to display.")
      )

      teal.devel::validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_summarize_vars),
        arm_var = input_arm_var[[1]]
      )
    })

    # generate r code for the analysis
    call_preparation <- reactive({
      validate_checks()

      teal.devel::chunks_reset()
      anl_m <- anl_merged()
      teal.devel::chunks_push_data_merge(anl_m)
      teal.devel::chunks_push_new_line()

      anl_adsl <- adsl_merged()
      teal.devel::chunks_push_data_merge(anl_adsl)
      teal.devel::chunks_push_new_line()
      sum_vars <- anl_merged()$columns_source$summarize_vars

      my_calls <- template_summary(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = anl_merged()$columns_source$arm_var,
        sum_vars = sum_vars,
        show_labels = "visible",
        add_total = input$add_total,
        var_labels = get_var_labels(datasets, dataname, sum_vars),
        na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE), # nolint
        na_level = na_level,
        numeric_stats = input$numeric_stats,
        denominator = input$denominator,
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = basic_table_args
      )
      mapply(expression = my_calls, teal.devel::chunks_push)
    })

    # Outputs to render.
    table <- reactive({
      call_preparation()
      teal.devel::chunks_safe_eval()
      teal.devel::chunks_get_var("result")
    })

    teal.devel::table_with_settings_srv(
      id = "table",
      table_r = table
    )

    # Render R code.
    teal.devel::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.devel::get_extract_datanames(list(arm_var, summarize_vars)),
      modal_title = "R Code for the current Summary Table",
      code_header = label
    )
  })
}
