#' Template: Summary of Variables
#'
#' Creates a valid expression to generate a table to summarize variables.
#'
#' @inheritParams template_arguments
#' @param arm_var_labels (`character` or `NULL`)\cr vector of column variable labels to display, of the same length as
#'   `arm_var`. If `NULL`, no labels will be displayed.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_summary()]
#'
#' @keywords internal
template_summary <- function(dataname,
                             parentname,
                             arm_var,
                             sum_vars,
                             add_total = TRUE,
                             total_label = default_total_label(),
                             var_labels = character(),
                             arm_var_labels = NULL,
                             na.rm = FALSE, # nolint: object_name.
                             na_level = tern::default_na_str(),
                             numeric_stats = c(
                               "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean"
                             ),
                             denominator = c("N", "n", "omit"),
                             drop_arm_levels = TRUE,
                             basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_character(arm_var, min.len = 1, max.len = 2)
  checkmate::assert_character(sum_vars)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_character(var_labels)
  checkmate::assert_character(arm_var_labels, len = length(arm_var), null.ok = TRUE)
  checkmate::assert_flag(na.rm)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_character(numeric_stats, min.len = 1)
  checkmate::assert_subset(
    numeric_stats,
    c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean")
  )

  denominator <- match.arg(denominator)

  y <- list()

  # Data processing
  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        tern::df_explicit_na(omit_columns = setdiff(names(df), c(sum_vars)), na_level = na_str),
      env = list(
        df = as.name(dataname),
        sum_vars = sum_vars,
        na_str = na_level
      )
    )
  )

  prepare_arm_levels_call <- lapply(arm_var, function(x) {
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = x,
      drop_arm_levels = drop_arm_levels
    )
  })
  data_list <- Reduce(add_expr, prepare_arm_levels_call, init = data_list)

  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- tern::df_explicit_na(parentname, na_level = na_str),
      env = list(
        parentname = as.name(parentname),
        na_str = na_level
      )
    )
  )

  y$data <- bracket_expr(data_list)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        show_colcounts = TRUE,
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

  # Build layout
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

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_overall_col(total_label),
        env = list(total_label = total_label)
      )
    )
  }

  env_sum_vars <- list(
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

  layout_list <- add_expr(
    layout_list,
    if (length(var_labels) > 0) {
      substitute(
        expr = tern::analyze_vars(
          vars = sum_vars,
          var_labels = sum_var_labels,
          show_labels = "visible",
          na.rm = na.rm,
          na_str = na_level,
          denom = denom,
          .stats = stats
        ),
        env = env_sum_vars
      )
    } else {
      substitute(
        expr = tern::analyze_vars(
          vars = sum_vars,
          show_labels = "visible",
          na.rm = na.rm,
          na_str = na_level,
          denom = denom,
          .stats = stats
        ),
        env = env_sum_vars
      )
    }
  )

  if (!is.null(arm_var_labels)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::append_topleft(arm_var_labels),
        env = list(arm_var_labels = c(arm_var_labels, ""))
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' teal Module: Summary of Variables
#'
#' This module produces a table to summarize variables.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_summary
#' @param arm_var ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table.
#'   If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param show_arm_var_labels (`flag`)\cr whether arm variable label(s) should be displayed. Defaults to `TRUE`.
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
#' tm_t_summary(
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
#' # Preparation of the test case - use `EOSDY` and `DCSREAS` variables to demonstrate missing data.
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADSL$EOSDY[1] <- NA_integer_
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#'
#' app <- init(
#'   data = data,
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
#' @export
tm_t_summary <- function(label,
                         dataname,
                         parentname = "ADSL",
                         arm_var,
                         summarize_vars,
                         add_total = TRUE,
                         total_label = default_total_label(),
                         show_arm_var_labels = TRUE,
                         useNA = c("ifany", "no"), # nolint: object_name.
                         na_level = tern::default_na_str(),
                         numeric_stats = c(
                           "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean"
                         ),
                         denominator = c("N", "n", "omit"),
                         drop_arm_levels = TRUE,
                         pre_output = NULL,
                         post_output = NULL,
                         basic_table_args = teal.widgets::basic_table_args(),
                         transformators = list(),
                         decorators = list()) {
  message("Initializing tm_t_summary")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(summarize_vars, "variables")
  checkmate::assert_string(na_level)
  checkmate::assert_character(numeric_stats, min.len = 1)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  checkmate::assert_flag(add_total)
  checkmate::assert_flag(show_arm_var_labels)
  checkmate::assert_string(total_label)
  assert_decorators(decorators, "table")

  useNA <- match.arg(useNA) # nolint: object_name.
  denominator <- match.arg(denominator)
  numeric_stats <- match.arg(numeric_stats, several.ok = TRUE)


  arm_var <- picks(datasets(parentname, parentname), arm_var) # multiple TRUE, ordered TRUE
  summarize_vars <- picks(datasets(dataname, dataname), summarize_vars) # multiple TRUE ordered TRUE

  args <- as.list(environment())

  module(
    label = label,
    server = srv_summary,
    ui = ui_summary,
    ui_args = args[names(args) %in% names(formals(ui_summary))],
    server_args = args[names(args) %in% names(formals(srv_summary))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_summary <- function(id,
                       dataname,
                       parentname,
                       arm_var,
                       summarize_vars,
                       add_total,
                       useNA,
                       numeric_stats,
                       denominator,
                       drop_arm_levels,
                       pre_output,
                       post_output,
                       decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Column Variable(s)"),
        picks_ui(ns("arm_var"), arm_var),
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = add_total),
      tags$div(
        tags$label("Summarize Variables"),
        picks_ui(ns("summarize_vars"), summarize_vars),
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          radioButtons(
            ns("useNA"),
            label = "Display NA counts",
            choices = c("ifany", "no"),
            selected = useNA
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
            selected = numeric_stats
          ),
          radioButtons(
            ns("denominator"),
            label = "Denominator choice",
            choices = c("N", "n", "omit"),
            selected = denominator
          ),
          if (dataname == parentname) {
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
              label = sprintf("Drop columns not in filtered %s", dataname),
              value = drop_arm_levels
            )
          }
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "table"))
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_summary <- function(id,
                        data,
                        dataname,
                        parentname,
                        arm_var,
                        summarize_vars,
                        add_total,
                        show_arm_var_labels,
                        total_label,
                        na_level,
                        drop_arm_levels,
                        label,
                        basic_table_args,
                        decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- picks_srv(id = "",
      picks = list(
        arm_var = arm_var,
        summarize_vars = summarize_vars
      ),
      data = data
    )

    anl_selectors <- selectors

    # ↓ add usubjid to make sure it is included in a merged dataset (primary_keys are not included if not selected)
    anl_selectors$usubjid <- reactive(picks(datasets(parentname, parentname), variables("USUBJID", "USUBJID")))
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
    merged_anl <- merge_srv("merge_anl", data = data_with_card, selectors = anl_selectors, output_name = "ANL")
    merged_adsl_anl <- merge_srv("merge_adsl_anl", data = merged_anl$data, selectors = adsl_selectors, output_name = "ANL_ADSL")
    anl_q <- merged_adsl_anl$data

    observeEvent(map_merged(anl_selectors)$summarize_vars$variables, {
      choices_classes <- sapply(
        map_merged(anl_selectors)$summarize_vars$variables,
        function(x) {
          summarize_var_data <- data()[[dataname]][[x]]
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

    # Validate inputs.
    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      # we take names of the columns source as they match names of the input data in merge_datasets
      # if we use $arm_var they might be renamed to <selector id>.arm_var
      input_arm_var <- map_merged(anl_selectors)$arm_var$variables
      input_summarize_vars <- map_merged(anl_selectors)$summarize_vars$variables

      validate(
        need(
          length(unique(anl$USUBJID)) == nrow(anl),
          paste0(
            "Please choose an analysis dataset where each row represents a different subject, ",
            "i.e. USUBJID is different in each row"
          )
        ),
        need(
          !any(vapply(anl_filtered[, input_summarize_vars], inherits, c("Date", "POSIXt"),
            FUN.VALUE = logical(1)
          )),
          "Date and POSIXt variables are not supported, please select other variables"
        ),
        if (length(input_arm_var) == 2) {
          need(
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

    # Generate r code for the analysis.
    all_q <- reactive({
      validate_checks()

      summarize_vars <- map_merged(anl_selectors)$summarize_vars$variables
      var_labels <- teal.data::col_labels(data()[[dataname]][, summarize_vars, drop = FALSE])

      arm_var_labels <- NULL
      if (show_arm_var_labels) {
        arm_vars <- map_merged(anl_selectors)$arm_var$variables
        arm_var_labels <- teal.data::col_labels(data()[[dataname]][, arm_vars, drop = FALSE], fill = TRUE)
      }

      my_calls <- template_summary(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = map_merged(anl_selectors)$arm_var$variables,
        sum_vars = summarize_vars,
        add_total = input$add_total,
        total_label = total_label,
        var_labels = var_labels,
        arm_var_labels = arm_var_labels,
        na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE),
        na_level = na_level,
        numeric_stats = input$numeric_stats,
        denominator = input$denominator,
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = basic_table_args
      )

      teal.code::eval_code(anl_q(), as.expression(unlist(my_calls)))
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

    teal.widgets::table_with_settings_srv(id = "table", table_r = table_r)

    decorated_table_q
  })
}
