#' Template: Shift by Arm
#'
#' Creates a valid expression to generate a summary table of analysis indicator levels by arm.
#'
#' @inheritParams template_arguments
#' @param aval_var (`character`)\cr name of the analysis reference range indicator variable.
#' @param baseline_var (`character`)\cr name of the baseline reference range indicator variable.
#' @param add_total (`logical`)\cr whether to include row with total number of patients.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_shift_by_arm()]
#'
#' @keywords internal
template_shift_by_arm <- function(dataname,
                                  parentname,
                                  arm_var = "ARM",
                                  paramcd = "PARAMCD",
                                  visit_var = "AVISIT",
                                  treatment_flag_var = "ONTRTFL",
                                  treatment_flag = "Y",
                                  aval_var = "ANRIND",
                                  baseline_var = "BNRIND",
                                  na.rm = FALSE, # nolint: object_name.
                                  na_level = tern::default_na_str(),
                                  add_total = FALSE,
                                  total_label = default_total_label(),
                                  basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(visit_var)
  checkmate::assert_string(paramcd, na.ok = TRUE)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(baseline_var)
  checkmate::assert_flag(na.rm)
  checkmate::assert_string(na_level)
  checkmate::assert_string(treatment_flag_var)
  checkmate::assert_string(treatment_flag)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)

  y <- list()

  # Start data steps.
  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- tern::df_explicit_na(parentname, na_level = na_str),
      env = list(parentname = as.name(parentname), na_str = na_level)
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      expr = dataname <- tern::df_explicit_na(dataname, na_level = na_str) %>%
        dplyr::filter(treatment_flag_var == treatment_flag),
      env = list(
        dataname = as.name(dataname),
        na_str = na_level,
        treatment_flag_var = as.name(treatment_flag_var),
        treatment_flag = treatment_flag
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = attr(dataname$baseline_var, "label") <- "Baseline Assessment",
      env = list(dataname = as.name(dataname), baseline_var = baseline_var)
    )
  )

  y$data <- bracket_expr(data_list)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args
    )
  )

  # Start layout steps.
  layout_list <- list()

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(visit_var,
            split_fun = rtables::drop_split_levels
          ) %>% # temp solution for over arching column
          rtables::split_cols_by(aval_var) %>%
          rtables::split_rows_by(
            arm_var,
            split_fun = rtables::add_overall_level(total_label, first = FALSE),
            label_pos = "topleft",
            split_label = rtables::obj_label(dataname$arm_var)
          ) %>%
          tern::add_rowcounts() %>%
          tern::analyze_vars(
            baseline_var,
            denom = "N_row",
            na_str = na_str,
            na.rm = na.rm,
            .stats = "count_fraction"
          ) %>%
          tern::append_varlabels(dataname, baseline_var, indent = 1L),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          baseline_var = baseline_var,
          dataname = as.name(dataname),
          visit_var = visit_var,
          na.rm = na.rm,
          na_str = na_level,
          total_label = total_label,
          expr_basic_table_args = parsed_basic_table_args
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(visit_var,
            split_fun = rtables::drop_split_levels
          ) %>% # temp solution for over arching column
          rtables::split_cols_by(aval_var) %>%
          rtables::split_rows_by(
            arm_var,
            split_fun = rtables::drop_split_levels,
            label_pos = "topleft",
            split_label = rtables::obj_label(dataname$arm_var)
          ) %>%
          tern::add_rowcounts() %>%
          tern::analyze_vars(
            baseline_var,
            denom = "N_row",
            na_str = na_str,
            na.rm = na.rm,
            .stats = "count_fraction"
          ) %>%
          tern::append_varlabels(dataname, baseline_var, indent = 1L),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          baseline_var = baseline_var,
          dataname = as.name(dataname),
          visit_var = visit_var,
          na.rm = na.rm,
          na_str = na_level,
          expr_basic_table_args = parsed_basic_table_args
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
    expr = {
      table <- rtables::build_table(lyt = lyt, df = dataname)
    },
    env = list(dataname = as.name(dataname))
  )

  y
}

#' teal Module: Shift by Arm
#'
#' This module produces a summary table of analysis indicator levels by arm.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_shift_by_arm
#' @inheritParams template_arguments
#' @param arm_var ([teal.picks::variables()])\cr
#'   variable for treatment arm.
#' @param paramcd ([teal.picks::variables()])\cr
#'   variable for lab parameter code. The `values()` element is added internally to allow
#'   users to filter the parameter values interactively.
#' @param visit_var ([teal.picks::variables()])\cr
#'   variable for analysis visit. The `values()` element is added internally to allow
#'   users to filter the visit values interactively.
#' @param aval_var ([teal.picks::variables()])\cr
#'   variable for analysis range indicator.
#' @param baseline_var ([teal.picks::variables()])\cr
#'   variable for baseline reference range indicator.
#' @param treatment_flag_var ([teal.picks::variables()])\cr
#'   variable for on-treatment flag.
#' @param treatment_flag ([teal.transform::choices_selected()])\cr
#'   value(s) indicating on-treatment records.
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
#' tm_t_shift_by_arm(
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
#'   ADEG <- tmc_ex_adeg
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_shift_by_arm(
#'       label = "Shift by Arm Table",
#'       dataname = "ADEG",
#'       arm_var = variables(choices = c("ARM", "ARMCD")),
#'       paramcd = variables(choices = "PARAMCD"),
#'       visit_var = variables(choices = "AVISIT"),
#'       aval_var = variables(choices = "ANRIND"),
#'       baseline_var = variables(choices = "BNRIND"),
#'       useNA = "ifany"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_shift_by_arm <- function(label,
                              dataname,
                              parentname = "ADSL",
                              arm_var = variables(choices = c("ARM", "ARMCD")),
                              paramcd = variables(choices = "PARAMCD"),
                              visit_var = variables(choices = "AVISIT"),
                              aval_var = variables(choices = "ANRIND"),
                              base_var = lifecycle::deprecated(),
                              baseline_var = variables(choices = "BNRIND"),
                              treatment_flag_var = variables(choices = "ONTRTFL"),
                              treatment_flag = teal.transform::choices_selected(
                                c("Y", "N"),
                                selected = "Y", fixed = TRUE
                              ),
                              useNA = c("ifany", "no"), # nolint: object_name.
                              na_level = tern::default_na_str(),
                              add_total = FALSE,
                              total_label = default_total_label(),
                              pre_output = NULL,
                              post_output = NULL,
                              basic_table_args = teal.widgets::basic_table_args(),
                              transformators = list(),
                              decorators = list()) {
  if (lifecycle::is_present(base_var)) {
    lifecycle::deprecate_stop(
      when = "0.8.16",
      what = "tm_t_shift_by_arm(base_var)",
      details = "Please use the `baseline_var` argument instead."
    )
    baseline_var <- base_var
  }

  message("Initializing tm_t_shift_by_arm")
  arm_var <- teal.picks::as.picks(arm_var, quiet = FALSE)
  paramcd <- teal.picks::as.picks(paramcd, quiet = FALSE)
  visit_var <- teal.picks::as.picks(visit_var, quiet = FALSE)
  aval_var <- teal.picks::as.picks(aval_var, quiet = FALSE)
  baseline_var <- teal.picks::as.picks(baseline_var, quiet = FALSE)
  treatment_flag_var <- teal.picks::as.picks(treatment_flag_var, quiet = FALSE)
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  useNA <- match.arg(useNA) # nolint: object_name.
  checkmate::assert_string(na_level)
  checkmate::assert_string(total_label)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(paramcd, "variables")
  checkmate::assert_class(visit_var, "variables")
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(baseline_var, "variables")
  checkmate::assert_class(treatment_flag_var, "variables")
  checkmate::assert_class(treatment_flag, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  arm_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), arm_var)
  paramcd <- teal.picks::picks(teal.picks::datasets(dataname, dataname), paramcd, values())
  visit_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), visit_var, values())
  aval_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), aval_var)
  baseline_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), baseline_var)
  treatment_flag_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), treatment_flag_var)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_shift_by_arm,
    ui = ui_shift_by_arm,
    ui_args = args[names(args) %in% names(formals(ui_shift_by_arm))],
    server_args = args[names(args) %in% names(formals(srv_shift_by_arm))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_shift_by_arm <- function(id,
                            arm_var,
                            paramcd,
                            visit_var,
                            aval_var,
                            baseline_var,
                            treatment_flag_var,
                            treatment_flag,
                            add_total,
                            useNA, # nolint: object_name.
                            pre_output,
                            post_output,
                            decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Treatment Variable"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      ),
      tags$div(
        tags$label("Select Endpoint"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Select Visit"),
        teal.picks::picks_ui(ns("visit_var"), visit_var)
      ),
      tags$div(
        tags$label("Select Analysis Range Indicator Variable"),
        teal.picks::picks_ui(ns("aval_var"), aval_var)
      ),
      tags$div(
        tags$label("Select Baseline Reference Range Indicator Variable"),
        teal.picks::picks_ui(ns("baseline_var"), baseline_var)
      ),
      checkboxInput(ns("add_total"), "Add All Patients row", value = add_total),
      radioButtons(
        ns("useNA"),
        label = "Display NA counts",
        choices = c("ifany", "no"),
        selected = useNA
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          tags$div(
            tags$label("On Treatment Flag Variable"),
            teal.picks::picks_ui(ns("treatment_flag_var"), treatment_flag_var)
          ),
          teal.widgets::optionalSelectInput(
            ns("treatment_flag"),
            label = "Value Indicating On Treatment",
            choices = treatment_flag$choices,
            selected = treatment_flag$selected,
            multiple = FALSE,
            fixed = treatment_flag$fixed
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_shift_by_arm <- function(id,
                             data,
                             dataname,
                             parentname,
                             arm_var,
                             paramcd,
                             visit_var,
                             treatment_flag_var,
                             treatment_flag,
                             aval_var,
                             baseline_var,
                             label,
                             na_level,
                             add_total,
                             total_label,
                             basic_table_args,
                             decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        arm_var = arm_var,
        paramcd = paramcd,
        visit_var = visit_var,
        aval_var = aval_var,
        baseline_var = baseline_var,
        treatment_flag_var = treatment_flag_var
      ),
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
    merged_anl <- merge_srv("merge_anl", data = data_with_card, selectors = anl_selectors, output_name = "ANL")
    merged_adsl_anl <- merge_srv(
      "merge_adsl_anl",
      data = merged_anl$data,
      selectors = adsl_selectors,
      output_name = "ANL_ADSL"
    )
    anl_q <- merged_adsl_anl$data

    # Validate inputs.
    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- anl_selectors$arm_var()$variables$selected
      input_aval_var <- anl_selectors$aval_var()$variables$selected
      input_baseline_var <- anl_selectors$baseline_var()$variables$selected

      validate(
        need(
          nrow(anl_q()[["ANL"]]) > 0,
          paste0(
            "Please make sure the analysis dataset is not empty or\n",
            "endpoint parameter and analysis visit are selected."
          )
        )
      )

      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_aval_var, input_baseline_var),
        arm_var = input_arm_var
      )
    })

    # Generate r code for the analysis.
    all_q <- reactive({
      validate_checks()

      my_calls <- template_shift_by_arm(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = anl_selectors$arm_var()$variables$selected,
        paramcd = anl_selectors$paramcd()$variables$selected,
        visit_var = anl_selectors$visit_var()$variables$selected,
        treatment_flag_var = anl_selectors$treatment_flag_var()$variables$selected,
        treatment_flag = input$treatment_flag,
        aval_var = anl_selectors$aval_var()$variables$selected,
        baseline_var = anl_selectors$baseline_var()$variables$selected,
        na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE),
        na_level = na_level,
        add_total = input$add_total,
        total_label = total_label,
        basic_table_args = basic_table_args
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Decoration of table output.
    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
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
