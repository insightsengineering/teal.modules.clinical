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
                                  base_var = lifecycle::deprecated(),
                                  baseline_var = "BNRIND",
                                  na.rm = FALSE, # nolint: object_name.
                                  na_level = default_na_str(),
                                  add_total = FALSE,
                                  total_label = default_total_label(),
                                  basic_table_args = teal.widgets::basic_table_args()) {
  if (lifecycle::is_present(base_var)) {
    baseline_var <- base_var
    warning(
      "The `base_var` argument of `template_shift_by_arm()` is deprecated as of teal.modules.clinical 0.8.16. ",
      "Please use the `baseline_var` argument instead.",
      call. = FALSE
    )
  }

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
      expr = parentname <- df_explicit_na(parentname, na_level = na_str),
      env = list(parentname = as.name(parentname), na_str = na_level)
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      expr = dataname <- df_explicit_na(dataname, na_level = na_str) %>%
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
          rtables::split_cols_by(visit_var, split_fun = drop_split_levels) %>% # temp solution for over arching column
          rtables::split_cols_by(aval_var) %>%
          rtables::split_rows_by(
            arm_var,
            split_fun = add_overall_level(total_label, first = FALSE),
            label_pos = "topleft",
            split_label = obj_label(dataname$arm_var)
          ) %>%
          add_rowcounts() %>%
          analyze_vars(
            baseline_var,
            denom = "N_row",
            na_str = na_str,
            na.rm = na.rm,
            .stats = "count_fraction"
          ) %>%
          append_varlabels(dataname, baseline_var, indent = 1L),
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
          rtables::split_cols_by(visit_var, split_fun = drop_split_levels) %>% # temp solution for over arching column
          rtables::split_cols_by(aval_var) %>%
          rtables::split_rows_by(
            arm_var,
            split_fun = drop_split_levels,
            label_pos = "topleft",
            split_label = obj_label(dataname$arm_var)
          ) %>%
          add_rowcounts() %>%
          analyze_vars(
            baseline_var,
            denom = "N_row",
            na_str = na_str,
            na.rm = na.rm,
            .stats = "count_fraction"
          ) %>%
          append_varlabels(dataname, baseline_var, indent = 1L),
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
#' ADSL <- data[["ADSL"]]
#' ADEG <- data[["ADEG"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_shift_by_arm(
#'       label = "Shift by Arm Table",
#'       dataname = "ADEG",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(ADEG, "PARAMCD"),
#'         selected = "HR"
#'       ),
#'       visit_var = choices_selected(
#'         value_choices(ADEG, "AVISIT"),
#'         selected = "POST-BASELINE MINIMUM"
#'       ),
#'       aval_var = choices_selected(
#'         variable_choices(ADEG, subset = "ANRIND"),
#'         selected = "ANRIND",
#'         fixed = TRUE
#'       ),
#'       baseline_var = choices_selected(
#'         variable_choices(ADEG, subset = "BNRIND"),
#'         selected = "BNRIND",
#'         fixed = TRUE
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
tm_t_shift_by_arm <- function(label,
                              dataname,
                              parentname = ifelse(
                                inherits(arm_var, "data_extract_spec"),
                                teal.transform::datanames_input(arm_var),
                                "ADSL"
                              ),
                              arm_var,
                              paramcd,
                              visit_var,
                              aval_var,
                              base_var = lifecycle::deprecated(),
                              baseline_var,
                              treatment_flag_var = teal.transform::choices_selected(
                                teal.transform::variable_choices(dataname, subset = "ONTRTFL"),
                                selected = "ONTRTFL"
                              ),
                              treatment_flag = teal.transform::choices_selected("Y"),
                              useNA = c("ifany", "no"), # nolint: object_name.
                              na_level = default_na_str(),
                              add_total = FALSE,
                              total_label = default_total_label(),
                              pre_output = NULL,
                              post_output = NULL,
                              basic_table_args = teal.widgets::basic_table_args(),
                              transformators = list(),
                              decorators = list()) {
  if (lifecycle::is_present(base_var)) {
    baseline_var <- base_var
    warning(
      "The `base_var` argument of `tm_t_shift_by_arm()` is deprecated as of teal.modules.clinical 0.8.16. ",
      "Please use the `baseline_var` argument instead.",
      call. = FALSE
    )
  } else {
    base_var <- baseline_var # resolves missing argument error
  }

  message("Initializing tm_t_shift_by_arm")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  useNA <- match.arg(useNA) # nolint: object_name.
  checkmate::assert_string(na_level)
  checkmate::assert_string(total_label)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(visit_var, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(baseline_var, "choices_selected")
  checkmate::assert_class(treatment_flag_var, "choices_selected")
  checkmate::assert_class(treatment_flag, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    visit_var = cs_to_des_filter(visit_var, dataname = dataname),
    treatment_flag_var = cs_to_des_select(treatment_flag_var, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    baseline_var = cs_to_des_select(baseline_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_shift_by_arm,
    ui = ui_shift_by_arm,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        total_label = total_label,
        na_level = na_level,
        treatment_flag = treatment_flag,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_shift_by_arm <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$id_var,
    a$arm_var,
    a$paramcd,
    a$visit_var,
    a$treatment_flag_var,
    a$treatment_flag,
    a$aval_var,
    a$baseline_var
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      ### Reporter
      teal.reporter::add_card_button_ui(ns("add_reporter"), label = "Add Report Card"),
      tags$br(), tags$br(),
      ###
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(a[c(
        "arm_var", "paramcd_var", "paramcd", "aval_var", "baseline_var", "visit_var", "treamtment_flag_var"
      )]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("visit_var"),
        label = "Select Visit",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Select Analysis Range Indicator Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("baseline_var"),
        label = "Select Baseline Reference Range Indicator Variable",
        data_extract_spec = a$baseline_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients row", value = a$add_total),
      radioButtons(
        ns("useNA"),
        label = "Display NA counts",
        choices = c("ifany", "no"),
        selected = a$useNA
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          teal.transform::data_extract_ui(
            id = ns("treatment_flag_var"),
            label = "On Treatment Flag Variable",
            data_extract_spec = a$treatment_flag_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.widgets::optionalSelectInput(
            ns("treatment_flag"),
            label = "Value Indicating On Treatment",
            multiple = FALSE,
            fixed_on_single = TRUE
          )
        )
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_shift_by_arm <- function(id,
                             data,
                             reporter,
                             filter_panel_api,
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
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        visit_var = visit_var,
        aval_var = aval_var,
        baseline_var = baseline_var,
        treatment_flag_var = treatment_flag_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis range indicator required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required"),
        treatment_flag_var = shinyvalidate::sv_required("An on treatment flag variable is required"),
        baseline_var = shinyvalidate::sv_required("A baseline reference range indicator is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("An endpoint is required"),
        visit_var = shinyvalidate::sv_required("A visit is required")
      )
    )

    isolate({
      resolved <- teal.transform::resolve_delayed(treatment_flag, as.list(data()))
      teal.widgets::updateOptionalSelectInput(
        session = session,
        inputId = "treatment_flag",
        choices = resolved$choices,
        selected = resolved$selected
      )
    })

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule(
        "treatment_flag",
        shinyvalidate::sv_required("An indicator value for on treatment records is required")
      )
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      data() %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    # Validate inputs.
    validate_checks <- reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_aval_var <- names(merged$anl_input_r()$columns_source$aval_var)
      input_baseline_var <- names(merged$anl_input_r()$columns_source$baseline_var)
      input_treatment_flag_var <- names(merged$anl_input_r()$columns_source$treatment_flag_var)

      validate(
        need(
          nrow(merged$anl_q()[["ANL"]]) > 0,
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
        arm_var = names(merged$anl_input_r()$columns_source$arm_var),
        paramcd = unlist(merged$anl_input_r()$filter)["vars_selected"],
        treatment_flag_var = names(merged$anl_input_r()$columns_source$treatment_flag_var),
        treatment_flag = input$treatment_flag,
        aval_var = names(merged$anl_input_r()$columns_source$aval_var),
        baseline_var = names(merged$anl_input_r()$columns_source$baseline_var),
        na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE),
        na_level = na_level,
        add_total = input$add_total,
        total_label = total_label,
        basic_table_args = basic_table_args
      )

      teal.code::eval_code(merged$anl_q(), as.expression(unlist(my_calls)))
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

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_table_q())))
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Shift by Arm Table",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(source_code_r())
        card
      }
      teal.reporter::add_card_button_srv("add_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
