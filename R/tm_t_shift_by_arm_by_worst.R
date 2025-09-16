#' Template: Shift by Arm by Worst Analysis Indicator Level
#'
#' Creates a valid expression to generate a summary table of worst analysis indicator variable level per subject by arm.
#'
#' @inheritParams template_shift_by_arm
#' @inheritParams template_arguments
#' @param worst_flag (`character`)\cr value indicating worst analysis indicator level.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_shift_by_arm()]
#'
#' @keywords internal
template_shift_by_arm_by_worst <- function(dataname,
                                           parentname,
                                           arm_var = "ARM",
                                           paramcd = "PARAMCD",
                                           worst_flag_var = "WORS02FL",
                                           worst_flag = "Y",
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
  checkmate::assert_character(worst_flag_var, null.ok = TRUE)
  checkmate::assert_string(paramcd)
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
        dplyr::filter(treatment_flag_var == treatment_flag, worst_flag_var == worst_flag) %>%
        dplyr::mutate(postbaseline_label = "Post-Baseline"),
      env = list(
        dataname = as.name(dataname),
        na_str = na_level,
        treatment_flag_var = as.name(treatment_flag_var),
        treatment_flag = treatment_flag,
        worst_flag_var = as.name(worst_flag_var),
        worst_flag = worst_flag
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

  basic_table_args$title <- "Shift by Arm by Worst Table"

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
          rtables::split_cols_by("postbaseline_label", split_fun = rtables::drop_split_levels) %>%
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
          rtables::split_cols_by("postbaseline_label", split_fun = rtables::drop_split_levels) %>%
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

#' teal Module: Shift by Arm by Worst Analysis Indicator Level
#'
#' This module produces a summary table of worst analysis indicator variable level per subject by arm.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_shift_by_arm_by_worst
#' @inheritParams template_arguments
#'
#' @inherit module_arguments return
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
#' tm_t_shift_by_arm_by_worst(
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
#' ADSL <- data[["ADSL"]]
#' ADEG <- data[["ADEG"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_shift_by_arm_by_worst(
#'       label = "Shift by Arm Table",
#'       dataname = "ADEG",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(ADEG, "PARAMCD"),
#'         selected = "ECGINTP"
#'       ),
#'       worst_flag_var = choices_selected(
#'         variable_choices(ADEG, c("WORS02FL", "WORS01FL")),
#'         selected = "WORS02FL"
#'       ),
#'       worst_flag = choices_selected(
#'         value_choices(ADEG, "WORS02FL"),
#'         selected = "Y",
#'         fixed = TRUE
#'       ),
#'       aval_var = choices_selected(
#'         variable_choices(ADEG, c("AVALC", "ANRIND")),
#'         selected = "AVALC"
#'       ),
#'       baseline_var = choices_selected(
#'         variable_choices(ADEG, c("BASEC", "BNRIND")),
#'         selected = "BASEC"
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
tm_t_shift_by_arm_by_worst <- function(label,
                                       dataname,
                                       parentname = ifelse(
                                         inherits(arm_var, "data_extract_spec"),
                                         teal.transform::datanames_input(arm_var),
                                         "ADSL"
                                       ),
                                       arm_var,
                                       paramcd,
                                       aval_var,
                                       base_var = lifecycle::deprecated(),
                                       baseline_var,
                                       worst_flag_var,
                                       worst_flag,
                                       treatment_flag_var = teal.transform::choices_selected(
                                         choices = teal.transform::variable_choices(dataname, subset = "ONTRTFL"),
                                         selected = "ONTRTFL"
                                       ),
                                       treatment_flag = teal.transform::choices_selected("Y"),
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
      what = "tm_t_shift_by_arm_by_worst(base_var)",
      details = "Please use the `baseline_var` argument instead."
    )
  }

  message("Initializing tm_t_shift_by_arm_by_worst")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  useNA <- match.arg(useNA) # nolint: object_name.
  checkmate::assert_string(na_level)
  checkmate::assert_string(total_label)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(baseline_var, "choices_selected")
  checkmate::assert_class(worst_flag_var, "choices_selected")
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
    treatment_flag_var = cs_to_des_select(treatment_flag_var, dataname = dataname),
    worst_flag_var = cs_to_des_select(worst_flag_var, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    baseline_var = cs_to_des_select(baseline_var, dataname = dataname)
  )


  module(
    label = label,
    server = srv_shift_by_arm_by_worst,
    ui = ui_shift_by_arm_by_worst,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        treatment_flag = treatment_flag,
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
ui_shift_by_arm_by_worst <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$id_var,
    a$arm_var,
    a$paramcd,
    a$worst_flag_var,
    a$treatment_flag_var,
    a$treatment_flag,
    a$aval_var,
    a$baseline_var
  )
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(a[c(
        "arm_var", "paramcd_var", "paramcd", "aval_var",
        "baseline_var", "worst_flag_var", "worst_flag", "treamtment_flag_var"
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
        id = ns("worst_flag_var"),
        label = "Select The worst flag",
        data_extract_spec = a$worst_flag_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::optionalSelectInput(
        ns("worst_flag"),
        "Value of worst flag",
        a$worst_flag$choices,
        a$worst_flag$selected,
        multiple = FALSE,
        fixed = a$worst_flag$fixed
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Select Analysis Value",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("baseline_var"),
        label = "Select Baseline Value",
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
            inputId = ns("treatment_flag"),
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
srv_shift_by_arm_by_worst <- function(id,
                                      data,
                                      dataname,
                                      parentname,
                                      arm_var,
                                      paramcd,
                                      treatment_flag_var,
                                      treatment_flag,
                                      worst_flag_var,
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
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        treatment_flag_var = treatment_flag_var,
        worst_flag_var = worst_flag_var,
        aval_var = aval_var,
        baseline_var = baseline_var,
        paramcd = paramcd
      ),
      datasets = data,
      select_validation_rule = list(
        arm_var = shinyvalidate::sv_required("A treatment variable is required"),
        treatment_flag_var = shinyvalidate::sv_required("A treatment flag variable is required"),
        worst_flag_var = shinyvalidate::sv_required("A worst flag variable is required"),
        aval_var = shinyvalidate::sv_required("An analysis range indicator required"),
        baseline_var = shinyvalidate::sv_required("A baseline reference range indicator is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("An endpoint is required")
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
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Shift by Arm by Worst Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
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

    # validate inputs
    validate_checks <- reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_aval_var <- names(merged$anl_input_r()$columns_source$aval_var)
      input_baseline_var <- names(merged$anl_input_r()$columns_source$baseline_var)

      validate(
        need(
          nrow(merged$anl_q()[["ANL"]]) > 0,
          paste0(
            "Please make sure the analysis dataset is not empty or\n",
            "endpoint parameter and analysis visit are selected."
          )
        ),
        need(
          length(unique(merged$anl_q()[["ANL"]][[input_aval_var]])) < 50,
          paste(
            "There are too many values of", input_aval_var, "for the selected endpoint.",
            "Please select either a different endpoint or a different analysis value."
          )
        ),
        need(
          length(unique(merged$anl_q()[["ANL"]][[input_baseline_var]])) < 50,
          paste(
            "There are too many values of", input_baseline_var, "for the selected endpoint.",
            "Please select either a different endpoint or a different baseline value."
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

      my_calls <- template_shift_by_arm_by_worst(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = names(merged$anl_input_r()$columns_source$arm_var),
        paramcd = unlist(paramcd$filter)["vars_selected"],
        worst_flag_var = names(merged$anl_input_r()$columns_source$worst_flag_var),
        worst_flag = input$worst_flag,
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

      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Decoration of table output
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

    decorated_table_q
  })
}
