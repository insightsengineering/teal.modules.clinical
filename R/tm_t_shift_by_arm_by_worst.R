#' Template: Shift by Arm
#'
#' @inheritParams template_arguments
#' @param aval_var (`character`)\cr the variable name for the analysis reference range indicator.
#' @param base_var (`character`)\cr the variable name for the baseline reference range indicator.
#' @param add_total (`logical`)\cr whether to include row with total number of patients.
#' @param worst_flag_var  (`character`)\cr Worst flag variable.
#' @param worst_flag (`character`)\cr Worst flag value.
#'
#' @seealso [tm_t_shift_by_arm()]
#' @keywords internal
#'
template_shift_by_arm_by_worst <- function(dataname,
                                           parentname,
                                           arm_var = "ARM",
                                           paramcd = "PARAMCD",
                                           worst_flag_var = "WORS02FL",
                                           worst_flag = "Y",
                                           treatment_flag_var = "ONTRTFL",
                                           treatment_flag = "Y",
                                           aval_var = "ANRIND",
                                           base_var = "BNRIND",
                                           na.rm = FALSE, # nolint
                                           na_level = "<Missing>",
                                           add_total = FALSE,
                                           total_label = "All Patients",
                                           basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    is.null(worst_flag_var) || is.character(worst_flag_var),
    assertthat::is.string(paramcd),
    assertthat::is.string(aval_var),
    assertthat::is.string(base_var),
    assertthat::is.flag(na.rm),
    assertthat::is.string(na_level),
    assertthat::is.string(treatment_flag_var),
    assertthat::is.string(treatment_flag),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label)
  )

  y <- list()

  # Start data steps.
  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- df_explicit_na(parentname, na_level = na_level),
      env = list(parentname = as.name(parentname), na_level = na_level)
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = dataname <- df_explicit_na(dataname, na_level = na_level) %>%
        dplyr::filter(treatment_flag_var == treatment_flag, worst_flag_var == worst_flag) %>%
        dplyr::mutate(postbaseline_label = "Post-Baseline"),
      env = list(
        dataname = as.name(dataname),
        na_level = na_level,
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
      expr = attr(dataname$base_var, "label") <- "Baseline Assessment",
      env = list(dataname = as.name(dataname), base_var = base_var)
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
          rtables::split_cols_by("postbaseline_label", split_fun = drop_split_levels) %>%
          rtables::split_cols_by(aval_var) %>%
          rtables::split_rows_by(
            arm_var,
            split_fun = add_overall_level(total_label, first = FALSE),
            label_pos = "topleft",
            split_label = obj_label(dataname$arm_var)
          ) %>%
          add_rowcounts() %>%
          summarize_vars(base_var, denom = "N_row", na_level = na_level, na.rm = na.rm, .stats = "count_fraction") %>%
          append_varlabels(dataname, base_var, indent = 1L),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          base_var = base_var,
          dataname = as.name(dataname),
          na.rm = na.rm,
          na_level = na_level,
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
          rtables::split_cols_by("postbaseline_label", split_fun = drop_split_levels) %>%
          rtables::split_cols_by(aval_var) %>%
          rtables::split_rows_by(
            arm_var,
            split_fun = drop_split_levels,
            label_pos = "topleft",
            split_label = obj_label(dataname$arm_var)
          ) %>%
          add_rowcounts() %>%
          summarize_vars(base_var, denom = "N_row", na_level = na_level, na.rm = na.rm, .stats = "count_fraction") %>%
          append_varlabels(dataname, base_var, indent = 1L),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          base_var = base_var,
          dataname = as.name(dataname),
          na.rm = na.rm,
          na_level = na_level,
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
      result <- rtables::build_table(lyt = lyt, df = dataname)
      result
    },
    env = list(dataname = as.name(dataname))
  )

  y
}

#' Teal Module: Shift by Arm by Worst
#'
#' @inheritParams module_arguments
#' @inheritParams template_shift_by_arm_by_worst
#' @param add_total (`logical`)\cr
#'   whether to include row with total number of patients.
#'
#' @export
#'
#' @examples
#' adsl <- tmc_ex_adsl
#' adeg <- tmc_ex_adeg
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADEG", adeg)
#'   ),
#'   modules = modules(
#'     tm_t_shift_by_arm_by_worst(
#'       label = "Shift by Arm Table",
#'       dataname = "ADEG",
#'       arm_var = choices_selected(
#'         variable_choices(adsl, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(adeg, "PARAMCD"),
#'         selected = "ECGINTP"
#'       ),
#'       worst_flag_var = choices_selected(
#'         variable_choices(adeg, c("WORS02FL", "WORS01FL")),
#'         selected = "WORS02FL"
#'       ),
#'       worst_flag = choices_selected(
#'         value_choices(adeg, "WORS02FL"),
#'         selected = "Y", fixed = TRUE
#'       ),
#'       aval_var = choices_selected(
#'         variable_choices(adeg, c("AVALC", "ANRIND")),
#'         selected = "AVALC"
#'       ),
#'       base_var = choices_selected(
#'         variable_choices(adeg, c("BASEC", "BNRIND")),
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
                                       base_var,
                                       worst_flag_var,
                                       worst_flag,
                                       treatment_flag_var = teal.transform::choices_selected(
                                         teal.transform::variable_choices(dataname, subset = "ONTRTFL"),
                                         selected = "ONTRTFL", fixed = TRUE
                                       ),
                                       treatment_flag = teal.transform::choices_selected(
                                         teal.transform::value_choices(dataname, "ONTRTFL"),
                                         selected = "Y", fixed = TRUE
                                       ),
                                       useNA = c("ifany", "no"), # nolint
                                       na_level = "<Missing>",
                                       add_total = FALSE,
                                       total_label = "All Patients",
                                       pre_output = NULL,
                                       post_output = NULL,
                                       basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_shift_by_arm_by_worst")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  useNA <- match.arg(useNA) # nolint
  checkmate::assert_string(na_level)
  checkmate::assert_string(total_label)
  checkmate::assert_class(treatment_flag, "choices_selected")
  checkmate::assert_class(treatment_flag_var, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())


  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    treatment_flag_var = cs_to_des_select(treatment_flag_var, dataname = dataname),
    worst_flag_var = cs_to_des_select(worst_flag_var, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    base_var = cs_to_des_select(base_var, dataname = dataname)
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
        total_label = total_label,
        na_level = na_level,
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_shift_by_arm_by_worst <- function(id, ...) {
  ns <- shiny::NS(id)
  a <- list(...)

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$id_var,
    a$arm_var,
    a$paramcd,
    a$worst_flag_var,
    a$treatment_flag_var,
    a$treatment_flag,
    a$aval_var,
    a$base_var
  )
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c(
        "arm_var", "paramcd_var", "paramcd", "aval_var",
        "base_var", "worst_flag_var", "worst_flag", "treamtment_flag_var"
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
        id = ns("base_var"),
        label = "Select Baseline Value",
        data_extract_spec = a$base_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::checkboxInput(ns("add_total"), "Add All Patients row", value = a$add_total),
      shiny::radioButtons(
        ns("useNA"),
        label = "Display NA counts",
        choices = c("ifany", "no"),
        selected = a$useNA
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional Variables Info",
          teal.transform::data_extract_ui(
            id = ns("treatment_flag_var"),
            label = "On Treatment Flag Variable",
            data_extract_spec = a$treatment_flag_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.widgets::optionalSelectInput(
            ns("treatment_flag"),
            "Value Indicating On Treatment",
            a$treatment_flag$choices,
            a$treatment_flag$selected,
            multiple = FALSE,
            fixed = a$treatment_flag$fixed
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
srv_shift_by_arm_by_worst <- function(id,
                                      data,
                                      reporter,
                                      filter_panel_api,
                                      dataname,
                                      parentname,
                                      arm_var,
                                      paramcd,
                                      treatment_flag_var,
                                      worst_flag_var,
                                      aval_var,
                                      base_var,
                                      label,
                                      na_level,
                                      add_total,
                                      total_label,
                                      basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")
  shiny::moduleServer(id, function(input, output, session) {
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        treatment_flag_var = treatment_flag_var,
        worst_flag_var = worst_flag_var,
        aval_var = aval_var,
        base_var = base_var,
        paramcd = paramcd
      ),
      datasets = data,
      select_validation_rule = list(
        arm_var = shinyvalidate::sv_required("A treatment variable is required"),
        treatment_flag_var = shinyvalidate::sv_required("A treatment flag variable is required"),
        worst_flag_var = shinyvalidate::sv_required("A worst flag variable is required"),
        aval_var = shinyvalidate::sv_required("An analysis range indicator required"),
        base_var = shinyvalidate::sv_required("A baseline reference range indicator is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("An endpoint is required")
      )
    )

    iv_r <- shiny::reactive({
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
      join_keys = get_join_keys(data),
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
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

    # validate inputs
    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_aval_var <- names(merged$anl_input_r()$columns_source$aval_var)
      input_base_var <- names(merged$anl_input_r()$columns_source$base_var)

      shiny::validate(
        shiny::need(
          nrow(merged$anl_q()[["ANL"]]) > 0,
          paste0(
            "Please make sure the analysis dataset is not empty or\n",
            "endpoint parameter and analysis visit are selected."
          )
        ),
        shiny::need(
          length(unique(merged$anl_q()[["ANL"]][[input_aval_var]])) < 50,
          paste(
            "There are too many values of", input_aval_var, "for the selected endpoint.",
            "Please select either a different endpoint or a different analysis value."
          )
        ),
        shiny::need(
          length(unique(merged$anl_q()[["ANL"]][[input_base_var]])) < 50,
          paste(
            "There are too many values of", input_base_var, "for the selected endpoint.",
            "Please select either a different endpoint or a different baseline value."
          )
        )
      )

      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_aval_var, input_base_var),
        arm_var = input_arm_var
      )
    })

    # generate r code for the analysis
    all_q <- shiny::reactive({
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
        base_var = names(merged$anl_input_r()$columns_source$base_var),
        na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE), # nolint
        na_level = na_level,
        add_total = input$add_total,
        total_label = total_label,
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
        card$set_name("Shift by Arm by Worst Table")
        card$append_text("Shift by Arm by Worst Table", "header2")
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
