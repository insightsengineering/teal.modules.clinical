#' Template: Event rates adjusted for patient-years
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis.
#' @param events_var (`integer`)\cr number of observed events.
#' @param label_paramcd (`string`)\cr title of table based on `paramcd`
#'
#' @seealso [tm_t_events_patyear()]
#' @keywords internal
#'
template_events_patyear <- function(dataname,
                                    parentname,
                                    arm_var,
                                    events_var,
                                    label_paramcd,
                                    aval_var = "AVAL",
                                    add_total = TRUE,
                                    total_label = "All Patients",
                                    control = control_incidence_rate(),
                                    drop_arm_levels = TRUE,
                                    basic_table_args = teal.widgets::basic_table_args()) {
  # initialize
  y <- list()
  # data
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
      arm_var = arm_var,
      drop_arm_levels = drop_arm_levels
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      dataname <- df_explicit_na(dataname, na_level = ""),
      env = list(dataname = as.name("anl"))
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- df_explicit_na(parentname, na_level = ""),
      env = list(parentname = as.name(parentname))
    )
  )

  y$data <- bracket_expr(data_list)

  # layout
  layout_list <- list()

  basic_title <- tools::toTitleCase(paste("Event Rates Adjusted for Patient-Years by", label_paramcd))
  basic_footer <- paste(
    "CI Method:",
    if (control$conf_type == "normal") {
      "Normal (rate)"
    } else if (control$conf_type == "normal_log") {
      "Normal (log rate)"
    } else if (control$conf_type == "exact") {
      "Exact"
    } else {
      "Byar's method"
    }
  )

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        title = basic_title,
        main_footer = basic_footer
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = expr_basic_table_args %>%
        rtables::split_cols_by(var = arm_var) %>%
        rtables::add_colcounts(),
      env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
    )
  )
  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_overall_col(label = total_label),
        env = list(total_label = total_label)
      )
    )
  }
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = estimate_incidence_rate(
        vars = aval_var,
        n_events = events_var,
        control = control_incidence_rate(
          conf_level = conf_level,
          conf_type = conf_type,
          input_time_unit = input_time_unit,
          num_pt_year = num_pt_year
        )
      ),
      env = list(
        aval_var = aval_var,
        events_var = events_var,
        conf_level = control$conf_level,
        conf_type = control$conf_type,
        input_time_unit = control$input_time_unit,
        num_pt_year = control$num_pt_year
      )
    )
  )
  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # table
  y$table <- substitute(
    expr = {
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      result
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' Teal module: Event rates adjusted for patient-years
#'
#' @inheritParams module_arguments
#' @param avalu_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all available choices and preselected option for the analysis unit variable.
#' @param events_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all event counts.
#'
#' @export
#' @examples
#' adsl <- tmc_ex_adsl
#' adaette <- tmc_ex_adaette %>%
#'   dplyr::filter(PARAMCD %in% c("AETTE1", "AETTE2", "AETTE3")) %>%
#'   dplyr::mutate(is_event = CNSR == 0) %>%
#'   dplyr::mutate(n_events = as.integer(is_event))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAETTE", adaette)
#'   ),
#'   modules = modules(
#'     tm_t_events_patyear(
#'       label = "AE Rate Adjusted for Patient-Years At Risk Table",
#'       dataname = "ADAETTE",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, c("ARM", "ARMCD")),
#'         selected = "ARMCD"
#'       ),
#'       add_total = TRUE,
#'       events_var = choices_selected(
#'         choices = variable_choices(adaette, "n_events"),
#'         selected = "n_events",
#'         fixed = TRUE
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(adaette, "PARAMCD", "PARAM"),
#'         selected = "AETTE1"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_events_patyear <- function(label,
                                dataname,
                                parentname = ifelse(
                                  inherits(arm_var, "data_extract_spec"),
                                  teal.transform::datanames_input(arm_var),
                                  "ADSL"
                                ),
                                arm_var,
                                events_var,
                                paramcd,
                                aval_var = teal.transform::choices_selected(
                                  teal.transform::variable_choices(dataname, "AVAL"), "AVAL",
                                  fixed = TRUE
                                ),
                                avalu_var = teal.transform::choices_selected(
                                  teal.transform::variable_choices(dataname, "AVALU"), "AVALU",
                                  fixed = TRUE
                                ),
                                add_total = TRUE,
                                total_label = "All Patients",
                                conf_level = teal.transform::choices_selected(
                                  c(0.95, 0.9, 0.8), 0.95,
                                  keep_order = TRUE
                                ),
                                drop_arm_levels = TRUE,
                                pre_output = NULL,
                                post_output = NULL,
                                basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_events_patyear")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(events_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(avalu_var, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    avalu_var = cs_to_des_select(avalu_var, dataname = dataname),
    events_var = cs_to_des_select(events_var, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_events_patyear,
    ui_args = c(data_extract_list, args),
    server = srv_events_patyear,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        total_label = total_label,
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_events_patyear <- function(id, ...) {
  ns <- shiny::NS(id)
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var, a$paramcd, a$aval_var, a$avalu_var, a$events_var
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("patyear_table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "aval_var", "avalu_var", "events_var")]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select an Event Type Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("events_var"),
        label = "Event Variable",
        data_extract_spec = a$events_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("avalu_var"),
        label = "Analysis Unit Variable",
        data_extract_spec = a$avalu_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      ),
      teal.widgets::optionalSelectInput(
        ns("conf_method"),
        "CI Method",
        choices = c("Normal (rate)", "Normal (log rate)", "Exact", "Byar's method"),
        selected = "Normal (rate)",
        multiple = FALSE,
        fixed = FALSE
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional table settings",
          shiny::checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          ),
          teal.widgets::optionalSelectInput(
            ns("num_pt_year"),
            "Time Unit for AE Rate (in Patient-Years)",
            choices = c(0.1, 1, 10, 100, 1000),
            selected = 100,
            multiple = FALSE,
            fixed = FALSE
          ),
          shiny::selectInput(
            ns("input_time_unit"),
            "Analysis Unit",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
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
srv_events_patyear <- function(id,
                               data,
                               reporter,
                               filter_panel_api,
                               dataname,
                               parentname,
                               arm_var,
                               paramcd,
                               aval_var,
                               avalu_var,
                               events_var,
                               add_total,
                               total_label,
                               drop_arm_levels,
                               label,
                               basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(anl_q(), {
      data <- merged$anl_q()[["ANL"]]
      aval_unit_var <- merged$anl_input_r()$columns_source$avalu_var
      if (length(aval_unit_var) > 0) {
        choices <- stats::na.omit(unique(data[[aval_unit_var]]))
        choices <- gsub("s$", "", tolower(choices))

        shiny::updateSelectInput(
          session,
          "input_time_unit",
          choices = choices,
          selected = choices[1]
        )
      }
    })

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        aval_var = aval_var,
        avalu_var = avalu_var,
        events_var = events_var
      ),
      datasets = data,
      select_validation_rule = list(
        arm_var = ~ if (length(.) != 1 && length(.) != 2) "Please select exactly 1 or 2 treatment variables",
        aval_var = shinyvalidate::sv_required("Analysis Variable is required"),
        events_var = shinyvalidate::sv_required("Events Variable is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("A Event Type Parameter is required")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(
          0, 1,
          inclusive = c(FALSE, FALSE),
          message_fmt = "Confidence level must be between 0 and 1"
        )
      )
      iv$add_rule("conf_method", shinyvalidate::sv_required("A CI method is required"))
      iv$add_rule("num_pt_year", shinyvalidate::sv_required("Time Unit for AE Rate is required"))
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

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_aval_var <- as.vector(merged$anl_input_r()$columns_source$aval_var)
      input_avalu_var <- as.vector(merged$anl_input_r()$columns_source$avalu_var)
      input_events_var <- as.vector(merged$anl_input_r()$columns_source$events_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_events_var, input_aval_var, input_avalu_var),
        arm_var = input_arm_var
      )

      shiny::validate(
        shiny::need(
          !any(is.na(merged$anl_q()[["ANL"]][[input_events_var]])),
          "`Event Variable` for selected parameter includes NA values."
        )
      )
      NULL
    })

    # The R-code corresponding to the analysis.
    table_q <- shiny::reactive({
      validate_checks()

      ANL <- merged$anl_q()[["ANL"]] # nolint
      label_paramcd <- get_paramcd_label(ANL, paramcd)

      my_calls <- template_events_patyear(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(merged$anl_input_r()$columns_source$arm_var),
        aval_var = as.vector(merged$anl_input_r()$columns_source$aval_var),
        events_var = as.vector(merged$anl_input_r()$columns_source$events_var),
        label_paramcd = label_paramcd,
        add_total = input$add_total,
        total_label = total_label,
        control = control_incidence_rate(
          conf_level = as.numeric(input$conf_level), # nolint
          conf_type = if (input$conf_method == "Normal (rate)") {
            "normal"
          } else if (input$conf_method == "Normal (log rate)") {
            "normal_log"
          } else if (input$conf_method == "Exact") {
            "exact"
          } else {
            "byar"
          },
          input_time_unit = if (input$input_time_unit %in% c("day", "week", "month", "year")) {
            input$input_time_unit
          } else {
            "year"
          },
          num_pt_year = as.numeric(input$num_pt_year)
        ),
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = basic_table_args
      )
      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
    })

    # Outputs to render.
    table_r <- shiny::reactive({
      table_q()[["result"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "patyear_table",
      table_r = table_r
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(table_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(table_q())))
    )

    # Render R code.
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(table_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Event Rates Adjusted For Patient-Years Table")
        card$append_text("Event Rates Adjusted For Patient-Years Table", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(table_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
