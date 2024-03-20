#' Template: Patient Profile Laboratory Table
#'
#' Creates a valid expression to generate a patient profile laboratory table using ADaM datasets.
#'
#' @inheritParams template_arguments
#' @param paramcd (`character`)\cr name of the parameter code variable.
#' @param param (`character`)\cr name of the parameter variable.
#' @param timepoints (`character`)\cr name of time variable.
#' @param anrind (`character`)\cr name of the analysis reference range indicator variable.
#' @param round_value (`numeric`)\cr number of decimal places to round to.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_pp_laboratory()]
#'
#' @keywords internal
template_laboratory <- function(dataname = "ANL",
                                paramcd = "PARAMCD",
                                param = "PARAM",
                                anrind = "ANRIND",
                                timepoints = "ADY",
                                aval = lifecycle::deprecated(),
                                aval_var = "AVAL",
                                avalu = lifecycle::deprecated(),
                                avalu_var = "AVALU",
                                patient_id = NULL,
                                round_value = 0L) {
  if (lifecycle::is_present(aval)) {
    aval_var <- aval
    warning(
      "The `aval` argument of `template_laboratory()` is deprecated as of teal.modules.clinical 0.8.16. ",
      "Please use the `aval_var` argument instead.",
      call. = FALSE
    )
  }

  if (lifecycle::is_present(avalu)) {
    avalu_var <- avalu
    warning(
      "The `avalu` argument of `template_laboratory()` is deprecated as of teal.modules.clinical 0.8.16. ",
      "Please use the `avalu_var` argument instead.",
      call. = FALSE
    )
  }

  checkmate::assert_string(dataname)
  checkmate::assert_string(paramcd)
  checkmate::assert_string(param)
  checkmate::assert_string(anrind)
  checkmate::assert_string(timepoints)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(avalu_var)
  checkmate::assert_integer(round_value, lower = 0)

  y <- list()
  y$table <- list()

  table_lab_list <- add_expr(
    list(),
    substitute(
      expr = {
        dataname[, aval_char] <- round(dataname[, aval_char], round_value)
        labor_table_base <- dataname %>%
          dplyr::select(timepoints, paramcd, param, aval_var, avalu_var, anrind) %>%
          dplyr::arrange(timepoints) %>%
          dplyr::select(-timepoints) %>%
          dplyr::group_by(paramcd, param) %>%
          dplyr::mutate(INDEX = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(aval_anrind = paste(aval_var, anrind)) %>%
          dplyr::select(-c(aval_var, anrind))

        labor_table_raw <- labor_table_base %>%
          as.data.frame() %>%
          stats::reshape(
            direction = "wide",
            idvar = c(paramcd_char, param_char, avalu_char),
            v.names = "aval_anrind",
            timevar = "INDEX"
          )
        colnames(labor_table_raw)[-c(1:3)] <- unique(labor_table_base$INDEX)

        labor_table_raw[[param_char]] <- clean_description(labor_table_raw[[param_char]])

        labor_table_raw <- rlistings::as_listing(
          labor_table_raw,
          key_cols = NULL,
          default_formatting = list(all = fmt_config(align = "left"))
        )
        main_title(labor_table_raw) <- paste("Patient ID:", patient_id)

        labor_table_html <- labor_table_base %>%
          dplyr::mutate(aval_anrind_col = color_lab_values(aval_anrind)) %>%
          dplyr::select(-aval_anrind) %>%
          as.data.frame() %>%
          stats::reshape(
            direction = "wide",
            idvar = c(paramcd_char, param_char, avalu_char),
            v.names = "aval_anrind_col",
            timevar = "INDEX"
          )
        colnames(labor_table_html)[-c(1:3)] <- unique(labor_table_base$INDEX)
        labor_table_html[[param_char]] <- clean_description(labor_table_html[[param_char]])

        labor_table_html_dt <- DT::datatable(labor_table_html, escape = FALSE)
        labor_table_html_dt$dependencies <- c(
          labor_table_html_dt$dependencies,
          list(rmarkdown::html_dependency_bootstrap("default"))
        )
        labor_table_html_dt
      },
      env = list(
        dataname = as.name(dataname),
        param = as.name(param),
        param_char = param,
        paramcd = as.name(paramcd),
        paramcd_char = paramcd,
        aval_var = as.name(aval_var),
        aval_char = aval_var,
        avalu_var = as.name(avalu_var),
        avalu_char = avalu_var,
        timepoints = as.name(timepoints),
        anrind = as.name(anrind),
        patient_id = patient_id,
        round_value = round_value
      )
    )
  )

  y$table <- bracket_expr(table_lab_list)
  y
}

#' teal Module: Patient Profile Laboratory Table
#'
#' This module produces a patient profile laboratory table using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @inheritParams template_laboratory
#' @param param ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `PARAM` variable from `dataname`.
#' @param timepoints ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the time variable from `dataname`.
#' @param anrind ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `ANRIND` variable from `dataname`. Variable should have the
#'   following 3 levels: `"HIGH"`, `"LOW"`, and `"NORMAL"`.
#'
#' @inherit module_arguments return
#'
#' @examples
#' ADSL <- tmc_ex_adsl
#' ADLB <- tmc_ex_adlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     ADSL = ADSL,
#'     ADLB = ADLB,
#'     code = "
#'       ADSL <- tmc_ex_adsl
#'       ADLB <- tmc_ex_adlb
#'     "
#'   ),
#'   modules = modules(
#'     tm_t_pp_laboratory(
#'       label = "Vitals",
#'       dataname = "ADLB",
#'       patient_col = "USUBJID",
#'       paramcd = choices_selected(
#'         choices = variable_choices(ADLB, "PARAMCD"),
#'         selected = "PARAMCD"
#'       ),
#'       param = choices_selected(
#'         choices = variable_choices(ADLB, "PARAM"),
#'         selected = "PARAM"
#'       ),
#'       timepoints = choices_selected(
#'         choices = variable_choices(ADLB, "ADY"),
#'         selected = "ADY"
#'       ),
#'       anrind = choices_selected(
#'         choices = variable_choices(ADLB, "ANRIND"),
#'         selected = "ANRIND"
#'       ),
#'       aval_var = choices_selected(
#'         choices = variable_choices(ADLB, "AVAL"),
#'         selected = "AVAL"
#'       ),
#'       avalu_var = choices_selected(
#'         choices = variable_choices(ADLB, "AVALU"),
#'         selected = "AVALU"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_pp_laboratory <- function(label,
                               dataname = "ADLB",
                               parentname = "ADSL",
                               patient_col = "USUBJID",
                               timepoints = NULL,
                               aval = lifecycle::deprecated(),
                               aval_var = NULL,
                               avalu = lifecycle::deprecated(),
                               avalu_var = NULL,
                               param = NULL,
                               paramcd = NULL,
                               anrind = NULL,
                               pre_output = NULL,
                               post_output = NULL) {
  if (lifecycle::is_present(aval)) {
    aval_var <- aval
    warning(
      "The `aval` argument of `tm_t_pp_laboratory()` is deprecated as of teal.modules.clinical 0.8.16. ",
      "Please use the `aval_var` argument instead.",
      call. = FALSE
    )
  } else {
    aval <- aval_var # resolves missing argument error
  }

  if (lifecycle::is_present(avalu)) {
    avalu_var <- avalu
    warning(
      "The `avalu` argument of `tm_t_pp_laboratory()` is deprecated as of teal.modules.clinical 0.8.16. ",
      "Please use the `avalu_var` argument instead.",
      call. = FALSE
    )
  } else {
    avalu <- avalu_var # resolves missing argument error
  }

  message("Initializing tm_t_pp_laboratory")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(timepoints, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(aval_var, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(avalu_var, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(param, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(paramcd, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(anrind, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  args <- as.list(environment())
  data_extract_list <- list(
    timepoints = `if`(is.null(timepoints), NULL, cs_to_des_select(timepoints, dataname = dataname)),
    aval_var = `if`(is.null(aval_var), NULL, cs_to_des_select(aval_var, dataname = dataname)),
    avalu_var = `if`(is.null(avalu_var), NULL, cs_to_des_select(avalu_var, dataname = dataname)),
    param = `if`(is.null(param), NULL, cs_to_des_select(param, dataname = dataname)),
    paramcd = `if`(is.null(paramcd), NULL, cs_to_des_select(paramcd, dataname = dataname)),
    anrind = `if`(is.null(anrind), NULL, cs_to_des_select(anrind, dataname = dataname))
  )

  module(
    label = label,
    ui = ui_g_laboratory,
    ui_args = c(data_extract_list, args),
    server = srv_g_laboratory,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        patient_col = patient_col
      )
    ),
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_laboratory <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$timepoints,
    ui_args$aval_var,
    ui_args$avalu_var,
    ui_args$param,
    ui_args$paramcd,
    ui_args$anrind
  )

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      htmlOutput(ns("title")),
      DT::DTOutput(outputId = ns("lab_values_table"))
    ),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(ui_args[c("timepoints", "aval_var", "avalu_var", "param", "paramcd", "anrind")]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select PARAMCD variable:",
        data_extract_spec = ui_args$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("param"),
        label = "Select PARAM variable:",
        data_extract_spec = ui_args$param,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("timepoints"),
        label = "Select timepoints variable:",
        data_extract_spec = ui_args$timepoints,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Select AVAL variable:",
        data_extract_spec = ui_args$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("avalu_var"),
        label = "Select AVALU variable:",
        data_extract_spec = ui_args$avalu_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("anrind"),
        label = "Select ANRIND variable:",
        data_extract_spec = ui_args$anrind,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        inputId = ns("round_value"),
        label = "Select number of decimal places for rounding:",
        choices = NULL
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

#' @keywords internal
srv_g_laboratory <- function(id,
                             data,
                             reporter,
                             filter_panel_api,
                             dataname,
                             parentname,
                             patient_col,
                             timepoints,
                             aval_var,
                             avalu_var,
                             param,
                             paramcd,
                             anrind,
                             label) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    patient_id <- reactive(input$patient_id)

    # Init
    patient_data_base <- reactive(unique(data()[[parentname]][[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session,
      "patient_id",
      choices = patient_data_base(),
      selected = restoreInput(ns("patient_id"), patient_data_base()[1])
    )

    observeEvent(patient_data_base(),
      handlerExpr = {
        teal.widgets::updateOptionalSelectInput(
          session,
          "patient_id",
          choices = patient_data_base(),
          selected = restoreInput(
            ns("patient_id"),
            if (length(patient_data_base()) == 1) {
              patient_data_base()
            } else {
              intersect(patient_id(), patient_data_base())
            }
          )
        )
      },
      ignoreInit = TRUE
    )

    # Update round_values
    aval_values <- isolate(data())[[dataname]][, aval_var$select$selected]
    decimal_nums <- aval_values[trunc(aval_values) != aval_values]
    max_decimal <- max(nchar(gsub("([0-9]+).([0-9]+)", "\\2", decimal_nums)))

    updateSelectInput(
      session,
      "round_value",
      choices = seq(0, max_decimal),
      selected = restoreInput(ns("round_value"), min(4, max_decimal))
    )

    # Laboratory values tab ----
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        timepoints = timepoints,
        aval_var = aval_var,
        avalu_var = avalu_var,
        param = param,
        paramcd = paramcd,
        anrind = anrind
      ),
      datasets = data,
      select_validation_rule = list(
        timepoints = shinyvalidate::sv_required("Please select timepoints variable."),
        aval_var = shinyvalidate::sv_required("Please select AVAL variable."),
        avalu_var = shinyvalidate::sv_required("Please select AVALU variable."),
        param = shinyvalidate::sv_required("Please select PARAM variable."),
        paramcd = shinyvalidate::sv_required("Please select PARAMCD variable."),
        anrind = shinyvalidate::sv_required("Please select ANRIND variable.")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select a patient"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list
    )

    anl_q <- reactive({
      data() %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    all_q <- reactive({
      teal::validate_inputs(iv_r())

      labor_calls <- template_laboratory(
        dataname = "ANL",
        timepoints = input[[extract_input("timepoints", dataname)]],
        aval_var = input[[extract_input("aval_var", dataname)]],
        avalu_var = input[[extract_input("avalu_var", dataname)]],
        param = input[[extract_input("param", dataname)]],
        paramcd = input[[extract_input("paramcd", dataname)]],
        anrind = input[[extract_input("anrind", dataname)]],
        patient_id = patient_id(),
        round_value = as.integer(input$round_value)
      )

      teal.code::eval_code(
        anl_q(),
        substitute(
          expr = {
            pt_id <- patient_id
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      ) %>%
        teal.code::eval_code(as.expression(labor_calls))
    })

    output$title <- renderText({
      paste("<h5><b>Patient ID:", all_q()[["pt_id"]], "</b></h5>")
    })

    table_r <- reactive({
      q <- all_q()
      list(
        html = q[["labor_table_html"]],
        raw = q[["labor_table_raw"]]
      )
    })

    output$lab_values_table <- DT::renderDataTable(
      expr = table_r()$html,
      escape = FALSE,
      options = list(
        lengthMenu = list(list(-1, 5, 10, 25), list("All", "5", "10", "25")),
        scrollX = TRUE
      )
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = reactive(is.null(teal.code::get_warnings(all_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Patient Profile Laboratory Table",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Table", "header3")
        card$append_table(table_r()$raw)
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(teal.code::get_code(all_q()))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
