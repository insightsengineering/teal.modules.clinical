#' Template: Laboratory
#'
#' Creates a laboratory template.
#' @inheritParams template_arguments
#' @param paramcd (`character`)\cr name of the parameter code variable.
#' @param param (`character`)\cr name of the parameter variable.
#' @param timepoints (`character`)\cr name of time variable used for
#' the laboratory table.
#' @param anrind (`character`)\cr name of the analysis reference range indicator variable.
#' @param aval (`character`)\cr name of the analysis value variable.
#' @param avalu (`character`)\cr name of the analysis value unit variable.
#' @param round_value (`numeric`)\cr number of decimal places to be used when rounding.
#' @keywords internal
#'
template_laboratory <- function(dataname = "ANL",
                                paramcd = "PARAMCD",
                                param = "PARAM",
                                anrind = "ANRIND",
                                timepoints = "ADY",
                                aval = "AVAL",
                                avalu = "AVALU",
                                round_value = 0L) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(paramcd),
    assertthat::is.string(param),
    assertthat::is.string(anrind),
    assertthat::is.string(timepoints),
    assertthat::is.string(aval),
    assertthat::is.string(avalu),
    is.integer(round_value) && round_value >= 0
  )

  y <- list()
  y$table <- list()

  table_lab_list <- add_expr(
    list(),
    substitute(
      expr = {
        dataname[, aval_char] <- round(dataname[, aval_char], round_value)
        labor_table_base <- dataname %>%
          dplyr::select(timepoints, paramcd, param, aval, avalu, anrind) %>%
          dplyr::arrange(timepoints) %>%
          dplyr::select(-timepoints) %>%
          dplyr::group_by(paramcd, param) %>%
          dplyr::mutate(INDEX = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(aval_anrind = paste(aval, anrind)) %>%
          dplyr::select(-c(aval, anrind))

        labor_table_html <- labor_table_base %>%
          dplyr::mutate(aval_anrind_col = color_lab_values(aval_anrind)) %>%
          dplyr::select(-aval_anrind) %>%
          tidyr::pivot_wider(names_from = INDEX, values_from = aval_anrind_col) %>%
          dplyr::mutate(param_char := clean_description(.data[[param_char]]))

        labor_table_raw <- labor_table_base %>%
          tidyr::pivot_wider(names_from = INDEX, values_from = aval_anrind) %>%
          dplyr::mutate(param_char := clean_description(.data[[param_char]]))

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
        aval = as.name(aval),
        aval_char = aval,
        avalu = as.name(avalu),
        timepoints = as.name(timepoints),
        anrind = as.name(anrind),
        round_value = round_value
      )
    )
  )

  y$table <- bracket_expr(table_lab_list)
  y
}

#' Teal Module: Patient Profile Laboratory Teal Module
#'
#' This teal module produces a patient profile laboratory table using `ADaM` datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param paramcd ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{PARAMCD} column of the `ADLB` dataset.
#' @param param ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{PARAM} column of the `ADLB` dataset.
#' @param timepoints ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' Time variable to be represented in the laboratory table.
#' @param anrind ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{ANRIND} column of the `ADLB` dataset with 3 possible levels "HIGH", "LOW" and "NORMAL".
#' @param aval ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AVAL} column of the `ADLB` dataset.
#' @param avalu ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AVALU} column of the `ADLB` dataset.
#' @inheritParams module_arguments
#'
#' @export
#'
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
#'     tm_t_pp_laboratory(
#'       label = "Vitals",
#'       dataname = "ADLB",
#'       patient_col = "USUBJID",
#'       paramcd = choices_selected(
#'         choices = variable_choices(adlb, "PARAMCD"),
#'         selected = "PARAMCD"
#'       ),
#'       param = choices_selected(
#'         choices = variable_choices(adlb, "PARAM"),
#'         selected = "PARAM"
#'       ),
#'       timepoints = choices_selected(
#'         choices = variable_choices(adlb, "ADY"),
#'         selected = "ADY"
#'       ),
#'       anrind = choices_selected(
#'         choices = variable_choices(adlb, "ANRIND"),
#'         selected = "ANRIND"
#'       ),
#'       aval = choices_selected(
#'         choices = variable_choices(adlb, "AVAL"),
#'         selected = "AVAL"
#'       ),
#'       avalu = choices_selected(
#'         choices = variable_choices(adlb, "AVALU"),
#'         selected = "AVALU"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_pp_laboratory <- function(label,
                               dataname = "ADLB",
                               parentname = "ADSL",
                               patient_col = "USUBJID",
                               timepoints = NULL,
                               aval = NULL,
                               avalu = NULL,
                               param = NULL,
                               paramcd = NULL,
                               anrind = NULL,
                               pre_output = NULL,
                               post_output = NULL) {
  logger::log_info("Initializing tm_t_pp_laboratory")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  args <- as.list(environment())
  data_extract_list <- list(
    timepoints = `if`(is.null(timepoints), NULL, cs_to_des_select(timepoints, dataname = dataname)),
    aval = `if`(is.null(aval), NULL, cs_to_des_select(aval, dataname = dataname)),
    avalu = `if`(is.null(avalu), NULL, cs_to_des_select(avalu, dataname = dataname)),
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
    datanames = "all"
  )
}

ui_g_laboratory <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$timepoints,
    ui_args$aval,
    ui_args$avalu,
    ui_args$param,
    ui_args$paramcd,
    ui_args$anrind
  )

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = shiny::div(
      DT::DTOutput(outputId = ns("lab_values_table"))
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(ui_args[c("timepoints", "aval", "avalu", "param", "paramcd", "anrind")]),
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
        id = ns("aval"),
        label = "Select AVAL variable:",
        data_extract_spec = ui_args$aval,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("avalu"),
        label = "Select AVALU variable:",
        data_extract_spec = ui_args$avalu,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("anrind"),
        label = "Select ANRIND variable:",
        data_extract_spec = ui_args$anrind,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::selectInput(
        inputId = ns("round_value"),
        label = "Select number of decimal places for rounding:",
        choices = NULL
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

srv_g_laboratory <- function(id,
                             data,
                             reporter,
                             filter_panel_api,
                             dataname,
                             parentname,
                             patient_col,
                             timepoints,
                             aval,
                             avalu,
                             param,
                             paramcd,
                             anrind,
                             label) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    patient_id <- shiny::reactive(input$patient_id)

    # Init
    patient_data_base <- shiny::reactive(unique(data[[parentname]]()[[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session,
      "patient_id",
      choices = patient_data_base(),
      selected = patient_data_base()[1]
    )

    shiny::observeEvent(patient_data_base(),
      handlerExpr = {
        teal.widgets::updateOptionalSelectInput(
          session,
          "patient_id",
          choices = patient_data_base(),
          selected = if (length(patient_data_base()) == 1) {
            patient_data_base()
          } else {
            intersect(patient_id(), patient_data_base())
          }
        )
      },
      ignoreInit = TRUE
    )

    # Update round_values
    aval_values <- data[[dataname]]()[, aval$select$selected]
    decimal_nums <- aval_values[trunc(aval_values) != aval_values]
    max_decimal <- max(nchar(gsub("([0-9]+).([0-9]+)", "\\2", decimal_nums)))

    shiny::updateSelectInput(
      session,
      "round_value",
      choices = seq(0, max_decimal),
      selected = min(4, max_decimal)
    )

    # Laboratory values tab ----
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        timepoints = timepoints,
        aval = aval,
        avalu = avalu,
        param = param,
        paramcd = paramcd,
        anrind = anrind
      ),
      datasets = data,
      select_validation_rule = list(
        timepoints = shinyvalidate::sv_required("Please select timepoints variable."),
        aval = shinyvalidate::sv_required("Please select AVAL variable."),
        avalu = shinyvalidate::sv_required("Please select AVALU variable."),
        param = shinyvalidate::sv_required("Please select PARAM variable."),
        paramcd = shinyvalidate::sv_required("Please select PARAMCD variable."),
        anrind = shinyvalidate::sv_required("Please select ANRIND variable.")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select a patient"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      join_keys = get_join_keys(data),
      selector_list = selector_list
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    all_q <- shiny::reactive({
      teal::validate_inputs(iv_r())

      labor_calls <- template_laboratory(
        dataname = "ANL",
        timepoints = input[[extract_input("timepoints", dataname)]],
        aval = input[[extract_input("aval", dataname)]],
        avalu = input[[extract_input("avalu", dataname)]],
        param = input[[extract_input("param", dataname)]],
        paramcd = input[[extract_input("paramcd", dataname)]],
        anrind = input[[extract_input("anrind", dataname)]],
        round_value = as.integer(input$round_value)
      )

      teal.code::eval_code(
        anl_q(),
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      ) %>%
        teal.code::eval_code(as.expression(labor_calls))
    })

    table_r <- shiny::reactive({
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
      verbatim_content = shiny::reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(all_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Patient Profile Laboratory Table")
        card$append_text("Patient Profile Laboratory Table", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r()$raw)
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
