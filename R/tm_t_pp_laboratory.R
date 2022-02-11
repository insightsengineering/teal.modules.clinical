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
#' This teal module produces a patient profile laboratory table using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param paramcd ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{PARAMCD} column of the
#' ADLB dataset.
#' @param param ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{PARAM} column of the ADLB dataset.
#' @param timepoints ([teal::choices_selected()] or [teal::data_extract_spec()])\cr Time variable to be represented in
#' the laboratory table.
#' @param anrind ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{ANRIND} column of the ADLB dataset
#' with 3 possible levels "HIGH", "LOW" and "NORMAL".
#' @param aval ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AVAL} column of the ADLB dataset.
#' @param avalu ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AVALU} column of the ADLB dataset.
#' @inheritParams module_arguments
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data('latest')$adsl"),
#'     cdisc_dataset("ADLB", ADLB, code = 'ADLB <- synthetic_cdisc_data("latest")$adlb'),
#'     check = TRUE
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
#'       aval = choices_selected(
#'         choices = variable_choices(ADLB, "AVAL"),
#'         selected = "AVAL"
#'       ),
#'       avalu = choices_selected(
#'         choices = variable_choices(ADLB, "AVALU"),
#'         selected = "AVALU"
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
    filters = "all"
  )
}

ui_g_laboratory <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.devel::is_single_dataset(
    ui_args$timepoints,
    ui_args$aval,
    ui_args$avalu,
    ui_args$param,
    ui_args$paramcd,
    ui_args$anrind
  )

  ns <- NS(id)
  teal.devel::standard_layout(
    output = div(
      teal.devel::get_dt_rows(ns("lab_values_table"), ns("lab_values_table_rows")),
      DT::DTOutput(outputId = ns("lab_values_table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(ui_args[c("timepoints", "aval", "avalu", "param", "paramcd", "anrind")]),
      optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.devel::data_extract_ui(
        id = ns("paramcd"),
        label = "Select PARAMCD variable:",
        data_extract_spec = ui_args$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("param"),
        label = "Select PARAM variable:",
        data_extract_spec = ui_args$param,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("timepoints"),
        label = "Select timepoints variable:",
        data_extract_spec = ui_args$timepoints,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("aval"),
        label = "Select AVAL variable:",
        data_extract_spec = ui_args$aval,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("avalu"),
        label = "Select AVALU variable:",
        data_extract_spec = ui_args$avalu,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
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
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

srv_g_laboratory <- function(id,
                             datasets,
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
  stopifnot(is_cdisc_data(datasets))
  moduleServer(id, function(input, output, session) {
    teal.devel::init_chunks()

    patient_id <- reactive(input$patient_id)

    # Init
    patient_data_base <- reactive(unique(datasets$get_data(parentname, filtered = TRUE)[[patient_col]]))
    updateOptionalSelectInput(session, "patient_id", choices = patient_data_base(), selected = patient_data_base()[1])

    observeEvent(patient_data_base(),
      handlerExpr = {
        updateOptionalSelectInput(
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
    aval_values <- datasets$get_data(dataname, filtered = TRUE)[, aval$select$selected]
    decimal_nums <- aval_values[trunc(aval_values) != aval_values]
    max_decimal <- max(nchar(gsub("([0-9]+).([0-9]+)", "\\2", decimal_nums)))

    updateSelectInput(
      session,
      "round_value",
      choices = seq(0, max_decimal),
      selected = min(4, max_decimal)
    )

    # Laboratory values tab ----
    labor_merged_data <- teal.devel::data_merge_module(
      datasets = datasets,
      data_extract = list(
        timepoints = timepoints,
        aval = aval,
        avalu = avalu,
        param = param,
        paramcd = paramcd,
        anrind = anrind
      )
    )

    labor_calls <- reactive({
      validate(need(patient_id(), "Please select a patient."))

      validate(
        need(
          input[[extract_input("timepoints", dataname)]],
          "Please select timepoints variable."
        ),
        need(
          input[[extract_input("aval", dataname)]],
          "Please select AVAL variable."
        ),
        need(
          input[[extract_input("avalu", dataname)]],
          "Please select AVALU variable."
        ),
        need(
          input[[extract_input("param", dataname)]],
          "Please select PARAM variable."
        ),
        need(
          input[[extract_input("paramcd", dataname)]],
          "Please select PARAMCD variable."
        ),
        need(
          input[[extract_input("anrind", dataname)]],
          "Please select ANRIND variable."
        )
      )

      labor_stack <- teal.devel::chunks$new()
      labor_stack$reset()
      labor_stack_push <- function(...) {
        teal.devel::chunks_push(..., chunks = labor_stack)
      }

      teal.devel::chunks_push_data_merge(labor_merged_data(), chunks = labor_stack)

      labor_stack_push(substitute(
        expr = {
          ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
        }, env = list(
          patient_col = patient_col,
          patient_id = patient_id()
        )
      ))

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

      lapply(labor_calls, labor_stack_push)
      teal.devel::chunks_safe_eval(chunks = labor_stack)
      labor_stack
    })

    output$lab_values_table <- DT::renderDataTable(
      expr = {
        teal.devel::chunks_reset()
        teal.devel::chunks_push_chunks(labor_calls())
        teal.devel::chunks_get_var("labor_table_html")
      },
      escape = FALSE,
      options = list(pageLength = input$lab_values_table_rows, scrollX = TRUE)
    )

    teal.devel::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.devel::get_extract_datanames(list(timepoints, aval, avalu, param, paramcd, anrind)),
      modal_title = label
    )
  })
}
