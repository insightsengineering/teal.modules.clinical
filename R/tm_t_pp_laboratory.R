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
#'
template_laboratory <- function(dataname = "ANL",
                                paramcd = "PARAMCD",
                                param = "PARAM",
                                anrind = "ANRIND",
                                timepoints = "ADY",
                                aval = "AVAL",
                                avalu = "AVALU",
                                round_value = 0L) {
  assert_that(
    is.string(dataname),
    is.string(paramcd),
    is.string(param),
    is.string(anrind),
    is.string(timepoints),
    is.string(aval),
    is.string(avalu),
    is.integer(round_value) && round_value >= 0
  )

  y <- list()
  y$table <- list()

  table_lab_list <- add_expr(
    list(),
    substitute({
      dataname[, aval_char] <- round(dataname[, aval_char], round_value)
      labor_table_base <- dataname %>%
        select(timepoints, paramcd, param, aval, avalu, anrind) %>%
        arrange(timepoints) %>%
        select(-timepoints) %>%
        group_by(paramcd, param) %>%
        mutate(INDEX = row_number()) %>%
        ungroup() %>%
        mutate(aval_anrind = paste(aval, anrind)) %>%
        select(-c(aval, anrind))

      labor_table_html <- labor_table_base %>%
        mutate(aval_anrind_col = color_lab_values(aval_anrind)) %>%
        select(-aval_anrind) %>%
        tidyr::pivot_wider(names_from = INDEX, values_from = aval_anrind_col) %>%
        mutate(param_char := clean_description(.data[[param_char]]))

      labor_table_raw <- labor_table_base %>%
        tidyr::pivot_wider(names_from = INDEX, values_from = aval_anrind) %>%
        mutate(param_char := clean_description(.data[[param_char]]))

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
#' @param patient_col (`character`) value patient ID column to be used.
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
#'   modules = root_modules(
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
  assert_that(is_character_single(label))
  assert_that(is_character_single(dataname))
  assert_that(is_character_single(parentname))
  assert_that(is_character_single(patient_col))
  assert_that(is.null(pre_output) || is(pre_output, "shiny.tag"),
    msg = "pre_output should be either null or shiny.tag type of object"
  )
  assert_that(is.null(post_output) || is(post_output, "shiny.tag"),
    msg = "post_output should be either null or shiny.tag type of object"
  )

  args <- as.list(environment())
  data_extract_list <- list(
    timepoints = if_not_null(timepoints, cs_to_des_select(timepoints, dataname = dataname)),
    aval = if_not_null(aval, cs_to_des_select(aval, dataname = dataname)),
    avalu = if_not_null(avalu, cs_to_des_select(avalu, dataname = dataname)),
    param = if_not_null(param, cs_to_des_select(param, dataname = dataname)),
    paramcd = if_not_null(paramcd, cs_to_des_select(paramcd, dataname = dataname)),
    anrind = if_not_null(anrind, cs_to_des_select(anrind, dataname = dataname))
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
  is_single_dataset_value <- is_single_dataset(
    ui_args$timepoints,
    ui_args$aval,
    ui_args$avalu,
    ui_args$param,
    ui_args$paramcd,
    ui_args$anrind
  )

  ns <- NS(id)
  standard_layout(
    output = div(
      get_dt_rows(ns("lab_values_table"), ns("lab_values_table_rows")),
      DT::DTOutput(outputId = ns("lab_values_table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(ui_args[c("timepoints", "aval", "avalu", "param", "paramcd", "anrind")]),
      optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = T)
      ),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select PARAMCD variable:",
        data_extract_spec = ui_args$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("param"),
        label = "Select PARAM variable:",
        data_extract_spec = ui_args$param,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("timepoints"),
        label = "Select timepoints variable:",
        data_extract_spec = ui_args$timepoints,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval"),
        label = "Select AVAL variable:",
        data_extract_spec = ui_args$aval,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("avalu"),
        label = "Select AVALU variable:",
        data_extract_spec = ui_args$avalu,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
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
    forms = get_rcode_ui(ns("rcode")),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

srv_g_laboratory <- function(input,
                             output,
                             session,
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

  init_chunks()

  patient_id <- reactive(input$patient_id)

  # Init
  patient_data_base <- reactive(unique(datasets$get_data(parentname, filtered = TRUE)[[patient_col]]))
  updateOptionalSelectInput(session, "patient_id", choices = patient_data_base(), selected = patient_data_base()[1])

  observeEvent(patient_data_base(), {
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
    selected = 4
  )

  # Laboratory values tab ----
  labor_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(timepoints, aval, avalu, param, paramcd, anrind),
    input_id = c("timepoints", "aval", "avalu", "param", "paramcd", "anrind")
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

    labor_stack <- chunks$new()
    labor_stack$reset()
    labor_stack_push <- function(...) {
      chunks_push(..., chunks = labor_stack)
    }

    chunks_push_data_merge(labor_merged_data(), chunks = labor_stack)

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
    chunks_safe_eval(chunks = labor_stack)
    labor_stack
  })

  output$lab_values_table <- DT::renderDataTable({
    chunks_reset()
    chunks_push_chunks(labor_calls())
    chunks_get_var("labor_table_html")
    },
    escape = FALSE,
    options = list(pageLength = input$lab_values_table_rows, scrollX = TRUE)
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(timepoints, aval, avalu, param, paramcd, anrind)),
    modal_title = label
  )
}
