#' Template: Basic Info
#'
#' Creates a basic info template.
#'
#' @inheritParams template_arguments
#' @param vars (`character`)\cr variable names to be shown in Basic Info tab.
#'
template_basic_info <- function(dataname = "ANL",
                                vars) {
  checkmate::assert_string(dataname)
  checkmate::assert_character(vars, min.len = 1)

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(
      expr = {
        values <- dataname %>%
          dplyr::select(vars) %>%
          # we are sure that only one row
          utils::head(1) %>%
          t()

        key <- get_labels(dataname)$column_labels[rownames(values)]

        result <-
          data.frame(key = key, value = values) %>%
          dplyr::select(key, value) %>%
          dplyr::rename(`   ` = key, ` ` = value)
        result
      }, env = list(
        dataname = as.name(dataname),
        vars = vars
      )
    )
  )
  y$table <- bracket_expr(table_list)

  y
}


#' Teal Module: Patient Profile Basic Info Teal Module
#'
#' This teal module produces a patient profile basic info report using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param vars ([teal::choices_selected()] or [teal::data_extract_spec()])\cr ADSL columns to be shown in
#'  Basic Info tab.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_pp_basic_info(
#'       label = "Basic info",
#'       dataname = "ADSL",
#'       patient_col = "USUBJID",
#'       vars = choices_selected(
#'         choices = variable_choices(ADSL),
#'         selected = c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT")
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_pp_basic_info <- function(label,
                               dataname = "ADSL",
                               patient_col = "USUBJID",
                               vars = NULL,
                               pre_output = NULL,
                               post_output = NULL) {
  logger::log_info("Initializing tm_t_pp_basic_info")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  args <- as.list(environment())
  data_extract_list <- list(
    vars = `if`(is.null(vars), NULL, cs_to_des_select(vars, dataname = dataname, multiple = TRUE))
  )

  module(
    label = label,
    ui = ui_t_basic_info,
    ui_args = c(data_extract_list, args),
    server = srv_t_basic_info,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        label = label,
        patient_col = patient_col
      )
    ),
    filters = "all"
  )
}

ui_t_basic_info <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.devel::is_single_dataset(ui_args$vars)

  ns <- NS(id)
  teal.devel::standard_layout(
    output = div(
      teal.devel::get_dt_rows(ns("basic_info_table"), ns("basic_info_table_rows")),
      DT::DTOutput(outputId = ns("basic_info_table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(ui_args[c("vars")]),
      optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.devel::data_extract_ui(
        id = ns("vars"),
        label = "Select variable:",
        data_extract_spec = ui_args$vars,
        is_single_dataset = is_single_dataset_value
      )
    ),
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}


srv_t_basic_info <- function(input,
                             output,
                             session,
                             datasets,
                             dataname,
                             patient_col,
                             vars,
                             label) {
  stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

  patient_id <- reactive(input$patient_id)

  # Init
  patient_data_base <- reactive(unique(datasets$get_data(dataname, filtered = TRUE)[[patient_col]]))
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

  # Basic Info tab ----
  binf_merged_data <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(vars = vars),
    merge_function = "dplyr::left_join"
  )

  basic_info_call <- reactive({
    validate(need(patient_id(), "Please select a patient."))
    validate(
      need(
        input[[extract_input("vars", dataname)]],
        "Please select basic info variables."
      )
    )

    call_stack <- teal.devel::chunks$new()
    call_stack_push <- function(...) {
      teal.devel::chunks_push(..., chunks = call_stack)
    }
    teal.devel::chunks_push_data_merge(binf_merged_data(), chunks = call_stack)

    call_stack_push(substitute(
      expr = {
        ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
      }, env = list(
        patient_col = patient_col,
        patient_id = patient_id()
      )
    ))

    my_calls <- template_basic_info(
      dataname = "ANL",
      vars = input[[extract_input("vars", dataname)]]
    )
    lapply(my_calls, call_stack_push)
    teal.devel::chunks_safe_eval(chunks = call_stack)
    call_stack
  })

  output$basic_info_table <- DT::renderDataTable(
    expr = {
      teal.devel::chunks_reset()
      teal.devel::chunks_push_chunks(basic_info_call())
      teal.devel::chunks_get_var("result")
    },
    options = list(pageLength = input$basic_info_table_rows)
  )

  callModule(
    teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(list(vars)),
    modal_title = label
  )
}
