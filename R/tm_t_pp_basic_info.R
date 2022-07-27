#' Template: Basic Info
#'
#' Creates a basic info template.
#'
#' @inheritParams template_arguments
#' @param vars (`character`)\cr variable names to be shown in Basic Info tab.
#' @keywords internal
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
#' @param vars ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' ADSL columns to be shown in Basic Info tab.
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
#'   modules = modules(
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
  is_single_dataset_value <- teal.transform::is_single_dataset(ui_args$vars)

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = shiny::div(
      teal.widgets::get_dt_rows(ns("basic_info_table"), ns("basic_info_table_rows")),
      DT::DTOutput(outputId = ns("basic_info_table"))
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(ui_args[c("vars")]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("vars"),
        label = "Select variable:",
        data_extract_spec = ui_args$vars,
        is_single_dataset = is_single_dataset_value
      )
    ),
    forms = teal::get_rcode_ui(ns("rcode")),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}


srv_t_basic_info <- function(id,
                             datasets,
                             reporter,
                             dataname,
                             patient_col,
                             vars,
                             label) {
  stopifnot(is_cdisc_data(datasets))
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")

  shiny::moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()

    patient_id <- shiny::reactive(input$patient_id)

    # Init
    patient_data_base <- shiny::reactive(unique(datasets$get_data(dataname, filtered = TRUE)[[patient_col]]))
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

    # Basic Info tab ----
    binf_merged_data <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(vars = vars),
      merge_function = "dplyr::left_join"
    )

    basic_info_call <- shiny::reactive({
      shiny::validate(shiny::need(patient_id(), "Please select a patient."))
      shiny::validate(
        shiny::need(
          input[[extract_input("vars", dataname)]],
          "Please select basic info variables."
        )
      )

      call_stack <- teal.code::chunks_new()
      call_stack_push <- function(...) {
        teal.code::chunks_push(..., chunks = call_stack)
      }
      teal.code::chunks_push_data_merge(binf_merged_data(), chunks = call_stack)

      call_stack_push(
        expression = substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        ),
        id = "patient_id_filter_call"
      )

      my_calls <- template_basic_info(
        dataname = "ANL",
        vars = input[[extract_input("vars", dataname)]]
      )
      mapply(expression = my_calls, id = paste(names(my_calls), "call", sep = "_"), call_stack_push)
      teal.code::chunks_safe_eval(chunks = call_stack)
      call_stack
    })

    table_r <- shiny::reactive({
      teal.code::chunks_reset()
      teal.code::chunks_push_chunks(basic_info_call())
      teal.code::chunks_get_var("result")
    })

    output$basic_info_table <- DT::renderDataTable(
      expr = table_r(),
      options = list(pageLength = input$basic_info_table_rows)
    )

    teal::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.transform::get_extract_datanames(list(vars)),
      modal_title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Patient Profile Basic Info Table")
        card$append_text("Patient Profile Basic Info Table", "header2")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(get_rcode(
          chunks = teal.code::get_chunks_object(parent_idx = 2L),
          datasets = datasets,
          title = "",
          description = ""
        ), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
