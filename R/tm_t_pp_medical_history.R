#' Template: Medical History
#'
#' Creates medical history template.
#'
#' @inheritParams template_arguments
#' @param mhterm (`character`)\cr name of the reported name for medical history variable.
#' @param mhbodsys (`character`)\cr name of the body system or organ class variable.
#' @param mhdistat (`character`)\cr name of the status of the disease variable.
#' @keywords internal
#'
template_medical_history <- function(dataname = "ANL",
                                     mhterm = "MHTERM",
                                     mhbodsys = "MHBODSYS",
                                     mhdistat = "MHDISTAT") {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(mhterm),
    assertthat::is.string(mhbodsys),
    assertthat::is.string(mhdistat)
  )

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      labels <- formatters::var_labels(dataname, fill = FALSE)[c(mhbodsys_char, mhterm_char, mhdistat_char)]
      mhbodsys_label <- labels[mhbodsys_char]

      result_raw <-
        dataname %>%
        dplyr::select(mhbodsys, mhterm, mhdistat) %>%
        dplyr::arrange(mhbodsys) %>%
        dplyr::mutate_if(is.character, as.factor) %>%
        dplyr::mutate_if(is.factor, function(x) explicit_na(x, "UNKNOWN")) %>%
        dplyr::distinct() %>%
        `colnames<-`(labels)

      result <- rtables::basic_table() %>%
        rtables::split_cols_by_multivar(colnames(result_raw)[2:3]) %>%
        rtables::split_rows_by(colnames(result_raw)[1],
          split_fun = rtables::drop_split_levels
        ) %>%
        rtables::split_rows_by(colnames(result_raw)[2],
          split_fun = rtables::drop_split_levels,
          child_labels = "hidden"
        ) %>%
        rtables::analyze_colvars(function(x) x[seq_along(x)]) %>%
        rtables::build_table(result_raw)

      result
    }, env = list(
      dataname = as.name(dataname),
      mhbodsys = as.name(mhbodsys),
      mhterm = as.name(mhterm),
      mhdistat = as.name(mhdistat),
      mhbodsys_char = mhbodsys,
      mhterm_char = mhterm,
      mhdistat_char = mhdistat
    ))
  )

  y$table <- bracket_expr(table_list)

  y
}

#' Teal Module: Patient Medical History Teal Module
#'
#' This teal module produces a patient medical history report using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param mhterm
#' ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{MHTERM} column of the ADMH dataset.
#' @param mhbodsys ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{MHBODSYS} column of the ADMH dataset.
#' @param mhdistat ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{MHDISTAT} column of the ADMH dataset.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADMH <- synthetic_cdisc_data("latest")$admh
#' ADMH[["MHDISTAT"]] <- "ONGOING"
#' formatters::var_labels(ADMH[c("MHDISTAT")]) <- c("Status of Disease")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADMH", ADMH, code = "ADMH <- synthetic_cdisc_data('latest')$admh
#'                    ADMH[['MHDISTAT']] <- 'ONGOING'
#'                    formatters::var_labels(ADMH[c('MHDISTAT')]) <- c('Status of Disease')"),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_t_pp_medical_history(
#'       label = "Medical history",
#'       dataname = "ADMH",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       mhterm = choices_selected(
#'         choices = variable_choices(ADMH, c("MHTERM")),
#'         selected = "MHTERM"
#'       ),
#'       mhbodsys = choices_selected(
#'         choices = variable_choices(ADMH, "MHBODSYS"),
#'         selected = "MHBODSYS"
#'       ),
#'       mhdistat = choices_selected(
#'         choices = variable_choices(ADMH, "MHDISTAT"),
#'         selected = "MHDISTAT"
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_pp_medical_history <- function(label,
                                    dataname = "ADMH",
                                    parentname = "ADSL",
                                    patient_col = "USUBJID",
                                    mhterm = NULL,
                                    mhbodsys = NULL,
                                    mhdistat = NULL,
                                    pre_output = NULL,
                                    post_output = NULL) {
  logger::log_info("Initializing tm_t_pp_medical_history")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  args <- as.list(environment())
  data_extract_list <- list(
    mhterm = `if`(is.null(mhterm), NULL, cs_to_des_select(mhterm, dataname = dataname)),
    mhbodsys = `if`(is.null(mhbodsys), NULL, cs_to_des_select(mhbodsys, dataname = dataname)),
    mhdistat = `if`(is.null(mhdistat), NULL, cs_to_des_select(mhdistat, dataname = dataname))
  )

  module(
    label = label,
    ui = ui_t_medical_history,
    ui_args = c(data_extract_list, args),
    server = srv_t_medical_history,
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

ui_t_medical_history <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$mhterm,
    ui_args$mhbodsys,
    ui_args$mhdistat
  )

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = shiny::div(
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = shiny::div(
      ### Reporter
      shiny::tags$div(
        teal.reporter::add_card_button_ui(ns("addReportCard")),
        teal.reporter::download_report_button_ui(ns("downloadButton")),
        teal.reporter::reset_report_button_ui(ns("resetButton"))
      ),
      shiny::tags$br(),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(ui_args[c("mhterm", "mhbodsys", "mhdistat")]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("mhterm"),
        label = "Select MHTERM variable:",
        data_extract_spec = ui_args$mhterm,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("mhbodsys"),
        label = "Select MHBODSYS variable:",
        data_extract_spec = ui_args$mhbodsys,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("mhdistat"),
        label = "Select MHDISTAT variable:",
        data_extract_spec = ui_args$mhdistat,
        is_single_dataset = is_single_dataset_value
      )
    ),
    forms = teal::get_rcode_ui(ns("rcode")),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}


srv_t_medical_history <- function(id,
                                  datasets,
                                  reporter,
                                  dataname,
                                  parentname,
                                  patient_col,
                                  mhterm,
                                  mhbodsys,
                                  mhdistat,
                                  label) {
  stopifnot(is_cdisc_data(datasets))
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")

  shiny::moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()

    patient_id <- shiny::reactive(input$patient_id)

    # Init
    patient_data_base <- shiny::reactive(unique(datasets$get_data(parentname, filtered = TRUE)[[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session, "patient_id",
      choices = patient_data_base(), selected = patient_data_base()[1]
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

    # Medical history tab ----
    mhist_merged_data <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(mhterm = mhterm, mhbodsys = mhbodsys, mhdistat = mhdistat),
      merge_function = "dplyr::left_join"
    )

    mhist_call <- shiny::reactive({
      shiny::validate(shiny::need(patient_id(), "Please select a patient."))

      shiny::validate(
        shiny::need(
          input[[extract_input("mhterm", dataname)]],
          "Please select MHTERM variable."
        ),
        shiny::need(
          input[[extract_input("mhbodsys", dataname)]],
          "Please select MHBODSYS variable."
        ),
        shiny::need(
          input[[extract_input("mhdistat", dataname)]],
          "Please select MHDISTAT variable."
        ),
        shiny::need(
          nrow(mhist_merged_data()$data()[mhist_merged_data()$data()[[patient_col]] == patient_id(), ]) > 0,
          "Patient has no data about medical history."
        )
      )

      mhist_stack <- teal.code::chunks_new()
      mhist_stack_push <- function(...) {
        teal.code::chunks_push(..., chunks = mhist_stack)
      }
      teal.code::chunks_push_data_merge(mhist_merged_data(), chunks = mhist_stack)

      mhist_stack_push(
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

      my_calls <- template_medical_history(
        dataname = "ANL",
        mhterm = input[[extract_input("mhterm", dataname)]],
        mhbodsys = input[[extract_input("mhbodsys", dataname)]],
        mhdistat = input[[extract_input("mhdistat", dataname)]]
      )
      mapply(
        expression = my_calls,
        id = paste(names(my_calls), "call", sep = "_"),
        mhist_stack_push
      )
      teal.code::chunks_safe_eval(chunks = mhist_stack)
      mhist_stack
    })

    table_r <- shiny::reactive({
      teal.code::chunks_reset()
      teal.code::chunks_push_chunks(mhist_call())
      teal.code::chunks_get_var("result")
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    teal::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.transform::get_extract_datanames(list(mhterm, mhbodsys, mhdistat)),
      modal_title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Patient Medical History Table")
        card$append_text("Patient Medical History Table", "header2")
        card$append_text("Filter State", "header3")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_text("Show R Code", "header3")
        card$append_src(paste(get_rcode(
          chunks = teal.code::get_chunks_object(parent_idx = 1L),
          datasets = datasets,
          title = "",
          description = ""
        ), collapse = "\n"))
        card
      }

      teal.reporter::add_card_button_srv("addReportCard", reporter = reporter, card_fun = card_fun)
      teal.reporter::download_report_button_srv("downloadButton", reporter = reporter)
      teal.reporter::reset_report_button_srv("resetButton", reporter)
    }
    ###
  })
}
