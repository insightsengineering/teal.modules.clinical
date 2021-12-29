#' Template: Medical History
#'
#' Creates medical history template.
#'
#' @inheritParams template_arguments
#' @param mhterm (`character`)\cr name of the reported name for medical history variable.
#' @param mhbodsys (`character`)\cr name of the body system or organ class variable.
#' @param mhdistat (`character`)\cr name of the status of the disease variable.
#'
template_medical_history <- function(dataname = "ANL",
                                     mhterm = "MHTERM",
                                     mhbodsys = "MHBODSYS",
                                     mhdistat = "MHDISTAT") {
  assert_that(
    is.string(dataname),
    is.string(mhterm),
    is.string(mhbodsys),
    is.string(mhdistat)
  )

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      labels <- rtables::var_labels(dataname)[c(mhbodsys_char, mhterm_char, mhdistat_char)]
      mhbodsys_label <- labels[mhbodsys_char]

      result <-
        dataname %>%
        dplyr::select(mhbodsys, mhterm, mhdistat) %>%
        dplyr::arrange(mhbodsys) %>%
        dplyr::mutate_if(is.character, as.factor) %>%
        dplyr::mutate_if(is.factor, function(x) explicit_na(x, "UNKNOWN")) %>%
        dplyr::distinct() %>%
        `colnames<-`(labels)

      result_without_mhbodsys <- result[, -1]
      result_kbl <- kableExtra::kable(result_without_mhbodsys, table.attr = "style='width:100%;'")

      result_kbl <- result_kbl %>%
        kableExtra::pack_rows(index = table(droplevels(result[[mhbodsys_label]]))) %>%
        kableExtra::kable_styling(bootstrap_options = c("basic"), full_width = TRUE)

      result_kbl
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
#' ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{MHTERM} column of the ADMH dataset.
#' @param mhbodsys ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{MHBODSYS} column of the
#' ADMH dataset.
#' @param mhdistat ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{MHDISTAT} column of the
#' ADMH dataset.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADMH <- synthetic_cdisc_data("latest")$admh
#' ADMH[["MHDISTAT"]] <- "ONGOING"
#' rtables::var_labels(ADMH[c("MHDISTAT")]) <- c("Status of Disease")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADMH", ADMH, code = "ADMH <- synthetic_cdisc_data('latest')$admh
#'                    ADMH[['MHDISTAT']] <- 'ONGOING'
#'                    rtables::var_labels(ADMH[c('MHDISTAT')]) <- c('Status of Disease')"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
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
    mhterm = if_not_null(mhterm, cs_to_des_select(mhterm, dataname = dataname)),
    mhbodsys = if_not_null(mhbodsys, cs_to_des_select(mhbodsys, dataname = dataname)),
    mhdistat = if_not_null(mhdistat, cs_to_des_select(mhdistat, dataname = dataname))
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
  is_single_dataset_value <- is_single_dataset(
    ui_args$mhterm,
    ui_args$mhbodsys,
    ui_args$mhdistat
  )

  ns <- NS(id)
  standard_layout(
    output = div(
      htmlOutput(outputId = ns("medical_history_table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(ui_args[c("mhterm", "mhbodsys", "mhdistat")]),
      optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      data_extract_ui(
        id = ns("mhterm"),
        label = "Select MHTERM variable:",
        data_extract_spec = ui_args$mhterm,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("mhbodsys"),
        label = "Select MHBODSYS variable:",
        data_extract_spec = ui_args$mhbodsys,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("mhdistat"),
        label = "Select MHDISTAT variable:",
        data_extract_spec = ui_args$mhdistat,
        is_single_dataset = is_single_dataset_value
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}


srv_t_medical_history <- function(input,
                                  output,
                                  session,
                                  datasets,
                                  dataname,
                                  parentname,
                                  patient_col,
                                  mhterm,
                                  mhbodsys,
                                  mhdistat,
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

  # Medical history tab ----
  mhist_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(mhterm = mhterm, mhbodsys = mhbodsys, mhdistat = mhdistat),
    merge_function = "dplyr::left_join"
  )

  mhist_call <- reactive({
    validate(need(patient_id(), "Please select a patient."))

    validate(
      need(
        input[[extract_input("mhterm", dataname)]],
        "Please select MHTERM variable."
      ),
      need(
        input[[extract_input("mhbodsys", dataname)]],
        "Please select MHBODSYS variable."
      ),
      need(
        input[[extract_input("mhdistat", dataname)]],
        "Please select MHDISTAT variable."
      ),
      need(
        nrow(mhist_merged_data()$data()[mhist_merged_data()$data()[[patient_col]] == patient_id(), ]) > 0,
        "Patient has no data about medical history."
      )
    )

    mhist_stack <- chunks$new()
    mhist_stack_push <- function(...) {
      chunks_push(..., chunks = mhist_stack)
    }
    chunks_push_data_merge(mhist_merged_data(), chunks = mhist_stack)

    mhist_stack_push(substitute(
      expr = {
        ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
      }, env = list(
        patient_col = patient_col,
        patient_id = patient_id()
      )
    ))

    my_calls <- template_medical_history(
      dataname = "ANL",
      mhterm = input[[extract_input("mhterm", dataname)]],
      mhbodsys = input[[extract_input("mhbodsys", dataname)]],
      mhdistat = input[[extract_input("mhdistat", dataname)]]
    )
    lapply(my_calls, mhist_stack_push)
    chunks_safe_eval(chunks = mhist_stack)
    mhist_stack
  })

  output$medical_history_table <- reactive({
    chunks_reset()
    chunks_push_chunks(mhist_call())
    chunks_get_var("result_kbl")
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(mhterm, mhbodsys, mhdistat)),
    modal_title = label
  )
}
