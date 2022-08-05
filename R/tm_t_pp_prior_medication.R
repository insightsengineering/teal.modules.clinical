#' Template: Prior Medication
#'
#' Creates a prior medication template.
#'
#' @inheritParams template_arguments
#' @param atirel (`character`)\cr name of time relation of medication variable.
#' @param cmdecod (`character`)\cr name of standardized medication name variable.
#' @param cmindc (`character`)\cr name of indication variable.
#' @param cmstdy (`character`)\cr name of study day of start of medication variable.
#' @keywords internal
#'
template_prior_medication <- function(dataname = "ANL",
                                      atirel = "ATIREL",
                                      cmdecod = "CMDECOD",
                                      cmindc = "CMINDC",
                                      cmstdy = "CMSTDY") {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(atirel),
    assertthat::is.string(cmdecod),
    assertthat::is.string(cmindc),
    assertthat::is.string(cmstdy)
  )

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      result <-
        dataname %>%
        dplyr::filter(atirel %in% c("PRIOR", "PRIOR_CONCOMITANT")) %>%
        dplyr::select(cmindc, cmdecod, cmstdy) %>%
        dplyr::filter(!is.na(cmdecod)) %>%
        dplyr::distinct() %>%
        `colnames<-`(get_labels(dataname)$column_labels[c(cmindc_char, cmdecod_char, cmstdy_char)])
      result
    }, env = list(
      dataname = as.name(dataname),
      atirel = as.name(atirel),
      cmdecod = as.name(cmdecod),
      cmindc = as.name(cmindc),
      cmstdy = as.name(cmstdy),
      atirel_char = atirel,
      cmdecod_char = cmdecod,
      cmindc_char = cmindc,
      cmstdy_char = cmstdy
    ))
  )
  # Note: l_html_concomitant_adcm is still not included since one column is available out of 9

  y$table <- bracket_expr(table_list)
  y
}

#' Teal Module: Patient Prior Medication Teal Module
#'
#' This teal module produces a patient prior medication report using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param atirel ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{ATIREL} column of the ADCM dataset.
#' @param cmdecod ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMDECOD} column of the ADCM dataset.
#' @param cmindc ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMINDC} column of the ADCM dataset.
#' @param cmstdy ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMSTDY} column of the ADCM dataset.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADCM <- synthetic_cdisc_data("latest")$adcm
#'
#' #' Modify ADCM
#' ADCM$CMINDC <- paste0("Indication_", as.numeric(ADCM$CMDECOD))
#' ADCM$CMDOSE <- 1
#' ADCM$CMTRT <- ADCM$CMCAT
#' ADCM$CMDOSU <- "U"
#' ADCM$CMROUTE <- "CMROUTE"
#' ADCM$CMDOSFRQ <- "CMDOSFRQ"
#' ADCM$CMASTDTM <- ADCM$ASTDTM
#' ADCM$CMAENDTM <- ADCM$AENDTM
#' formatters::var_labels(
#'   ADCM[c("CMINDC", "CMTRT", "ASTDY")]
#' ) <- c(
#'   "Indication",
#'   "Reported Name of Drug, Med, or Therapy",
#'   "Study Day of Start of Medication"
#' )
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADCM", ADCM,
#'       code = 'ADCM <- synthetic_cdisc_data("latest")$adcm
#'       ADCM$CMINDC <- paste0("Indication_", as.numeric(ADCM$CMDECOD))
#'       ADCM$CMDOSE <- 1
#'       ADCM$CMTRT <- ADCM$CMCAT
#'       ADCM$CMDOSU <- "U"
#'       ADCM$CMROUTE <- "CMROUTE"
#'       ADCM$CMDOSFRQ <- "CMDOSFRQ"
#'       ADCM$CMASTDTM <- ADCM$ASTDTM
#'       ADCM$CMAENDTM <- ADCM$AENDTM
#'       formatters::var_labels(
#'         ADCM[c("CMINDC", "CMTRT", "ASTDY")]) <- c(
#'           "Indication",
#'           "Reported Name of Drug, Med, or Therapy",
#'           "Study Day of Start of Medication"
#'          )',
#'       keys = adcm_keys
#'     ),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_t_pp_prior_medication(
#'       label = "Prior medication",
#'       dataname = "ADCM",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       atirel = choices_selected(
#'         choices = variable_choices(ADCM, "ATIREL"),
#'         selected = "ATIREL"
#'       ),
#'       cmdecod = choices_selected(
#'         choices = variable_choices(ADCM, "CMDECOD"),
#'         selected = "CMDECOD"
#'       ),
#'       cmindc = choices_selected(
#'         choices = variable_choices(ADCM, "CMINDC"),
#'         selected = "CMINDC"
#'       ),
#'       cmstdy = choices_selected(
#'         choices = variable_choices(ADCM, "ASTDY"),
#'         selected = "ASTDY"
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_pp_prior_medication <- function(label,
                                     dataname = "ADCM",
                                     parentname = "ADSL",
                                     patient_col = "USUBJID",
                                     atirel = NULL,
                                     cmdecod = NULL,
                                     cmindc = NULL,
                                     cmstdy = NULL,
                                     pre_output = NULL,
                                     post_output = NULL) {
  logger::log_info("Initializing tm_t_pp_prior_medication")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  args <- as.list(environment())
  data_extract_list <- list(
    atirel = `if`(is.null(atirel), NULL, cs_to_des_select(atirel, dataname = dataname)),
    cmdecod = `if`(is.null(cmdecod), NULL, cs_to_des_select(cmdecod, dataname = dataname)),
    cmindc = `if`(is.null(cmindc), NULL, cs_to_des_select(cmindc, dataname = dataname)),
    cmstdy = `if`(is.null(cmstdy), NULL, cs_to_des_select(cmstdy, dataname = dataname))
  )

  module(
    label = label,
    ui = ui_t_prior_medication,
    ui_args = c(data_extract_list, args),
    server = srv_t_prior_medication,
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

ui_t_prior_medication <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$atirel,
    ui_args$cmdecod,
    ui_args$cmindc,
    ui_args$cmstdy
  )

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = shiny::div(
      teal.widgets::get_dt_rows(ns("prior_medication_table"), ns("prior_medication_table_rows")),
      DT::DTOutput(outputId = ns("prior_medication_table"))
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(ui_args[c("atirel", "cmdecod", "cmindc", "cmstdy")]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("cmdecod"),
        label = "Select the medication decoding column:",
        data_extract_spec = ui_args$cmdecod,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("atirel"),
        label = "Select ATIREL variable:",
        data_extract_spec = ui_args$atirel,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmindc"),
        label = "Select CMINDC variable:",
        data_extract_spec = ui_args$cmindc,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmstdy"),
        label = "Select CMSTDY variable:",
        data_extract_spec = ui_args$cmstdy,
        is_single_dataset = is_single_dataset_value
      )
    ),
    forms = teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code"),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}


srv_t_prior_medication <- function(id,
                                   data,
                                   reporter,
                                   filter_panel_api,
                                   dataname,
                                   parentname,
                                   patient_col,
                                   atirel,
                                   cmdecod,
                                   cmindc,
                                   cmstdy,
                                   label) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")

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

    # Prior medication tab ----
    merge_input_r <- teal.transform::merge_expression_module(
      datasets = data,
      join_keys = attr(data, "join_keys"),
      data_extract = list(atirel = atirel, cmdecod = cmdecod, cmindc = cmindc, cmstdy = cmstdy),
      merge_function = "dplyr::left_join"
    )

    merge_q_r <- reactive({
      q <- new_quosure(env = data)
      eval_code(q, as.expression(merge_input_r()$expr))
    })

    output_q <- shiny::reactive({
      shiny::validate(shiny::need(patient_id(), "Please select a patient."))

      shiny::validate(
        shiny::need(
          input[[extract_input("atirel", dataname)]],
          "Please select ATIREL variable."
        ),
        shiny::need(
          input[[extract_input("cmdecod", dataname)]],
          "Please select Medication decoding variable."
        ),
        shiny::need(
          input[[extract_input("cmindc", dataname)]],
          "Please select CMINDC variable."
        ),
        shiny::need(
          input[[extract_input("cmstdy", dataname)]],
          "Please select CMSTDY variable."
        )
      )

      my_calls <- template_prior_medication(
        dataname = "ANL",
        atirel = input[[extract_input("atirel", dataname)]],
        cmdecod = input[[extract_input("cmdecod", dataname)]],
        cmindc = input[[extract_input("cmindc", dataname)]],
        cmstdy = input[[extract_input("cmstdy", dataname)]]
      )

      eval_code(
        merge_q_r(),
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        ),
        name = "patient_id_filter_call"
      ) |>
      eval_code(as.expression(my_calls), name = "call")
    })

    table_r <- shiny::reactive(output_q()[["result"]])

    output$prior_medication_table <- DT::renderDataTable(
      expr = table_r(),
      options = list(pageLength = input$prior_medication_table_rows)
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(output_q())),
      title = label
    )

    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Patient Prior Medication Table")
        card$append_text("Patient Prior Medication Table", "header2")
        card$append_fs(filter_panel_api$get_filter_state())
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
