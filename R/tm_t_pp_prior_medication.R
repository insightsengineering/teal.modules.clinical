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
#' This teal module produces a patient prior medication report using `ADaM` datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param atirel ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{ATIREL} column of the `ADCM` dataset.
#' @param cmdecod ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMDECOD} column of the `ADCM` dataset.
#' @param cmindc ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMINDC} column of the `ADCM` dataset.
#' @param cmstdy ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMSTDY} column of the `ADCM` dataset.
#'
#' @export
#'
#' @examples
#' adcm <- tmc_ex_adcm
#' adsl <- tmc_ex_adsl %>% dplyr::filter(USUBJID %in% adcm$USUBJID)
#' adcm$CMASTDTM <- adcm$ASTDTM
#' adcm$CMAENDTM <- adcm$AENDTM
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADCM", adcm, keys = adcm_keys)
#'   ),
#'   modules = modules(
#'     tm_t_pp_prior_medication(
#'       label = "Prior Medication",
#'       dataname = "ADCM",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       atirel = choices_selected(
#'         choices = variable_choices(adcm, "ATIREL"),
#'         selected = "ATIREL"
#'       ),
#'       cmdecod = choices_selected(
#'         choices = variable_choices(adcm, "CMDECOD"),
#'         selected = "CMDECOD"
#'       ),
#'       cmindc = choices_selected(
#'         choices = variable_choices(adcm, "CMINDC"),
#'         selected = "CMINDC"
#'       ),
#'       cmstdy = choices_selected(
#'         choices = variable_choices(adcm, "ASTDY"),
#'         selected = "ASTDY"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
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
    datanames = "all"
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
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
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
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    patient_id <- shiny::reactive(input$patient_id)

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        atirel = atirel,
        cmdecod = cmdecod,
        cmindc = cmindc,
        cmstdy = cmstdy
      ),
      datasets = data,
      select_validation_rule = list(
        atirel = shinyvalidate::sv_required("An ATIREL variable is required"),
        cmdecod = shinyvalidate::sv_required("A medication decoding variable is required"),
        cmindc = shinyvalidate::sv_required("A CMINDC variable is required"),
        cmstdy = shinyvalidate::sv_required("A CMSTDY variable is required")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select patient id"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

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
    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      join_keys = get_join_keys(data),
      merge_function = "dplyr::left_join"
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    all_q <- shiny::reactive({
      teal::validate_inputs(iv_r())

      my_calls <- template_prior_medication(
        dataname = "ANL",
        atirel = input[[extract_input("atirel", dataname)]],
        cmdecod = input[[extract_input("cmdecod", dataname)]],
        cmindc = input[[extract_input("cmindc", dataname)]],
        cmstdy = input[[extract_input("cmstdy", dataname)]]
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
        teal.code::eval_code(as.expression(my_calls))
    })

    table_r <- shiny::reactive(all_q()[["result"]])

    output$prior_medication_table <- DT::renderDataTable(
      expr = table_r(),
      options = list(
        lengthMenu = list(list(-1, 5, 10, 25), list("All", "5", "10", "25"))
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

    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Patient Prior Medication Table")
        card$append_text("Patient Prior Medication Table", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
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
