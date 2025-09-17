#' Template: Patient Profile Prior Medication
#'
#' Creates a valid expression to generate a patient profile prior medication report using ADaM datasets.
#'
#' @inheritParams template_arguments
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_pp_prior_medication()]
#'
#' @keywords internal
template_prior_medication <- function(dataname = "ANL",
                                      atirel = "ATIREL",
                                      cmdecod = "CMDECOD",
                                      cmindc = "CMINDC",
                                      cmstdy = "CMSTDY") {
  checkmate::assert_string(dataname)
  checkmate::assert_string(atirel)
  checkmate::assert_string(cmdecod)
  checkmate::assert_string(cmindc)
  checkmate::assert_string(cmstdy)

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      table_data <-
        dataname %>%
        dplyr::filter(atirel %in% c("PRIOR", "PRIOR_CONCOMITANT")) %>%
        dplyr::select(cmindc, cmdecod, cmstdy) %>%
        dplyr::filter(!is.na(cmdecod)) %>%
        dplyr::distinct() %>%
        `colnames<-`(teal.data::col_labels(dataname, fill = TRUE)[c(cmindc_char, cmdecod_char, cmstdy_char)])

      table <- table_data %>%
        dplyr::mutate( # Exception for columns of type difftime that is not supported by as_listing
          dplyr::across(
            dplyr::where(~ inherits(., what = "difftime")), ~ as.double(., units = "auto")
          )
        ) %>%
        rlistings::as_listing()
      table
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

#' teal Module: Patient Profile Prior Medication
#'
#' This module produces a patient profile prior medication report using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_prior_medication
#'
#' @inherit module_arguments return
#'
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' library(dplyr)
#' data <- teal_data()
#' data <- within(data, {
#'   ADCM <- tmc_ex_adcm
#'   ADSL <- tmc_ex_adsl %>% filter(USUBJID %in% ADCM$USUBJID)
#'   ADCM$CMASTDTM <- ADCM$ASTDTM
#'   ADCM$CMAENDTM <- ADCM$AENDTM
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#' join_keys(data)["ADCM", "ADCM"] <- adcm_keys
#'
#' ADSL <- data[["ADSL"]]
#' ADCM <- data[["ADCM"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_pp_prior_medication(
#'       label = "Prior Medication",
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
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_pp_prior_medication <- function(label,
                                     dataname = "ADCM",
                                     parentname = "ADSL",
                                     patient_col = "USUBJID",
                                     atirel = NULL,
                                     cmdecod = NULL,
                                     cmindc = NULL,
                                     cmstdy = NULL,
                                     pre_output = NULL,
                                     post_output = NULL,
                                     transformators = list(),
                                     decorators = lifecycle::deprecated()) {
  if (lifecycle::is_present(decorators)) {
    lifecycle::deprecate_warn(
      when = "0.11.0",
      what = "tm_t_pp_laboratory(decorators)",
      details = "Decorators functionality was removed from this module. The `decorators` argument will be ignored."
    )
  }


  message("Initializing tm_t_pp_prior_medication")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(atirel, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(cmdecod, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(cmindc, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(cmstdy, "choices_selected", null.ok = TRUE)
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
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_t_prior_medication <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$atirel,
    ui_args$cmdecod,
    ui_args$cmindc,
    ui_args$cmstdy
  )

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      DT::DTOutput(outputId = ns("prior_medication_table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
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
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

#' @keywords internal
srv_t_prior_medication <- function(id,
                                   data,
                                   dataname,
                                   parentname,
                                   patient_col,
                                   atirel,
                                   cmdecod,
                                   cmindc,
                                   cmstdy,
                                   label) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    patient_id <- reactive(input$patient_id)

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

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select patient id"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    # Init
    patient_data_base <- reactive(unique(data()[[parentname]][[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session,
      "patient_id",
      choices = patient_data_base(),
      selected = patient_data_base()[1]
    )

    observeEvent(patient_data_base(),
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
      merge_function = "dplyr::left_join"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Patient Prior Medication Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    # Generate r code for the analysis.
    all_q <- reactive({
      teal::validate_inputs(iv_r())

      my_calls <- template_prior_medication(
        dataname = "ANL",
        atirel = input[[extract_input("atirel", dataname)]],
        cmdecod = input[[extract_input("cmdecod", dataname)]],
        cmindc = input[[extract_input("cmindc", dataname)]],
        cmstdy = input[[extract_input("cmstdy", dataname)]]
      )

      obj <- anl_q() %>%
        teal.code::eval_code(
          substitute(
            expr = {
              ANL <- ANL[ANL[[patient_col]] == patient_id, ]
            }, env = list(
              patient_col = patient_col,
              patient_id = patient_id()
            )
          )
        )
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
      obj %>% teal.code::eval_code(as.expression(unlist(my_calls)))
    })

    # Outputs to render.
    table_r <- reactive({
      q <- all_q()

      table_html <- DT::datatable(
        data = q[["table"]],
        options = list(
          lengthMenu = list(list(-1, 5, 10, 25), list("All", "5", "10", "25"))
        )
      )

      list(html = table_html, report = q[["table"]])
    })

    output$prior_medication_table <- DT::renderDataTable(expr = table_r()$html)

    all_q
  })
}
