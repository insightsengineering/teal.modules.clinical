#' teal Module: Patient Profile Prior Medication (teal.picks)
#'
#' This module produces a patient profile prior medication report using ADaM datasets.
#' This is the `teal.picks` variant of [tm_t_pp_prior_medication()].
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_prior_medication
#' @param atirel (`teal.picks::variables`)\cr variable specification for the
#'   `ATIREL` variable from `dataname`.
#' @param cmdecod (`teal.picks::variables`)\cr variable specification for the
#'   `CMDECOD` (medication decoding) variable from `dataname`.
#' @param cmindc (`teal.picks::variables`)\cr variable specification for the
#'   `CMINDC` variable from `dataname`.
#' @param cmstdy (`teal.picks::variables`)\cr variable specification for the
#'   `CMSTDY` variable from `dataname`.
#'
#' @inherit module_arguments return seealso
#'
#' @keywords internal
tm_t_pp_prior_medication.picks <- function(label,
                                           dataname = "ADCM",
                                           parentname = "ADSL",
                                           patient_col = "USUBJID",
                                           atirel = teal.picks::variables("ATIREL", fixed = TRUE),
                                           cmdecod = teal.picks::variables("CMDECOD", fixed = TRUE),
                                           cmindc = teal.picks::variables("CMINDC", fixed = TRUE),
                                           cmstdy = teal.picks::variables("ASTDY", fixed = TRUE),
                                           pre_output = NULL,
                                           post_output = NULL,
                                           transformators = list()) {
  message("Initializing tm_t_pp_prior_medication.picks")

  # Compatibility: accept choices_selected and convert
  for (arg in c("atirel", "cmdecod", "cmindc", "cmstdy")) {
    val <- get(arg)
    if (inherits(val, "choices_selected")) {
      assign(arg, teal.picks::as.picks(val))
    }
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(atirel, "variables", null.ok = TRUE)
  checkmate::assert_class(cmdecod, "variables", null.ok = TRUE)
  checkmate::assert_class(cmindc, "variables", null.ok = TRUE)
  checkmate::assert_class(cmstdy, "variables", null.ok = TRUE)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  # Build picks bound to the dataset
  if (!is.null(atirel))  atirel  <- teal.picks::picks(datasets(dataname), atirel)
  if (!is.null(cmdecod)) cmdecod <- teal.picks::picks(datasets(dataname), cmdecod)
  if (!is.null(cmindc))  cmindc  <- teal.picks::picks(datasets(dataname), cmindc)
  if (!is.null(cmstdy))  cmstdy  <- teal.picks::picks(datasets(dataname), cmstdy)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_prior_medication.picks,
    ui_args = args[names(args) %in% names(formals(ui_t_prior_medication.picks))],
    server = srv_t_prior_medication.picks,
    server_args = args[names(args) %in% names(formals(srv_t_prior_medication.picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_t_prior_medication.picks <- function(id,
                                        atirel,
                                        cmdecod,
                                        cmindc,
                                        cmstdy,
                                        pre_output,
                                        post_output) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      DT::DTOutput(outputId = ns("prior_medication_table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      if (!is.null(cmdecod)) tags$div(
        tags$label("Select the medication decoding column:"),
        teal.picks::picks_ui(ns("cmdecod"), cmdecod)
      ),
      if (!is.null(atirel)) tags$div(
        tags$label("Select ATIREL variable:"),
        teal.picks::picks_ui(ns("atirel"), atirel)
      ),
      if (!is.null(cmindc)) tags$div(
        tags$label("Select CMINDC variable:"),
        teal.picks::picks_ui(ns("cmindc"), cmindc)
      ),
      if (!is.null(cmstdy)) tags$div(
        tags$label("Select CMSTDY variable:"),
        teal.picks::picks_ui(ns("cmstdy"), cmstdy)
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_t_prior_medication.picks <- function(id,
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

    # Patient selector initialisation
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

    # Build selector list — only include non-NULL picks
    picks_list <- Filter(Negate(is.null), list(
      atirel  = atirel,
      cmdecod = cmdecod,
      cmindc  = cmindc,
      cmstdy  = cmstdy
    ))

    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId = "cmdecod-variables-selected",
        condition = !is.null(selectors$cmdecod()$variables$selected),
        message = "A medication decoding variable is required"
      )
      teal:::validate_input(
        inputId = "atirel-variables-selected",
        condition = !is.null(selectors$atirel()$variables$selected),
        message = "An ATIREL variable is required"
      )
      teal:::validate_input(
        inputId = "cmindc-variables-selected",
        condition = !is.null(selectors$cmindc()$variables$selected),
        message = "A CMINDC variable is required"
      )
      teal:::validate_input(
        inputId = "cmstdy-variables-selected",
        condition = !is.null(selectors$cmstdy()$variables$selected),
        message = "A CMSTDY variable is required"
      )
      teal:::validate_input(
        inputId = "patient_id",
        condition = !is.null(input$patient_id) && length(input$patient_id) > 0,
        message = "Please select patient id"
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's output(s)")
      )
      obj
    })

    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data = validated_q,
      selectors = selectors,
      join_fun = "dplyr::left_join",
      output_name = "ANL"
    )

    all_q <- reactive({
      obj <- anl_inputs$data()

      my_calls <- template_prior_medication(
        dataname = "ANL",
        atirel  = anl_inputs$variables()$atirel,
        cmdecod = anl_inputs$variables()$cmdecod,
        cmindc  = anl_inputs$variables()$cmindc,
        cmstdy  = anl_inputs$variables()$cmstdy
      )

      obj <- teal.code::eval_code(
        obj,
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          },
          env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      obj |> teal.code::eval_code(as.expression(unlist(my_calls)))
    })

    output$prior_medication_table <- DT::renderDataTable(
      expr = DT::datatable(
        data = all_q()[["table"]],
        options = list(
          lengthMenu = list(list(-1, 5, 10, 25), list("All", "5", "10", "25"))
        )
      )
    )

    all_q
  })
}
