#' teal Module: Patient Profile Laboratory Table (teal.picks)
#'
#' This module produces a patient profile laboratory table using ADaM datasets.
#' This is the `teal.picks` variant of [tm_t_pp_laboratory()].
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_laboratory
#' @param timepoints (`teal.picks::variables`)\cr variable specification for the
#'   time variable from `dataname`.
#' @param aval_var (`teal.picks::variables`)\cr variable specification for the
#'   `AVAL` variable from `dataname`.
#' @param avalu_var (`teal.picks::variables`)\cr variable specification for the
#'   `AVALU` variable from `dataname`.
#' @param param (`teal.picks::variables`)\cr variable specification for the
#'   `PARAM` variable from `dataname`.
#' @param paramcd (`teal.picks::variables`)\cr variable specification for the
#'   `PARAMCD` variable from `dataname`.
#' @param anrind (`teal.picks::variables`)\cr variable specification for the
#'   `ANRIND` variable from `dataname`.
#'
#' @inherit module_arguments return seealso
#'
#' @keywords internal
tm_t_pp_laboratory.picks <- function(label,
                                     dataname = "ADLB",
                                     parentname = "ADSL",
                                     patient_col = "USUBJID",
                                     timepoints = teal.picks::variables("ADY",    fixed = TRUE),
                                     aval_var   = teal.picks::variables("AVAL",   fixed = TRUE),
                                     avalu_var  = teal.picks::variables("AVALU",  fixed = TRUE),
                                     param      = teal.picks::variables("PARAM",  fixed = TRUE),
                                     paramcd    = teal.picks::variables("PARAMCD", fixed = TRUE),
                                     anrind     = teal.picks::variables("ANRIND", fixed = TRUE),
                                     pre_output = NULL,
                                     post_output = NULL,
                                     transformators = list()) {
  message("Initializing tm_t_pp_laboratory.picks")

  # Compatibility: accept choices_selected and convert
  for (arg in c("timepoints", "aval_var", "avalu_var", "param", "paramcd", "anrind")) {
    val <- get(arg)
    if (inherits(val, "choices_selected")) {
      assign(arg, teal.picks::as.picks(val))
    }
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(timepoints, "variables", null.ok = TRUE)
  checkmate::assert_class(aval_var,   "variables", null.ok = TRUE)
  checkmate::assert_class(avalu_var,  "variables", null.ok = TRUE)
  checkmate::assert_class(param,      "variables", null.ok = TRUE)
  checkmate::assert_class(paramcd,    "variables", null.ok = TRUE)
  checkmate::assert_class(anrind,     "variables", null.ok = TRUE)
  checkmate::assert_class(pre_output,  classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  # Build picks bound to the dataset
  if (!is.null(timepoints)) timepoints <- teal.picks::picks(datasets(dataname), timepoints)
  if (!is.null(aval_var))   aval_var   <- teal.picks::picks(datasets(dataname), aval_var)
  if (!is.null(avalu_var))  avalu_var  <- teal.picks::picks(datasets(dataname), avalu_var)
  if (!is.null(param))      param      <- teal.picks::picks(datasets(dataname), param)
  if (!is.null(paramcd))    paramcd    <- teal.picks::picks(datasets(dataname), paramcd)
  if (!is.null(anrind))     anrind     <- teal.picks::picks(datasets(dataname), anrind)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_laboratory.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_laboratory.picks))],
    server = srv_g_laboratory.picks,
    server_args = args[names(args) %in% names(formals(srv_g_laboratory.picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_laboratory.picks <- function(id,
                                  timepoints,
                                  aval_var,
                                  avalu_var,
                                  param,
                                  paramcd,
                                  anrind,
                                  pre_output,
                                  post_output) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      htmlOutput(ns("title")),
      DT::DTOutput(outputId = ns("lab_values_table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      if (!is.null(paramcd)) tags$div(
        tags$label("Select PARAMCD variable:"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      if (!is.null(param)) tags$div(
        tags$label("Select PARAM variable:"),
        teal.picks::picks_ui(ns("param"), param)
      ),
      if (!is.null(timepoints)) tags$div(
        tags$label("Select timepoints variable:"),
        teal.picks::picks_ui(ns("timepoints"), timepoints)
      ),
      if (!is.null(aval_var)) tags$div(
        tags$label("Select AVAL variable:"),
        teal.picks::picks_ui(ns("aval_var"), aval_var)
      ),
      if (!is.null(avalu_var)) tags$div(
        tags$label("Select AVALU variable:"),
        teal.picks::picks_ui(ns("avalu_var"), avalu_var)
      ),
      if (!is.null(anrind)) tags$div(
        tags$label("Select ANRIND variable:"),
        teal.picks::picks_ui(ns("anrind"), anrind)
      ),
      selectInput(
        inputId = ns("round_value"),
        label = "Select number of decimal places for rounding:",
        choices = NULL
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_laboratory.picks <- function(id,
                                   data,
                                   dataname,
                                   parentname,
                                   patient_col,
                                   timepoints,
                                   aval_var,
                                   avalu_var,
                                   param,
                                   paramcd,
                                   anrind,
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

    # Populate round_value choices from the aval column once data is available.
    # The aval column name comes from the default/fixed picks selection.
    observeEvent(data(), once = TRUE, {
      aval_col <- isolate(aval_var)$variables$selected %||%
        teal.picks::picks_default(aval_var)
      if (!is.null(aval_col) && aval_col %in% names(data()[[dataname]])) {
        aval_values <- data()[[dataname]][[aval_col]]
        decimal_nums <- aval_values[!is.na(aval_values) & trunc(aval_values) != aval_values]
        if (length(decimal_nums) > 0) {
          max_decimal <- max(nchar(gsub("([0-9]+).([0-9]+)", "\\2", as.character(decimal_nums))))
        } else {
          max_decimal <- 4L
        }
        updateSelectInput(
          session,
          "round_value",
          choices = seq(0L, max_decimal),
          selected = min(4L, max_decimal)
        )
      }
    })

    # Build selector list — only include non-NULL picks
    picks_list <- Filter(Negate(is.null), list(
      timepoints = timepoints,
      aval_var   = aval_var,
      avalu_var  = avalu_var,
      param      = param,
      paramcd    = paramcd,
      anrind     = anrind
    ))

    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId = "timepoints-variables-selected",
        condition = !is.null(selectors$timepoints()$variables$selected),
        message = "Please select timepoints variable."
      )
      teal:::validate_input(
        inputId = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message = "Please select AVAL variable."
      )
      teal:::validate_input(
        inputId = "avalu_var-variables-selected",
        condition = !is.null(selectors$avalu_var()$variables$selected),
        message = "Please select AVALU variable."
      )
      teal:::validate_input(
        inputId = "param-variables-selected",
        condition = !is.null(selectors$param()$variables$selected),
        message = "Please select PARAM variable."
      )
      teal:::validate_input(
        inputId = "paramcd-variables-selected",
        condition = !is.null(selectors$paramcd()$variables$selected),
        message = "Please select PARAMCD variable."
      )
      teal:::validate_input(
        inputId = "anrind-variables-selected",
        condition = !is.null(selectors$anrind()$variables$selected),
        message = "Please select ANRIND variable."
      )
      teal:::validate_input(
        inputId = "patient_id",
        condition = !is.null(input$patient_id) && length(input$patient_id) > 0,
        message = "Please select a patient"
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

      labor_calls <- template_laboratory(
        dataname   = "ANL",
        timepoints = anl_inputs$variables()$timepoints,
        aval_var   = anl_inputs$variables()$aval_var,
        avalu_var  = anl_inputs$variables()$avalu_var,
        param      = anl_inputs$variables()$param,
        paramcd    = anl_inputs$variables()$paramcd,
        anrind     = anl_inputs$variables()$anrind,
        patient_id = patient_id(),
        round_value = as.integer(input$round_value)
      )

      obj <- teal.code::eval_code(
        obj,
        substitute(
          expr = {
            pt_id <- patient_id
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          },
          env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      obj <- obj |> teal.code::eval_code(as.expression(labor_calls))
      # Remove table_data_html — only needed for the display, not the report
      teal.reporter::teal_card(obj) <- utils::head(teal.reporter::teal_card(obj), -3)
      obj
    })

    output$title <- renderText({
      req(all_q())
      paste("<h5><b>Patient ID:", all_q()[["pt_id"]], "</b></h5>")
    })

    output$lab_values_table <- DT::renderDataTable(
      expr = {
        q <- req(all_q())
        table_html <- DT::datatable(
          data = q[["table_data_html"]],
          escape = FALSE,
          options = list(
            lengthMenu = list(list(-1, 5, 10, 25), list("All", "5", "10", "25")),
            scrollX = TRUE
          )
        )
        table_html$dependencies <- c(
          table_html$dependencies,
          list(rmarkdown::html_dependency_bootstrap("default"))
        )
        table_html
      }
    )

    all_q
  })
}
