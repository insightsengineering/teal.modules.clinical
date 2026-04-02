#' teal Module: Patient Profile Adverse Events Table and Plot (teal.picks)
#'
#' This module produces an adverse events table and [ggplot2::ggplot()] type plot using ADaM datasets.
#' This is the `teal.picks` implementation of [tm_g_pp_adverse_events()].
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_adverse_events
#' @param aeterm (`teal.picks::variables`)\cr variable specification for `AETERM`.
#' @param tox_grade (`teal.picks::variables`)\cr variable specification for `AETOXGR`.
#' @param causality (`teal.picks::variables`)\cr variable specification for `AEREL`.
#' @param outcome (`teal.picks::variables`)\cr variable specification for `AEOUT`.
#' @param action (`teal.picks::variables`)\cr variable specification for `AEACN`.
#' @param time (`teal.picks::variables`)\cr variable specification for `ASTDY`.
#' @param decod (`teal.picks::variables` or `NULL`)\cr variable specification for `AEDECOD`.
#'
#' @inherit module_arguments return
#'
#' @export
tm_g_pp_adverse_events.picks <- function(label,
                                         dataname = "ADAE",
                                         parentname = "ADSL",
                                         patient_col = "USUBJID",
                                         aeterm    = teal.picks::variables("AETERM",  fixed = TRUE),
                                         tox_grade = teal.picks::variables("AETOXGR", fixed = TRUE),
                                         causality = teal.picks::variables("AEREL",   fixed = TRUE),
                                         outcome   = teal.picks::variables("AEOUT",   fixed = TRUE),
                                         action    = teal.picks::variables("AEACN",   fixed = TRUE),
                                         time      = teal.picks::variables("ASTDY",   fixed = TRUE),
                                         decod     = NULL,
                                         font_size = c(12L, 12L, 25L),
                                         plot_height = c(700L, 200L, 2000L),
                                         plot_width = NULL,
                                         pre_output = NULL,
                                         post_output = NULL,
                                         ggplot2_args = teal.widgets::ggplot2_args(),
                                         transformators = list(),
                                         decorators = list()) {
  message("Initializing tm_g_pp_adverse_events.picks")

  # Compatibility: accept choices_selected and coerce
  for (arg in c("aeterm", "tox_grade", "causality", "outcome", "action", "time")) {
    val <- get(arg)
    if (inherits(val, "choices_selected")) assign(arg, teal.picks::as.picks(val))
  }
  if (!is.null(decod) && inherits(decod, "choices_selected")) {
    decod <- teal.picks::as.picks(decod)
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(aeterm,    "variables")
  checkmate::assert_class(tox_grade, "variables")
  checkmate::assert_class(causality, "variables")
  checkmate::assert_class(outcome,   "variables")
  checkmate::assert_class(action,    "variables")
  checkmate::assert_class(time,      "variables")
  checkmate::assert_class(decod, "variables", null.ok = TRUE)
  checkmate::assert_numeric(font_size, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(font_size[1], lower = font_size[2], upper = font_size[3], .var.name = "font_size")
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")
  assert_decorators(decorators, names = "plot")

  # Build picks — all vars bound to dataname
  picks_ae <- teal.picks::picks(
    teal.picks::datasets(dataname),
    aeterm    = aeterm,
    tox_grade = tox_grade,
    causality = causality,
    outcome   = outcome,
    action    = action,
    time      = time
  )
  if (!is.null(decod)) {
    picks_ae[["decod"]] <- teal.picks::picks(
      teal.picks::datasets(dataname),
      decod = decod
    )[["decod"]]
  }

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_adverse_events.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_adverse_events.picks))],
    server = srv_g_adverse_events.picks,
    server_args = args[names(args) %in% names(formals(srv_g_adverse_events.picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_adverse_events.picks <- function(id,
                                      picks_ae,
                                      decod,
                                      font_size,
                                      pre_output,
                                      post_output,
                                      decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      htmlOutput(ns("title")),
      teal.widgets::get_dt_rows(ns("table"), ns("table_rows")),
      tags$div(
        style = "overflow: auto;",
        DT::DTOutput(outputId = ns("table"))
      ),
      teal.widgets::plot_with_settings_ui(id = ns("chart"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      tags$div(
        tags$label("Select AETERM variable:"),
        teal.picks::picks_ui(ns("aeterm"), picks_ae["aeterm"])
      ),
      tags$div(
        tags$label("Select AETOXGR variable:"),
        teal.picks::picks_ui(ns("tox_grade"), picks_ae["tox_grade"])
      ),
      tags$div(
        tags$label("Select AEREL variable:"),
        teal.picks::picks_ui(ns("causality"), picks_ae["causality"])
      ),
      tags$div(
        tags$label("Select AEOUT variable:"),
        teal.picks::picks_ui(ns("outcome"), picks_ae["outcome"])
      ),
      tags$div(
        tags$label("Select AEACN variable:"),
        teal.picks::picks_ui(ns("action"), picks_ae["action"])
      ),
      tags$div(
        tags$label("Select ASTDY variable:"),
        teal.picks::picks_ui(ns("time"), picks_ae["time"])
      ),
      if (!is.null(decod)) {
        tags$div(
          tags$label("Select AEDECOD variable:"),
          teal.picks::picks_ui(ns("decod"), picks_ae["decod"])
        )
      },
      ui_decorate_teal_data(ns("d_plot"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"),
            "Font Size",
            font_size,
            ticks = FALSE, step = 1
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_adverse_events.picks <- function(id,
                                       data,
                                       dataname,
                                       parentname,
                                       patient_col,
                                       picks_ae,
                                       decod,
                                       plot_height,
                                       plot_width,
                                       label,
                                       ggplot2_args,
                                       decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    patient_id <- reactive(input$patient_id)

    # Init patient selector
    patient_data_base <- reactive(unique(data()[[parentname]][[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session, "patient_id",
      choices = patient_data_base(),
      selected = patient_data_base()[1]
    )
    observeEvent(patient_data_base(), ignoreInit = TRUE, {
      teal.widgets::updateOptionalSelectInput(
        session, "patient_id",
        choices = patient_data_base(),
        selected = if (length(patient_data_base()) == 1) {
          patient_data_base()
        } else {
          intersect(patient_id(), patient_data_base())
        }
      )
    })

    # Picks selectors
    selector_ids <- c("aeterm", "tox_grade", "causality", "outcome", "action", "time")
    if (!is.null(decod)) selector_ids <- c(selector_ids, "decod")

    selectors <- teal.picks::picks_srv(
      picks = picks_ae[selector_ids],
      data  = data
    )

    validated_q <- reactive({
      obj <- req(data())

      validate_input(
        inputId  = "patient_id",
        condition = !is.null(input$patient_id) && nzchar(input$patient_id),
        message  = "Please select a patient."
      )
      validate_input(
        inputId  = "aeterm-variables-selected",
        condition = length(selectors$aeterm()$variables$selected) > 0,
        message  = "Please select AETERM variable."
      )
      validate_input(
        inputId  = "tox_grade-variables-selected",
        condition = length(selectors$tox_grade()$variables$selected) > 0,
        message  = "Please select AETOXGR variable."
      )
      validate_input(
        inputId  = "causality-variables-selected",
        condition = length(selectors$causality()$variables$selected) > 0,
        message  = "Please select AEREL variable."
      )
      validate_input(
        inputId  = "outcome-variables-selected",
        condition = length(selectors$outcome()$variables$selected) > 0,
        message  = "Please select AEOUT variable."
      )
      validate_input(
        inputId  = "action-variables-selected",
        condition = length(selectors$action()$variables$selected) > 0,
        message  = "Please select AEACN variable."
      )
      validate_input(
        inputId  = "time-variables-selected",
        condition = length(selectors$time()$variables$selected) > 0,
        message  = "Please select ASTDY variable."
      )
      if (!is.null(decod)) {
        validate_input(
          inputId  = "decod-variables-selected",
          condition = length(selectors$decod()$variables$selected) > 0,
          message  = "Please select AEDECOD variable."
        )
      }

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's output(s)")
      )
      obj
    })

    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data       = validated_q,
      selectors  = selectors,
      output_name = "ANL"
    )

    all_q <- reactive({
      ANL <- anl_inputs$data()[["ANL"]]

      teal::validate_has_data(ANL[ANL[[patient_col]] == input$patient_id, ], min_nrow = 1)

      vars <- anl_inputs$variables()
      anl_q2 <- teal.code::eval_code(
        anl_inputs$data(),
        substitute(
          expr = {
            pt_id <- patient_id
            ANL   <- ANL[ANL[[patient_col]] == patient_id, ]
          },
          env = list(patient_col = patient_col, patient_id = patient_id())
        )
      )

      calls <- template_adverse_events(
        dataname  = "ANL",
        aeterm    = vars$aeterm,
        tox_grade = vars$tox_grade,
        causality = vars$causality,
        outcome   = vars$outcome,
        action    = vars$action,
        time      = vars$time,
        decod     = if (!is.null(decod)) vars$decod else NULL,
        patient_id = patient_id(),
        font_size  = input[["font_size"]],
        ggplot2_args = ggplot2_args
      )

      obj <- anl_q2
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table and Plot")
      teal.code::eval_code(obj, as.expression(calls))
    })

    output$title <- renderText({
      paste("<h5><b>Patient ID:", all_q()[["pt_id"]], "</b></h5>")
    })

    table_q <- reactive({
      req(all_q())
      within(all_q(), {
        table <- rtables::df_to_tt(table_data)
        table
      })
    })

    plot_q <- reactive({
      req(all_q())
      within(all_q(), plot <- plot_output)
    })

    decorated_all_q_plot <- srv_decorate_teal_data(
      "d_plot",
      data       = plot_q,
      decorators = select_decorators(decorators, "plot"),
      expr       = plot
    )

    plot_r <- reactive({
      req(decorated_all_q_plot())[["plot"]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id     = "chart",
      plot_r = plot_r,
      height = plot_height,
      width  = plot_width
    )

    table_r <- reactive({
      q <- req(table_q())
      list(
        html   = DT::datatable(q[["table_data"]], options = list(pageLength = input$table_rows)),
        report = q[["table"]]
      )
    })

    output$table <- DT::renderDataTable(table_r()[["html"]])

    decorated_all_q <- reactive(
      suppressWarnings(c(table_q(), decorated_all_q_plot()))
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
