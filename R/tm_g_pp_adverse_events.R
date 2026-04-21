#' Template: Patient Profile Adverse Events Table and Plot
#'
#' Creates a valid expression to generate an adverse events table and [ggplot2::ggplot()] plot using ADaM datasets.
#'
#' @inheritParams template_arguments
#' @param aeterm (`character`)\cr name of the reported term for the adverse event variable.
#' @param tox_grade (`character`)\cr name of the standard toxicity grade variable.
#' @param causality (`character`)\cr name of the causality variable.
#' @param outcome (`character`)\cr name of outcome of adverse event variable.
#' @param action (`character`)\cr name of action taken with study treatment variable.
#' @param time (`character`)\cr name of study day of start of adverse event variable.
#' @param decod (`character`)\cr name of dictionary derived term variable.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_pp_adverse_events()]
#'
#' @keywords internal
template_adverse_events <- function(dataname = "ANL",
                                    aeterm = "AETERM",
                                    tox_grade = "AETOXGR",
                                    causality = "AEREL",
                                    outcome = "AEOUT",
                                    action = "AEACN",
                                    time = "ASTDY",
                                    decod = NULL,
                                    patient_id,
                                    font_size = 12L,
                                    ggplot2_args = teal.widgets::ggplot2_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(aeterm)
  checkmate::assert_string(tox_grade)
  checkmate::assert_string(causality)
  checkmate::assert_string(outcome)
  checkmate::assert_string(action)
  checkmate::assert_string(time, null.ok = TRUE)
  checkmate::assert_string(decod, null.ok = TRUE)
  checkmate::assert_string(patient_id)
  checkmate::assert_number(font_size)

  y <- list()

  y$table <- list()
  y$chart <- list()

  table_list <- add_expr(
    list(),
    substitute(
      expr = {
        table_data <- dataname %>%
          dplyr::select(
            aeterm, tox_grade, causality, outcome, action, time, decod
          ) %>%
          dplyr::arrange(dplyr::desc(tox_grade)) %>%
          `colnames<-`(teal.data::col_labels(dataname, fill = TRUE)[vars]) %>%
          dplyr::mutate( # Exception for columns of type difftime that is not supported by as_listing
            dplyr::across(
              dplyr::where(~ inherits(., what = "difftime")), ~ as.double(., units = "auto")
            )
          )
      },
      env = list(
        dataname = as.name(dataname),
        aeterm = as.name(aeterm),
        tox_grade = as.name(tox_grade),
        causality = as.name(causality),
        outcome = as.name(outcome),
        action = as.name(action),
        time = as.name(time),
        decod = `if`(is.null(decod), NULL, as.name(decod)),
        vars = c(aeterm, tox_grade, causality, outcome, action, time, decod),
        patient_id = patient_id
      )
    )
  )

  parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
    teal.widgets::resolve_ggplot2_args(
      user_plot = ggplot2_args,
      module_plot = teal.widgets::ggplot2_args(
        labs = list(y = "Adverse Events", title = paste0("Patient ID: ", patient_id)),
        theme = list(
          text = substitute(ggplot2::element_text(size = font), list(font = font_size[1])),
          axis.text.y = quote(ggplot2::element_blank()),
          axis.ticks.y = quote(ggplot2::element_blank()),
          legend.position = "right",
          panel.grid.minor = quote(ggplot2::element_line(
            linewidth = 0.5,
            linetype = "dotted",
            colour = "grey"
          )),
          panel.grid.major = quote(ggplot2::element_line(
            linewidth = 0.5,
            linetype = "dotted",
            colour = "grey"
          ))
        )
      )
    )
  )

  chart_list <- add_expr(
    list(),
    substitute(
      expr = plot_output <- dataname %>%
        dplyr::select(aeterm, time, tox_grade, causality) %>%
        dplyr::mutate(ATOXGR = as.character(tox_grade)) %>%
        dplyr::arrange(dplyr::desc(ATOXGR)) %>%
        dplyr::mutate(ATOXGR = dplyr::case_when(
          ATOXGR == "." ~ "UNKNOWN",
          TRUE ~ ATOXGR
        )) %>%
        ggplot2::ggplot(ggplot2::aes(
          fill = ATOXGR, color = aeterm, y = aeterm, x = time
        )) +
        ggrepel::geom_label_repel(
          ggplot2::aes(label = aeterm),
          color = "black",
          hjust = "right",
          size = font_size_var[1] / 3.5,
          show.legend = FALSE
        ) +
        ggplot2::scale_fill_manual(values = c(
          "1" = "#E2264633",
          "2" = "#E2264666",
          "3" = "#E2264699",
          "4" = "#E22646CC",
          "5" = "#E22646FF",
          "UNKNOWN" = "#ACADB1FF"
        )) +
        ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 1.2)) +
        ggplot2::xlim(1, 1.2 * max(dataname[[time_var]])) +
        ggplot2::geom_point(color = "black", size = 2, shape = 24, position = ggplot2::position_nudge(y = -0.15)) +
        labs +
        themes,
      env = list(
        dataname = as.name(dataname),
        aeterm = as.name(aeterm),
        time = as.name(time),
        tox_grade = as.name(tox_grade),
        causality = as.name(causality),
        time_var = time,
        font_size_var = font_size,
        patient_id = patient_id,
        labs = parsed_ggplot2_args$labs,
        themes = parsed_ggplot2_args$theme
      )
    )
  )

  y$table <- bracket_expr(table_list)
  y$chart <- bracket_expr(chart_list)

  y
}

#' teal Module: Patient Profile Adverse Events Table and Plot
#'
#' This module produces an adverse events table and [ggplot2::ggplot()] type plot using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_adverse_events
#' @param aeterm ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `AETERM` variable from `dataname`.
#' @param tox_grade ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `AETOXGR` variable from `dataname`.
#' @param causality ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `AEREL` variable from `dataname`.
#' @param outcome ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `AEOUT` variable from `dataname`.
#' @param action ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `AEACN` variable from `dataname`.
#' @param time ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `ASTDY` variable from `dataname`.
#' @param decod ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `AEDECOD` variable from `dataname`.
#'
#' @inherit module_arguments return
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators::
#' - `plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_pp_adverse_events(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied only to `plot` output
#'    )
#' )
#' ```
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.clinical")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' library(nestcolor)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   library(teal.modules.clinical)
#'   library(dplyr)
#'   ADAE <- tmc_ex_adae
#'   ADSL <- filter(tmc_ex_adsl, USUBJID %in% ADAE$USUBJID)
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADAE <- data[["ADAE"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_pp_adverse_events(
#'       label = "Adverse Events",
#'       dataname = "ADAE",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       aeterm = choices_selected(
#'         choices = variable_choices(ADAE, "AETERM"),
#'         selected = "AETERM"
#'       ),
#'       tox_grade = choices_selected(
#'         choices = variable_choices(ADAE, "AETOXGR"),
#'         selected = "AETOXGR"
#'       ),
#'       causality = choices_selected(
#'         choices = variable_choices(ADAE, "AEREL"),
#'         selected = "AEREL"
#'       ),
#'       outcome = choices_selected(
#'         choices = variable_choices(ADAE, "AEOUT"),
#'         selected = "AEOUT"
#'       ),
#'       action = choices_selected(
#'         choices = variable_choices(ADAE, "AEACN"),
#'         selected = "AEACN"
#'       ),
#'       time = choices_selected(
#'         choices = variable_choices(ADAE, "ASTDY"),
#'         selected = "ASTDY"
#'       ),
#'       decod = NULL
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_pp_adverse_events <- function(label,
                                   dataname = "ADAE",
                                   parentname = "ADSL",
                                   patient_col = "USUBJID",
                                   aeterm = teal.picks::variables("AETERM"),
                                   tox_grade = teal.picks::variables("AETOXGR"),
                                   causality = teal.picks::variables("AEREL"),
                                   outcome = teal.picks::variables("AEOUT"),
                                   action = teal.picks::variables("AEACN"),
                                   time = teal.picks::variables("ASTDY"),
                                   decod = NULL,
                                   font_size = c(12L, 12L, 25L),
                                   plot_height = c(700L, 200L, 2000L),
                                   plot_width = NULL,
                                   pre_output = NULL,
                                   post_output = NULL,
                                   ggplot2_args = teal.widgets::ggplot2_args(),
                                   transformators = list(),
                                   decorators = list()) {
  message("Initializing tm_g_pp_adverse_events.picks")

  aeterm <- deprecate_pick_variables_arg(aeterm, "aeterm", TRUE)
  tox_grade <- deprecate_pick_variables_arg(tox_grade, "tox_grade", TRUE)
  causality <- deprecate_pick_variables_arg(causality, "causality", TRUE)
  outcome <- deprecate_pick_variables_arg(outcome, "outcome", TRUE)
  action <- deprecate_pick_variables_arg(action, "action", TRUE)
  time <- deprecate_pick_variables_arg(time, "time", TRUE)
  decod <- deprecate_pick_variables_arg(decod, "decod", TRUE)

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(aeterm, "variables", null.ok = TRUE)
  checkmate::assert_class(tox_grade, "variables", null.ok = TRUE)
  checkmate::assert_class(causality, "variables", null.ok = TRUE)
  checkmate::assert_class(outcome, "variables", null.ok = TRUE)
  checkmate::assert_class(action, "variables", null.ok = TRUE)
  checkmate::assert_class(time, "variables", null.ok = TRUE)
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
  aeterm <- if (!is.null(aeterm)) teal.picks::picks(teal.picks::datasets(dataname, dataname), aeterm)
  tox_grade <- if (!is.null(tox_grade)) teal.picks::picks(teal.picks::datasets(dataname, dataname), tox_grade)
  causality <- if (!is.null(causality)) teal.picks::picks(teal.picks::datasets(dataname, dataname), causality)
  outcome <- if (!is.null(outcome)) teal.picks::picks(teal.picks::datasets(dataname, dataname), outcome)
  action <- if (!is.null(action)) teal.picks::picks(teal.picks::datasets(dataname, dataname), action)
  time <- if (!is.null(time)) teal.picks::picks(teal.picks::datasets(dataname, dataname), time)
  decod <- if (!is.null(decod)) teal.picks::picks(teal.picks::datasets(dataname, dataname), decod)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_adverse_events,
    ui_args = args[names(args) %in% names(formals(ui_g_adverse_events))],
    server = srv_g_adverse_events,
    server_args = args[names(args) %in% names(formals(srv_g_adverse_events))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_adverse_events <- function(id,
                                aeterm,
                                tox_grade,
                                causality,
                                outcome,
                                action,
                                time,
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
        teal.picks::picks_ui(ns("aeterm"), aeterm)
      ),
      tags$div(
        tags$label("Select AETOXGR variable:"),
        teal.picks::picks_ui(ns("tox_grade"), tox_grade)
      ),
      tags$div(
        tags$label("Select AEREL variable:"),
        teal.picks::picks_ui(ns("causality"), causality)
      ),
      tags$div(
        tags$label("Select AEOUT variable:"),
        teal.picks::picks_ui(ns("outcome"), outcome)
      ),
      tags$div(
        tags$label("Select AEACN variable:"),
        teal.picks::picks_ui(ns("action"), action)
      ),
      tags$div(
        tags$label("Select ASTDY variable:"),
        teal.picks::picks_ui(ns("time"), time)
      ),
      if (!is.null(decod)) {
        tags$div(
          tags$label("Select AEDECOD variable:"),
          teal.picks::picks_ui(ns("decod"), decod)
        )
      },
      teal::ui_transform_teal_data(ns("d_plot"), transformators = select_decorators(decorators, "plot")),
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
srv_g_adverse_events <- function(id,
                                 data,
                                 dataname,
                                 parentname,
                                 patient_col,
                                 aeterm,
                                 tox_grade,
                                 causality,
                                 outcome,
                                 action,
                                 time,
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
    selectors <- teal.picks::picks_srv(
      picks = list(
        aeterm = aeterm,
        tox_grade = tox_grade,
        causality = causality,
        outcome = outcome,
        action = action,
        time = time,
        decod = decod
      ),
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      validate_input(
        inputId = "patient_id",
        condition = !is.null(input$patient_id) && nzchar(input$patient_id),
        message = "Please select a patient."
      )
      validate_input(
        inputId = "aeterm-variables-selected",
        condition = length(selectors$aeterm()$variables$selected) > 0,
        message = "Please select AETERM variable."
      )
      validate_input(
        inputId = "tox_grade-variables-selected",
        condition = length(selectors$tox_grade()$variables$selected) > 0,
        message = "Please select AETOXGR variable."
      )
      validate_input(
        inputId = "causality-variables-selected",
        condition = length(selectors$causality()$variables$selected) > 0,
        message = "Please select AEREL variable."
      )
      validate_input(
        inputId = "outcome-variables-selected",
        condition = length(selectors$outcome()$variables$selected) > 0,
        message = "Please select AEOUT variable."
      )
      validate_input(
        inputId = "action-variables-selected",
        condition = length(selectors$action()$variables$selected) > 0,
        message = "Please select AEACN variable."
      )
      validate_input(
        inputId = "time-variables-selected",
        condition = length(selectors$time()$variables$selected) > 0,
        message = "Please select ASTDY variable."
      )
      if (!is.null(decod)) {
        validate_input(
          inputId = "decod-variables-selected",
          condition = length(selectors$decod()$variables$selected) > 0,
          message = "Please select AEDECOD variable."
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
      data = validated_q,
      selectors = selectors,
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
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          },
          env = list(patient_col = patient_col, patient_id = patient_id())
        )
      )

      calls <- template_adverse_events(
        dataname = "ANL",
        aeterm = vars$aeterm,
        tox_grade = vars$tox_grade,
        causality = vars$causality,
        outcome = vars$outcome,
        action = vars$action,
        time = vars$time,
        decod = if (!is.null(decod)) vars$decod else NULL,
        patient_id = patient_id(),
        font_size = input[["font_size"]],
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

    decorated_all_q_plot <- teal::srv_transform_teal_data(
      "d_plot",
      data = plot_q,
      transformators = select_decorators(decorators, "plot"),
      expr = quote(plot)
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
