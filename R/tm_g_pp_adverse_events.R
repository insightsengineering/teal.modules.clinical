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
                                   aeterm = NULL,
                                   tox_grade = NULL,
                                   causality = NULL,
                                   outcome = NULL,
                                   action = NULL,
                                   time = NULL,
                                   decod = NULL,
                                   font_size = c(12L, 12L, 25L),
                                   plot_height = c(700L, 200L, 2000L),
                                   plot_width = NULL,
                                   pre_output = NULL,
                                   post_output = NULL,
                                   ggplot2_args = teal.widgets::ggplot2_args(),
                                   transformators = list(),
                                   decorators = list()) {
  message("Initializing tm_g_pp_adverse_events")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(aeterm, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(tox_grade, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(causality, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(outcome, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(action, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(time, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(decod, "choices_selected", null.ok = TRUE)
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

  args <- as.list(environment())
  data_extract_list <- list(
    aeterm = `if`(is.null(aeterm), NULL, cs_to_des_select(aeterm, dataname = dataname)),
    tox_grade = `if`(is.null(tox_grade), NULL, cs_to_des_select(tox_grade, dataname = dataname)),
    causality = `if`(is.null(causality), NULL, cs_to_des_select(causality, dataname = dataname)),
    outcome = `if`(is.null(outcome), NULL, cs_to_des_select(outcome, dataname = dataname)),
    action = `if`(is.null(action), NULL, cs_to_des_select(action, dataname = dataname)),
    time = `if`(is.null(time), NULL, cs_to_des_select(time, dataname = dataname)),
    decod = `if`(is.null(decod), NULL, cs_to_des_select(decod, dataname = dataname))
  )

  module(
    label = label,
    ui = ui_g_adverse_events,
    ui_args = c(data_extract_list, args),
    server = srv_g_adverse_events,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        patient_col = patient_col,
        plot_height = plot_height,
        plot_width = plot_width,
        ggplot2_args = ggplot2_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_adverse_events <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$aeterm,
    ui_args$tox_grade,
    ui_args$causality,
    ui_args$outcome,
    ui_args$action,
    ui_args$time,
    ui_args$decod
  )

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
      teal.transform::datanames_input(ui_args[c(
        "aeterm", "tox_grade", "causality", "outcome",
        "action", "time", "decod"
      )]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("aeterm"),
        label = "Select AETERM variable:",
        data_extract_spec = ui_args$aeterm,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("tox_grade"),
        label = "Select AETOXGR variable:",
        data_extract_spec = ui_args$tox_grade,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("causality"),
        label = "Select AEREL variable:",
        data_extract_spec = ui_args$causality,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("outcome"),
        label = "Select AEOUT variable:",
        data_extract_spec = ui_args$outcome,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("action"),
        label = "Select AEACN variable:",
        data_extract_spec = ui_args$action,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("time"),
        label = "Select ASTDY variable:",
        data_extract_spec = ui_args$time,
        is_single_dataset = is_single_dataset_value
      ),
      `if`(
        is.null(ui_args$decod),
        NULL,
        teal.transform::data_extract_ui(
          id = ns("decod"),
          label = "Select DECOD variable:",
          data_extract_spec = ui_args$decod,
          is_single_dataset = is_single_dataset_value
        )
      ),
      ui_decorate_teal_data(ns("d_plot"), decorators = select_decorators(ui_args$decorators, "plot")),
      bslib::accordion_panel(
        title = "Plot settings",
        open = TRUE,
        teal.widgets::optionalSliderInputValMinMax(
          ns("font_size"),
          "Font Size",
          ui_args$font_size,
          ticks = FALSE, step = 1
        )
      )
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
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

    # Adverse events tab ----
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = Filter(
        Negate(is.null),
        list(
          aeterm = aeterm,
          tox_grade = tox_grade,
          causality = causality,
          outcome = outcome,
          action = action,
          time = time,
          decod = decod
        )
      ),
      datasets = data,
      select_validation_rule = list(
        aeterm = shinyvalidate::sv_required("Please select AETERM variable."),
        tox_grade = shinyvalidate::sv_required("Please select AETOXGR variable."),
        causality = shinyvalidate::sv_required("Please select AEREL variable."),
        outcome = shinyvalidate::sv_required("Please select AEOUT variable."),
        action = shinyvalidate::sv_required("Please select AEACN variable."),
        time = shinyvalidate::sv_required("Please select ASTDY variable."),
        decod = shinyvalidate::sv_required("Please select ANRIND variable.")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select a patient"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj %>% teal.code::eval_code(code = as.expression(anl_inputs()$expr))
    })

    all_q <- reactive({
      teal::validate_inputs(iv_r())
      anl_m <- anl_inputs()

      ANL <- anl_q()[["ANL"]]

      teal::validate_has_data(ANL[ANL[[patient_col]] == input$patient_id, ], min_nrow = 1)

      anl_q2 <- teal.code::eval_code(
        anl_q(),
        substitute(
          expr = {
            pt_id <- patient_id
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )

      calls <- template_adverse_events(
        dataname = "ANL",
        aeterm = input[[extract_input("aeterm", dataname)]],
        tox_grade = input[[extract_input("tox_grade", dataname)]],
        causality = input[[extract_input("causality", dataname)]],
        outcome = input[[extract_input("outcome", dataname)]],
        action = input[[extract_input("action", dataname)]],
        time = input[[extract_input("time", dataname)]],
        decod = input[[extract_input("decod", dataname)]],
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

    # Allow for the table and plot qenv to be joined
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
      data = plot_q,
      decorators = select_decorators(decorators, "plot"),
      expr = plot
    )

    plot_r <- reactive({
      req(iv_r()$is_valid(), decorated_all_q_plot())
      req(decorated_all_q_plot())[["plot"]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "chart",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    table_r <- reactive({
      q <- req(table_q())

      list(
        html = DT::datatable(
          data = q[["table_data"]],
          options = list(pageLength = input$table_rows)
        ),
        report = q[["table"]]
      )
    })

    output$table <- DT::renderDataTable(table_r()[["html"]])

    decorated_all_q <- reactive(
      c(table_q(), decorated_all_q_plot(), verbose = FALSE)
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
