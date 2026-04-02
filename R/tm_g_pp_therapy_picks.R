#' teal Module: Patient Profile Therapy Table and Plot (teal.picks implementation)
#'
#' This module produces a patient profile therapy table and [ggplot2::ggplot()] type plot using ADaM datasets.
#' This is the `teal.picks` re-implementation of [tm_g_pp_therapy()].
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_therapy
#' @param atirel ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `ATIREL` variable from `dataname`.
#' @param cmdecod ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `CMDECOD` variable from `dataname`.
#' @param cmindc ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `CMINDC` variable from `dataname`.
#' @param cmdose ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `CMDOSE` variable from `dataname`.
#' @param cmtrt ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `CMTRT` variable from `dataname`.
#' @param cmdosu ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `CMDOSU` variable from `dataname`.
#' @param cmroute ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `CMROUTE` variable from `dataname`.
#' @param cmdosfrq ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `CMDOSFRQ` variable from `dataname`.
#' @param cmstdy ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `CMSTDY` variable from `dataname`.
#' @param cmendy ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `CMENDY` variable from `dataname`.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_pp_therapy.picks(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied only to `plot` output
#'    )
#' )
#' ```
#'
#' @export
tm_g_pp_therapy.picks <- function(label,
                                  dataname = "ADCM",
                                  parentname = "ADSL",
                                  patient_col = "USUBJID",
                                  atirel = teal.picks::variables("ATIREL"),
                                  cmdecod = teal.picks::variables("CMDECOD"),
                                  cmindc = teal.picks::variables("CMINDC"),
                                  cmdose = teal.picks::variables("CMDOSE"),
                                  cmtrt = teal.picks::variables("CMTRT"),
                                  cmdosu = teal.picks::variables("CMDOSU"),
                                  cmroute = teal.picks::variables("CMROUTE"),
                                  cmdosfrq = teal.picks::variables("CMDOSFRQ"),
                                  cmstdy = teal.picks::variables("CMSTDY"),
                                  cmendy = teal.picks::variables("CMENDY"),
                                  font_size = c(12L, 12L, 25L),
                                  plot_height = c(700L, 200L, 2000L),
                                  plot_width = NULL,
                                  pre_output = NULL,
                                  post_output = NULL,
                                  ggplot2_args = teal.widgets::ggplot2_args(),
                                  transformators = list(),
                                  decorators = list()) {
  message("Initializing tm_g_pp_therapy")

  # Compatibility layer: convert choices_selected to teal.picks variables
  for (arg in c(
    "atirel", "cmdecod", "cmindc", "cmdose", "cmtrt",
    "cmdosu", "cmroute", "cmdosfrq", "cmstdy", "cmendy"
  )) {
    if (inherits(get(arg), "choices_selected")) {
      assign(arg, teal.picks::as.picks(get(arg)))
    }
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(atirel, "variables", null.ok = TRUE)
  checkmate::assert_class(cmdecod, "variables", null.ok = TRUE)
  checkmate::assert_class(cmindc, "variables", null.ok = TRUE)
  checkmate::assert_class(cmdose, "variables", null.ok = TRUE)
  checkmate::assert_class(cmtrt, "variables", null.ok = TRUE)
  checkmate::assert_class(cmdosu, "variables", null.ok = TRUE)
  checkmate::assert_class(cmroute, "variables", null.ok = TRUE)
  checkmate::assert_class(cmdosfrq, "variables", null.ok = TRUE)
  checkmate::assert_class(cmstdy, "variables", null.ok = TRUE)
  checkmate::assert_class(cmendy, "variables", null.ok = TRUE)
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

  # Build picks objects from variable specs
  atirel <- teal.picks::picks(teal.picks::datasets(dataname), atirel)
  cmdecod <- teal.picks::picks(teal.picks::datasets(dataname), cmdecod)
  cmindc <- teal.picks::picks(teal.picks::datasets(dataname), cmindc)
  cmdose <- teal.picks::picks(teal.picks::datasets(dataname), cmdose)
  cmtrt <- teal.picks::picks(teal.picks::datasets(dataname), cmtrt)
  cmdosu <- teal.picks::picks(teal.picks::datasets(dataname), cmdosu)
  cmroute <- teal.picks::picks(teal.picks::datasets(dataname), cmroute)
  cmdosfrq <- teal.picks::picks(teal.picks::datasets(dataname), cmdosfrq)
  cmstdy <- teal.picks::picks(teal.picks::datasets(dataname), cmstdy)
  cmendy <- teal.picks::picks(teal.picks::datasets(dataname), cmendy)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_pp_therapy.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_pp_therapy.picks))],
    server = srv_g_pp_therapy.picks,
    server_args = args[names(args) %in% names(formals(srv_g_pp_therapy.picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_pp_therapy.picks <- function(id,
                                  atirel,
                                  cmdecod,
                                  cmindc,
                                  cmdose,
                                  cmtrt,
                                  cmdosu,
                                  cmroute,
                                  cmdosfrq,
                                  cmstdy,
                                  cmendy,
                                  font_size,
                                  pre_output,
                                  post_output,
                                  decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      htmlOutput(ns("title")),
      teal.widgets::get_dt_rows(ns("therapy_table"), ns("therapy_table_rows")),
      DT::DTOutput(outputId = ns("therapy_table")),
      teal.widgets::plot_with_settings_ui(id = ns("therapy_plot"))
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
        tags$label("Select the medication decoding column:"),
        teal.picks::picks_ui(ns("cmdecod"), cmdecod)
      ),
      tags$div(
        tags$label("Select ATIREL variable:"),
        teal.picks::picks_ui(ns("atirel"), atirel)
      ),
      tags$div(
        tags$label("Select CMINDC variable:"),
        teal.picks::picks_ui(ns("cmindc"), cmindc)
      ),
      tags$div(
        tags$label("Select CMDOSE variable:"),
        teal.picks::picks_ui(ns("cmdose"), cmdose)
      ),
      tags$div(
        tags$label("Select CMTRT variable:"),
        teal.picks::picks_ui(ns("cmtrt"), cmtrt)
      ),
      tags$div(
        tags$label("Select CMDOSU variable:"),
        teal.picks::picks_ui(ns("cmdosu"), cmdosu)
      ),
      tags$div(
        tags$label("Select CMROUTE variable:"),
        teal.picks::picks_ui(ns("cmroute"), cmroute)
      ),
      tags$div(
        tags$label("Select CMDOSFRQ variable:"),
        teal.picks::picks_ui(ns("cmdosfrq"), cmdosfrq)
      ),
      tags$div(
        tags$label("Select CMSTDY variable:"),
        teal.picks::picks_ui(ns("cmstdy"), cmstdy)
      ),
      tags$div(
        tags$label("Select CMENDY variable:"),
        teal.picks::picks_ui(ns("cmendy"), cmendy)
      ),
      ui_decorate_teal_data(ns("d_plot"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"),
            "Font Size",
            font_size,
            ticks = FALSE,
            step = 1
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_pp_therapy.picks <- function(id,
                                   data,
                                   dataname,
                                   parentname,
                                   patient_col,
                                   atirel,
                                   cmdecod,
                                   cmindc,
                                   cmdose,
                                   cmtrt,
                                   cmdosu,
                                   cmroute,
                                   cmdosfrq,
                                   cmstdy,
                                   cmendy,
                                   plot_height,
                                   plot_width,
                                   label,
                                   ggplot2_args,
                                   decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    patient_id <- reactive(input$patient_id)

    # Init patient selector
    patient_data_base <- reactive(unique(data()[[parentname]][[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session, "patient_id",
      choices = patient_data_base(), selected = patient_data_base()[1]
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

    selectors <- teal.picks::picks_srv(
      picks = list(
        atirel = atirel,
        cmdecod = cmdecod,
        cmindc = cmindc,
        cmdose = cmdose,
        cmtrt = cmtrt,
        cmdosu = cmdosu,
        cmroute = cmroute,
        cmdosfrq = cmdosfrq,
        cmstdy = cmstdy,
        cmendy = cmendy
      ),
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())
      validate_input(
        inputId = "patient_id",
        condition = !is.null(input$patient_id) && length(input$patient_id) > 0,
        message = "Please select a patient."
      )
      validate_input(
        inputId = "atirel-variables-selected",
        condition = !is.null(selectors$atirel()$variables$selected),
        message = "Please select ATIREL variable."
      )
      validate_input(
        inputId = "cmdecod-variables-selected",
        condition = !is.null(selectors$cmdecod()$variables$selected),
        message = "Please select medication decoding variable."
      )
      validate_input(
        inputId = "cmindc-variables-selected",
        condition = !is.null(selectors$cmindc()$variables$selected),
        message = "Please select CMINDC variable."
      )
      validate_input(
        inputId = "cmdose-variables-selected",
        condition = !is.null(selectors$cmdose()$variables$selected),
        message = "Please select CMDOSE variable."
      )
      validate_input(
        inputId = "cmtrt-variables-selected",
        condition = !is.null(selectors$cmtrt()$variables$selected),
        message = "Please select CMTRT variable."
      )
      validate_input(
        inputId = "cmdosu-variables-selected",
        condition = !is.null(selectors$cmdosu()$variables$selected),
        message = "Please select CMDOSU variable."
      )
      validate_input(
        inputId = "cmroute-variables-selected",
        condition = !is.null(selectors$cmroute()$variables$selected),
        message = "Please select CMROUTE variable."
      )
      validate_input(
        inputId = "cmdosfrq-variables-selected",
        condition = !is.null(selectors$cmdosfrq()$variables$selected),
        message = "Please select CMDOSFRQ variable."
      )
      validate_input(
        inputId = "cmstdy-variables-selected",
        condition = !is.null(selectors$cmstdy()$variables$selected),
        message = "Please select CMSTDY variable."
      )
      validate_input(
        inputId = "cmendy-variables-selected",
        condition = !is.null(selectors$cmendy()$variables$selected),
        message = "Please select CMENDY variable."
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

      teal::validate_has_data(obj[["ANL"]], 1)

      validate(
        need(
          nrow(obj[["ANL"]][input$patient_id == obj[["ANL"]][, patient_col], ]) > 0,
          "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
        )
      )

      my_calls <- template_therapy(
        dataname = "ANL",
        atirel = selectors$atirel()$variables$selected,
        cmdecod = selectors$cmdecod()$variables$selected,
        cmtrt = selectors$cmtrt()$variables$selected,
        cmdosu = selectors$cmdosu()$variables$selected,
        cmroute = selectors$cmroute()$variables$selected,
        cmdosfrq = selectors$cmdosfrq()$variables$selected,
        cmstdy = selectors$cmstdy()$variables$selected,
        cmendy = selectors$cmendy()$variables$selected,
        cmindc = selectors$cmindc()$variables$selected,
        cmdose = selectors$cmdose()$variables$selected,
        patient_id = patient_id(),
        font_size = input[["font_size"]],
        ggplot2_args = ggplot2_args
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
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table and Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    output$title <- renderText({
      paste("<h5><b>Patient ID:", all_q()[["pt_id"]], "</b></h5>")
    })

    table_r <- reactive({
      q <- req(all_q())
      list(
        html = DT::datatable(
          data = q[["table_data"]],
          options = list(pageLength = input$therapy_table_rows)
        ),
        report = q[["table"]]
      )
    })

    output$therapy_table <- DT::renderDataTable(table_r()[["html"]])

    decorated_all_q_plot <- srv_decorate_teal_data(
      "d_plot",
      data = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr = plot
    )

    plot_r <- reactive({
      req(decorated_all_q_plot())[["plot"]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "therapy_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_all_q_plot)
  })
}
