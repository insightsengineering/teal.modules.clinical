#' teal Module: Patient Profile Vitals Plot (teal.picks implementation)
#'
#' This module produces a patient profile vitals [ggplot2::ggplot()] type plot using ADaM datasets.
#' This is the `teal.picks` re-implementation of [tm_g_pp_vitals()].
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_vitals
#' @param paramcd ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the `PARAMCD` variable from `dataname`.
#' @param aval_var ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the AVAL variable from `dataname`.
#' @param xaxis ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for the time variable from `dataname`
#'   to be put on the plot x-axis.
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
#' tm_g_pp_vitals.picks(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied only to `plot` output
#'    )
#' )
#' ```
#'
#' @export
tm_g_pp_vitals.picks <- function(label,
                                 dataname = "ADVS",
                                 parentname = "ADSL",
                                 patient_col = "USUBJID",
                                 paramcd = teal.picks::variables("PARAMCD"),
                                 aval_var = teal.picks::variables("AVAL"),
                                 xaxis = teal.picks::variables("ADY"),
                                 font_size = c(12L, 12L, 25L),
                                 plot_height = c(700L, 200L, 2000L),
                                 plot_width = NULL,
                                 pre_output = NULL,
                                 post_output = NULL,
                                 ggplot2_args = teal.widgets::ggplot2_args(),
                                 transformators = list(),
                                 decorators = list()) {
  message("Initializing tm_g_pp_vitals")

  # Compatibility layer: convert choices_selected to teal.picks variables
  for (arg in c("paramcd", "aval_var", "xaxis")) {
    if (inherits(get(arg), "choices_selected")) {
      assign(arg, teal.picks::as.picks(get(arg)))
    }
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(paramcd, "variables", null.ok = TRUE)
  checkmate::assert_class(aval_var, "variables", null.ok = TRUE)
  checkmate::assert_class(xaxis, "variables", null.ok = TRUE)
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
  assert_decorators(decorators, "plot")

  paramcd <- teal.picks::picks(teal.picks::datasets(dataname), paramcd)
  aval_var <- teal.picks::picks(teal.picks::datasets(dataname), aval_var)
  xaxis <- teal.picks::picks(teal.picks::datasets(dataname), xaxis)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_pp_vitals.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_pp_vitals.picks))],
    server = srv_g_pp_vitals.picks,
    server_args = args[names(args) %in% names(formals(srv_g_pp_vitals.picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_pp_vitals.picks <- function(id,
                                 paramcd,
                                 aval_var,
                                 xaxis,
                                 font_size,
                                 pre_output,
                                 post_output,
                                 decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("vitals_plot")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      tags$div(
        tags$label("Select PARAMCD variable:"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      uiOutput(ns("paramcd_levels")),
      tags$div(
        tags$label("Select vital plot x-axis:"),
        teal.picks::picks_ui(ns("xaxis"), xaxis)
      ),
      tags$div(
        tags$label("Select AVAL variable:"),
        teal.picks::picks_ui(ns("aval_var"), aval_var)
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"), "Font Size", font_size,
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
srv_g_pp_vitals.picks <- function(id,
                                  data,
                                  dataname,
                                  parentname,
                                  patient_col,
                                  paramcd,
                                  aval_var,
                                  xaxis,
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

    selectors <- teal.picks::picks_srv(
      picks = list(
        paramcd = paramcd,
        aval_var = aval_var,
        xaxis = xaxis
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
        inputId = "paramcd-variables-selected",
        condition = !is.null(selectors$paramcd()$variables$selected),
        message = "Please select PARAMCD variable."
      )
      validate_input(
        inputId = "xaxis-variables-selected",
        condition = !is.null(selectors$xaxis()$variables$selected),
        message = "Please select Vitals x-axis variable."
      )
      validate_input(
        inputId = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message = "Please select AVAL variable."
      )
      validate_input(
        inputId = "paramcd_levels_vals",
        condition = !is.null(input$paramcd_levels_vals) && length(input$paramcd_levels_vals) > 0,
        message = "Please select PARAMCD variable levels."
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

    output$paramcd_levels <- renderUI({
      paramcd_var <- selectors$paramcd()$variables$selected
      req(paramcd_var)
      req(input$patient_id)

      vitals_dat <- anl_inputs$data()[["ANL"]]
      vitals_dat_sub <- vitals_dat[vitals_dat[[patient_col]] == patient_id(), ]
      paramcd_col <- vitals_dat_sub[[paramcd_var]]
      paramcd_col_levels <- unique(paramcd_col)

      cur_selected <- isolate(input$paramcd_levels_vals)
      selected <- if (length(cur_selected) > 0) cur_selected else paramcd_col_levels

      selectInput(
        session$ns("paramcd_levels_vals"),
        "Select PARAMCD variable levels:",
        selected = selected,
        choices = paramcd_col_levels,
        multiple = TRUE
      )
    })

    all_q <- reactive({
      obj <- anl_inputs$data()

      teal::validate_has_data(obj[["ANL"]], 1)

      validate(
        need(
          nrow(obj[["ANL"]][input$patient_id == obj[["ANL"]][, patient_col], ]) > 0,
          "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
        )
      )

      paramcd_var <- selectors$paramcd()$variables$selected
      aval_var_sel <- selectors$aval_var()$variables$selected
      xaxis_sel <- selectors$xaxis()$variables$selected

      my_calls <- template_vitals(
        dataname = "ANL",
        paramcd = paramcd_var,
        paramcd_levels = input[["paramcd_levels_vals"]],
        xaxis = xaxis_sel,
        aval_var = aval_var_sel,
        patient_id = patient_id(),
        font_size = input[["font_size"]],
        ggplot2_args = ggplot2_args
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

      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_all_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr = plot
    )

    plot_r <- reactive(decorated_all_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "vitals_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
