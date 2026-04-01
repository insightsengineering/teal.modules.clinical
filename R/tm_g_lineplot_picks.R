#' teal Module: Line Plot (teal.picks implementation)
#'
#' This module produces a [ggplot2::ggplot()] type line plot using `teal.picks` for
#' variable/value selection instead of `teal.transform::choices_selected`.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_g_lineplot
#' @param group_var ([teal.picks::variables()])\cr variable selector for the grouping/treatment variable.
#' @param param_var ([teal.picks::variables()])\cr variable selector for the parameter code column
#'   (typically `"PARAMCD"`).
#' @param param_value ([teal.picks::values()])\cr value selector for the parameter to filter by.
#' @param x ([teal.picks::variables()])\cr variable selector for the time/visit axis.
#' @param y ([teal.picks::variables()])\cr variable selector for the analysis value axis.
#' @param y_unit ([teal.picks::variables()])\cr variable selector for the analysis unit column.
#' @param conf_level ([teal.picks::values()])\cr value selector for confidence level.
#'
#' @keywords internal
#' @export
tm_g_lineplot.picks <- function(label,
                                dataname,
                                parentname = "ADSL",
                                group_var = teal.picks::variables(c("ARM", "ARMCD", "ACTARMCD"), selected = "ARM"),
                                x = teal.picks::variables("AVISIT", fixed = TRUE),
                                y = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), selected = "AVAL"),
                                y_unit = teal.picks::variables("AVALU", fixed = TRUE),
                                param_var = teal.picks::variables("PARAMCD", fixed = TRUE),
                                param_value = teal.picks::values(multiple = FALSE),
                                conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95", keep_order = TRUE),
                                interval = "mean_ci",
                                mid = "mean",
                                whiskers = c("mean_ci_lwr", "mean_ci_upr"),
                                table = c("n", "mean_sd", "median", "range"),
                                mid_type = "pl",
                                mid_point_size = c(2, 1, 5),
                                table_font_size = c(4, 2, 6),
                                plot_height = c(1000L, 200L, 4000L),
                                plot_width = NULL,
                                pre_output = NULL,
                                post_output = NULL,
                                ggplot2_args = teal.widgets::ggplot2_args(),
                                transformators = list(),
                                decorators = list()) {
  message("Initializing tm_g_lineplot.picks")

  # Compatibility: accept choices_selected and convert to picks
  for (arg in c("group_var", "x", "y", "y_unit")) {
    if (inherits(get(arg), "choices_selected")) {
      assign(arg, teal.picks::as.picks(get(arg)))
    }
  }
  if (inherits(conf_level, "choices_selected")) {
    conf_level <- teal.picks::as.picks(conf_level)
    class(conf_level) <- gsub("variables", "values", class(conf_level), fixed = TRUE)
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(group_var, "variables")
  checkmate::assert_class(x, "variables")
  checkmate::assert_class(y, "variables")
  checkmate::assert_class(y_unit, "variables")
  checkmate::assert_class(param_var, "variables")
  checkmate::assert_class(param_value, "values")
  checkmate::assert_class(conf_level, "values")
  checkmate::assert_string(mid)
  checkmate::assert_string(interval, null.ok = TRUE)
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

  # Build picks objects bound to their datasets
  group_var <- teal.picks::picks(datasets(parentname), group_var)
  x         <- teal.picks::picks(datasets(dataname), x)
  y         <- teal.picks::picks(datasets(dataname), y)
  y_unit    <- teal.picks::picks(datasets(dataname), y_unit)
  param     <- teal.picks::picks(datasets(dataname), variables = param_var, values = param_value)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_lineplot.picks,
    ui = ui_g_lineplot.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_lineplot.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_lineplot.picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_lineplot.picks <- function(id,
                                group_var,
                                param,
                                x,
                                y,
                                y_unit,
                                conf_level,
                                mid_point_size,
                                table_font_size,
                                table,
                                pre_output,
                                post_output,
                                decorators) {
  ns <- NS(id)
  conf_level$fixed <- conf_level$fixed %||% FALSE

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      verbatimTextOutput(outputId = ns("text")),
      teal.widgets::plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Biomarker:"),
        teal.picks::picks_ui(ns("param"), param)
      ),
      tags$div(
        tags$label("Select Treatment Variable:"),
        teal.picks::picks_ui(ns("group_var"), group_var)
      ),
      tags$div(
        tags$label("Analysis Variable:"),
        teal.picks::picks_ui(ns("y"), y)
      ),
      tags$div(
        tags$label("Time Variable:"),
        teal.picks::picks_ui(ns("x"), x)
      ),
      selectInput(
        ns("mid"),
        "Midpoint Statistic",
        choices = c("Mean" = "mean", "Median" = "median"),
        selected = "mean"
      ),
      teal.widgets::optionalSelectInput(
        ns("interval"),
        "Interval",
        choices = c(
          "Mean CI" = "mean_ci",
          "Median CI" = "median_ci",
          "25% and 75%-ile" = "quantiles",
          "Min - Max" = "range"
        ),
        selected = "mean_ci"
      ),
      checkboxInput(ns("incl_screen"), "Include screening visit", value = TRUE),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional plot settings",
          teal.widgets::optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            choices = conf_level$choices,
            selected = conf_level$selected,
            multiple = FALSE,
            fixed = conf_level$fixed
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("mid_point_size"),
            "Midpoint symbol size",
            mid_point_size,
            ticks = FALSE
          ),
          checkboxGroupInput(
            ns("whiskers"),
            "Whiskers to display",
            choices = c("Upper", "Lower"),
            selected = c("Upper", "Lower")
          ),
          radioButtons(
            ns("mid_type"),
            label = "Plot type",
            choices = c("Point and line" = "pl", "Point" = "p", "Line" = "l"),
            selected = "pl"
          ),
          tags$div(
            tags$label("Analysis Unit Variable:"),
            teal.picks::picks_ui(ns("y_unit"), y_unit)
          )
        )
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          teal.widgets::optionalSliderInputValMinMax(
            ns("table_font_size"),
            "Table Font Size",
            table_font_size,
            ticks = FALSE
          ),
          checkboxGroupInput(
            ns("table"),
            label = "Choose the statistics to display in the table",
            choices = c(
              "n" = "n",
              "Mean (SD)" = "mean_sd",
              "Mean CI" = "mean_ci",
              "Median" = "median",
              "Median CI" = "median_ci",
              "25% and 75%-ile" = "quantiles",
              "Min - Max" = "range"
            ),
            selected = table
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_lineplot.picks <- function(id,
                                 data,
                                 dataname,
                                 parentname,
                                 group_var,
                                 param,
                                 x,
                                 y,
                                 y_unit,
                                 label,
                                 plot_height,
                                 plot_width,
                                 ggplot2_args,
                                 decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- teal.picks::picks_srv(
      picks = list(
        group_var = group_var,
        param     = param,
        x         = x,
        y         = y,
        y_unit    = y_unit
      ),
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId = "group_var-variables-selected",
        condition = !is.null(selectors$group_var()$variables$selected),
        message = "Please select a treatment variable."
      )
      teal:::validate_input(
        inputId = "y-variables-selected",
        condition = !is.null(selectors$y()$variables$selected),
        message = "Please select an analysis variable."
      )
      teal:::validate_input(
        inputId = "x-variables-selected",
        condition = !is.null(selectors$x()$variables$selected),
        message = "Please select a time variable."
      )
      teal:::validate_input(
        inputId = "param-values-selected",
        condition = !is.null(selectors$param()$values$selected),
        message = "Please select a Biomarker filter."
      )
      teal:::validate_input(
        inputId = "conf_level",
        condition = !is.null(input$conf_level),
        message = "Please choose a confidence level."
      )
      teal:::validate_input(
        inputId = "conf_level",
        condition = as.numeric(input$conf_level) > 0 && as.numeric(input$conf_level) < 1,
        message = "Confidence level must be a number strictly between 0 and 1."
      )

      obj
    })

    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data = validated_q,
      selectors = selectors,
      join_fun = "dplyr::inner_join",
      output_name = "ANL"
    )

    validate_checks <- reactive({
      anl_q <- anl_inputs$data()
      ANL <- anl_q[["ANL"]]
      teal::validate_has_data(ANL, 2)

      adsl_filtered <- anl_q[[parentname]]
      anl_filtered  <- anl_q[[dataname]]

      input_strata  <- anl_inputs$variables()$group_var
      input_x_var   <- anl_inputs$variables()$x
      input_y       <- anl_inputs$variables()$y
      input_y_unit  <- anl_inputs$variables()$y_unit
      input_paramcd <- anl_inputs$variables()$param_var %||% "PARAMCD"

      validate_args <- list(
        adsl    = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_strata),
        anl     = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_x_var, input_y, input_y_unit),
        arm_var = input_strata
      )

      if (length(input_strata) > 0 && length(unique(adsl_filtered[[input_strata]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }

      do.call(what = "validate_standard_inputs", validate_args)
      NULL
    })

    all_q <- reactive({
      validate_checks()

      obj <- anl_inputs$data()
      ANL <- obj[["ANL"]]

      whiskers_selected <- if ("Lower" %in% input$whiskers) 1L else NULL
      if ("Upper" %in% input$whiskers) whiskers_selected <- c(whiskers_selected, 2L)

      if (is.null(input$interval) || is.null(whiskers_selected)) {
        input_whiskers <- NULL
        input_interval <- NULL
      } else {
        input_interval <- input$interval
        input_whiskers <- names(tern::s_summary(0)[[input_interval]][whiskers_selected])
      }

      my_calls <- template_g_lineplot(
        dataname      = "ANL",
        group_var     = anl_inputs$variables()$group_var,
        y             = anl_inputs$variables()$y,
        x             = anl_inputs$variables()$x,
        paramcd       = anl_inputs$variables()$param_var %||% "PARAMCD",
        y_unit        = anl_inputs$variables()$y_unit,
        conf_level    = as.numeric(input$conf_level),
        incl_screen   = input$incl_screen,
        mid           = input$mid,
        interval      = input_interval,
        whiskers      = input_whiskers,
        table         = input$table,
        mid_type      = input$mid_type,
        mid_point_size   = input$mid_point_size,
        table_font_size  = input$table_font_size,
        ggplot2_args  = ggplot2_args
      )

      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_all_q <- srv_decorate_teal_data(
      id         = "decorator",
      data       = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr       = plot
    )
    plot_r <- reactive(decorated_all_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id     = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width  = plot_width
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
