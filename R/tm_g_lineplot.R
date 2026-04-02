#' Template: Line Plot
#'
#' Creates a valid expression to generate a [ggplot2::ggplot()] line plot.
#'
#' @inheritParams tern::g_lineplot
#' @inheritParams tern::control_lineplot_vars
#' @inheritParams template_arguments
#' @param group_var (`string` or `NA`)\cr group variable name.
#' @param param (`character`)\cr parameter to filter the data by.
#' @param incl_screen (`logical`)\cr whether the screening visit should be included.
#' @param ggplot2_args (`ggplot2_args`) optional\cr object created by [teal.widgets::ggplot2_args()] with settings
#' for the module plot. For this module, this argument will only accept `ggplot2_args` object with `labs` list of
#' following child elements: `title`, `subtitle`, `caption`, `y`, `lty`. No other elements would be taken into
#' account. The argument is merged with option `teal.ggplot2_args` and with default module arguments (hard coded in
#' the module body).
#'
#' For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_lineplot()]
#'
#' @keywords internal
template_g_lineplot <- function(dataname = "ANL",
                                group_var = "ARM",
                                x = "AVISIT",
                                y = "AVAL",
                                y_unit = "AVALU",
                                paramcd = "PARAMCD",
                                param = "ALT",
                                mid = "mean",
                                interval = "mean_ci",
                                whiskers = c("mean_ci_lwr", "mean_ci_upr"),
                                table = c("n", "mean_sd", "median", "range"),
                                mid_type = "pl",
                                conf_level = 0.95,
                                incl_screen = TRUE,
                                mid_point_size = 2,
                                table_font_size = 4,
                                title = "Line Plot",
                                y_lab = "",
                                ggplot2_args = teal.widgets::ggplot2_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(group_var)
  checkmate::assert_string(x)
  checkmate::assert_string(y)
  checkmate::assert_string(y_unit)
  checkmate::assert_string(paramcd)
  checkmate::assert_string(title)
  checkmate::assert_string(y_lab)

  z <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl,
      env = list(anl = as.name(dataname))
    )
  )

  if (!incl_screen) {
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = dplyr::filter(x_var != "SCREENING") %>%
          dplyr::mutate(x_var = factor(x_var)),
        names = list(x_var = as.name(x))
      )
    )
  }

  # droplevels for group_var
  data_list <- add_expr(
    data_list,
    substitute_names(
      expr = dplyr::mutate(
        arm_var = droplevels(arm_var)
      ),
      names = list(
        arm_var = as.name(group_var)
      )
    )
  )

  z$data <- substitute(
    expr = {
      anl <- data_pipe
    },
    env = list(
      data_pipe = pipe_expr(data_list)
    )
  )

  z$variables <- substitute(
    expr = variables <- tern::control_lineplot_vars(x = x, y = y, group_var = arm, paramcd = paramcd, y_unit = y_unit),
    env = list(x = x, y = y, arm = group_var, paramcd = paramcd, y_unit = y_unit)
  )

  mid_choices <- c(
    "Mean" = "mean",
    "Median" = "median"
  )

  interval_choices <- c(
    "Mean Confidence Interval" = "mean_ci",
    "Median Confidence Interval" = "median_ci",
    "25% and 75% Quantiles" = "quantiles",
    "Range" = "range"
  )

  graph_list <- list()

  graph_list <- add_expr(
    graph_list,
    quote(grid::grid.newpage())
  )

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = teal.widgets::ggplot2_args(
      labs = list(
        title = paste0(
          "Plot of ", names(which(mid_choices == mid)),
          if (!is.null(interval)) {
            paste0(
              " and ",
              if (interval %in% c("mean_ci", "median_ci")) paste0(conf_level * 100, "% "),
              names(which(interval_choices == interval))
            )
          },
          " of ", y, " by Visit"
        ),
        subtitle = "",
        y = sprintf("%s %s Values for", y, names(which(mid_choices == mid)))
      )
    )
  )

  plot_call <- substitute(
    tern::g_lineplot(
      df = anl,
      variables = variables,
      interval = interval,
      mid = mid,
      whiskers = whiskers,
      table = table,
      mid_type = mid_type,
      mid_point_size = mid_point_size,
      table_font_size = table_font_size,
      newpage = FALSE,
      title = ggplot2_args_title,
      subtitle = ggplot2_args_subtitle,
      caption = ggplot2_args_caption,
      y_lab = ggplot2_args_ylab,
      legend_title = ggplot2_args_legend_title,
      ggtheme = ggplot2::theme_minimal(),
      control = tern::control_analyze_vars(conf_level = conf_level),
      subtitle_add_paramcd = FALSE,
      subtitle_add_unit = FALSE
    ),
    env = list(
      conf_level = conf_level,
      interval = interval,
      mid = mid,
      whiskers = whiskers,
      table = table,
      mid_type = mid_type,
      mid_choices = mid_choices,
      interval_choices = interval_choices,
      mid_point_size = mid_point_size,
      table_font_size = table_font_size,
      y = y,
      ggplot2_args_title = all_ggplot2_args$labs$title,
      ggplot2_args_subtitle = all_ggplot2_args$labs$subtitle,
      ggplot2_args_caption = all_ggplot2_args$labs$caption,
      ggplot2_args_ylab = all_ggplot2_args$labs$y,
      ggplot2_args_legend_title = all_ggplot2_args$labs$lty
    )
  )

  graph_list <- add_expr(
    graph_list,
    substitute(
      expr = plot <- plot_call,
      env = list(plot_call = plot_call)
    )
  )

  z$graph <- bracket_expr(graph_list)

  z
}

#' teal Module: Line Plot
#'
#' This module produces a [ggplot2::ggplot()] type line plot, with optional summary table, for standard ADaM data.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_g_lineplot
#' @param strata `r lifecycle::badge("deprecated")` Please use the `group_var` argument instead.
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
#' tm_g_lineplot(
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
#'   library(forcats)
#'   ADSL <- tmc_ex_adsl
#'   ADLB <- tmc_ex_adlb %>%
#'     mutate(AVISIT == fct_reorder(AVISIT, AVISITN, min))
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_lineplot(
#'       label = "Line Plot",
#'       dataname = "ADLB",
#'       group_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")),
#'         "ARM"
#'       ),
#'       y = choices_selected(
#'         variable_choices(ADLB, c("AVAL", "BASE", "CHG", "PCHG")),
#'         "AVAL"
#'       ),
#'       param = choices_selected(
#'         value_choices(ADLB, "PARAMCD", "PARAM"),
#'         "ALT"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_lineplot <- function(label,
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
  message("Initializing tm_g_lineplot")

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
    server = srv_g_lineplot,
    ui = ui_g_lineplot,
    ui_args = args[names(args) %in% names(formals(ui_g_lineplot))],
    server_args = args[names(args) %in% names(formals(srv_g_lineplot))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_lineplot <- function(id,
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
srv_g_lineplot <- function(id,
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
