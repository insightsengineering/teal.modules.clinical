#' Template: Line Plot
#'
#' @inheritParams tern::g_lineplot
#' @inheritParams tern::control_lineplot_vars
#' @inheritParams template_arguments
#' @param param (`character`)\cr
#'   parameter chosen to filter the data by.
#' @param incl_screen (`logical`)\cr
#'   should the screening visit be included.
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `ggplot2_args` object with `labs` list of following child elements:
#' `title`, `subtitle`, `caption`, `y`, `lty`.
#' No other elements would be taken into account. The argument is merged with option `teal.ggplot2_args` and
#' with default module arguments (hard coded in the module body).
#'
#' For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @seealso [tm_g_lineplot()]
#' @keywords internal
#'
template_g_lineplot <- function(dataname = "ANL",
                                strata = "ARM",
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
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(strata),
    assertthat::is.string(x),
    assertthat::is.string(y),
    assertthat::is.string(y_unit),
    assertthat::is.string(paramcd),
    assertthat::is.string(title),
    assertthat::is.string(y_lab)
  )
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

  # droplevels for strata
  data_list <- add_expr(
    data_list,
    substitute_names(
      expr = dplyr::mutate(
        arm_var = droplevels(arm_var)
      ),
      names = list(
        arm_var = as.name(strata)
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
    expr = variables <- control_lineplot_vars(x = x, y = y, strata = arm, paramcd = paramcd, y_unit = y_unit),
    env = list(x = x, y = y, arm = strata, paramcd = paramcd, y_unit = y_unit)
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
        title = sprintf(
          "Plot of %s and %s %s of %s by Visit",
          names(which(mid_choices == mid)),
          `if`(interval %in% c("mean_ci", "median_ci"), paste0(conf_level * 100, "%"), ""),
          names(which(interval_choices == interval)),
          y
        ),
        subtitle = "",
        y = sprintf("%s %s Values for", y, names(which(mid_choices == mid)))
      )
    )
  )

  plot_call <- substitute(
    g_lineplot(
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
      control = control_summarize_vars(conf_level = conf_level),
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
      expr = {
        plot <- plot_call
        plot
      },
      env = list(plot_call = plot_call)
    )
  )

  z$graph <- bracket_expr(graph_list)

  z
}


#' Teal Module: Line Plot
#'
#' This teal module produces a grid style Line Plot for data with
#' `ADaM` structure.
#'
#' @inheritParams template_g_lineplot
#' @inheritParams module_arguments
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `ggplot2_args` object with `labs` list of following child elements:
#' `title`, `subtitle`, `caption`, `y`, `lty`.
#' No other elements would be taken into account. The argument is merged with option `teal.ggplot2_args` and
#' with default module arguments (hard coded in the module body)
#'
#' For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @export
#'
#' @examples
#' library(nestcolor)
#'
#' adsl <- tmc_ex_adsl
#' adlb <- tmc_ex_adlb %>% dplyr::mutate(AVISIT == forcats::fct_reorder(AVISIT, AVISITN, min))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADLB", adlb)
#'   ),
#'   modules = modules(
#'     tm_g_lineplot(
#'       label = "Line Plot",
#'       dataname = "ADLB",
#'       strata = choices_selected(
#'         variable_choices(adsl, c("ARM", "ARMCD", "ACTARMCD")),
#'         "ARM"
#'       ),
#'       y = choices_selected(
#'         variable_choices(adlb, c("AVAL", "BASE", "CHG", "PCHG")),
#'         "AVAL"
#'       ),
#'       param = choices_selected(
#'         value_choices(adlb, "PARAMCD", "PARAM"),
#'         "ALT"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_lineplot <- function(label,
                          dataname,
                          parentname = ifelse(
                            inherits(strata, "data_extract_spec"),
                            teal.transform::datanames_input(strata),
                            "ADSL"
                          ),
                          strata = teal.transform::choices_selected(
                            teal.transform::variable_choices(parentname, c("ARM", "ARMCD", "ACTARMCD")), "ARM"
                          ),
                          x = teal.transform::choices_selected(
                            teal.transform::variable_choices(dataname, "AVISIT"), "AVISIT",
                            fixed = TRUE
                          ),
                          y = teal.transform::choices_selected(
                            teal.transform::variable_choices(dataname, c("AVAL", "BASE", "CHG", "PCHG")), "AVAL"
                          ),
                          y_unit = teal.transform::choices_selected(
                            teal.transform::variable_choices(dataname, "AVALU"), "AVALU",
                            fixed = TRUE
                          ),
                          paramcd = teal.transform::choices_selected(
                            teal.transform::variable_choices(dataname, "PARAMCD"), "PARAMCD",
                            fixed = TRUE
                          ),
                          param = teal.transform::choices_selected(
                            teal.transform::value_choices(dataname, "PARAMCD", "PARAM"), "ALT"
                          ),
                          conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                          interval = "mean_ci",
                          mid = "mean",
                          whiskers = c("mean_ci_lwr", "mean_ci_upr"),
                          table = NULL,
                          mid_type = "pl",
                          mid_point_size = c(2, 1, 5),
                          table_font_size = c(4, 2, 6),
                          plot_height = c(1000L, 200L, 4000L),
                          plot_width = NULL,
                          pre_output = NULL,
                          post_output = NULL,
                          ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_lineplot")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(mid)
  checkmate::assert_string(interval)
  whiskers <- match.arg(whiskers)
  checkmate::assert_class(conf_level, "choices_selected")
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

  args <- as.list(environment())
  data_extract_list <- list(
    strata = cs_to_des_select(strata, dataname = parentname),
    param = cs_to_des_filter(param, dataname = dataname),
    x = cs_to_des_select(x, dataname = dataname, multiple = FALSE),
    y = cs_to_des_select(y, dataname = dataname, multiple = FALSE),
    y_unit = cs_to_des_select(y_unit, dataname = dataname),
    paramcd = cs_to_des_select(paramcd, dataname = dataname)
  )

  module(
    label = label,
    server = srv_g_lineplot,
    ui = ui_g_lineplot,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        label = label,
        parentname = parentname,
        plot_height = plot_height,
        plot_width = plot_width,
        ggplot2_args = ggplot2_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}


#' User Interface for Line Plot Module
#' @noRd
ui_g_lineplot <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$strata,
    a$paramcd,
    a$x,
    a$param,
    a$y,
    a$y_unit
  )

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      shiny::verbatimTextOutput(outputId = ns("text")),
      teal.widgets::plot_with_settings_ui(
        id = ns("myplot")
      )
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("strata", "paramcd", "x", "y", "y_unit", "param")]),
      teal.transform::data_extract_ui(
        id = ns("param"),
        label = "Select Biomarker",
        data_extract_spec = a$param,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("strata"),
        label = "Select Treatment Variable",
        data_extract_spec = a$strata,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("y"),
        label = "Analysis Variable",
        data_extract_spec = a$y,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("x"),
        label = "Time Variable",
        data_extract_spec = a$x,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::selectInput(
        ns("mid"),
        "Midpoint Statistic",
        choices = c(
          "Mean" = "mean",
          "Median" = "median"
        ),
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
      shiny::checkboxInput(
        ns("incl_screen"),
        "Include screening visit",
        value = TRUE
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional plot settings",
          teal.widgets::optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("mid_point_size"),
            "Midpoint symbol size",
            a$mid_point_size,
            ticks = FALSE
          ),
          shiny::checkboxGroupInput(
            ns("whiskers"),
            "Whiskers to display",
            choices = c("Lower", "Upper"),
            selected = c("Lower", "Upper")
          ),
          shiny::radioButtons(
            ns("mid_type"),
            label = "Plot type",
            choices = c(
              "Point and line" = "pl",
              "Point" = "p",
              "Line" = "l"
            ),
            selected = "pl"
          ),
          teal.transform::data_extract_ui(
            id = ns("y_unit"),
            label = "Analysis Unit Variable",
            data_extract_spec = a$y_unit,
            is_single_dataset = is_single_dataset_value
          ),
          teal.transform::data_extract_ui(
            id = ns("paramcd"),
            label = "Parameter Code Variable",
            data_extract_spec = a$paramcd,
            is_single_dataset = is_single_dataset_value
          )
        )
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional table settings",
          teal.widgets::optionalSliderInputValMinMax(
            ns("table_font_size"),
            "Table Font Size",
            a$table_font_size,
            ticks = FALSE
          ),
          shiny::checkboxGroupInput(
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
            selected = c("n", "mean_sd", "median", "range"),
          )
        )
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


#' Server for Line Plot Module
#' @noRd
#'
srv_g_lineplot <- function(id,
                           data,
                           reporter,
                           filter_panel_api,
                           dataname,
                           parentname,
                           paramcd,
                           strata,
                           x,
                           y,
                           param,
                           y_unit,
                           label,
                           plot_height,
                           plot_width,
                           ggplot2_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(x = x, y = y, strata = strata, paramcd = paramcd, y_unit = y_unit, param = param),
      datasets = data,
      select_validation_rule = list(
        x = shinyvalidate::sv_required("Please select a time variable"),
        y = shinyvalidate::sv_required("Please select an analysis variable"),
        strata = shinyvalidate::sv_required("Please select a treatment variable")
      ),
      filter_validation_rule = list(
        param = shinyvalidate::sv_required(message = "Please select Biomarker filter.")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(
          0, 1,
          message_fmt = "Please choose a confidence level between 0 and 1", inclusive = c(FALSE, FALSE)
        )
      )
      iv$add_rule("interval", shinyvalidate::sv_required("Please select an interval for the midpoint statistic"))
      iv$add_rule("whiskers", shinyvalidate::sv_required("At least one of the whiskers must be selected"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      join_keys = get_join_keys(data),
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    merged <- list(anl_input_r = anl_inputs, anl_q = anl_q)

    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_strata <- names(merged$anl_input_r()$columns_source$strata)
      input_x_var <- names(merged$anl_input_r()$columns_source$x)
      input_y <- names(merged$anl_input_r()$columns_source$y)
      input_param <- unlist(param$filter)["vars_selected"]
      input_paramcd <- names(merged$anl_input_r()$columns_source$paramcd)
      input_y_unit <- names(merged$anl_input_r()$columns_source$y_unit)

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_strata),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_x_var, input_y, input_y_unit, input_param),
        arm_var = input_strata
      )

      # validate arm levels
      if (length(input_strata) > 0 && length(unique(adsl_filtered[[input_strata]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }

      do.call(what = "validate_standard_inputs", validate_args)
      NULL
    })

    all_q <- shiny::reactive({
      validate_checks()
      ANL <- merged$anl_q()[["ANL"]] # nolint
      teal::validate_has_data(ANL, 2)

      whiskers_selected <- ifelse(input$whiskers == "Lower", 1, ifelse(input$whiskers == "Upper", 2, 1:2))
      input_whiskers <- names(tern::s_summary(0)[[input$interval]][whiskers_selected])
      input_interval <- input$interval
      input_param <- as.character(unique(ANL[[names(merged$anl_input_r()$columns_source$param)[1]]]))

      my_calls <- template_g_lineplot(
        dataname = "ANL",
        strata = names(merged$anl_input_r()$columns_source$strata),
        y = names(merged$anl_input_r()$columns_source$y),
        x = names(merged$anl_input_r()$columns_source$x),
        paramcd = names(merged$anl_input_r()$columns_source$paramcd),
        y_unit = names(merged$anl_input_r()$columns_source$y_unit),
        conf_level = as.numeric(input$conf_level),
        incl_screen = input$incl_screen,
        mid = input$mid,
        interval = input_interval,
        whiskers = input_whiskers,
        table = input$table,
        mid_type = input$mid_type,
        mid_point_size = input$mid_point_size,
        table_font_size = input$table_font_size,
        ggplot2_args = ggplot2_args
      )
      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
    })

    plot_r <- shiny::reactive(all_q()[["plot"]])

    # Insert the plot into a plot with settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(all_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Line Plot")
        card$append_text("Line Plot", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(all_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
