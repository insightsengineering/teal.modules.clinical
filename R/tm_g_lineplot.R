#' Template: Line Plot
#'
#' @inheritParams tern::g_lineplot
#' @inheritParams tern::control_lineplot_vars
#' @inheritParams template_arguments
#' @param param (`character`)\cr
#'   parameter chosen to filter the data by.
#' @param incl_screen (`logical`)\cr
#'   should the screening visit be included.
#'
#' @seealso [tm_g_lineplot()]
#'
#' @importFrom grid grid.newpage grid.layout viewport pushViewport
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
                                y_lab = "") {
  assert_that(
    is.string(dataname),
    is.string(strata),
    is.string(x),
    is.string(y),
    is.string(y_unit),
    is.string(paramcd),
    is.string(title),
    is.string(y_lab)
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
        expr = dplyr::filter(x != "SCREENING") %>%
          dplyr::mutate(x = droplevels(x)),
        names = list(x = as.name(x)),
        others = list(
          anl = as.name(dataname),
          x_var = as.name(x)
        )
      )
    )
  }

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
    "n" = "n",
    "Mean" = "mean",
    "Standard Deviation" = "sd",
    "Median" = "median")

  interval_choices <- c(
    "Mean Confidence Interval" = "mean_ci",
    "Median Confidence Interval" = "median_ci",
    "25% and 75% Quantiles" = "quantiles",
    "Range" = "range")

  graph_list <- list()

  graph_list <- add_expr(
    graph_list,
    quote(grid::grid.newpage())
  )

  graph_list <- add_expr(
    graph_list,
    substitute(
      expr = {
        result <- g_lineplot(
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
          title = paste0(
            "Plot of ", names(which(mid_choices == mid)), " and ",
            ifelse(interval %in% c("mean_ci", "median_ci"), paste0(as.character(conf_level * 100), "% "), ""),
            names(which(interval_choices == interval)), " by Visit"),
          y_lab = paste(y, names(which(mid_choices == mid)), "Values for"),
          ggtheme = theme_minimal(),
          control = control_summarize_vars(conf_level = conf_level),
          subtitle_add_paramcd = FALSE,
          subtitle_add_unit = FALSE
        )
      },
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
        y = y
      )
    )
  )

  z$graph <- bracket_expr(graph_list)

  z
}


#' Teal Module: Line Plot
#'
#' This teal module produces a grid style Line Plot for data with
#' ADaM structure.
#'
#' @inheritParams template_g_lineplot
#' @inheritParams module_arguments
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADLB", ADLB, code = 'ADLB <- synthetic_cdisc_data("latest")$adlb'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_lineplot(
#'       label = "Line Plot",
#'       dataname = "ADLB",
#'       strata = choices_selected(
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
#'
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_lineplot <- function(label,
                          dataname,
                          parentname = ifelse(is(strata, "data_extract_spec"), datanames_input(strata), "ADSL"),
                          strata = choices_selected(variable_choices(parentname, c("ARM", "ARMCD", "ACTARMCD")), "ARM"),
                          x = choices_selected(variable_choices(dataname, "AVISIT"), "AVISIT", fixed = TRUE),
                          y = choices_selected(
                            variable_choices(dataname, c("AVAL", "BASE", "CHG", "PCHG")), "AVAL"),
                          y_unit = choices_selected(variable_choices(dataname, "AVALU"), "AVALU", fixed = TRUE),
                          paramcd = choices_selected(variable_choices(dataname, "PARAMCD"), "PARAMCD", fixed = TRUE),
                          param = choices_selected(value_choices(dataname, "PARAMCD", "PARAM"), "ALT"),
                          conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
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
                          post_output = NULL) {
  logger::log_info("Initializing tm_g_lineplot")
  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is.choices_selected(conf_level),
    is_character_single(mid),
    is_character_single(interval),
    is_character_vector(whiskers),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())
  data_extract_list <- list(
    strata = cs_to_des_select(strata, dataname = parentname),
    param = cs_to_des_filter(param, dataname = dataname),
    x = cs_to_des_select(x, dataname = dataname, multiple = FALSE),
    y = cs_to_des_select(y, dataname = dataname),
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
        plot_width = plot_width
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}


#' User Interface for Line Plot Module
#' @noRd
#'
#' @importFrom shinyWidgets switchInput
ui_g_lineplot <- function(id, ...) {

  a <- list(...)
  is_single_dataset_value <- is_single_dataset(
    a$strata,
    a$paramcd,
    a$x,
    a$param,
    a$y,
    a$y_unit
  )

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      verbatimTextOutput(outputId = ns("text")),
      plot_with_settings_ui(
        id = ns("myplot")
      )
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("strata", "paramcd", "x", "y", "y_unit", "param")]),
      data_extract_ui(
        id = ns("param"),
        label = "Select Biomarker",
        data_extract_spec = a$param,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("strata"),
        label = "Select Treatment Variable",
        data_extract_spec = a$strata,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("y"),
        label = "Analysis Variable",
        data_extract_spec = a$y,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("x"),
        label = "Time Variable",
        data_extract_spec = a$x,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        ns("mid"),
        "Midpoint Statistic",
        choices = c(
          "n" = "n",
          "Mean" = "mean",
          "Standard deviation" = "sd",
          "Median" = "median"
        ),
        selected = "mean"
      ),
      optionalSelectInput(
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
      checkboxInput(
        ns("incl_screen"),
        "Include screening visit",
        value = TRUE
      ),
      panel_group(
        panel_item(
          "Additional plot settings",
          optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          optionalSliderInputValMinMax(
            ns("mid_point_size"),
            "Midpoint symbol size",
            a$mid_point_size,
            ticks = FALSE
          ),
          checkboxGroupInput(
            ns("whiskers"),
            "Whiskers to display",
            choices = c("Lower", "Upper"),
            selected = c("Lower", "Upper")
          ),
          radioButtons(
            ns("mid_type"),
            label = "Plot type",
            choices = c(
              "Point and line" = "pl",
              "Point" = "p",
              "Line" = "l"
            ),
            selected = "pl"
          ),
          data_extract_ui(
            id = ns("y_unit"),
            label = "Analysis Unit Variable",
            data_extract_spec = a$y_unit,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_ui(
            id = ns("paramcd"),
            label = "Parameter Code Variable",
            data_extract_spec = a$paramcd,
            is_single_dataset = is_single_dataset_value
          )
        )
      ),
      panel_group(
        panel_item(
          "Additional table settings",
          optionalSliderInputValMinMax(
            ns("table_font_size"),
            "Table Font Size",
            a$table_font_size,
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
            selected = c("n", "mean_sd", "median", "range"),
          ),
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


#' Server for Line Plot Module
#' @noRd
#'
srv_g_lineplot <- function(input,
                           output,
                           session,
                           datasets,
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
                           plot_width) {

  stopifnot(is_cdisc_data(datasets))
  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(x = x, y = y, strata = strata, paramcd = paramcd, y_unit = y_unit, param = param),
    merge_function = "dplyr::inner_join"
  )

  validate_checks <- reactive({

    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_strata <- as.vector(anl_m$columns_source$strata)
    input_x_var <- as.vector(anl_m$columns_source$x)
    input_y <- as.vector(anl_m$columns_source$y)
    input_param <- unlist(param$filter)["vars_selected"]
    input_paramcd <- as.vector(anl_m$columns_source$paramcd)
    input_y_unit <- as.vector(anl_m$columns_source$y_unit)

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

    validate(need(
      input$conf_level > 0 && input$conf_level < 1,
      "Please choose a confidence level between 0 and 1"
    ))

    validate(need(is_character_single(input_y), "Analysis variable should be a single column."))
    validate(need(is_character_single(input_x_var), "Time variable should be a single column."))

    NULL
  })

  call_preparation <- reactive({

    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 2)

    whiskers_selected <- ifelse(input$whiskers == "Lower", 1, ifelse(input$whiskers == "Upper", 2, 1:2))
    if (is_empty(whiskers_selected) | is.null(input$interval)) {
      input_whiskers <- NULL
      input_interval <- NULL
    } else {
      input_whiskers <- names(tern::s_summary(0)[[input$interval]][whiskers_selected])
      input_interval <- input$interval
    }
    input_param <- as.character(unique(anl_m$data()[[as.vector(anl_m$columns_source$param)]]))

    my_calls <- template_g_lineplot(
      dataname = "ANL",
      strata = as.vector(anl_m$columns_source$strata),
      y = as.vector(anl_m$columns_source$y),
      x = as.vector(anl_m$columns_source$x),
      paramcd = as.vector(anl_m$columns_source$paramcd),
      y_unit = as.vector(anl_m$columns_source$y_unit),
      conf_level = as.numeric(input$conf_level),
      incl_screen = input$incl_screen,
      mid = input$mid,
      interval = input_interval,
      whiskers = input_whiskers,
      table = input$table,
      mid_type = input$mid_type,
      mid_point_size = input$mid_point_size,
      table_font_size = input$table_font_size
    )
    mapply(expression = my_calls, chunks_push)
  })

  line_plot <- reactive({
    call_preparation()
    chunks_safe_eval()
  })

  # Insert the plot into a plot with settings module from teal.devel
  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = line_plot,
    height = plot_height,
    width = plot_width
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(strata, paramcd, y, x, y_unit, param)
    ),
    modal_title = label
  )

}
