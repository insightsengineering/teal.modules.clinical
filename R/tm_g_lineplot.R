#' Template: Line Plot
#'
#' @inheritParams template_arguments
#' @inheritParams tern::g_lineplot
#' @param facet_var (`character`)\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used for facet plotting.
#'
#' @seealso [tm_g_lineplot()]
#'
#' @importFrom grid grid.newpage grid.layout viewport pushViewport
template_g_lineplot <- function(dataname = "ANL",
                                arm_var = "ARM",
                                aval_var = "AVAL",
                                x_var = "AVISIT",
                                y_var = "AVAL",
                                y_unit_var = "AVALU",
                                paramcd = "PARAMCD",
                                biomarker = "ALT",
                                conf_level = 0.95,
                                title = "Line Plot",
                                xticks = NULL) {
  assert_that(
    is.string(dataname),
    is.string(arm_var),
    is.string(aval_var),
    is.string(x_var),
    is.string(y_var),
    is.string(y_unit_var),
    is.string(biomarker),
    is.null(xticks) | is.numeric(xticks),
    is.string(title)
  )
  # browser()
  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl %>% dplyr::filter(paramcd == biomarker),
      env = list(anl = as.name(dataname), paramcd = as.name(paramcd), biomarker = biomarker)
    )
  )

  y$data <- substitute(
    expr = {
      anl <- data_pipe
    },
    env = list(
      data_pipe = pipe_expr(data_list)
    )
  )

  y$variables <- substitute(
      expr = variables <- control_lineplot_vars(x = x, y = y, strata = arm, paramcd = paramcd, y_unit = y_unit),
      env = list(x = x_var, y = y_var, arm = arm_var, paramcd = paramcd, y_unit = y_unit_var)
    )

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
              # xlab = paste0(
              #   xlab,
              #   " (",
              #   gsub(
              #     "(^|[[:space:]])([[:alpha:]])",
              #     "\\1\\U\\2",
              #     tolower(anl$y_unit_var[1]),
              #     perl = TRUE
              #   ),
              #   ")"
              # ),
              # y_val = y_val,
              newpage = FALSE,
              title = title,
              xticks = xticks,
              # title = paste(
              #   title, ",", quote(biomarker),
              #   "=", as.character(unique(df$biomarker))
              # ),
              ggtheme = theme_minimal(),
              control = control_summarize_vars(conf_level = conf_level)
        )
      },
      env = list(
        conf_level = conf_level,
        title = title,
        xticks = xticks
      )
    )
  )

  y$graph <- bracket_expr(graph_list)

  y
}


#' Teal Module: Line Plot
#'
#' This teal module produces a grid style Line Plot for data with
#' ADaM structure.
#'
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
#'       label = "LINE PLOT",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")),
#'         "ARM"
#'       ),
#'       y_var = choices_selected(
#'         variable_choices(ADLB, "AVAL"),
#'         "AVAL",
#'         fixed = TRUE
#'       ),
#'       paramcd = choices_selected(
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
                          parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                          arm_var = choices_selected(variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")), "ARM"),
                          x_var = choices_selected(variable_choices(dataname, "AVISIT"), "AVISIT"),
                          y_var = choices_selected(variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE),
                          y_unit_var = choices_selected(variable_choices(dataname, "AVALU"), "AVALU", fixed = TRUE),
                          paramcd = choices_selected(value_choices(dataname, "PARAMCD", "PARAM"), "ALT"),
                          conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                          plot_height = c(1200L, 400L, 5000L),
                          plot_width = NULL,
                          pre_output = NULL,
                          post_output = NULL) {

  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is.choices_selected(conf_level),
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
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    x_var = cs_to_des_select(x_var, dataname = dataname, multiple = FALSE),
    y_var = cs_to_des_select(y_var, dataname = dataname),
    y_unit_var = cs_to_des_select(y_unit_var, dataname = dataname)
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
    a$arm_var,
    a$paramcd,
    a$x_var,
    # a$biomarker,
    a$y_var,
    a$y_unit_var
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
      datanames_input(a[c("arm_var", "paramcd", "x_var", "y_var", "y_unit_var")]),#, "biomarker")]),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Biomarker",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("y_var"),
        label = "Analysis Variable",
        data_extract_spec = a$y_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("x_var"),
        label = "X Variable",
        data_extract_spec = a$x_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("y_unit_var"),
        label = "Analysis Unit Variable",
        data_extract_spec = a$y_unit_var,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        panel_item(
          "Additional plot settings",
          textInput(
            inputId = ns("xticks"),
            label = "Specify break intervals for x-axis e.g. 0 ; 500"
          ),
          checkboxInput(
            inputId = ns("show_lineplot_table"),
            label = "Show Line Plot table",
            value = TRUE,
            width = "100%"
          ),
          optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          )
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
                     arm_var,
                     x_var,
                     y_var,
                     # biomarker,
                     y_unit_var,
                     label,
                     plot_height,
                     plot_width) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(x_var, y_var, arm_var, paramcd, y_unit_var),#, biomarker),
    input_id = c("x_var", "y_var", "arm_var", "paramcd", "y_unit_var"),#, "biomarker"),
    merge_function = "dplyr::inner_join"
  )

  validate_checks <- reactive({

    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_x_var <- as.vector(anl_m$columns_source$x_var)
    input_y_var <- as.vector(anl_m$columns_source$y_var)
    # input_biomarker <- as.vector(anl_m$columns_source$biomarker)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]
    input_y_unit_var <- as.vector(anl_m$columns_source$y_unit_var)
    input_xticks <- gsub(";", ",", trimws(input$xticks)) %>%
      strsplit(",") %>%
      unlist() %>%
      as.numeric()

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_x_var, input_y_var, input_y_unit_var),#, input_biomarker),
      arm_var = input_arm_var
    )

    # validate arm levels
    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
    }

    do.call(what = "validate_standard_inputs", validate_args)

    # validate xticks
    if (length(input_xticks) == 0) {
      input_xticks <- NULL
    }
    else {
      validate(need(all(!is.na(input_xticks)), "Not all values entered were numeric"))
      validate(need(all(input_xticks >= 0), "All break intervals for x-axis must be non-negative"))
      validate(need(any(input_xticks > 0), "At least one break interval for x-axis must be positive"))
    }

    validate(need(
      input$conf_level > 0 && input$conf_level < 1,
      "Please choose a confidence level between 0 and 1"
    ))

    validate(need(is_character_single(input_y_var), "Analysis variable should be a single column."))
    validate(need(is_character_single(input_x_var), "X variable should be a single column."))

    # validate font size
    # validate(need(input$font_size >= 5, "Plot tables font size must be greater than or equal to 5."))

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

    input_xticks <- gsub(";", ",", trimws(input$xticks)) %>%
      strsplit(",") %>%
      unlist() %>%
      as.numeric()

    if (length(input_xticks) == 0) {
      input_xticks <- NULL
    }

    input_paramcd <- as.character(unique(anl_m$data()[[as.vector(anl_m$columns_source$paramcd)]]))
    title <- paste("Line Plot of", input_paramcd)

    my_calls <- template_g_lineplot(
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      y_var = as.vector(anl_m$columns_source$y_var),
      x_var = as.vector(anl_m$columns_source$x_var),
      paramcd = as.vector(anl_m$columns_source$paramcd),
      # biomarker = as.vector(anl_m$columns_source$biomarker),
      y_unit_var = as.vector(anl_m$columns_source$y_unit_var),
      xticks = input_xticks,
      conf_level = as.numeric(input$conf_level),
      title = title
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
      list(arm_var, paramcd, y_var, x_var, y_unit_var)#, biomarker)
    ),
    modal_title = label
  )

}
