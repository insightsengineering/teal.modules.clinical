#' Template: Individual Patient Plots
#'
#' @inheritParams template_arguments
#' @param avalu_var (`string`)\cr variable name designating the unit of the analysis variable.
#' @param base_var (`string`)\cr variable name designating the baseline values of analysis variable.
#' @param visit_var (`string`)\cr variable name designating the visit timepoint variable.
#' @param add_baseline_hline (`flag`)\cr adds horizontal line at baseline y-value on plot
#' @param separate_by_obs (`flag`)\cr creates multi panel plots when TRUE
#' @param arm_levels (`character`)\cr vector of all arm variable levels.
#' @param avalu_first (`string`)\cr `avalu` value.
#' @param paramcd_first (`string`)\cr `paramcd` value.
#' @param ggplot2_args (`ggplot2_args`) object created by [teal.devel::ggplot2_args()]
#'  with settings for the module plot.
#'  For this module this argument will accepted only such `labs` arguments:  `title`, `subtitle`, `x`, `y`.
#'  `theme` arguments will be not taken into account.
#'  For more details see the help vignette:
#'  `vignette("Custom ggplot2_args arguments module", package = "teal.devel")`
#'  The argument is merged with options variable `teal.ggplot2_args` and default module setup.
#'
#' @importFrom grid grid.newpage grid.draw

template_g_ipp <- function(dataname = "ANL",
                           paramcd,
                           arm_var,
                           arm_levels,
                           avalu_first,
                           paramcd_first,
                           aval_var = "AVAL",
                           avalu_var = "AVALU",
                           id_var = "USUBJID",
                           visit_var = "AVISIT",
                           base_var = "BASE",
                           add_baseline_hline = FALSE,
                           separate_by_obs = FALSE,
                           ggplot2_args = teal.devel::ggplot2_args()) {

  assert_that(
    is.string(dataname),
    is.string(paramcd),
    is.string(arm_var),
    is.string(aval_var),
    is.string(avalu_var),
    is.string(id_var),
    is.string(visit_var),
    is.string(base_var),
    is.flag(add_baseline_hline),
    is.flag(separate_by_obs)
    )

  y <- list()
  # Data preprocessing

  y$data <- substitute(
    expr = anl <- df %>% droplevels(),
    env = list(df = as.name(dataname))
  )

  all_ggplot2_args <- resolve_ggplot2_args(
    user_plot =  ggplot2_args,
    module_plot = ggplot2_args(
      labs = list(
        title = sprintf("Individual Patient Plot for %s Values (%s) over Time", paramcd_first, avalu_first),
        x = "Visit",
        y = sprintf("%s (%s)", paramcd_first, avalu_first),
        subtitle = paste(arm_levels, collapse = ", ")
      )
    )
  )



  graph_list <- list()
  graph_list <- add_expr(
    graph_list,
    substitute(
      expr = {
        plot <- h_g_ipp(
          df = anl,
          xvar  = visit,
          yvar = aval,
          xlab = xlab_val,
          ylab = ylab_val,
          title = title_val,
          subtitle = subtitle_val,
          id_var = id,
          add_baseline_hline = add_baseline_hline,
          yvar_baseline = base
        )
      },
      env = list(
        xlab_val = all_ggplot2_args$labs$x,
        ylab_val = all_ggplot2_args$labs$y,
        title_val = all_ggplot2_args$labs$title,
        subtitle_val = all_ggplot2_args$labs$subtitle,
        paramcd = paramcd,
        visit = visit_var,
        aval = aval_var,
        id = id_var,
        add_baseline_hline = add_baseline_hline,
        base = base_var,
        avalu = avalu_var,
        arm = arm_var
      )
    )
  )

  if (separate_by_obs) {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = plot <- plot + ggplot2::facet_grid(rows = vars(id)),
        env = list(id = as.name(id_var))
      )
    )
  }

  graph_list <- add_expr(
    graph_list,
    quote(grid::grid.newpage())
  )

  graph_list <- add_expr(
    graph_list,
    quote(grid::grid.draw(plot))
  )

  y$graph <- bracket_expr(graph_list)

  y
}

#' Teal Module: Individual Patient Plot
#'
#' This teal module produces grid style Individual patient plot(s) that show
#' trends in parameter values over time for each patient using data with
#' ADaM structure.
#'
#' @inheritParams module_arguments
#' @inheritParams template_g_ipp
#' @param arm_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all available choices
#'   and preselected option for variable values that can be used as `arm_var`.
#' @param avalu_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all available choices
#'   and preselected option for variable values that can be used as `avalu_var`.
#' @param base_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all available choices
#'   and preselected option for variable values that can be used as `base_var`.
#' @param ggplot2_args (`ggplot2_args`) object created by [teal.devel::ggplot2_args()]
#'  with settings for the module plot.
#'  For this module this argument will accepted only such `labs` arguments:  `title`, `subtitle`, `x`, `y`.
#'  `theme` arguments will be not taken into account.
#'  For more details see the help vignette:
#'  `vignette("Custom ggplot2_args arguments module", package = "teal.devel")`
#'  The argument is merged with options variable `teal.ggplot2_args` and default module setup.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl %>% slice(1:20)
#' adlb <- synthetic_cdisc_data("latest")$adlb
#' adlb <- adlb %>% filter(USUBJID %in% adsl$USUBJID)
#'
#' adsl <- df_explicit_na(adsl)
#' adlb <- df_explicit_na(adlb) %>%
#'  dplyr::filter(AVISIT != "SCREENING")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset(
#'       "ADSL",
#'       adsl,
#'       code = "ADSL <- synthetic_cdisc_data('latest')$adsl %>% slice(1:20)
#'       ADSL <- df_explicit_na(ADSL)"
#'     ),
#'     cdisc_dataset(
#'       "ADLB",
#'       adlb,
#'       code = "ADLB <- synthetic_cdisc_data('latest')$adlb
#'       ADLB <- df_explicit_na(ADLB) %>%
#'       dplyr::filter(AVISIT != 'SCREENING')"
#'     )
#'   ),
#'   modules = root_modules(
#'     tm_g_ipp(
#'       label = "Individual Patient Plot",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         value_choices(adlb, "ARMCD"),
#'         "ARM A"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(adlb, "PARAMCD"),
#'         "ALT"
#'       ),
#'       aval_var = choices_selected(
#'         variable_choices(adlb, c("AVAL", "CHG")),
#'         "AVAL"
#'       ),
#'       avalu_var = choices_selected(
#'         variable_choices(adlb, c("AVALU")),
#'         "AVALU",
#'         fixed = TRUE
#'       ),
#'       id_var = choices_selected(
#'         variable_choices(adlb, c("USUBJID")),
#'         "USUBJID",
#'         fixed = TRUE
#'       ),
#'       visit_var = choices_selected(
#'         variable_choices(adlb, c("AVISIT")),
#'         "AVISIT"
#'       ),
#'       base_var = choices_selected(
#'          variable_choices(adlb, c("BASE")),
#'          "BASE",
#'          fixed = TRUE
#'       ),
#'       add_baseline_hline = FALSE,
#'       separate_by_obs = FALSE
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_ipp <- function(label,
                     dataname,
                     parentname = ifelse(
                       is(arm_var, "data_extract_spec"),
                       datanames_input(arm_var),
                       "ADSL"
                       ),
                     arm_var,
                     paramcd,
                     id_var = choices_selected(
                       variable_choices(dataname, "USUBJID"),
                       "USUBJID",
                       fixed = TRUE
                       ),
                     visit_var = choices_selected(
                       variable_choices(dataname, "AVISIT"),
                       "AVISIT",
                       fixed = TRUE
                       ),
                     aval_var = choices_selected(
                       variable_choices(dataname, "AVAL"),
                       "AVAL",
                       fixed = TRUE
                       ),
                     avalu_var = choices_selected(
                       variable_choices(dataname, "AVALU"),
                       "AVALU",
                       fixed = TRUE
                       ),
                     base_var = choices_selected(
                       variable_choices(dataname, "BASE"),
                       "BASE",
                       fixed = TRUE
                       ),
                     add_baseline_hline = FALSE,
                     separate_by_obs = FALSE,
                     plot_height = c(1200L, 400L, 5000L),
                     plot_width = NULL,
                     pre_output = NULL,
                     post_output = NULL,
                     ggplot2_args = teal.devel::ggplot2_args()) {
  logger::log_info("Initializing tm_g_ipp")
  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is_logical_single(add_baseline_hline),
    is_logical_single(separate_by_obs),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(plot_width[1], lower = plot_width[2], upper = plot_width[3], null.ok = TRUE,
                            .var.name = "plot_width")

  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  args <- as.list(environment())
  data_extract_list <- list(
    arm_var = cs_to_des_filter(arm_var, dataname = parentname, multiple = TRUE, include_vars = TRUE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    avalu_var = cs_to_des_select(avalu_var, dataname = dataname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    visit_var = cs_to_des_select(visit_var, dataname = dataname),
    base_var = cs_to_des_select(base_var, dataname = dataname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname)
  )

  module(
    label = label,
    server = srv_g_ipp,
    ui = ui_g_ipp,
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
    filters = get_extract_datanames(data_extract_list)
  )
}


ui_g_ipp <- function(id, ...) {
  a <- list(...) # module args
  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$aval_var,
    a$avalu_var,
    a$id_var,
    a$visit_var,
    a$paramcd,
    a$base_var
  )

  ns <- NS(id)

  standard_layout(
    output = plot_with_settings_ui(id = ns("myplot")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "aval_var", "avalu_var", "id_var", "visit_var", "paramcd", "base_var")]),
      data_extract_ui(
        id = ns("arm_var"),
        label = "Select Arm",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("paramcd"),
        label = "Select Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("visit_var"),
        label = "Timepoint Variable",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("aval_var"),
        label = "Parameter values over Time",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("id_var"),
        label = "Patient ID",
        data_extract_spec = a$id_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("avalu_var"),
        label = "Analysis Variable Unit",
        data_extract_spec = a$avalu_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("base_var"),
        label = "Baseline Parameter Values",
        data_extract_spec = a$base_var,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        panel_item(
          "Additional plot settings",
          checkboxInput(
            ns("add_baseline_hline"),
            "Add reference lines at baseline value",
            value = a$add_baseline_hline
            ),
          checkboxInput(
            ns("separate_by_obs"),
            "Separate plots by ID",
            value = a$separate_by_obs
            )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_ipp <- function(input,
                      output,
                      session,
                      datasets,
                      dataname,
                      parentname,
                      arm_var,
                      paramcd,
                      aval_var,
                      avalu_var,
                      id_var,
                      visit_var,
                      base_var,
                      plot_height,
                      plot_width,
                      label,
                      ggplot2_args) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(
      arm_var = arm_var,
      aval_var = aval_var,
      avalu_var = avalu_var,
      id_var = id_var,
      paramcd = paramcd,
      visit_var = visit_var,
      base_var = base_var
    ),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var = arm_var, id_var = id_var),
    anl_name = "ANL_ADSL"
  )

  # Prepare the analysis environment (filter data, check data, populate envir).
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- unlist(arm_var$filter)["vars_selected"]
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_avalu_var <- as.vector(anl_m$columns_source$avalu_var)
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_visit_var <- as.vector(anl_m$columns_source$visit_var)
    input_base_var <- as.vector(anl_m$columns_source$base_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("STUDYID", input_id_var, input_arm_var),
      anl = anl_filtered,
      anlvars = c(
        "STUDYID",
        input_id_var,
        input_arm_var,
        input_aval_var,
        input_avalu_var,
        input_paramcd,
        input_visit_var,
        input_base_var
      ),
      arm_var = input_arm_var
    )

    do.call(what = "validate_standard_inputs", validate_args)

    validate(
      need(is_character_single(input_aval_var), "Analysis variable should be a single column.")
      )

    validate(
      need(is_character_single(input_visit_var), "Please select a timepoint variable.")
    )

    NULL
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 2)

    arm_var <- unlist(arm_var$filter)["vars_selected"]
    avalu_var <- as.vector(anl_m$columns_source$avalu_var)
    paramcd <- unlist(paramcd$filter)["vars_selected"]

    avalu_first <- as.character(ANL[[avalu_var]][1])
    paramcd_first <- as.character(ANL[[paramcd]][1])
    arm_levels <- levels(ANL[[arm_var]])

    my_calls <- template_g_ipp(
      dataname = "ANL",
      aval_var = as.vector(anl_m$columns_source$aval_var),
      avalu_var = avalu_var,
      avalu_first = avalu_first,
      id_var = as.vector(anl_m$columns_source$id_var),
      visit_var = as.vector(anl_m$columns_source$visit_var),
      base_var = as.vector(anl_m$columns_source$base_var),
      add_baseline_hline = input$add_baseline_hline,
      separate_by_obs = input$separate_by_obs,
      paramcd = paramcd,
      paramcd_first = paramcd_first,
      arm_var = arm_var,
      arm_levels = arm_levels,
      ggplot2_args = ggplot2_args
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  get_plot <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("plot")
  })

  # Insert the plot into a plot with settings module from teal.devel
  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = get_plot,
    height = plot_height,
    width = plot_width
  )

  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm_var, aval_var, avalu_var, id_var, paramcd, base_var, visit_var)
    ),
    modal_title = label
  )
}
