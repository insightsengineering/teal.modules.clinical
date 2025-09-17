#' Template: Individual Patient Plots
#'
#' Creates a valid expression to generate [ggplot2::ggplot()] plots of individual patients.
#'
#' @inheritParams template_arguments
#' @param visit_var (`character`)\cr name of the variable for visit timepoints.
#' @param add_baseline_hline (`logical`)\cr whether a horizontal line should be added to the plot at baseline y-value.
#' @param separate_by_obs (`logical`)\cr whether to create multi-panel plots.
#' @param suppress_legend (`logical`)\cr whether to suppress the plot legend.
#' @param arm_levels (`character`)\cr vector of all levels of `arm_var`.
#' @param avalu_first (`character`)\cr `avalu_var` text to append to the plot title and y-axis label if `add_avalu` is
#'   `TRUE`.
#' @param paramcd_first (`character`)\cr `paramcd` text to append to the plot title and y-axis label.
#' @param add_avalu (`logical`)\cr whether `avalu_first` text should be appended to the plot title and y-axis label.
#' @param ggplot2_args (`ggplot2_args`) optional\cr object created by [teal.widgets::ggplot2_args()] with settings
#'   for the module plot. For this module, this argument will only accept `ggplot2_args` object with `labs` list of
#'   the following child elements: `title`, `subtitle`, `x`, `y`. No other elements are taken into account. The
#'   argument is merged with option `teal.ggplot2_args` and with default module arguments (hard coded in the module
#'   body).
#'
#'   For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_ipp()]
#'
#' @keywords internal
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
                           baseline_var = "BASE",
                           add_baseline_hline = FALSE,
                           separate_by_obs = FALSE,
                           ggplot2_args = teal.widgets::ggplot2_args(),
                           suppress_legend = FALSE,
                           add_avalu = TRUE) {

  checkmate::assert_string(dataname)
  checkmate::assert_string(paramcd)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(avalu_var)
  checkmate::assert_string(id_var)
  checkmate::assert_string(visit_var)
  checkmate::assert_string(baseline_var)
  checkmate::assert_flag(add_baseline_hline)
  checkmate::assert_flag(separate_by_obs)
  checkmate::assert_flag(suppress_legend)
  checkmate::assert_flag(add_avalu)

  y <- list()
  # Data preprocessing

  y$data <- substitute(
    expr = anl <- df %>% droplevels(),
    env = list(df = as.name(dataname))
  )

  title <- ifelse(
    add_avalu,
    sprintf("Individual Patient Plot for %s Values (%s) over Time", paramcd_first, avalu_first),
    sprintf("Individual Patient Plot for %s Values over Time", paramcd_first)
  )
  y_axis <- ifelse(
    add_avalu,
    sprintf("%s (%s)", paramcd_first, avalu_first),
    paramcd_first
  )

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = teal.widgets::ggplot2_args(
      labs = list(
        title = title,
        x = "Visit",
        y = y_axis,
        subtitle = paste(arm_levels, collapse = ", ")
      )
    )
  )

  graph_list <- list()
  graph_list <- add_expr(
    graph_list,
    substitute(
      expr = {
        plot <- tern::h_g_ipp(
          df = anl,
          xvar = visit,
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
        base = baseline_var,
        avalu = avalu_var,
        arm = arm_var
      )
    )
  )

  if (separate_by_obs) {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(id)),
        env = list(id = as.name(id_var))
      )
    )
  }

  if (suppress_legend) {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = {
          plot <- plot + ggplot2::theme(legend.position = "none")
        },
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

#' teal Module: Individual Patient Plots
#'
#' This module produces [ggplot2::ggplot()] type individual patient plots that display trends in parameter
#' values over time for each patient, using data with ADaM structure.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_g_ipp
#' @inheritParams template_arguments
#' @param arm_var ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for variable values that can be used as arm variable.
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
#' tm_g_ipp(
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
#' library(dplyr)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl %>%
#'     slice(1:20) %>%
#'     tern::df_explicit_na()
#'   ADLB <- tmc_ex_adlb %>%
#'     filter(USUBJID %in% ADSL$USUBJID) %>%
#'     tern::df_explicit_na() %>%
#'     filter(AVISIT != "SCREENING")
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_ipp(
#'       label = "Individual Patient Plot",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         value_choices(ADLB, "ARMCD"),
#'         "ARM A"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(ADLB, "PARAMCD"),
#'         "ALT"
#'       ),
#'       aval_var = choices_selected(
#'         variable_choices(ADLB, c("AVAL", "CHG")),
#'         "AVAL"
#'       ),
#'       avalu_var = choices_selected(
#'         variable_choices(ADLB, c("AVALU")),
#'         "AVALU",
#'         fixed = TRUE
#'       ),
#'       id_var = choices_selected(
#'         variable_choices(ADLB, c("USUBJID")),
#'         "USUBJID",
#'         fixed = TRUE
#'       ),
#'       visit_var = choices_selected(
#'         variable_choices(ADLB, c("AVISIT")),
#'         "AVISIT"
#'       ),
#'       baseline_var = choices_selected(
#'         variable_choices(ADLB, c("BASE")),
#'         "BASE",
#'         fixed = TRUE
#'       ),
#'       add_baseline_hline = FALSE,
#'       separate_by_obs = FALSE
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_ipp <- function(label,
                     dataname,
                     parentname = ifelse(
                       inherits(arm_var, "data_extract_spec"),
                       teal.transform::datanames_input(arm_var),
                       "ADSL"
                     ),
                     arm_var,
                     paramcd,
                     id_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "USUBJID"),
                       "USUBJID",
                       fixed = TRUE
                     ),
                     visit_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "AVISIT"),
                       "AVISIT",
                       fixed = TRUE
                     ),
                     aval_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "AVAL"),
                       "AVAL",
                       fixed = TRUE
                     ),
                     avalu_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "AVALU"),
                       "AVALU",
                       fixed = TRUE
                     ),
                     base_var = lifecycle::deprecated(),
                     baseline_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "BASE"),
                       "BASE",
                       fixed = TRUE
                     ),
                     add_baseline_hline = FALSE,
                     separate_by_obs = FALSE,
                     suppress_legend = FALSE,
                     add_avalu = TRUE,
                     plot_height = c(1200L, 400L, 5000L),
                     plot_width = NULL,
                     pre_output = NULL,
                     post_output = NULL,
                     ggplot2_args = teal.widgets::ggplot2_args(),
                     transformators = list(),
                     decorators = list()) {
  if (lifecycle::is_present(base_var)) {
    lifecycle::deprecate_stop(
      when = "0.8.16",
      what = "tm_g_ipp(base_var)",
      details = "Please use the `baseline_var` argument instead."
    )
  }

  message("Initializing tm_g_ipp")
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(visit_var, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(avalu_var, "choices_selected")
  checkmate::assert_class(baseline_var, "choices_selected")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_flag(add_baseline_hline)
  checkmate::assert_flag(separate_by_obs)
  checkmate::assert_flag(suppress_legend)
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

  args <- as.list(environment())
  data_extract_list <- list(
    arm_var = cs_to_des_filter(arm_var, dataname = parentname, multiple = TRUE, include_vars = TRUE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    avalu_var = cs_to_des_select(avalu_var, dataname = dataname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    visit_var = cs_to_des_select(visit_var, dataname = dataname),
    baseline_var = cs_to_des_select(baseline_var, dataname = dataname),
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
        ggplot2_args = ggplot2_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_g_ipp <- function(id, ...) {
  a <- list(...) # module args
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$aval_var,
    a$avalu_var,
    a$id_var,
    a$visit_var,
    a$paramcd,
    a$baseline_var
  )

  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(
        a[c("arm_var", "aval_var", "avalu_var", "id_var", "visit_var", "paramcd", "baseline_var")]
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Arm",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("visit_var"),
        label = "Timepoint Variable",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Parameter values over Time",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("id_var"),
        label = "Patient ID",
        data_extract_spec = a$id_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("avalu_var"),
        label = "Analysis Variable Unit",
        data_extract_spec = a$avalu_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("baseline_var"),
        label = "Baseline Parameter Values",
        data_extract_spec = a$baseline_var,
        is_single_dataset = is_single_dataset_value
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional plot settings",
          checkboxInput(
            ns("add_baseline_hline"),
            "Add reference lines at baseline value",
            value = a$add_baseline_hline
          ),
          checkboxInput(
            ns("separate_by_obs"),
            "Separate plots by ID",
            value = a$separate_by_obs
          ),
          checkboxInput(
            ns("suppress_legend"),
            "Suppress legend",
            value = a$suppress_legend
          ),
          checkboxInput(
            ns("add_avalu"),
            "Add unit value in title/y axis",
            value = a$add_avalu
          )
        )
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_g_ipp <- function(id,
                      data,
                      dataname,
                      parentname,
                      arm_var,
                      paramcd,
                      aval_var,
                      avalu_var,
                      id_var,
                      visit_var,
                      baseline_var,
                      plot_height,
                      plot_width,
                      label,
                      ggplot2_args,
                      decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    selector_list <- teal.transform::data_extract_multiple_srv(
      datasets = data,
      data_extract = list(
        arm_var = arm_var,
        aval_var = aval_var,
        avalu_var = avalu_var,
        id_var = id_var,
        paramcd = paramcd,
        visit_var = visit_var,
        baseline_var = baseline_var
      ),
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("A Parameter values over Time must be selected"),
        avalu_var = shinyvalidate::sv_required("An Analysis Variable Unit must be selected"),
        visit_var = shinyvalidate::sv_required("A Timepoint Variable must be selected"),
        id_var = shinyvalidate::sv_required("A Patient ID must be selected"),
        baseline_var = shinyvalidate::sv_required("Baseline Parameter Values must be selected")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required(message = "Please select Parameter filter."),
        arm_var = shinyvalidate::sv_required(message = "Please select Arm filter.")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      join_keys = teal.data::join_keys(data),
      data_extract = list(arm_var = arm_var, id_var = id_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Individual Patient Plot"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(code = as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      anl_m <- anl_inputs()
      input_arm_var <- unlist(arm_var$filter)["vars_selected"]
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_avalu_var <- as.vector(anl_m$columns_source$avalu_var)
      input_id_var <- as.vector(anl_m$columns_source$id_var)
      input_visit_var <- as.vector(anl_m$columns_source$visit_var)
      input_baseline_var <- as.vector(anl_m$columns_source$baseline_var)
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
          input_baseline_var
        ),
        arm_var = input_arm_var
      )

      do.call(what = "validate_standard_inputs", validate_args)
      NULL
    })

    # The R-code corresponding to the analysis.
    all_q <- reactive({
      validate_checks()
      anl_m <- anl_inputs()

      ANL <- anl_q()[["ANL"]]
      teal::validate_has_data(ANL, 2)

      arm_var <- unlist(arm_var$filter)["vars_selected"]
      avalu_var <- as.vector(anl_m$columns_source$avalu_var)
      paramcd <- unlist(paramcd$filter)["vars_selected"]

      avalu_first <- as.character(ANL[[avalu_var]][1])
      paramcd_first <- as.character(ANL[[paramcd]][1])
      arm_levels <- levels(droplevels(ANL[[arm_var]]))

      my_calls <- template_g_ipp(
        dataname = "ANL",
        aval_var = as.vector(anl_m$columns_source$aval_var),
        avalu_var = avalu_var,
        avalu_first = avalu_first,
        id_var = as.vector(anl_m$columns_source$id_var),
        visit_var = as.vector(anl_m$columns_source$visit_var),
        baseline_var = as.vector(anl_m$columns_source$baseline_var),
        add_baseline_hline = input$add_baseline_hline,
        separate_by_obs = input$separate_by_obs,
        suppress_legend = input$suppress_legend,
        paramcd = paramcd,
        paramcd_first = paramcd_first,
        arm_var = arm_var,
        arm_levels = arm_levels,
        ggplot2_args = ggplot2_args,
        add_avalu = input$add_avalu
      )
      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Outputs to render.
    decorated_all_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "plot")
    )
    plot_r <- reactive(decorated_all_q()[["plot"]])

    # Insert the plot into a plot with settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
