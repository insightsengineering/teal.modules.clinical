#' Template: Kaplan-Meier Plot
#'
#' Creates a valid expression to generate a Kaplan-Meier plot.
#'
#' @inheritParams template_arguments
#' @inheritParams tern::g_km
#' @inheritParams tern::control_coxreg
#' @param facet_var (`character`)\cr name of the variable to use to facet the plot.
#' @param conf_type (`string`)\cr confidence interval type for median survival time CI. Options are "plain" (default),
#'   "log", "log-log".
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_km()]
#'
#' @keywords internal
template_g_km <- function(dataname = "ANL",
                          arm_var = "ARM",
                          ref_arm = NULL,
                          comp_arm = NULL,
                          compare_arm = FALSE,
                          combine_comp_arms = FALSE,
                          aval_var = "AVAL",
                          cnsr_var = "CNSR",
                          xticks = NULL,
                          strata_var = NULL,
                          time_points = NULL,
                          facet_var = "SEX",
                          font_size = 11,
                          conf_level = 0.95,
                          conf_type = "plain",
                          ties = "efron",
                          xlab = "Survival time",
                          time_unit_var = "AVALU",
                          yval = "Survival",
                          ylim = NULL,
                          pval_method = "log-rank",
                          annot_surv_med = TRUE,
                          annot_coxph = TRUE,
                          control_annot_surv_med = control_surv_med_annot(),
                          control_annot_coxph = tern::control_coxph_annot(x = 0.27, y = 0.35, w = 0.3),
                          legend_pos = NULL,
                          rel_height_plot = 0.80,
                          ci_ribbon = FALSE,
                          title = "KM Plot") {
  checkmate::assert_string(dataname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(cnsr_var)
  checkmate::assert_string(time_unit_var)
  checkmate::assert_flag(compare_arm)
  checkmate::assert_flag(combine_comp_arms)
  checkmate::assert_numeric(xticks, null.ok = TRUE)
  checkmate::assert_string(title)
  checkmate::assert_number(font_size)
  checkmate::assert_number(rel_height_plot, lower = 0, upper = 1)

  ref_arm_val <- paste(ref_arm, collapse = "/")
  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      compare_arm = compare_arm,
      ref_arm_val = ref_arm_val
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = dplyr::mutate(
        is_event = cnsr_var == 0
      ),
      env = list(
        anl = as.name(dataname),
        cnsr_var = as.name(cnsr_var)
      )
    )
  )

  if (compare_arm && combine_comp_arms) {
    comp_arm_val <- paste(comp_arm, collapse = "/")
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = dplyr::mutate(arm_var = tern::combine_levels(arm_var, levels = comp_arm, new_level = comp_arm_val)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm, comp_arm_val = comp_arm_val)
      )
    )
  }

  y$data <- substitute(
    expr = {
      anl <- data_pipe
    },
    env = list(
      data_pipe = pipe_expr(data_list)
    )
  )

  y$variables <- if (length(strata_var) != 0) {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm, strata = strata_var),
      env = list(tte = aval_var, arm = arm_var, strata_var = strata_var)
    )
  } else {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm),
      env = list(tte = aval_var, arm = arm_var)
    )
  }
  graph_list <- list()

  if (length(facet_var) != 0L) {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = {
          facets <- droplevels(anl$facet_var)
          anl <- split(anl, f = facets)
        },
        env = list(
          facet_var = as.name(facet_var)
        )
      )
    )
  } else {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = {
          facets <- NULL
          anl <- list(anl)
        }
      )
    )
  }

  graph_list <- add_expr(
    graph_list,
    substitute(
      expr = {
        g_km_counter_generator <- function() {
          plot_number <- 0L
          function(x) {
            plot_number <<- plot_number + 1L
            tern::g_km(
              x,
              variables = variables,
              control_surv = tern::control_surv_timepoint(conf_level = conf_level, conf_type = conf_type),
              xticks = xticks,
              xlab = sprintf(
                "%s (%s)",
                xlab,
                gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(x$time_unit_var[1]), perl = TRUE)
              ),
              yval = yval,
              ylim = ylim,
              title = sprintf(
                "%s%s",
                sprintf(
                  "%s%s",
                  title,
                  if (!is.null(facets)) {
                    sprintf(", %s = %s", as.character(quote(facet_var)), unique(x[[as.character(quote(facet_var))]]))
                  } else {
                    ""
                  }
                ),
                if (length(strata_var) != 0) {
                  sprintf("\nStratified by %s", toString(strata_var))
                } else {
                  ""
                }
              ),
              footnotes = if (annot_coxph) {
                paste(
                  "Ties for Coxph (Hazard Ratio):", ties, "\n",
                  "p-value Method for Coxph (Hazard Ratio):", pval_method
                )
              },
              font_size = font_size,
              ci_ribbon = ci_ribbon,
              annot_surv_med = annot_surv_med,
              annot_coxph = annot_coxph,
              control_coxph_pw = tern::control_coxph(conf_level = conf_level, pval_method = pval_method, ties = ties),
              control_annot_surv_med = control_annot_surv_med,
              control_annot_coxph = control_annot_coxph,
              legend_pos = legend_pos,
              rel_height_plot = rel_height_plot
            )
          }
        }

        g_km_counter <- g_km_counter_generator()

        plot_list <- lapply(
          anl,
          g_km_counter
        )

        plot <- cowplot::plot_grid(
          plotlist = plot_list,
          ncol = 1
        )
      },
      env = list(
        facet_var = if (length(facet_var) != 0L) as.name(facet_var),
        font_size = font_size,
        strata_var = strata_var,
        xticks = xticks,
        xlab = xlab,
        time_unit_var = as.name(time_unit_var),
        yval = yval,
        ylim = ylim,
        conf_level = conf_level,
        conf_type = conf_type,
        pval_method = pval_method,
        annot_surv_med = annot_surv_med,
        annot_coxph = annot_coxph,
        control_annot_surv_med = control_annot_surv_med,
        control_annot_coxph = control_annot_coxph,
        legend_pos = legend_pos,
        ties = ties,
        ci_ribbon = ci_ribbon,
        rel_height_plot = rel_height_plot,
        title = title
      )
    )
  )

  y$graph <- bracket_expr(graph_list)
  y
}

#' teal Module: Kaplan-Meier Plot
#'
#' This module produces a `ggplot`-style Kaplan-Meier plot for data with ADaM structure.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_g_km
#' @param xticks (`numeric` or `NULL`)\cr numeric vector of tick positions or a single number with spacing
#'   for the x-axis. If `NULL` (default), users can specify this interactively in the module.
#'   If provided, the interactive input field is pre-populated with the specified values as a default.
#'   Users can then modify these values interactively, and their changes will take precedence over the default.
#' @param facet_var ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for names of variable that can be used for plot faceting.
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
#' tm_g_km(
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
#'   library(dplyr)
#'   library(teal.modules.clinical)
#'   ADSL <- tmc_ex_adsl
#'   ADTTE <- tmc_ex_adtte
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADTTE <- data[["ADTTE"]]
#'
#' arm_ref_comp <- list(
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   ),
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   )
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_km(
#'       label = "Kaplan-Meier Plot",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")),
#'         "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"),
#'         "OS"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("SEX", "BMRKR2")),
#'         "SEX"
#'       ),
#'       facet_var = choices_selected(
#'         variable_choices(ADSL, c("SEX", "BMRKR2")),
#'         NULL
#'       ),
#'       xticks = c(0, 30, 60, 90, 120, 150, 180)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_km <- function(label,
                    dataname,
                    parentname = ifelse(
                      inherits(arm_var, "data_extract_spec"),
                      teal.transform::datanames_input(arm_var),
                      "ADSL"
                    ),
                    arm_var,
                    arm_ref_comp = NULL,
                    paramcd,
                    strata_var,
                    facet_var,
                    time_unit_var = teal.transform::choices_selected(
                      teal.transform::variable_choices(dataname, "AVALU"), "AVALU",
                      fixed = TRUE
                    ),
                    aval_var = teal.transform::choices_selected(
                      teal.transform::variable_choices(dataname, "AVAL"), "AVAL",
                      fixed = TRUE
                    ),
                    cnsr_var = teal.transform::choices_selected(
                      teal.transform::variable_choices(dataname, "CNSR"), "CNSR",
                      fixed = TRUE
                    ),
                    conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                    conf_type = teal.transform::choices_selected(c("plain", "log", "log-log"), "plain", TRUE),
                    font_size = c(11L, 1L, 30),
                    xticks = NULL,
                    control_annot_surv_med = tern::control_surv_med_annot(),
                    control_annot_coxph = tern::control_coxph_annot(x = 0.27, y = 0.35, w = 0.3),
                    legend_pos = c(0.9, 0.5),
                    rel_height_plot = c(80L, 0L, 100L),
                    plot_height = c(800L, 400L, 5000L),
                    plot_width = NULL,
                    pre_output = NULL,
                    post_output = NULL,
                    transformators = list(),
                    decorators = list()) {
  message("Initializing tm_g_km")

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(strata_var, "choices_selected")
  checkmate::assert_class(facet_var, "choices_selected")
  checkmate::assert_class(time_unit_var, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(cnsr_var, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_class(conf_type, "choices_selected")
  checkmate::assert_numeric(xticks, null.ok = TRUE)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  assert_decorators(decorators, "plot")

  args <- as.list(environment())
  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    facet_var = cs_to_des_select(facet_var, dataname = parentname, multiple = FALSE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    time_unit_var = cs_to_des_select(time_unit_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_g_km,
    ui = ui_g_km,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        label = label,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        plot_height = plot_height,
        plot_width = plot_width,
        xticks = xticks,
        control_annot_surv_med = control_annot_surv_med,
        control_annot_coxph = control_annot_coxph,
        legend_pos = legend_pos,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_g_km <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$strata_var,
    a$facet_var,
    a$aval_var,
    a$cnsr_var,
    a$time_unit_var
  )

  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      verbatimTextOutput(outputId = ns("text")),
      teal.widgets::plot_with_settings_ui(
        id = ns("myplot")
      )
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "strata_var", "facet_var", "aval_var", "cnsr_var")]),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("facet_var"),
        label = "Facet Plots by",
        data_extract_spec = a$facet_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      tags$div(
        class = "arm-comp-box",
        bslib::input_switch(
          id = ns("compare_arms"),
          label = "Compare Treatments",
          value = !is.null(a$arm_ref_comp)
        ),
        conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          tags$div(
            uiOutput(
              ns("arms_buckets"),
              title = paste(
                "Multiple reference groups are automatically combined into a single group when more than one",
                "value is selected."
              )
            ),
            checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            ),
            teal.transform::data_extract_ui(
              id = ns("strata_var"),
              label = "Stratify by",
              data_extract_spec = a$strata_var,
              is_single_dataset = is_single_dataset_value
            )
          )
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "plot")),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Comparison settings",
            radioButtons(
              ns("pval_method_coxph"),
              label = HTML(
                paste(
                  "p-value method for ",
                  tags$span(class = "text-primary", "Coxph"),
                  " (Hazard Ratio)",
                  sep = ""
                )
              ),
              choices = c("wald", "log-rank", "likelihood"),
              selected = "log-rank"
            ),
            radioButtons(
              ns("ties_coxph"),
              label = HTML(
                paste(
                  "Ties for ",
                  tags$span(class = "text-primary", "Coxph"),
                  " (Hazard Ratio)",
                  sep = ""
                )
              ),
              choices = c("exact", "breslow", "efron"),
              selected = "exact"
            )
          )
        )
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional plot settings",
          textInput(
            inputId = ns("xticks"),
            label = "Specify break intervals for x-axis e.g. 0 ; 500",
            value = if (!is.null(a$xticks)) {
              paste(a$xticks, collapse = " ; ")
            }
          ),
          radioButtons(
            ns("yval"),
            tags$label("Value on y-axis", class = "text-primary"),
            choices = c("Survival probability", "Failure probability"),
            selected = c("Survival probability"),
          ),
          teal.widgets::optionalSliderInput(
            ns("ylim"),
            tags$label("y-axis limits", class = "text-primary"),
            value = c(0, 1),
            min = 0, max = 1
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"),
            "Table Font Size",
            a$font_size,
            ticks = FALSE, step = 1
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("rel_height_plot"),
            "Relative Height of Plot (%)",
            a$rel_height_plot,
            ticks = FALSE, step = 1
          ),
          checkboxInput(
            inputId = ns("show_ci_ribbon"),
            label = "Show CI ribbon",
            value = FALSE,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns("show_km_table"),
            label = "Show KM table",
            value = TRUE,
            width = "100%"
          ),
          teal.widgets::optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          teal.widgets::optionalSelectInput(
            ns("conf_type"),
            "Confidence Interval Type",
            a$conf_type$choices,
            a$conf_type$selected,
            multiple = FALSE,
            fixed = a$conf_type$fixed
          ),
          textInput(ns("xlab"), "X-axis label", "Time"),
          teal.transform::data_extract_ui(
            id = ns("time_unit_var"),
            label = "Time Unit Variable",
            data_extract_spec = a$time_unit_var,
            is_single_dataset = is_single_dataset_value
          )
        )
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_g_km <- function(id,
                     data,
                     dataname,
                     parentname,
                     paramcd,
                     arm_var,
                     arm_ref_comp,
                     strata_var,
                     facet_var,
                     aval_var,
                     cnsr_var,
                     label,
                     time_unit_var,
                     plot_height,
                     plot_width,
                     xticks,
                     control_annot_surv_med,
                     control_annot_coxph,
                     legend_pos,
                     decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    # Setup arm variable selection, default reference arms and default
    # comparison arms for encoding panel
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data()[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_tte",
      on_off = reactive(input$compare_arms)
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        arm_var = arm_var,
        paramcd = paramcd,
        strata_var = strata_var,
        facet_var = facet_var,
        time_unit_var = time_unit_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        cnsr_var = shinyvalidate::sv_required("A censor variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("An endpoint is required")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()

      if (isTRUE(input$compare_arms)) {
        iv$add_validator(iv_arm_ref)
      }

      iv$add_rule("font_size", shinyvalidate::sv_required("Plot tables font size must be greater than or equal to 5"))
      iv$add_rule("font_size", shinyvalidate::sv_gte(5, "Plot tables font size must be greater than or equal to 5"))
      iv$add_rule("ylim", shinyvalidate::sv_required("Please choose a range for y-axis limits"))
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule("conf_type", shinyvalidate::sv_required("Please choose a confidence interval type"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(
          0, 1,
          inclusive = c(FALSE, FALSE),
          message_fmt = "Confidence level must be between 0 and 1"
        )
      )
      iv$add_rule(
        "conf_type",
        shinyvalidate::sv_in_set(
          c("plain", "log", "log-log"),
          message_fmt = "Confidence interval type must be one of {values_text}."
        )
      )
      iv$add_rule("xticks", shinyvalidate::sv_optional())
      iv$add_rule(
        "xticks",
        function(value) {
          val <- as_numeric_from_comma_sep_str(value, sep = ";")
          if (anyNA(val) || any(val < 0)) {
            "All break intervals for x-axis must be non-negative numbers separated by semicolons"
          } else if (all(val == 0)) {
            "At least one break interval for x-axis must be > 0"
          }
        }
      )
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
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

    validate_checks <- reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      anl_m <- anl_inputs()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_facet_var <- as.vector(anl_m$columns_source$facet_var)
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]
      input_time_unit_var <- as.vector(anl_m$columns_source$time_unit_var)

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var, input_facet_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var, input_time_unit_var),
        arm_var = input_arm_var
      )

      # validate arm levels
      if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }
      if (isTRUE(input$compare_arms)) {
        validate_args <- append(
          validate_args,
          list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
        )
      }
      do.call(what = "validate_standard_inputs", validate_args)

      NULL
    })

    all_q <- reactive({
      validate_checks()

      anl_m <- anl_inputs()

      anl <- anl_q()[["ANL"]]
      teal::validate_has_data(anl, 2)

      input_xticks <- if (!is.null(input$xticks)) {
        as_numeric_from_comma_sep_str(input$xticks, sep = ";")
      }

      input_paramcd <- as.character(unique(anl[[as.vector(anl_m$columns_source$paramcd)]]))
      title <- paste("KM Plot of", input_paramcd)

      my_calls <- template_g_km(
        dataname = "ANL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        compare_arm = input$compare_arms,
        combine_comp_arms = input$combine_comp_arms,
        aval_var = as.vector(anl_m$columns_source$aval_var),
        cnsr_var = as.vector(anl_m$columns_source$cnsr_var),
        strata_var = as.vector(anl_m$columns_source$strata_var),
        time_points = NULL,
        time_unit_var = as.vector(anl_m$columns_source$time_unit_var),
        facet_var = as.vector(anl_m$columns_source$facet_var),
        annot_surv_med = input$show_km_table,
        annot_coxph = input$compare_arms,
        control_annot_surv_med = control_annot_surv_med,
        control_annot_coxph = control_annot_coxph,
        legend_pos = legend_pos,
        xticks = input_xticks,
        font_size = input$font_size,
        pval_method = input$pval_method_coxph,
        conf_level = as.numeric(input$conf_level),
        conf_type = input$conf_type,
        ties = input$ties_coxph,
        xlab = input$xlab,
        yval = ifelse(input$yval == "Survival probability", "Survival", "Failure"),
        ylim = input$ylim,
        rel_height_plot = input$rel_height_plot / 100,
        ci_ribbon = input$show_ci_ribbon,
        title = title
      )
      obj <- anl_q()
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
