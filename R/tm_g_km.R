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
#' @param arm_var,paramcd,strata_var,aval_var,cnsr_var,time_unit_var ([`teal.picks::variables()`],
#'   [`teal.picks::picks()`], or legacy `choices_selected` / `value_choices`)\cr encodings; legacy inputs are coerced with a deprecation warning.
#' @param facet_var (`NULL`, [`teal.picks::variables()`], or legacy `choices_selected`)\cr
#'   faceting variable(s). The default `NULL` means no faceting (same as legacy `choices_selected` with
#'   `selected = NULL`). Do not pass [`teal.picks::variables()`] with `selected` of length 0 — [`teal.picks`]
#'   requires at least one selected level; use `NULL` or legacy [`teal.transform::choices_selected()`] instead.
#' @param conf_level,conf_type ([`teal.picks::values()`] or legacy `choices_selected`)\cr confidence UI inputs.
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
#'       parentname = "ADSL",
#'       arm_var = variables(
#'         choices = c("ARM", "ARMCD", "ACTARMCD"),
#'         selected = "ARM",
#'         multiple = FALSE
#'       ),
#'       paramcd = picks(
#'         datasets("ADTTE"),
#'         variables("PARAMCD", fixed = TRUE),
#'         values(
#'           choices = levels(ADTTE$PARAMCD),
#'           selected = "OS",
#'           multiple = FALSE
#'         )
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       strata_var = variables(
#'         choices = c("SEX", "BMRKR2"),
#'         selected = "SEX",
#'         multiple = TRUE
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
                    parentname = NULL,
                    arm_var = teal.picks::variables(
                      choices = c("ARM", "ARMCD", "ACTARMCD"),
                      selected = "ARM",
                      multiple = FALSE
                    ),
                    arm_ref_comp = NULL,
                    paramcd = teal.picks::picks(
                      teal.picks::variables("PARAMCD", fixed = TRUE),
                      teal.picks::values(
                        choices = c("OS", "PFS", "EFS"),
                        selected = "OS",
                        multiple = FALSE
                      ),
                      check_dataset = FALSE
                    ),
                    strata_var = teal.picks::variables(
                      choices = c("SEX", "BMRKR2"),
                      selected = "SEX",
                      multiple = TRUE
                    ),
                    facet_var = NULL,
                    time_unit_var = teal.picks::variables("AVALU", fixed = TRUE),
                    aval_var = teal.picks::variables("AVAL", fixed = TRUE),
                    cnsr_var = teal.picks::variables("CNSR", fixed = TRUE),
                    conf_level = teal.picks::values(
                      c("0.95", "0.9", "0.8"),
                      selected = "0.95",
                      keep_order = TRUE,
                      multiple = FALSE
                    ),
                    conf_type = teal.picks::values(
                      c("plain", "log", "log-log"),
                      selected = "plain",
                      keep_order = TRUE,
                      multiple = FALSE
                    ),
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

  if (is.null(parentname)) {
    parentname <- "ADSL"
  }

  if (is.null(facet_var)) {
    facet_var <- teal.transform::choices_selected(
      teal.transform::variable_choices(parentname, c("SEX", "BMRKR2")),
      selected = NULL,
      multiple = FALSE
    )
  }

  arm_var <- migrate_choices_selected_to_variables(arm_var, arg_name = "arm_var")
  strata_var <- migrate_choices_selected_to_variables(strata_var, arg_name = "strata_var", multiple = TRUE)
  facet_var <- migrate_choices_selected_to_variables(facet_var, arg_name = "facet_var")
  aval_var <- migrate_choices_selected_to_variables(aval_var, arg_name = "aval_var")
  cnsr_var <- migrate_choices_selected_to_variables(cnsr_var, arg_name = "cnsr_var")
  time_unit_var <- migrate_choices_selected_to_variables(time_unit_var, arg_name = "time_unit_var")
  paramcd <- migrate_value_choices_to_picks(paramcd, multiple = FALSE, arg_name = "paramcd")
  conf_level <- migrate_choices_selected_to_values(
    conf_level,
    arg_name = "conf_level",
    multiple = FALSE
  )
  conf_type <- migrate_choices_selected_to_values(
    conf_type,
    arg_name = "conf_type",
    multiple = FALSE
  )

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
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
  teal::assert_decorators(decorators, "plot")

  arm_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), arm_var)
  strata_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), strata_var)
  facet_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), facet_var)
  aval_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), aval_var)
  cnsr_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), cnsr_var)
  time_unit_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), time_unit_var)
  paramcd <- create_picks_helper(teal.picks::datasets(dataname, dataname), paramcd)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_km,
    ui = ui_g_km,
    ui_args = args[names(args) %in% names(formals(ui_g_km))],
    server_args = args[names(args) %in% names(formals(srv_g_km))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_km <- function(id,
                    arm_var,
                    paramcd,
                    strata_var,
                    facet_var,
                    aval_var,
                    cnsr_var,
                    time_unit_var,
                    conf_level,
                    conf_type,
                    arm_ref_comp,
                    font_size,
                    rel_height_plot,
                    xticks,
                    pre_output,
                    post_output,
                    decorators) {
  ns <- NS(id)
  conf_level$fixed <- conf_level$fixed %||% FALSE
  conf_type$fixed <- conf_type$fixed %||% FALSE

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      verbatimTextOutput(outputId = ns("text")),
      teal.widgets::plot_with_settings_ui(
        id = ns("myplot")
      )
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Endpoint:"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Analysis Variable:"),
        teal.picks::picks_ui(ns("aval_var"), aval_var)
      ),
      tags$div(
        tags$label("Censor Variable:"),
        teal.picks::picks_ui(ns("cnsr_var"), cnsr_var)
      ),
      tags$div(
        tags$label("Facet Plots by:"),
        teal.picks::picks_ui(ns("facet_var"), facet_var)
      ),
      tags$div(
        tags$label("Select Treatment Variable:"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      ),
      tags$div(
        class = "arm-comp-box",
        bslib::input_switch(
          id = ns("compare_arms"),
          label = "Compare Treatments",
          value = !is.null(arm_ref_comp)
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
            tags$div(
              tags$label("Stratify by:"),
              teal.picks::picks_ui(ns("strata_var"), strata_var)
            )
          )
        )
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "plot")),
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
        open = FALSE,
        bslib::accordion_panel(
          title = "Additional plot settings",
          textInput(
            inputId = ns("xticks"),
            label = "Specify break intervals for x-axis e.g. 0 ; 500",
            value = if (!is.null(xticks)) {
              paste(xticks, collapse = " ; ")
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
            font_size,
            ticks = FALSE, step = 1
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("rel_height_plot"),
            "Relative Height of Plot (%)",
            rel_height_plot,
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
            conf_level$choices,
            conf_level$selected,
            multiple = FALSE,
            fixed = conf_level$fixed
          ),
          teal.widgets::optionalSelectInput(
            ns("conf_type"),
            "Confidence Interval Type",
            conf_type$choices,
            conf_type$selected,
            multiple = FALSE,
            fixed = conf_type$fixed
          ),
          textInput(ns("xlab"), "X-axis label", "Time"),
          tags$div(
            tags$label("Time Unit Variable:"),
            teal.picks::picks_ui(ns("time_unit_var"), time_unit_var)
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
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

    selectors <- teal.picks::picks_srv(
      picks = list(
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        arm_var = arm_var,
        paramcd = paramcd,
        strata_var = strata_var,
        facet_var = facet_var,
        time_unit_var = time_unit_var
      ),
      data = data
    )

    arm_var_r <- reactive(selectors$arm_var()$variables$selected)

    arm_ref_iv <- arm_ref_comp_observer_picks(
      session,
      input,
      output,
      id_arm_var = "arm_var-variables-selected",
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_g_km",
      on_off = reactive(input$compare_arms),
      arm_var_r = arm_var_r
    )

    iv_plot <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("font_size", shinyvalidate::sv_required("Plot tables font size must be greater than or equal to 5"))
      iv$add_rule("font_size", shinyvalidate::sv_gte(5, "Plot tables font size must be greater than or equal to 5"))
      iv$add_rule("ylim", shinyvalidate::sv_required("Please choose a range for y-axis limits"))
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
      iv
    })

    validated_q <- reactive({
      obj <- req(data())

      validate_input(
        inputId = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message = "Please select an analysis variable."
      )
      validate_input(
        inputId = "cnsr_var-variables-selected",
        condition = !is.null(selectors$cnsr_var()$variables$selected),
        message = "Please select a censor variable."
      )
      validate_input(
        inputId = "arm_var-variables-selected",
        condition = !is.null(selectors$arm_var()$variables$selected),
        message = "Please select a treatment variable."
      )
      validate_input(
        inputId = "paramcd-values-selected",
        condition = !is.null(selectors$paramcd()$values$selected),
        message = "Please select an endpoint."
      )
      validate_input(
        inputId = "time_unit_var-variables-selected",
        condition = !is.null(selectors$time_unit_var()$variables$selected),
        message = "Please select a time unit variable."
      )
      validate_input(
        inputId = "conf_level",
        condition = !is.null(input$conf_level),
        message = "Please choose a confidence level."
      )
      validate_input(
        inputId = "conf_level",
        condition = as.numeric(input$conf_level) > 0 && as.numeric(input$conf_level) < 1,
        message = "Confidence level must be between 0 and 1."
      )
      validate_input(
        inputId = "conf_type",
        condition = !is.null(input$conf_type) && input$conf_type %in% c("plain", "log", "log-log"),
        message = "Please choose a confidence interval type."
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
      if (isTRUE(input$compare_arms)) {
        arm_ref_iv()
      }
      teal::validate_inputs(iv_plot())

      anl_q <- anl_inputs$data()
      ANL <- anl_q[["ANL"]]
      teal::validate_has_data(ANL, 2)

      adsl_filtered <- anl_q[[parentname]]
      anl_filtered <- anl_q[[dataname]]

      vm <- anl_inputs$variables()
      input_arm_var <- vm$arm_var[[1L]]
      input_strata_var <- vm$strata_var[[1L]]
      input_facet_var <- vm$facet_var[[1L]]
      input_aval_var <- vm$aval_var[[1L]]
      input_cnsr_var <- vm$cnsr_var[[1L]]
      input_paramcd_col <- vm$paramcd[[1L]]
      input_time_unit_var <- vm$time_unit_var[[1L]]

      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var, input_facet_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd_col, input_aval_var, input_cnsr_var, input_time_unit_var),
        arm_var = input_arm_var
      )

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

      obj <- anl_inputs$data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )

      ANL <- obj[["ANL"]]
      teal::validate_has_data(ANL, 2)

      vm <- anl_inputs$variables()

      input_xticks <- if (!is.null(input$xticks)) {
        as_numeric_from_comma_sep_str(input$xticks, sep = ";")
      }

      param_col <- vm$paramcd[[1L]]
      param_vals <- as.character(unique(ANL[[param_col]]))
      title <- paste("KM Plot of", paste(param_vals, collapse = ", "))

      my_calls <- template_g_km(
        dataname = "ANL",
        arm_var = vm$arm_var[[1L]],
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        compare_arm = input$compare_arms,
        combine_comp_arms = input$combine_comp_arms,
        aval_var = vm$aval_var[[1L]],
        cnsr_var = vm$cnsr_var[[1L]],
        strata_var = vm$strata_var[[1L]],
        time_points = NULL,
        time_unit_var = vm$time_unit_var[[1L]],
        facet_var = vm$facet_var[[1L]],
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
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_all_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "plot"),
      expr = quote(plot)
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
