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
#' @name tm_g_km
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_g_km
#' @param xticks (`numeric` or `NULL`)\cr numeric vector of tick positions or a single number with spacing
#'   for the x-axis. If `NULL` (default), users can specify this interactively in the module.
#'   If provided, the interactive input field is pre-populated with the specified values as a default.
#'   Users can then modify these values interactively, and their changes will take precedence over the default.
#'
#' @details
#' Encoding arguments use [`teal.picks`] (`variables()`, `picks()`, [`teal.picks::values()`], etc.).
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
#' @section Faceting:
#' Never pass [`teal.picks::variables()`] with empty `selected` (for example
#' `selected = character(0)`): that fails inside \pkg{teal.picks} while arguments are evaluated,
#' before `tm_g_km()` runs. For no faceting, omit `facet_var` or pass `facet_var = NULL`; the module
#' adds the `-- no selection --` choice via [`teal.transform::add_no_selected_choices()`].
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
#'           choices = unique(ADTTE$PARAMCD),
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
                    parentname = "ADSL",
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
                    font_size = c(11L, 1L, 30L),
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

  if (is.null(facet_var)) {
    facet_var <- teal.picks::picks(
      teal.picks::datasets(parentname, parentname),
      teal.picks::variables(c("SEX", "BMRKR2"), NULL, multiple = FALSE)
    )
  }

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
    server = srv_g_km_picks,
    ui = ui_g_km_picks,
    ui_args = args[names(args) %in% names(formals(ui_g_km_picks))],
    server_args = args[names(args) %in% names(formals(srv_g_km_picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

