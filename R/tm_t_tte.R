#' Control Function for Time-To-Event teal Module
#'
#' Controls the arguments for Cox regression and survival analysis results.
#'
#' @param coxph (`list`)\cr control parameters for Cox-PH model. See [tern::control_coxph()] for details.
#' @param surv_time (`list`)\cr control parameters for `survfit` model. See [tern::control_surv_time()] for details.
#' @param surv_timepoint (`list`)\cr control parameters for `survfit` model at time point. See
#'   [tern::control_surv_timepoint()] for details.
#'
#' @seealso [template_tte()], [tm_t_tte()]
#'
#' @keywords internal
control_tte <- function(
    surv_time = list(
      conf_level = 0.95,
      conf_type = "plain",
      quantiles = c(0.25, 0.75)
    ),
    coxph = list(
      pval_method = "log-rank",
      ties = "efron",
      conf_level = 0.95
    ),
    surv_timepoint = tern::control_surv_timepoint(
      conf_level = 0.95,
      conf_type = c("plain", "none", "log", "log-log")
    )) {
  list(
    surv_time = do.call("control_surv_time", surv_time, envir = getNamespace("tern")),
    coxph = do.call("control_coxph", coxph, envir = getNamespace("tern")),
    surv_timepoint = do.call("control_surv_timepoint", surv_timepoint, envir = getNamespace("tern"))
  )
}

#' Template: Time-To-Event
#'
#' Creates a valid expression to generate a time-to-event analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis. See [control_tte()] for details.
#' @param event_desc_var (`character`)\cr name of the variable with events description.
#' @param paramcd (`character`)\cr endpoint parameter value to use in the table title.
#'
#' @inherit template_arguments return
#'
#' @seealso [control_tte()], [tm_t_tte()]
#'
#' @keywords internal
template_tte <- function(dataname = "ANL",
                         parentname = "ADSL",
                         arm_var = "ARM",
                         paramcd,
                         ref_arm = NULL,
                         comp_arm = NULL,
                         compare_arm = FALSE,
                         combine_comp_arms = FALSE,
                         aval_var = "AVAL",
                         cnsr_var = "CNSR",
                         strata_var = NULL,
                         time_points = NULL,
                         time_unit_var = "AVALU",
                         event_desc_var = "EVNTDESC",
                         control = control_tte(),
                         add_total = FALSE,
                         total_label = default_total_label(),
                         na_level = tern::default_na_str(),
                         basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(cnsr_var)
  checkmate::assert_string(time_unit_var)
  checkmate::assert_string(event_desc_var)
  checkmate::assert_flag(compare_arm)
  checkmate::assert_flag(combine_comp_arms)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)

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
        is_event = cnsr_var == 0,
        is_not_event = cnsr_var == 1,
        EVNT1 = factor(
          dplyr::case_when(
            is_event == TRUE ~ "Patients with event (%)",
            is_event == FALSE ~ "Patients without event (%)"
          ),
          levels = c("Patients with event (%)", "Patients without event (%)")
        ),
        EVNTDESC = factor(event_desc_var)
      ),
      env = list(
        cnsr_var = as.name(cnsr_var),
        event_desc_var = as.name(event_desc_var)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = tern::df_explicit_na(na_level = na_str),
      env = list(na_str = na_level)
    )
  )

  y$data <- substitute(
    expr = {
      anl <- data_pipe
      parentname <- arm_preparation %>% tern::df_explicit_na(na_level = na_str)
    },
    env = list(
      data_pipe = pipe_expr(data_list),
      parentname = as.name(parentname),
      arm_preparation = prepare_arm(
        dataname = parentname,
        arm_var = arm_var,
        ref_arm = ref_arm,
        comp_arm = comp_arm,
        compare_arm = compare_arm,
        ref_arm_val = ref_arm_val
      ),
      na_str = na_level
    )
  )

  if (compare_arm && combine_comp_arms) {
    y$combine_comp_arms <- substitute(
      expr = groups <- combine_groups(fct = df[[group]], ref = ref_arm_val),
      env = list(
        df = as.name(parentname),
        group = arm_var,
        ref_arm_val = ref_arm_val
      )
    )
  }
  layout_list <- list()

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        show_colcounts = TRUE,
        title = paste("Time-To-Event Table for", paramcd),
        main_footer = if (compare_arm) {
          c(
            paste("p-value method for Coxph (Hazard Ratio):", control$coxph$pval_method),
            paste("Ties for Coxph (Hazard Ratio):", control$coxph$ties),
            paste("Confidence Level Type for Survfit:", control$surv_time$conf_type)
          )
        } else {
          paste("Confidence Level Type for Survfit:", control$surv_time$conf_type)
        }
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  if (!compare_arm && !combine_comp_arms && add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::split_cols_by(
          var = arm_var,
          split_fun = add_overall_level(total_label, first = FALSE)
        ),
        env = list(
          arm_var = arm_var,
          total_label = total_label
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      split_col_expr(
        compare = compare_arm,
        combine = combine_comp_arms,
        arm_var = arm_var,
        ref = ref_arm_val
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = tern::analyze_vars(
        "is_event",
        .stats = "count_fraction",
        .labels = c(count_fraction = "Patients with event (%)"),
        na_str = na_str
      ) %>%
        rtables::split_rows_by(
          "EVNT1",
          split_label = "Earliest contributing event",
          split_fun = rtables::keep_split_levels("Patients with event (%)"),
          label_pos = "visible",
          child_labels = "hidden",
          indent_mod = 1L,
        ) %>%
        rtables::split_rows_by(event_desc_var, split_fun = rtables::drop_split_levels) %>%
        rtables::summarize_row_groups(format = "xx", na_str = na_str) %>%
        tern::analyze_vars(
          "is_not_event",
          .stats = "count_fraction",
          .labels = c(count_fraction = "Patients without event (%)"),
          nested = FALSE,
          show_labels = "hidden",
          na_str = na_str
        ),
      env = list(
        event_desc_var = event_desc_var,
        na_str = na_level
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = tern::surv_time(
        vars = aval_var,
        var_labels = paste0("Time to Event (", as.character(anl$time_unit_var[1]), ")"),
        is_event = "is_event",
        control = list(
          conf_level = conf_level,
          conf_type = conf_type,
          quantiles = quantiles
        ),
        na_str = na_str,
        table_names = "time_to_event"
      ),
      env = c(
        aval_var = aval_var,
        control$surv_time,
        time_unit_var = as.name(time_unit_var),
        na_str = na_level
      )
    )
  )

  if (compare_arm) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::coxph_pairwise(
          vars = aval_var,
          is_event = "is_event",
          var_labels = c("Unstratified Analysis"),
          control = list(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          ),
          na_str = na_str,
          table_names = "unstratified"
        ),
        env = c(
          aval_var = aval_var,
          control$coxph,
          na_str = na_level
        )
      )
    )
  }

  if (compare_arm && !is.null(strata_var)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::coxph_pairwise(
          vars = aval_var,
          is_event = "is_event",
          var_labels = paste0("Stratified By: ", paste(strata_var, collapse = ", ")),
          strata = strata_var,
          control = tern::control_coxph(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          ),
          na_str = na_str,
          table_names = "stratified"
        ),
        env = list(
          aval_var = aval_var,
          strata_var = strata_var,
          pval_method = control$coxph$pval_method,
          ties = control$coxph$ties,
          conf_level = control$coxph$conf_level,
          na_str = na_level
        )
      )
    )
  }

  if (!is.null(time_points)) {
    method <- ifelse(compare_arm, "both", "surv")
    indents <- if (compare_arm) {
      c(
        "pt_at_risk" = 0L, "event_free_rate" = 0L, "rate_ci" = 0L,
        "rate_diff" = 1L, "rate_diff_ci" = 1L, "ztest_pval" = 1L
      )
    } else {
      NULL
    }
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::surv_timepoint(
          vars = aval_var,
          var_labels = as.character(anl$time_unit_var[1]),
          is_event = "is_event",
          time_point = time_points,
          method = method,
          control = tern::control_surv_timepoint(
            conf_level = conf_level,
            conf_type = conf_type
          ),
          .indent_mods = indents,
          na_str = na_str
        ),
        env = list(
          aval_var = aval_var,
          time_points = time_points,
          method = method,
          indents = indents,
          time_unit_var = as.name(time_unit_var),
          conf_level = control$surv_timepoint$conf_level,
          conf_type = control$surv_timepoint$conf_type,
          na_str = na_level
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parentname)
    },
    env = list(parentname = as.name(parentname))
  )

  y
}

#' teal Module: Time-To-Event Table
#'
#' This module produces a time-to-event analysis summary table, consistent with the TLG Catalog
#' template for `TTET01` available [here](
#' https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/ttet01.html).
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_tte
#' @param arm_var ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr variable for treatment arm.
#' @param paramcd ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr variable for parameter code.
#'   A `values()` selector is added internally to allow filtering parameter values.
#' @param strata_var ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr variable(s) for stratification.
#' @param aval_var ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr variable for analysis value (time-to-event).
#' @param cnsr_var ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr variable for censoring indicator.
#' @param time_unit_var ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr variable for time unit.
#' @param event_desc_var ([`teal.picks::variables()`], [`teal.picks::picks()`], legacy [`teal.transform::choices_selected()`],
#'   [`teal.transform::data_extract_spec()`], or a `list` of `data_extract_spec`)\cr
#'   variable for event description. S3 dispatch uses the class of `event_desc_var`
#'   ([`tm_t_tte.picks()`] / [`tm_t_tte.variables()`] vs legacy [`tm_t_tte.default()`] / [`tm_t_tte.list()`]).
#' @param conf_level_coxph ([teal.transform::choices_selected()])\cr object with all available choices and
#'   pre-selected option for confidence level, each within range of (0, 1).
#' @param conf_level_survfit ([teal.transform::choices_selected()])\cr object with all available choices and
#'   pre-selected option for confidence level, each within range of (0, 1).
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`TableTree` - output of `rtables::build_table()`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_tte(
#'    ..., # arguments for module
#'    decorators = list(
#'      table = teal_transform_module(...) # applied only to `table` output
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
#' @details
#' * The core functionality of this module is based on [tern::coxph_pairwise()], [tern::surv_timepoint()],
#'   and [tern::surv_time()] from the `tern` package.
#' * The arm and stratification variables are taken from the `parentname` data.
#' * The following variables are used in the module:
#'
#'   * `AVAL`: time to event
#'   * `CNSR`: 1 if record in `AVAL` is censored, 0 otherwise
#'   * `PARAMCD`: variable used to filter for endpoint (e.g. OS). After
#'     filtering for `PARAMCD` one observation per patient is expected
#'
#' @inherit module_arguments return seealso
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADTTE <- tmc_ex_adtte
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
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
#'     tm_t_tte(
#'       label = "Time To Event Table",
#'       dataname = "ADTTE",
#'       arm_var = variables(choices = c("ARM", "ARMCD", "ACTARMCD")),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = variables(choices = "PARAMCD"),
#'       strata_var = variables(choices = c("SEX", "BMRKR2"), selected = "SEX"),
#'       time_points = teal.transform::choices_selected(c(182, 243), 182)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_tte <- function(label,
                     dataname,
                     parentname = "ADSL",
                     arm_var = teal.picks::variables(choices = c("ARM", "ARMCD", "ACTARMCD")),
                     arm_ref_comp = NULL,
                     paramcd = teal.picks::variables(choices = "PARAMCD"),
                     strata_var = teal.picks::variables(
                       choices = c("SEX", "BMRKR2"),
                       selected = "SEX"
                     ),
                     aval_var = teal.picks::variables(choices = "AVAL", fixed = TRUE),
                     cnsr_var = teal.picks::variables(choices = "CNSR", fixed = TRUE),
                     conf_level_coxph = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                     conf_level_survfit = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                     time_points = teal.transform::choices_selected(c(182, 243), 182),
                     time_unit_var = teal.picks::variables(choices = "AVALU", fixed = TRUE),
                     event_desc_var = teal.picks::variables(choices = "EVNTDESC", fixed = TRUE),
                     add_total = FALSE,
                     total_label = default_total_label(),
                     na_level = tern::default_na_str(),
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.widgets::basic_table_args(),
                     transformators = list(),
                     decorators = list()) {
  message("Initializing tm_t_tte")
  if (inherits(event_desc_var, "choices_selected")) {
    event_desc_var <- migrate_choices_selected_to_variables(event_desc_var, arg_name = "event_desc_var")
  }
  UseMethod("tm_t_tte", event_desc_var)
}
