#' Time To Event Table Teal Module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @name timetoevent
#'
NULL


#' Control Function for Time-to-Event Teal Module
#'
#' Controls the arguments for Cox regressions and Survival analysis results.
#'
#' @param coxph (`list`)\cr
#'   parameters for comparison, specified using [tern::control_coxph].
#' @param surv_time (`list`)\cr
#'   parameters for comparison, specified using [tern::control_surv_time].
#' @param surv_timepoint (`list`)\cr
#'   parameters for comparison, specified using [tern::control_surv_timepoint].
#' @export
#' @examples
#'
#' control_tte(
#'   coxph = control_coxph(conf_level = 0.8),
#'   surv_time = control_surv_time(conf_type = "log-log")
#' )
#'
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
  surv_timepoint = control_surv_timepoint(
    conf_level = 0.95,
    conf_type = c("plain", "none", "log", "log-log")
  )
) {

  list(
    surv_time = do.call("control_surv_time", surv_time),
    coxph = do.call("control_coxph", coxph),
    surv_timepoint = do.call("control_surv_timepoint", surv_timepoint)
  )
}


#' @describeIn timetoevent template for time-to-event analysis.
#'
#' @param time_points (`numeric`)\cr time points for the survival estimations.
#' @param time_unit (`string`).
#' @param event_desc_var (`string`)\cr variable name with the event description
#'   information.
#' @param control (`list`)\cr list of settings for the analysis.
#' @export
#' @examples
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' a <- template_tte(
#'   dataname = "ADTTE",
#'   arm_var = "ARMCD",
#'   compare_arm = TRUE,
#'   ref_arm = "ARM A",
#'   param = "OS",
#'   strata_var = "SEX",
#'   time_points = c(183, 365, 548),
#'   event_desc_var = "EVNTDESC",
#'   control = control_tte(
#'     coxph = control_coxph(conf_level = 0.987),
#'     surv_time = control_surv_time(quantiles = c(0.1, 0.9)),
#'     surv_timepoint = control_surv_timepoint(conf_level = 0.123)
#'   )
#' )
#'
#' b <- mapply(eval, a)
#' b$table
#'
template_tte <- function(dataname,
                         arm_var = "ARMCD",
                         ref_arm = NULL,
                         compare_arm = TRUE,
                         paramcd = "OS",
                         strata_var = NULL,
                         time_points = NULL,
                         time_unit = "Days",
                         event_desc_var = "EVNTDESC",
                         control = control_tte()) {

  y <- list()

  y$data <- substitute(
    expr = anl <- df  %>%
      filter(PARAMCD == paramcd) %>%
      mutate(
        is_event = CNSR == 0,
        is_not_event = CNSR == 1,
        EVNT1 = factor(
          case_when(
            is_event == TRUE ~ "Patients with event (%)",
            is_event == FALSE ~ "Patients without event (%)"
          )
        ),
        EVNTDESC = factor(event_desc_var)
      ),
    env = list(
      df = as.name(dataname),
      paramcd = paramcd,
      event_desc_var = as.name(event_desc_var)
    )
  )

  layout_list <- list()

  layout_list <- add_expr(layout_list, quote(basic_table()))

  layout_list <- add_expr(
    layout_list,
    if (compare_arm) {
      substitute(
        expr = split_cols_by(var = arm_var, ref_group = ref_arm),
        env = list(arm_var = arm_var, ref_arm = ref_arm)
      )
    } else {
      substitute(
        expr = split_cols_by(var = arm_var),
        env = list(arm_var = arm_var)
      )
    }
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = add_colcounts() %>%
        split_rows_by(
          var = "EVNT1",
          split_fun = keep_split_levels("Patients with event (%)")
        ) %>%
        summarize_row_groups() %>%
        summarize_vars(vars = event_desc_var, .stats = "count") %>%
        summarize_vars(
          "is_not_event", .stats = "count_fraction",
          .labels = c(count_fraction = "Patients without event (%)"),
          nested = FALSE, show_labels = "hidden"
        ),
      env = list(event_desc_var = event_desc_var)
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = surv_time(
        vars = "AVAL",
        var_labels = "Time to Event (Months)",
        is_event = "is_event",
        control = list(
          conf_level = conf_level,
          conf_type = conf_type,
          quantiles = quantiles
        )
      ),
      env = control$surv_time
    )
  )

  if (compare_arm) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = coxph_pairwise(
          vars = "AVAL", is_event = "is_event",
          var_labels = c("Unstratified Analysis"),
          control = list(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          )
        ),
        env = control$coxph
      )
    )
  }

  if (compare_arm && !is.null(strata_var)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = coxph_pairwise(
          vars = "AVAL", is_event = "is_event",
          var_labels = c("Stratified Analysis"),
          strat = strata_var,
          control = control_coxph(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          )
        ),
        env = list(
          strata_var = strata_var,
          pval_method = control$coxph$pval_method,
          ties = control$coxph$ties,
          conf_level = control$coxph$conf_level
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl)
  )
  y
}


#' Time To Event Table Teal Module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param dataname (\code{character}) analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as \code{arm_var}
#' @param arm_ref_comp (\code{\link[teal]{choices_selected}}) optional, if specified it must be a named list with each
#'   element corresponding to an arm variable in \code{ADSL} and the element must
#'   be another list with the elements named \code{ref} and \code{comp} that the
#'   defined the default reference and comparison arms when the arm variable is
#'   changed.
#' @param paramcd \code{\link[teal]{choices_selected}} object with all available choices and preselected option for
#' variable names that can be used as \code{PARAMCD} variable
#' @param strata_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for stratification
#' @param time_points \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used \code{REFACTOR}
#' @param time_unit (\code{character}) with unit of \code{dataname$AVAL}, please use singular e.g. month instead
#'   of months
#' @param event_desc_var (\code{character}) variable name with the event description information,
#'   optional
#'
#' @details This module produces a response summary table that is similar to
#'   STREAM template \code{ttet01}. The core functionality is based on
#'   \code{REFACTOR} from the \code{tern} package.\cr
#' This modules expects that the analysis data has the following variables
#'
#' \tabular{ll}{
#'  \code{AVAL} \tab time to event\cr
#'  \code{CNSR} \tab boolean or 0,1 is element in \code{AVAL} censored\cr
#'  \code{PARAMCD} \tab variable used to filter for endpoint (e.g. OS), after
#'  filtering for \code{paramcd} one observation per patient is expected
#' }
#'
#' The arm variables, stratification variables and taken from the \code{ADSL}
#' data.
#'
#'
#' @template author_waddella
#'
#' @export
#' @import magrittr
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' arm_ref_comp = list(
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   ),
#'    ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   )
#' )
#'
#' app <- init(
#'     data = cdisc_data(
#'       cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'       cdisc_dataset("ADTTE", ADTTE, code = "ADTTE <- radtte(cached = TRUE)"),
#'       check = TRUE
#'     ),
#'     modules = root_modules(
#'         tm_t_tte(
#'             label = "Time To Event Table",
#'             dataname = 'ADTTE',
#'             arm_var = choices_selected(c("ARM", "ARMCD", "ACTARMCD"), "ARM"),
#'             arm_ref_comp = arm_ref_comp,
#'             paramcd = choices_selected(value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"),
#'             strata_var = choices_selected(c("SEX", "BMRKR2"), "SEX"),
#'             time_points = choices_selected(c(6, 8), 6),
#'             time_unit = "month",
#'             event_desc_var = "EVNTDESC"
#'         )
#'     )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_tte <- function(label,
                     dataname,
                     arm_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     strata_var,
                     time_points,
                     time_unit = "months",
                     event_desc_var = NULL,
                     pre_output = NULL,
                     post_output = NULL
) {
  module(
    label = label,
    ui = function(id, datasets) {
      ns <- NS(id)
      htmlOutput(ns("tbd"))
    },
    server = function(input, output, session, datasets) {
      output$tbd <- renderUI({
        p("Module is currently refactored")
      })
    },
    filters = "ADSL"
  )
}
