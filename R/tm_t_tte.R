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
#' @param arm_var ([teal.picks::variables()])\cr variable for treatment arm.
#' @param paramcd ([teal.picks::variables()])\cr variable for parameter code.
#'   A `values()` selector is added internally to allow filtering parameter values.
#' @param strata_var ([teal.picks::variables()])\cr variable(s) for stratification.
#' @param aval_var ([teal.picks::variables()])\cr variable for analysis value (time-to-event).
#' @param cnsr_var ([teal.picks::variables()])\cr variable for censoring indicator.
#' @param time_unit_var ([teal.picks::variables()])\cr variable for time unit.
#' @param event_desc_var ([teal.picks::variables()])\cr variable for event description.
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
                     arm_var = variables(choices = c("ARM", "ARMCD", "ACTARMCD")),
                     arm_ref_comp = NULL,
                     paramcd = variables(choices = "PARAMCD"),
                     strata_var = variables(
                       choices = c("SEX", "BMRKR2"),
                       selected = "SEX"
                     ),
                     aval_var = variables(choices = "AVAL"),
                     cnsr_var = variables(choices = "CNSR"),
                     conf_level_coxph = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                     conf_level_survfit = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                     time_points = teal.transform::choices_selected(c(182, 243), 182),
                     time_unit_var = variables(choices = "AVALU"),
                     event_desc_var = variables(choices = "EVNTDESC"),
                     add_total = FALSE,
                     total_label = default_total_label(),
                     na_level = tern::default_na_str(),
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.widgets::basic_table_args(),
                     transformators = list(),
                     decorators = list()) {
  message("Initializing tm_t_tte")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(paramcd, "variables")
  checkmate::assert_class(strata_var, "variables")
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(cnsr_var, "variables")
  checkmate::assert_class(conf_level_coxph, "choices_selected")
  checkmate::assert_class(conf_level_survfit, "choices_selected")
  checkmate::assert_class(time_points, "choices_selected")
  checkmate::assert_class(time_unit_var, "variables")
  checkmate::assert_class(event_desc_var, "variables")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  arm_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), arm_var)
  strata_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), strata_var)
  paramcd <- teal.picks::picks(teal.picks::datasets(dataname, dataname), paramcd, values())
  aval_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), aval_var)
  cnsr_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), cnsr_var)
  time_unit_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), time_unit_var)
  event_desc_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), event_desc_var)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_tte,
    ui = ui_t_tte,
    ui_args = args[names(args) %in% names(formals(ui_t_tte))],
    server_args = args[names(args) %in% names(formals(srv_t_tte))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_t_tte <- function(id,
                     arm_var,
                     arm_ref_comp,
                     paramcd,
                     aval_var,
                     cnsr_var,
                     strata_var,
                     event_desc_var,
                     time_unit_var,
                     time_points,
                     conf_level_coxph,
                     conf_level_survfit,
                     add_total,
                     pre_output,
                     post_output,
                     decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Endpoint"),
        picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Analysis Variable"),
        picks_ui(ns("aval_var"), aval_var)
      ),
      tags$div(
        tags$label("Censor Variable"),
        picks_ui(ns("cnsr_var"), cnsr_var)
      ),
      tags$div(
        tags$label("Select Treatment Variable"),
        picks_ui(ns("arm_var"), arm_var)
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
            uiOutput(ns("arms_buckets")),
            uiOutput(ns("helptext_ui")),
            checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            ),
            tags$div(
              tags$label("Stratify by"),
              picks_ui(ns("strata_var"), strata_var)
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("!input['", ns("compare_arms"), "']"),
        checkboxInput(ns("add_total"), "Add All Patients column", value = add_total)
      ),
      teal.widgets::optionalSelectInput(ns("time_points"),
        "Time Points",
        time_points$choices,
        time_points$selected,
        multiple = TRUE,
        fixed = time_points$fixed
      ),
      tags$div(
        tags$label("Event Description Variable"),
        picks_ui(ns("event_desc_var"), event_desc_var)
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        bslib::accordion_panel(
          "Comparison settings",
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
          ),
          teal.widgets::optionalSelectInput(
            inputId = ns("conf_level_coxph"),
            label = HTML(
              paste(
                "Confidence Level for ",
                tags$span(class = "text-primary", "Coxph"),
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            conf_level_coxph$choices,
            conf_level_coxph$selected,
            multiple = FALSE,
            fixed = conf_level_coxph$fixed
          )
        )
      ),
      bslib::accordion_panel(
        "Additional table settings",
        open = TRUE,
        teal.widgets::optionalSelectInput(
          inputId = ns("conf_level_survfit"),
          label = HTML(
            paste(
              "Confidence Level for ",
              tags$span(class = "text-primary", "Survfit"),
              " (KM Median Estimate & Event Free Rate)",
              sep = ""
            )
          ),
          conf_level_survfit$choices,
          conf_level_survfit$selected,
          multiple = FALSE,
          fixed = conf_level_survfit$fixed
        ),
        radioButtons(
          ns("conf_type_survfit"),
          "Confidence Level Type for Survfit",
          choices = c("plain", "log", "log-log"),
          selected = "plain"
        ),
        sliderInput(
          inputId = ns("probs_survfit"),
          label = "KM Estimate Percentiles",
          min = 0.01,
          max = 0.99,
          value = c(0.25, 0.75),
          width = "100%"
        ),
        tags$div(
          tags$label("Time Unit Variable"),
          picks_ui(ns("time_unit_var"), time_unit_var)
        )
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table")),
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_t_tte <- function(id,
                      data,
                      arm_var,
                      paramcd,
                      aval_var,
                      cnsr_var,
                      strata_var,
                      event_desc_var,
                      dataname,
                      parentname,
                      arm_ref_comp,
                      time_unit_var,
                      add_total,
                      total_label,
                      label,
                      na_level,
                      basic_table_args,
                      decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- picks_srv(id = "",
      picks = list(
        arm_var = arm_var,
        paramcd = paramcd,
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        strata_var = strata_var,
        event_desc_var = event_desc_var,
        time_unit_var = time_unit_var
      ),
      data = data
    )

    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = "arm_var-variables-selected",
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_tte",
      on_off = reactive(input$compare_arms)
    )

    output$helptext_ui <- renderUI({
      req(selectors$arm_var()$variables$selected)
      helpText("Multiple reference groups are automatically combined into a single group.")
    })

    anl_selectors <- selectors
    adsl_selectors <- selectors[c("arm_var", "strata_var")]

    data_with_card <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj
    })
    merged_anl <- merge_srv("merge_anl", data = data_with_card, selectors = anl_selectors, output_name = "ANL")
    merged_adsl_anl <- merge_srv(
      "merge_adsl_anl",
      data = merged_anl$data,
      selectors = adsl_selectors,
      output_name = "ANL_ADSL"
    )
    anl_q <- merged_adsl_anl$data

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      input_arm_var <- anl_selectors$arm_var()$variables$selected
      input_strata_var <- anl_selectors$strata_var()$variables$selected
      input_aval_var <- anl_selectors$aval_var()$variables$selected
      input_cnsr_var <- anl_selectors$cnsr_var()$variables$selected
      input_event_desc <- anl_selectors$event_desc_var()$variables$selected
      input_time_unit_var <- anl_selectors$time_unit_var()$variables$selected
      input_paramcd_var <- anl_selectors$paramcd()$variables$selected

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_paramcd_var, input_aval_var,
          input_cnsr_var, input_event_desc, input_time_unit_var
        ),
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

      # check that there is at least one record with no missing data
      validate(shiny::need(
        !all(is.na(anl[[input_aval_var]])),
        "ANCOVA table cannot be calculated as all values are missing."
      ))

      NULL
    })

    # The R-code corresponding to the analysis.
    all_q <- reactive({
      validate_checks()

      input_strata_var <- anl_selectors$strata_var()$variables$selected

      my_calls <- template_tte(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = anl_selectors$arm_var()$variables$selected,
        paramcd = anl_selectors$paramcd()$variables$selected,
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        compare_arm = input$compare_arms,
        combine_comp_arms = input$combine_comp_arms && input$compare_arms,
        aval_var = anl_selectors$aval_var()$variables$selected,
        cnsr_var = anl_selectors$cnsr_var()$variables$selected,
        strata_var = if (length(input_strata_var) != 0) input_strata_var else NULL,
        time_points = as.numeric(input$time_points),
        time_unit_var = anl_selectors$time_unit_var()$variables$selected,
        event_desc_var = anl_selectors$event_desc_var()$variables$selected,
        control = control_tte(
          coxph = tern::control_coxph(
            pval_method = input$pval_method_coxph,
            ties = input$ties_coxph,
            conf_level = as.numeric(input$conf_level_coxph)
          ),
          surv_time = control_surv_time(
            conf_level = as.numeric(input$conf_level_survfit),
            conf_type = input$conf_type_survfit,
            quantiles = input$probs_survfit
          ),
          surv_timepoint = tern::control_surv_timepoint(
            conf_level = as.numeric(input$conf_level_survfit),
            conf_type = input$conf_type_survfit
          )
        ),
        add_total = input$add_total,
        total_label = total_label,
        na_level = na_level,
        basic_table_args = basic_table_args
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(id = "table", table_r = table_r)

    decorated_table_q
  })
}
