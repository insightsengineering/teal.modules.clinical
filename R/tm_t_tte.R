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
#' @keywords internal
#'
control_tte <- function(surv_time = list(
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
                        )) {
  list(
    surv_time = do.call("control_surv_time", surv_time),
    coxph = do.call("control_coxph", coxph),
    surv_timepoint = do.call("control_surv_timepoint", surv_timepoint)
  )
}


#' Template: Time-to-Event
#'
#' Creates a valid expression for time-to-event analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis,
#'   see [control_tte()].
#' @param event_desc_var (`character`)\cr name of the variable with events description.
#' @param paramcd (`character`)\cr endpoint parameter value to use in the table title.
#'
#' @seealso [tm_t_tte()]
#' @keywords internal
#'
template_tte <- function(dataname = "ANL",
                         parentname = "ADSL_FILTERED",
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
                         basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(aval_var),
    assertthat::is.string(cnsr_var),
    assertthat::is.string(time_unit_var),
    assertthat::is.string(event_desc_var),
    assertthat::is.flag(compare_arm),
    assertthat::is.flag(combine_comp_arms)
  )

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

  data_list <- add_expr(data_list, quote(df_explicit_na()))

  y$data <- substitute(
    expr = {
      anl <- data_pipe
      parentname <- arm_preparation %>% df_explicit_na()
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
      )
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
      module_table = teal.widgets::basic_table_args(title = paste("Time-To-Event Table for", paramcd))
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
          split_fun = add_overall_level("All Patients", first = FALSE)
        ),
        env = list(
          arm_var = arm_var
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
      expr = rtables::add_colcounts() %>%
        summarize_vars(
          "is_event",
          .stats = "count_fraction",
          .labels = c(count_fraction = "Patients with event (%)")
        ) %>%
        rtables::split_rows_by(
          "EVNT1",
          split_label = "Earliest contributing event",
          split_fun = keep_split_levels("Patients with event (%)"),
          label_pos = "visible",
          child_labels = "hidden",
          indent_mod = 1L,
        ) %>%
        rtables::split_rows_by(event_desc_var, split_fun = drop_split_levels) %>%
        rtables::summarize_row_groups(format = "xx") %>%
        summarize_vars(
          "is_not_event",
          .stats = "count_fraction",
          .labels = c(count_fraction = "Patients without event (%)"),
          nested = FALSE,
          show_labels = "hidden"
        ),
      env = list(
        event_desc_var = event_desc_var
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = surv_time(
        vars = aval_var,
        var_labels = paste0("Time to Event (", as.character(anl$time_unit_var[1]), ")"),
        is_event = "is_event",
        control = list(
          conf_level = conf_level,
          conf_type = conf_type,
          quantiles = quantiles
        ),
        table_names = "time_to_event"
      ),
      env = c(
        aval_var = aval_var,
        control$surv_time,
        time_unit_var = as.name(time_unit_var)
      )
    )
  )

  if (compare_arm) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = coxph_pairwise(
          vars = aval_var,
          is_event = "is_event",
          var_labels = c("Unstratified Analysis"),
          control = list(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          ),
          table_names = "unstratified"
        ),
        env = c(
          aval_var = aval_var,
          control$coxph
        )
      )
    )
  }

  if (compare_arm && !is.null(strata_var)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = coxph_pairwise(
          vars = aval_var,
          is_event = "is_event",
          var_labels = paste0("Stratified By: ", paste(strata_var, collapse = ", ")),
          strat = strata_var,
          control = control_coxph(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          ),
          table_names = "stratified"
        ),
        env = list(
          aval_var = aval_var,
          strata_var = strata_var,
          pval_method = control$coxph$pval_method,
          ties = control$coxph$ties,
          conf_level = control$coxph$conf_level
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
        expr = surv_timepoint(
          vars = aval_var,
          var_labels = as.character(anl$time_unit_var[1]),
          is_event = "is_event",
          time_point = time_points,
          method = method,
          control = control_surv_timepoint(
            conf_level = conf_level,
            conf_type = conf_type
          ),
          .indent_mods = indents,
          table_names = "time_points"
        ),
        env = list(
          aval_var = aval_var,
          time_points = time_points,
          method = method,
          indents = indents,
          time_unit_var = as.name(time_unit_var),
          conf_level = control$surv_timepoint$conf_level,
          conf_type = control$surv_timepoint$conf_type
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parentname)
      result
    },
    env = list(parentname = as.name(parentname))
  )

  y
}


#' Teal Module: Time To Event Table Teal Module
#'
#' @inheritParams module_arguments
#' @param conf_level_coxph ([choices_selected()])\cr object with all available choices and pre-selected option
#'   for confidence level, each within range of (0, 1).
#' @param conf_level_survfit ([choices_selected()])\cr object with all available choices and pre-selected option
#'   for confidence level, each within range of (0, 1).
#' @param event_desc_var (`character` or [data_extract_spec()])\cr variable name with the event description
#'   information, optional.
#'
#' @details This module produces a response summary table that is similar to
#'   STREAM template `ttet01`. The core functionality is based on
#'   [coxph_pairwise()], [surv_timepoint()] and [surv_time()] from package `tern`.\cr
#'   The following variables are used in the module:
#'
#' \tabular{ll}{
#'  `AVAL` \tab time to event\cr
#'  `CNSR` \tab boolean or 0,1 is element in `AVAL` censored\cr
#'  `PARAMCD` \tab variable used to filter for endpoint (e.g. OS), after
#'  filtering for `paramcd` one observation per patient is expected
#' }
#'
#' The arm and stratification variables and taken from the `parentname` data.
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
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
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADTTE", ADTTE, code = 'ADTTE <- synthetic_cdisc_data("latest")$adtte'),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_t_tte(
#'       label = "Time To Event Table",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")),
#'         "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"),
#'         "OS"
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("SEX", "BMRKR2")),
#'         "SEX"
#'       ),
#'       time_points = choices_selected(c(182, 243), 182),
#'       event_desc_var = choices_selected(
#'         variable_choices(ADTTE, "EVNTDESC"),
#'         "EVNTDESC",
#'         fixed = TRUE
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_tte <- function(label,
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
                     aval_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "AVAL"), "AVAL",
                       fixed = TRUE
                     ),
                     cnsr_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "CNSR"), "CNSR",
                       fixed = TRUE
                     ),
                     conf_level_coxph = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                     conf_level_survfit = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                     time_points,
                     time_unit_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "AVALU"), "AVALU",
                       fixed = TRUE
                     ),
                     event_desc_var = teal.transform::choices_selected("EVNTDESC", "EVNTDESC", fixed = TRUE),
                     add_total = FALSE,
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_tte")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(time_points, "choices_selected")
  checkmate::assert_class(conf_level_coxph, "choices_selected")
  checkmate::assert_class(conf_level_survfit, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    event_desc_var = cs_to_des_select(event_desc_var, dataname = dataname),
    time_unit_var = cs_to_des_select(time_unit_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_t_tte,
    ui = ui_t_tte,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_tte <- function(id, ...) {
  a <- list(...) # module args
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$aval_var,
    a$cnsr_var,
    a$strata_var,
    a$event_desc_var,
    a$time_unit_var
  )

  ns <- shiny::NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(
        a[c("arm_var", "paramcd", "aval_var", "cnsr_var", "strata_var", "event_desc_var")]
      ),
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
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::div(
        class = "arm-comp-box",
        shiny::tags$label("Compare Treatments"),
        shinyWidgets::switchInput(
          inputId = ns("compare_arms"),
          value = !is.null(a$arm_ref_comp),
          size = "mini"
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          shiny::div(
            shiny::selectInput(
              ns("ref_arm"),
              "Reference Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            shiny::helpText("Multiple reference groups are automatically combined into a single group."),
            shiny::selectInput(
              ns("comp_arm"),
              "Comparison Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            shiny::checkboxInput(
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
      shiny::conditionalPanel(
        condition = paste0("!input['", ns("compare_arms"), "']"),
        shiny::checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total)
      ),
      teal.widgets::optionalSelectInput(ns("time_points"),
        "Time Points",
        a$time_points$choices,
        a$time_points$selected,
        multiple = TRUE,
        fixed = a$time_points$fixed
      ),
      teal.transform::data_extract_ui(
        id = ns("event_desc_var"),
        label = "Event Description Variable",
        data_extract_spec = a$event_desc_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        teal.widgets::panel_item(
          "Comparison settings",
          shiny::radioButtons(
            ns("pval_method_coxph"),
            label = shiny::HTML(
              paste(
                "p-value method for ",
                shiny::tags$span(style = "color:darkblue", "Coxph"), # nolint
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            choices = c("wald", "log-rank", "likelihood"),
            selected = "log-rank"
          ),
          shiny::radioButtons(
            ns("ties_coxph"),
            label = shiny::HTML(
              paste(
                "Ties for ",
                shiny::tags$span(style = "color:darkblue", "Coxph"), # nolint
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            choices = c("exact", "breslow", "efron"),
            selected = "exact"
          ),
          teal.widgets::optionalSelectInput(
            inputId = ns("conf_level_coxph"),
            label = shiny::HTML(
              paste(
                "Confidence Level for ",
                shiny::tags$span(style = "color:darkblue", "Coxph"), # nolint
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            a$conf_level_coxph$choices,
            a$conf_level_coxph$selected,
            multiple = FALSE,
            fixed = a$conf_level_coxph$fixed
          )
        )
      ),
      teal.widgets::panel_item(
        "Additional table settings",
        teal.widgets::optionalSelectInput(
          inputId = ns("conf_level_survfit"),
          label = shiny::HTML(
            paste(
              "Confidence Level for ",
              shiny::tags$span(style = "color:darkblue", "Survfit"), # nolint
              " (KM Median Estimate & Event Free Rate)",
              sep = ""
            )
          ),
          a$conf_level_survfit$choices,
          a$conf_level_survfit$selected,
          multiple = FALSE,
          fixed = a$conf_level_survfit$fixed
        ),
        shiny::radioButtons(
          ns("conf_type_survfit"),
          "Confidence Level Type for Survfit",
          choices = c("plain", "log", "log-log"),
          selected = "plain"
        ),
        shiny::sliderInput(
          inputId = ns("probs_survfit"),
          label = "KM Estimate Percentiles",
          min = 0.01,
          max = 0.99,
          value = c(0.25, 0.75),
          width = "100%"
        ),
        teal.transform::data_extract_ui(
          id = ns("time_unit_var"),
          label = "Time Unit Variable",
          data_extract_spec = a$time_unit_var,
          is_single_dataset = is_single_dataset_value
        )
      )
    ),
    forms = teal::get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_tte <- function(id,
                      datasets,
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
                      label,
                      basic_table_args) {
  stopifnot(is_cdisc_data(datasets))
  shiny::moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()

    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel
    arm_ref_comp_observer(
      session, input,
      id_ref = "ref_arm", # from UI
      id_comp = "comp_arm", # from UI
      id_arm_var = extract_input("arm_var", parentname),
      datasets = datasets,
      dataname = parentname,
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_tte",
      on_off = shiny::reactive(input$compare_arms)
    )

    anl_merged <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        strata_var = strata_var,
        event_desc_var = event_desc_var,
        time_unit_var = time_unit_var
      ),
      merge_function = "dplyr::inner_join"
    )

    adsl_merged <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(arm_var = arm_var, strata_var = strata_var),
      anl_name = "ANL_ADSL"
    )

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- shiny::reactive({
      adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
      anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

      anl_m <- anl_merged()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
      input_event_desc <- as.vector(anl_m$columns_source$event_desc_var)
      input_time_unit_var <- as.vector(anl_m$columns_source$time_unit_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_paramcd, input_aval_var,
          input_cnsr_var, input_event_desc, input_time_unit_var
        ),
        arm_var = input_arm_var
      )

      # validate arm levels
      if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }
      if (input$compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
      }

      do.call(what = "validate_standard_inputs", validate_args)

      shiny::validate(shiny::need(
        input$conf_level_coxph >= 0 && input$conf_level_coxph <= 1,
        "Please choose a confidence level between 0 and 1"
      ))

      shiny::validate(shiny::need(
        input$conf_level_survfit >= 0 && input$conf_level_survfit <= 1,
        "Please choose a confidence level between 0 and 1"
      ))

      shiny::validate(
        shiny::need(checkmate::test_string(input_aval_var), "Analysis variable should be a single column.")
      )
      shiny::validate(shiny::need(checkmate::test_string(input_cnsr_var), "Censor variable should be a single column."))
      shiny::validate(shiny::need(
        checkmate::test_string(input_event_desc),
        "Event description variable should be a single column."
      ))

      # check that there is at least one record with no missing data
      shiny::validate(shiny::need(
        !all(is.na(anl_m$data()[[input_aval_var]])),
        "ANCOVA table cannot be calculated as all values are missing."
      ))

      shiny::validate(shiny::need(
        length(input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]]) > 0,
        "`Select Endpoint` field is NULL"
      ))

      NULL
    })

    # The R-code corresponding to the analysis.

    call_preparation <- shiny::reactive({
      validate_checks()

      teal.code::chunks_reset()
      anl_m <- anl_merged()
      teal.code::chunks_push_data_merge(anl_m)
      teal.code::chunks_push_new_line()

      anl_adsl <- adsl_merged()
      teal.code::chunks_push_data_merge(anl_adsl)
      teal.code::chunks_push_new_line()

      ANL <- teal.code::chunks_get_var("ANL") # nolint

      strata_var <- as.vector(anl_m$columns_source$strata_var)

      my_calls <- template_tte(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        paramcd = unlist(anl_m$filter_info$paramcd)["selected"],
        ref_arm = input$ref_arm,
        comp_arm = input$comp_arm,
        compare_arm = input$compare_arms,
        combine_comp_arms = input$combine_comp_arms && input$compare_arms,
        aval_var = as.vector(anl_m$columns_source$aval_var),
        cnsr_var = as.vector(anl_m$columns_source$cnsr_var),
        strata_var = if (length(strata_var) != 0) strata_var else NULL,
        time_points = as.numeric(input$time_points),
        time_unit_var = as.vector(anl_m$columns_source$time_unit_var),
        event_desc_var = as.vector(anl_m$columns_source$event_desc_var),
        control = control_tte(
          coxph = control_coxph(
            pval_method = input$pval_method_coxph,
            ties = input$ties_coxph,
            conf_level = as.numeric(input$conf_level_coxph)
          ),
          surv_time = control_surv_time(
            conf_level = as.numeric(input$conf_level_survfit),
            conf_type = input$conf_type_survfit,
            quantiles = input$probs_survfit
          ),
          surv_timepoint = control_surv_timepoint(
            conf_level = as.numeric(input$conf_level_survfit),
            conf_type = input$conf_type_survfit
          )
        ),
        add_total = input$add_total,
        basic_table_args = basic_table_args
      )
      mapply(expression = my_calls, id = paste(names(my_calls), "call", sep = "_"), teal.code::chunks_push)
    })

    table <- shiny::reactive({
      call_preparation()
      teal.code::chunks_safe_eval()
      teal.code::chunks_get_var("result")
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table
    )

    teal::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.transform::get_extract_datanames(
        list(arm_var, paramcd, strata_var, event_desc_var)
      ),
      modal_title = label
    )
  })
}
