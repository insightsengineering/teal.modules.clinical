
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


#' Template for Time-to-Event
#'
#' Creates a valid expression for time-to-event analysis.
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @inheritParams tm_t_tte
#' @param control (`list`)\cr list of settings for the analysis,
#'   see [control_tte()].
#'
#' @seealso [tm_t_tte()]
#' @examples
#'
template_tte <- function(dataname = "ANL",
                         parentname = "ADSL_FILTERED",
                         arm_var = "ARM",
                         arm_ref_comp = NULL,
                         comp_arm = NULL,
                         compare_arm = FALSE,
                         combine_comp_arms = FALSE,
                         aval = "AVAL",
                         cnsr = "CNSR",
                         strata_var = NULL,
                         time_points = NULL,
                         time_unit = "Days",
                         event_desc_var = "EVNTDESC",
                         control = control_tte()) {
  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(aval),
    is.string(cnsr),
    is.string(time_unit),
    is.string(event_desc_var),
    is.flag(compare_arm),
    is.flag(combine_comp_arms)
  )

  ref_arm_val <- paste(arm_ref_comp, collapse = "/")
  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = arm_ref_comp,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = mutate(
        is_event = cnsr == 0,
        is_not_event = cnsr == 1,
        EVNT1 = factor(
          case_when(
            is_event == TRUE ~ "Patients with event (%)",
            is_event == FALSE ~ "Patients without event (%)"
          )
        ),
        EVNTDESC = factor(event_desc_var)
      ),
      env = list(
        anl = as.name(dataname),
        cnsr = as.name(cnsr),
        event_desc_var = as.name(event_desc_var)
      )
    )
  )

  y$data <- substitute(
    expr = {
      anl <- data_pipe
      parentname <- arm_preparation
    },
    env = list(
      data_pipe = pipe_expr(data_list),
      parentname = as.name(parentname),
      arm_preparation = prepare_arm(
        dataname = parentname,
        arm_var = arm_var,
        ref_arm = arm_ref_comp,
        comp_arm = comp_arm,
        ref_arm_val = ref_arm_val
      )
    )
  )

  if (combine_comp_arms) {
    y$combine_arm <- substitute(
      expr = groups <- combine_groups(fct = anl[[group]], ref = ref_arm),
      env = list(anl = as.name(dataname), group = arm_var, ref_arm = arm_ref_comp)
    )
  }
  y$col_counts <- if (combine_comp_arms) {
    substitute(
      expr = col_counts <- combine_counts(fct = parentname[[group]], groups_list = groups),
      env = list(group = arm_var, parentname = as.name(parentname))
    )
  } else {
    substitute(
      expr = col_counts <- combine_counts(fct = parentname[[group]]),
      env = list(group = arm_var, parentname = as.name(parentname))
    )
  }

  layout_list <- list()
  layout_list <- add_expr(layout_list, substitute(basic_table()))
  layout_list <- add_expr(
    layout_list,
    split_col_expr(
      compare = compare_arm,
      combine = combine_comp_arms,
      group = arm_var,
      ref = ref_arm_val
    )
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
        vars = aval,
        var_labels = paste0("Time to Event (", time_unit, ")"),
        is_event = "is_event",
        control = list(
          conf_level = conf_level,
          conf_type = conf_type,
          quantiles = quantiles
        )
      ),
      env = c(
        aval = aval,
        control$surv_time,
        time_unit = time_unit
      )
    )
  )

  if (compare_arm) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = coxph_pairwise(
          vars = aval,
          is_event = "is_event",
          var_labels = c("Unstratified Analysis"),
          control = list(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          )
        ),
        env = c(
          aval = aval,
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
          vars = aval,
          is_event = "is_event",
          var_labels = paste0("Stratified By: ", paste(strata_var, collapse = ", ")),
          strat = strata_var,
          control = control_coxph(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          )
        ),
        env = list(
          aval = aval,
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
          vars = aval,
          var_labels = time_unit,
          is_event = "is_event",
          time_point = time_points,
          method = method,
          control = control_surv_timepoint(
            conf_level = conf_level,
            conf_type = conf_type
          ),
          .indent_mods = indents
        ),
        env = list(
          aval = aval,
          time_points = time_points,
          method = method,
          indents = indents,
          time_unit = time_unit,
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

  col_counts <- if (combine_comp_arms) {
    substitute(
      expr = sapply(groups, function(x) sum(table(adsl$arm_var)[x])),
      env = list(adsl = as.name(parentname), arm_var = arm_var)
    )
  } else {
    substitute(
      expr = table(adsl$arm_var),
      env = list(adsl = as.name(parentname), arm_var = arm_var)
    )
  }

  y$table <- quote({
    result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
    result
  })

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
#' @param parentname (\code{character}) name of \code{ADSL} dataset used in the analysis.
#' @param arm_var (\code{\link[teal]{choices_selected}} or \code{data_extract_spec}) object with all available choices
#'   and preselected option for variable names that can be used as \code{arm_var}
#' @param arm_ref_comp (\code{named list of \link[teal]{choices_selected}}) optional, if specified it must be a named
#'   list with each element corresponding to an arm variable in \code{ADSL} and the element must
#'   be another list with the elements named \code{ref} and \code{comp} that the
#'   defined the default reference and comparison arms when the arm variable is
#'   changed.
#' @param paramcd (\code{\link[teal]{choices_selected}} or \code{data_extract_spec}) object with all available choices
#'   and preselected option for variable names that can be used as \code{PARAMCD} variable
#' @param strata_var (\code{\link[teal]{choices_selected}} or \code{data_extract_spec}) object with all available
#'   choices and preselected option for variable names that can be used for stratification
#' @param aval_var (\code{\link[teal]{choices_selected}} or \code{data_extract_spec}) object with all available choices
#'   and preselected option for analysis variable
#' @param cnsr_var (\code{\link[teal]{choices_selected}} or \code{data_extract_spec}) object with all available choices
#'   and preselected option for censor variable
#' @param time_points (\code{\link[teal]{choices_selected}}) object with all available choices and preselected option
#'   for variable names that can be used \code{REFACTOR}
#' @param time_unit (\code{character}) with unit of \code{dataname$AVAL}, please use singular e.g. month instead
#'   of months
#' @param event_desc_var (\code{character} or \code{data_extract_spec}) variable name with the event description
#'   information, optional
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
#'             arm_var = choices_selected(
#'               variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")),
#'               "ARM"),
#'             arm_ref_comp = arm_ref_comp,
#'             paramcd = choices_selected(
#'               value_choices(ADTTE, "PARAMCD", "PARAM"),
#'               "OS"),
#'             strata_var = choices_selected(
#'               variable_choices(ADSL, c("SEX", "BMRKR2")),
#'               "SEX"),
#'             time_points = choices_selected(c(6, 8), 6),
#'             time_unit = "month",
#'             event_desc_var = choices_selected(
#'               variable_choices(ADTTE, "EVNTDESC"),
#'               "EVNTDESC",
#'               fixed = TRUE)
#'         )
#'     )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_tte <- function(label,
                     dataname,
                     parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                     arm_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     strata_var,
                     aval_var = choices_selected(variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE),
                     cnsr_var = choices_selected(variable_choices(dataname, "CNSR"), "CNSR", fixed = TRUE),
                     time_points,
                     time_unit = "months",
                     event_desc_var = choices_selected("EVNTDESC", "EVNTDESC", fixed = TRUE),
                     pre_output = NULL,
                     post_output = NULL) {

  stopifnot(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is.choices_selected(time_points)
  )


  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    event_desc_var = cs_to_des_select(event_desc_var, dataname = dataname)
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
        time_unit = time_unit,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @import teal.devel
ui_t_tte <- function(id, ...) {

  a <- list(...) # module args
  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$aval_var,
    a$cnsr_var,
    a$strata_var,
    a$event_desc_var
  )

  ns <- NS(id)

  standard_layout(
    output = white_small_well(uiOutput(ns("as_html"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "paramcd", "aval_var", "cnsr_var", "strata_var", "event_desc_var")]),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      div(
        class = "arm-comp-box",
        tags$label("Compare Arms"),
        shinyWidgets::switchInput(
          inputId = ns("compare_arms"),
          value = !is.null(a$arm_ref_comp),
          size = "mini"
        ),
        conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          div(
            selectInput(
              ns("ref_arm"),
              "Reference Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            helpText("Multiple reference groups are automatically combined into a single group."),
            selectInput(
              ns("comp_arm"),
              "Comparison Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            ),
            data_extract_input(
              id = ns("strata_var"),
              label = "Stratify by",
              data_extract_spec = a$strata_var,
              is_single_dataset = is_single_dataset_value
            )
          )
        )
      ),
      optionalSelectInput(ns("time_points"),
                          "Time Points",
                          a$time_points$choices,
                          a$time_points$selected,
                          multiple = TRUE,
                          fixed = a$time_points$fixed
      ),
      data_extract_input(
        id = ns("event_desc_var"),
        label = "Event Description Variable",
        data_extract_spec = a$event_desc_var,
        is_single_dataset = is_single_dataset_value
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        panel_item(
          "Comparison settings",
          radioButtons(
            ns("pval_method_coxph"),
            label = HTML(
              paste(
                "p-value method for ",
                tags$span(style="color:darkblue", "Coxph"), # nolint
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
                tags$span(style="color:darkblue", "Coxph"), # nolint
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            choices = c("exact", "breslow", "efron"),
            selected = "exact"
          ),
          numericInput(
            inputId = ns("conf_level_coxph"),
            label = HTML(
              paste(
                "Confidence Level for ",
                tags$span(style="color:darkblue", "Coxph"), # nolint
                " (Hazard Ratio)", sep = ""
              )
            ),
            value = 0.95,
            min = 0.01,
            max = 0.99,
            step = 0.01,
            width = "100%"
          )
        )
      ),
      panel_item(
        "Additional table settings",
        numericInput(
          inputId = ns("conf_level_survfit"),
          label = HTML(
            paste(
              "Confidence Level for ",
              tags$span(style="color:darkblue", "Survfit"), # nolint
              " (KM Median Estimate & Event Free Rate)",
              sep = ""
            )
          ),
          value = 0.95,
          min = 0.01,
          max = 0.99,
          step = 0.01,
          width = "100%"
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
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @import teal.devel
#' @importFrom rtables as_html
srv_t_tte <- function(input,
                      output,
                      session,
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
                      time_unit,
                      label) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", # from UI
    id_comp = "comp_arm", # from UI
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte",
    on_off = reactive(input$compare_arms)
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, paramcd, aval_var, cnsr_var, strata_var, event_desc_var),
    input_id = c("arm_var", "paramcd", "aval_var", "cnsr_var", "strata_var", "event_desc_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, strata_var),
    input_id = c("arm_var", "strata_var"),
    anl_name = "ANL_ADSL"
  )

  # Prepare the analysis environment (filter data, check data, populate envir).
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
    input_event_desc <- as.vector(anl_m$columns_source$event_desc_var)
    input_paramcd <- unlist(paramcd$filter)["vars"]

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var, input_event_desc),
      arm_var = input_arm_var
    )

    # validate arm levels
    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      if (input$compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = input$ref_arm))
      }
    } else {
      if (input$compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
      }
    }

    do.call(what = "validate_standard_inputs", validate_args)

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
    validate_has_data(ANL, 10)

    my_calls <- template_tte(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      arm_ref_comp = input$ref_arm,
      comp_arm = input$comp_arm,
      compare_arm = input$compare_arms,
      combine_comp_arms = input$combine_comp_arms,
      aval = as.vector(anl_m$columns_source$aval_var),
      cnsr = as.vector(anl_m$columns_source$cnsr_var),
      strata_var = as.vector(anl_m$columns_source$strata_var),
      time_points = as.numeric(input$time_points),
      time_unit = time_unit,
      event_desc_var = as.vector(anl_m$columns_source$event_desc_var),
      control = control_tte(
        coxph = control_coxph(
          pval_method = input$pval_method_coxph,
          ties = input$ties_coxph,
          conf_level = input$conf_level_coxph
        ),
        surv_time = control_surv_time(
          conf_level = input$conf_level_survfit,
          conf_type = input$conf_type_survfit,
          quantiles = input$probs_survfit
        ),
        surv_timepoint = control_surv_timepoint(
          conf_level = input$conf_level_survfit,
          conf_type = input$conf_type_survfit
        )
      )
    )
    mapply(expression = my_calls, chunks_push)
  })

  output$as_html <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm_var, paramcd, strata_var, event_desc_var)
    ),
    modal_title = label
  )

}
