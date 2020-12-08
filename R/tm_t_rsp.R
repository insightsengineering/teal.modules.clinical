#' Template for Responders
#'
#' Creates a valid expression for responder analysis.
#'
#' @inheritParams argument_convention
#' @param control (`list`)\cr list of settings for the analysis.
#' @param responder_val (`character`)\cr the short label for observations to
#'   translate `AVALC` into responder / non-responder.
#' @param show_rsp_cat (`flag`)\cr display the multinomial response estimations.
#'
#' @examples
#'
#' \dontrun{
#' # Preparation of the test case.
#' library(dplyr)
#' library(random.cdisc.data)
#' library(tern)
#' adsl <- radsl(cached = TRUE)
#' adrs <- radrs(cached = TRUE) %>%
#'   filter(PARAMCD == "BESRSPI")
#'
#' # Generate an expression for the analysis of responders.
#' a <- template_rsp(
#'   dataname = "adrs",
#'   parentname = "adsl",
#'   arm_var = "ARMCD",
#'   ref_arm = "ARM A",
#'   comp_arm = c("ARM B"),
#'   compare_arm = TRUE,
#'   show_rsp_cat = TRUE
#' )
#'
#' styled_expr(a$data)
#' styled_expr(a$layout)
#' styled_expr(a$table)
#'
#' b <- mapply(expr = a, FUN = eval)
#' b$data
#' b$layout
#' b$table
#' }
#'
template_rsp <- function(dataname,
                         parentname,
                         arm_var,
                         ref_arm = NULL,
                         comp_arm = NULL,
                         compare_arm = FALSE,
                         combine_comp_arms = FALSE,
                         aval_var = "AVALC",
                         show_rsp_cat = TRUE,
                         responder_val = c("CR", "PR"),
                         control = list(
                           global = list(
                             method = "waldcc",
                             conf_level = 0.95
                           ),
                           unstrat = list(
                             method_ci = "waldcc",
                             method_test = "schouten",
                             odds = TRUE
                           ),
                           strat = list(
                             method_ci = "waldcc",
                             method_test = "cmh",
                             strat = NULL
                           )
                         )
) {
  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(aval_var),
    is.flag(compare_arm),
    is.flag(combine_comp_arms),
    is.flag(show_rsp_cat)
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
      ref_arm_val = ref_arm_val,
      compare_arm = compare_arm
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = mutate(rsp_lab = d_onco_rsp_label(aval_var)) %>%
        mutate(is_rsp = aval_var %in% responder_val),
      env = list(
        responder_val = responder_val,
        aval_var = as.name(aval_var)
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
        ref_arm = ref_arm,
        comp_arm = comp_arm,
        ref_arm_val = ref_arm_val,
        compare_arm = compare_arm
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

  y$col_counts <- if (compare_arm && combine_comp_arms) {
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
      arm_var = arm_var,
      ref = ref_arm_val
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = conf_level,
          method = method,
          table_names = "prop_est"
        ),
      env = list(
        conf_level = control$global$conf_level,
        method = control$global$method
      )
    )
  )

  if (compare_arm) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = conf_level,
          method = method_ci,
          table_names = "u_prop_diff"
        ) %>%
          test_proportion_diff(
            vars = "is_rsp",
            method = method_test,
            table_names = "u_test_diff"
          ),
        env = list(
          conf_level = control$global$conf_level,
          method_ci = control$unstrat$method_ci,
          method_test = control$unstrat$method_test
        )
      )
    )

    if (control$unstrat$odds) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          expr = estimate_odds_ratio(
            vars = "is_rsp",
            conf_level = conf_level,
            table_names = "u_est_or"
          ),
          env = list(conf_level = control$global$conf_level)
        )
      )
    }

    if (!is.null(control$strat$strat)) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          expr = estimate_proportion_diff(
            vars = "is_rsp", show_labels = "visible",
            var_labels = "Stratified Analysis",
            variables = list(strata = strata),
            conf_level = conf_level,
            method = method_ci,
            table_names = "s_prop_diff"
          ) %>%
            test_proportion_diff(
              vars = "is_rsp",
              method = method_test,
              variables = list(strata = strata),
              table_names = "s_test_diff"
            ) %>%
            estimate_odds_ratio(
              vars = "is_rsp",
              variables = list(arm = arm_var, strata = strata),
              conf_level = conf_level,
              table_names = "s_est_or"
            ),
          env = list(
            conf_level = control$global$conf_level,
            method_ci = control$strat$method_ci,
            strata = control$strat$strat,
            method_test = control$strat$method_test,
            arm_var = arm_var
          )
        )
      )
    }
  }

  if (show_rsp_cat) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        estimate_multinomial_response(
          var = "rsp_lab",
          conf_level = conf_level,
          method = method
        ),
        list(
          conf_level = control$global$conf_level,
          method = control$global$method
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- quote({
    result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
    print(result)
  })
  y
}

#' Response Table Teal Module
#'
#' This module produces a response summary table that matches the
#' STREAM template `rspt01`.
#'
#' @inheritParams argument_convention
#' @inheritParams tm_t_tte
#' @param aval_var (\code{\link[teal]{choices_selected}} or \code{data_extract_spec}) object with all available choices
#'   and preselected option for analysis variable
#'
#' @details Additional standard UI inputs include `responders`,
#'   `ref_arm`, `comp_arm` and `combine_comp_arms` (default FALSE)
#'
#'   Default values of the inputs `var_arm`, `ref_arm` and
#'   `comp_arm` are set to NULL, and updated accordingly based on selection
#'   of `paramcd` and `var_arm`
#'
#'   This display order of response categories in partitioned statistics section
#'   inherits the factor level order of the source data. Use
#'   [base::factor()] and its `levels` argument to manipulate
#'   the source data in order to include/exclude or re-categorize response
#'   categories and arrange the display order. If response categories are
#'   "Missing" or "Not Evaluable (NE)" or "Missing or unevaluable", 95\%
#'   confidence interval will not be calculated.
#'
#'   Reference arms automatically combined if multiple arms selected as
#'   reference group.
#'
#' @return an [teal::module()] object
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE) %>%
#'   mutate(Dum_ARM = factor(rep("Single ARM", nrow(.))))
#' ADRS <- radrs(cached = TRUE) %>%
#'   mutate(Dum_ARM = factor(rep("Single ARM", nrow(.))))
#'
#' arm_ref_comp = list(
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
#'     cdisc_dataset(
#'       dataname = "ADSL", data = ADSL, code =
#'         "ADSL <- radsl(cached = TRUE) %>%
#'            mutate(Dum_ARM = factor(rep(\"Single ARM\", nrow(.))))"
#'     ),
#'     cdisc_dataset(
#'       dataname = "ADRS", data = ADRS, code =
#'         "ADRS <- radrs(cached = TRUE) %>%
#'           mutate(Dum_ARM = factor(rep(\"Single ARM\", nrow(.))))"
#'     )
#'   ),
#'   modules = root_modules(
#'     tm_t_rsp(
#'       label = "Responders",
#'       dataname = "ADRS",
#'       paramcd = choices_selected(
#'         choices = value_choices(ADRS, "PARAMCD", "PARAM"),
#'         selected = "BESRSPI"
#'       ),
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADRS, c("ARM", "ARMCD", "ACTARMCD", "Dum_ARM")),
#'         selected = "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       strata_var = choices_selected(
#'         choices = variable_choices(ADRS, c("SEX", "BMRKR2")),
#'         select = NULL
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_t_rsp <- function(label,
                     dataname,
                     parentname = ifelse(
                       test = is(arm_var, "data_extract_spec"),
                       yes = datanames_input(arm_var),
                       no = "ADSL"
                     ),
                     arm_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     strata_var,
                     aval_var = choices_selected(variable_choices(dataname, "AVALC"), "AVALC", fixed = TRUE),
                     pre_output = NULL,
                     post_output = NULL) {

  stopifnot(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname)
  )

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE)
  )

  module(
    label = label,
    ui = ui_t_rsp,
    ui_args = c(data_extract_list, args),
    server = srv_t_rsp,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
#'
ui_t_rsp <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(
    a$paramcd,
    a$arm_var,
    a$aval_var,
    a$strata_var
  )

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      uiOutput(outputId = ns("table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("paramcd", "arm_var", "aval_var", "strata_var")]),
      data_extract_input(
        id = ns("paramcd"),
        label = "Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        ns("responders"),
        "Responders",
        choices = c("CR", "PR"),
        selected = c("CR", "PR"),
        multiple = TRUE
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
            )
          )
        )
      ),
      data_extract_input(
        id = ns("strata_var"),
        label = "Stratification Factors",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
#'
srv_t_rsp <- function(input,
                      output,
                      session,
                      datasets,
                      dataname,
                      parentname,
                      paramcd,
                      aval_var,
                      arm_var,
                      arm_ref_comp,
                      strata_var,
                      label) {
  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte",
    on_off = reactive(input$compare_arms)
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, paramcd, strata_var, aval_var),
    input_id = c("arm_var", "paramcd", "strata_var", "aval_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, strata_var),
    input_id = c("arm_var", "strata_var"),
    anl_name = "ANL_ADSL"
  )

  # Because the AVALC values depends on the selected PARAMCD.
  observe({
    aval_var <- anl_merged()$columns_source$aval_var
    responder_choices <- if (is_empty(aval_var)) {
      character(0)
    } else {
      unique(anl_merged()$data()[[aval_var]])
    }
    updateSelectInput(
      session, "responders",
      choices = responder_choices,
      selected = intersect(responder_choices, isolate(input$responders))
    )
  })

  validate_check <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_paramcd <- unlist(paramcd$filter)["vars"]

    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var),
      arm_var = input_arm_var
    )

    validate_one_row_per_id(anl_m$data(), key = c("USUBJID", "STUDYID", input_paramcd))

    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- c(validate_args, list(min_n_levels_armvar = NULL))
    }
    if (input$compare_arms) {
      validate_args <- c(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
    }

    do.call(what = "validate_standard_inputs", validate_args)

    validate(
      need(is_character_single(input_aval_var), "Analysis variable should be a single column."),
      need(input$responders, "`Responders` field is empty"))

    NULL
  })

  call_preparation <- reactive({
    validate_check()
    chunks_reset()

    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    anl <- chunks_get_var("ANL")
    validate_has_data(anl, 10)
    validate_one_row_per_id(anl, key = c("USUBJID", "STUDYID"))

    strata_var <- as.vector(anl_m$columns_source$strata_var)

    my_calls <- template_rsp(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      compare_arm = input$compare_arms,
      combine_comp_arms = input$combine_comp_arms,
      aval_var = as.vector(anl_m$columns_source$aval_var),
      show_rsp_cat = TRUE,
      responder_val = input$responders,
      control = list(
        global = list(
          method = "clopper-pearson",
          conf_level = 0.95
        ),
        unstrat = list(
          method_ci = "wald",
          method_test = "chisq",
          odds = TRUE
        ),
        strat = list(
          method_ci = "wald",
          method_test = "cmh",
          strat = if (length(strata_var) != 0) strata_var else NULL
        )
      )
    )
    mapply(expression = my_calls, chunks_push)
  })

  output$table <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm_var, paramcd, aval_var, strata_var)
    ),
    modal_title = "Response",
    code_header = label
  )
}
