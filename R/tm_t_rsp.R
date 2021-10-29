#' Template: Responders
#'
#' Creates a valid expression for responder analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis.
#' @param responder_val (`character`)\cr the short label for observations to
#'   translate `AVALC` into responder / non-responder.
#' @param show_rsp_cat (`logical`)\cr display the multinomial response estimations.
#'
#' @seealso [tm_t_rsp()]
#'
#' @examples
#'
#' \dontrun{
#' # Preparation of the test case.
#' library(dplyr)
#' library(scda)
#' library(tern)
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adrs <- synthetic_cdisc_data("latest")$adrs %>%
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
                             method_ci = "cmh",
                             method_test = "cmh",
                             strat = NULL
                           )
                         ),
                         add_total = FALSE
) {
  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(aval_var),
    is.flag(compare_arm),
    is.flag(combine_comp_arms),
    is.flag(show_rsp_cat),
    is.flag(add_total)
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
      expr = dplyr::mutate(is_rsp = aval_var %in% responder_val),
      env = list(
        responder_val = responder_val,
        aval_var = as.name(aval_var)
      )
    )
  )

  data_list <- add_expr(data_list, quote(df_explicit_na(na_level = "")))

  y$data <- substitute(
    expr = {
      anl <- data_pipe
      parentname <- arm_preparation %>% df_explicit_na(na_level = "")
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


  layout_list <- list()
  layout_list <- add_expr(layout_list, substitute(basic_table()))

  if(!compare_arm && !combine_comp_arms && add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        split_cols_by(
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

  if (compare_arm && !is.null(control$strat$strat)) {
    layout_list <- if (combine_comp_arms) {
      add_expr(
        layout_list,
        substitute(
          expr = estimate_odds_ratio(
            vars = "is_rsp",
            variables = list(arm = arm_var, strata = strata),
            conf_level = conf_level,
            table_names = "s_est_or",
            groups_list = groups
          ),
          env = list(
            conf_level = control$global$conf_level,
            strata = control$strat$strat,
            arm_var = arm_var
          )
        )
      )
    } else {
      add_expr(
        layout_list,
        substitute(
          expr = estimate_odds_ratio(
            vars = "is_rsp",
            variables = list(arm = arm_var, strata = strata),
            conf_level = conf_level,
            table_names = "s_est_or"
          ),
          env = list(
            conf_level = control$global$conf_level,
            strata = control$strat$strat,
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
          var = aval_var,
          conf_level = conf_level,
          method = method
        ),
        list(
          conf_level = control$global$conf_level,
          method = control$global$method,
          aval_var = aval_var
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
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = parentname)
      result
    },
    env = list(parentname = as.name(parentname))
  )
  y
}

#' Teal Module: Response Table Teal Module
#'
#' This module produces a response summary table that matches the
#' STREAM template `rspt01`.
#'
#' @inheritParams module_arguments
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
#' library(scda)
#' library(dplyr)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl %>%
#'   mutate(Dum_ARM = factor(rep("Single ARM", nrow(.))))
#' ADRS <- synthetic_cdisc_data("latest")$adrs %>%
#'   mutate(
#'     Dum_ARM = factor(rep("Single ARM", nrow(.))),
#'     AVALC = d_onco_rsp_label(AVALC)
#'   )
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
#'       dataname = "ADSL",
#'       x = ADSL,
#'       code =
#'         "ADSL <- synthetic_cdisc_data('latest')$adsl %>%
#'            mutate(Dum_ARM = factor(rep('Single ARM', nrow(.))))"
#'     ),
#'     cdisc_dataset(
#'       dataname = "ADRS",
#'       x = ADRS,
#'       code =
#'         "ADRS <- synthetic_cdisc_data('latest')$adrs %>%
#'           mutate(Dum_ARM = factor(rep('Single ARM', nrow(.))))"
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
                     conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                     add_total = TRUE,
                     pre_output = NULL,
                     post_output = NULL) {

  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is.choices_selected(conf_level),
    is.flag(add_total),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
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
        add_total = add_total,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
#'
#' @importFrom shinyWidgets switchInput
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
      table_with_settings_ui(ns("table"))
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
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      div(
        class = "arm-comp-box",
        tags$label("Compare Treatments"),
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
              label = "Stratification Factors",
              data_extract_spec = a$strata_var,
              is_single_dataset = is_single_dataset_value
            )
          )
        )
        # conditionalPanel(
        #   condition = paste0("input['", ns("compare_arms"), "'] == NULL"),
        #   div(
        #     checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total)
        #   )
        # )
      ),
      optionalSelectInput(
        inputId = ns("conf_level"),
        label = HTML(paste("Confidence Level")),
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
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
                      add_total,
                      label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    dataname = parentname,
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
  observeEvent(anl_merged(), {
    aval_var <- anl_merged()$columns_source$aval_var
    responder_choices <- if (is_empty(aval_var)) {
      character(0)
    } else {
      unique(anl_merged()$data()[[aval_var]])
    }
    common_rsp <- c("CR", "PR", "Y", "Complete Response (CR)", "Partial Response (PR)")
    updateSelectInput(
      session, "responders",
      choices = responder_choices,
      selected = intersect(responder_choices, common_rsp)
    )
  }, once = TRUE, ignoreInit = TRUE)

  validate_check <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]

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
      need(input$responders, "`Responders` field is empty")
    )

    validate(
      if (length(input_strata_var) >= 1L) {
        need(
          sum(
            vapply(
              anl_m$data()[input_strata_var],
              FUN = function(x) {
                length(unique(x)) > 1
              },
              logical(1)
            )
          ) > 0,
          "At least one strata variable must have more than one non-empty level after filtering."
        )
      }
    )

    validate(
      if (length(input_strata_var) >= 1L) {
        need(
          sum(summary(
            anl_merged()$data()$ARM[!anl_merged()$data()[[input_aval_var]] %in% input$responders]
            ) > 0) > 1L,
            "After filtering at least one combination of strata variable levels
            has too few observations to calculate the odds ratio.")
      }
    )

    validate(need(
      input$conf_level >= 0 && input$conf_level <= 1,
      "Please choose a confidence level between 0 and 1"
    ))

    NULL
  })

  call_preparation <- reactive({
    validate_check()
    chunks_reset()

    anl_m <- anl_merged()
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    req(input$responders %in% anl_m$data()[[input_aval_var]])

    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    anl <- chunks_get_var("ANL")
    validate_one_row_per_id(anl, key = c("USUBJID", "STUDYID"))

    input_strata_var <- as.vector(anl_m$columns_source$strata_var)

    my_calls <- template_rsp(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      compare_arm = input$compare_arms,
      combine_comp_arms = input$combine_comp_arms,
      aval_var = input_aval_var,
      show_rsp_cat = TRUE,
      responder_val = input$responders,
      control = list(
        global = list(
          method = "clopper-pearson",
          conf_level = as.numeric(input$conf_level)
        ),
        unstrat = list(
          method_ci = "wald",
          method_test = "chisq",
          odds = TRUE
        ),
        strat = list(
          method_ci = "cmh",
          method_test = "cmh",
          strat = if (length(input_strata_var) != 0) input_strata_var else NULL
        )
      ),
      add_total = input$add_total
    )
    mapply(expression = my_calls, chunks_push)
  })

  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("result")
  })

  callModule(
    table_with_settings_srv,
    id = "table",
    table_r = table
  )

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
