#' Template: Binary Outcome
#'
#' Creates a valid expression for binary outcome analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis.
#' @param responder_val (`character`)\cr the short label for observations to
#'   translate `AVALC` into responder / non-responder.
#' @param responder_val_levels (`character`)\cr the levels of responses that will be shown in the multinomial
#' response estimations.
#' @param show_rsp_cat (`logical`)\cr display the multinomial response estimations.
#' @param paramcd (`character`)\cr response parameter value to use in the table title.
#'
#' @seealso [tm_t_binary_outcome()]
#' @keywords internal
#' @examples
#' if (interactive()) {
#'   # Preparation of the test case.
#'   adsl <- tmc_ex_adsl
#'   adrs <- tmc_ex_adrs
#'
#'   # Generate an expression for the analysis of responders.
#'   a <- template_binary_outcome(
#'     dataname = "adrs",
#'     parentname = "adsl",
#'     arm_var = "ARMCD",
#'     paramcd = "BESRSPI",
#'     ref_arm = "ARM A",
#'     comp_arm = c("ARM B"),
#'     compare_arm = TRUE,
#'     show_rsp_cat = TRUE
#'   )
#'
#'   b <- mapply(expr = a, FUN = eval)
#'   b$data
#'   b$layout
#'   b$table
#' }
#'
template_binary_outcome <- function(dataname,
                                    parentname,
                                    arm_var,
                                    paramcd,
                                    ref_arm = NULL,
                                    comp_arm = NULL,
                                    compare_arm = FALSE,
                                    combine_comp_arms = FALSE,
                                    aval_var = "AVALC",
                                    show_rsp_cat = TRUE,
                                    responder_val = c("Complete Response (CR)", "Partial Response (PR)"),
                                    responder_val_levels = responder_val,
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
                                    add_total = FALSE,
                                    total_label = "All Patients",
                                    basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(aval_var),
    assertthat::is.flag(compare_arm),
    assertthat::is.flag(combine_comp_arms),
    assertthat::is.flag(show_rsp_cat),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label)
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
    substitute_names(
      expr = dplyr::mutate(is_rsp = aval_var %in% responder_val) %>%
        dplyr::mutate(aval = factor(aval_var, levels = responder_val_levels)),
      names = list(
        aval = as.name(aval_var)
      ),
      others = list(
        responder_val = responder_val,
        responder_val_levels = responder_val_levels,
        aval_var = as.name(aval_var)
      )
    )
  )

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

  table_title <- if (length(responder_val) > 1) {
    paste(
      "Table of", paramcd, "for", paste(utils::head(responder_val, -1), collapse = ", "),
      "and", utils::tail(responder_val, 1), "Responders"
    )
  } else {
    paste("Table of", paramcd, "for", responder_val, "Responders")
  }

  strata_var <- control$strat$strat
  subtitle <- ifelse(length(strata_var) == 0, "", paste("Stratified by", paste(strata_var, collapse = " and ")))

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        title = table_title,
        subtitles = subtitle
      )
    )
  )

  layout_list <- list()
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
      rtables::add_colcounts() %>%
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parentname)
      result
    },
    env = list(parentname = as.name(parentname))
  )
  y
}

#' Teal Module: Binary Outcome Table
#'
#' @description
#'   This module produces a binary outcome response summary
#'   table, with the option to match the STREAM template `RSPT01`.
#'
#' @inheritParams module_arguments
#' @param default_responses (`list` or `character`) \cr defines
#'   the default codes for the response variable in the module per value of `paramcd`.
#'   A passed vector is transmitted for all `paramcd` values. A passed `list` must be named
#'   and contain arrays, each name corresponding to a single value of `paramcd`. Each array
#'   may contain default response values or named arrays `rsp` of default selected response
#'   values and `levels` of default level choices.
#'
#' @param rsp_table (`logical`)\cr should the initial set-up of the module match `RSPT01`. (default FALSE)
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
#' adsl <- tmc_ex_adsl
#' adrs <- tmc_ex_adrs %>%
#'   dplyr::mutate(
#'     AVALC = tern::d_onco_rsp_label(AVALC) %>%
#'       formatters::with_label("Character Result/Finding")
#'   ) %>%
#'   dplyr::filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
#'
#' arm_ref_comp <- list(
#'   ARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
#'   ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
#' )
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADRS", adrs)
#'   ),
#'   modules = modules(
#'     tm_t_binary_outcome(
#'       label = "Responders",
#'       dataname = "ADRS",
#'       paramcd = choices_selected(
#'         choices = value_choices(adrs, "PARAMCD", "PARAM"),
#'         selected = "BESRSPI"
#'       ),
#'       arm_var = choices_selected(
#'         choices = variable_choices(adrs, c("ARM", "ARMCD", "ACTARMCD")),
#'         selected = "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       strata_var = choices_selected(
#'         choices = variable_choices(adrs, c("SEX", "BMRKR2", "RACE")),
#'         select = "RACE"
#'       ),
#'       default_responses = list(
#'         BESRSPI = list(
#'           rsp = c("Complete Response (CR)", "Partial Response (PR)"),
#'           levels = c(
#'             "Complete Response (CR)", "Partial Response (PR)",
#'             "Stable Disease (SD)", "Progressive Disease (PD)"
#'           )
#'         ),
#'         INVET = list(
#'           rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
#'           levels = c(
#'             "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
#'             "Progressive Disease (PD)", "Stable Disease (SD)"
#'           )
#'         ),
#'         OVRINV = list(
#'           rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
#'           levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_binary_outcome <- function(label,
                                dataname,
                                parentname = ifelse(
                                  test = inherits(arm_var, "data_extract_spec"),
                                  yes = teal.transform::datanames_input(arm_var),
                                  no = "ADSL"
                                ),
                                arm_var,
                                arm_ref_comp = NULL,
                                paramcd,
                                strata_var,
                                aval_var = teal.transform::choices_selected(
                                  choices = teal.transform::variable_choices(dataname, c("AVALC", "SEX")),
                                  selected = "AVALC", fixed = FALSE
                                ),
                                conf_level = teal.transform::choices_selected(
                                  c(0.95, 0.9, 0.8), 0.95,
                                  keep_order = TRUE
                                ),
                                default_responses =
                                  c("CR", "PR", "Y", "Complete Response (CR)", "Partial Response (PR)", "M"),
                                rsp_table = FALSE,
                                add_total = FALSE,
                                total_label = "All Patients",
                                pre_output = NULL,
                                post_output = NULL,
                                basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_binary_outcome")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert(
    checkmate::check_class(default_responses, classes = "list"),
    checkmate::check_class(default_responses, classes = "character"),
    checkmate::check_class(default_responses, classes = "numeric"),
    checkmate::check_class(default_responses, classes = "NULL")
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE)
  )

  module(
    label = label,
    ui = ui_t_binary_outcome,
    ui_args = c(data_extract_list, args),
    server = srv_t_binary_outcome,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        total_label = total_label,
        default_responses = default_responses,
        rsp_table = rsp_table,
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

ui_t_binary_outcome <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$paramcd,
    a$arm_var,
    a$aval_var,
    a$strata_var
  )

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("paramcd", "arm_var", "aval_var", "strata_var")]),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::selectInput(
        ns("responders"),
        "Responders",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
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
            shiny::uiOutput(
              ns("arms_buckets"),
              title = paste(
                "Multiple reference groups are automatically combined into a single group when more than one",
                "value is selected."
              )
            ),
            shiny::helpText("Multiple reference groups are automatically combined into a single group."),
            shiny::checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        teal.widgets::panel_group(
          teal.widgets::panel_item(
            "Unstratified analysis settings",
            teal.widgets::optionalSelectInput(
              ns("u_diff_ci"),
              label = "Method for Difference of Proportions CI",
              choices = c(
                "Wald, without correction" = "wald",
                "Wald, with correction" = "waldcc",
                "Anderson-Hauck" = "ha",
                "Newcombe, without correction" = "newcombe",
                "Newcombe, with correction" = "newcombecc"
              ),
              selected = ifelse(a$rsp_table, "wald", "waldcc"),
              multiple = FALSE,
              fixed = FALSE
            ),
            teal.widgets::optionalSelectInput(
              ns("u_diff_test"),
              label = "Method for Difference of Proportions Test",
              choices = c(
                "Chi-squared Test" = "chisq",
                "Fisher's Exact Test" = "fisher",
                "Chi-Squared Test with Schouten correction" = "schouten"
              ),
              selected = "chisq",
              multiple = FALSE,
              fixed = FALSE
            ),
            shiny::tags$label("Odds Ratio Estimation"),
            shinyWidgets::switchInput(
              inputId = ns("u_odds_ratio"), value = TRUE, size = "mini"
            )
          )
        ),
        teal.widgets::panel_group(
          teal.widgets::panel_item(
            "Stratified analysis settings",
            teal.transform::data_extract_ui(
              id = ns("strata_var"),
              label = "Stratification Factors",
              data_extract_spec = a$strata_var,
              is_single_dataset = is_single_dataset_value
            ),
            teal.widgets::optionalSelectInput(
              ns("s_diff_ci"),
              label = "Method for Difference of Proportions CI",
              choices = c(
                "Wald, without correction" = "wald",
                "Wald, with correction" = "waldcc",
                "CMH, without correction" = "cmh",
                "Anderson-Hauck" = "ha",
                "Stratified Newcombe, without correction" = "strat_newcombe",
                "Stratified Newcombe, with correction" = "strat_newcombecc"
              ),
              selected = "cmh",
              multiple = FALSE
            ),
            teal.widgets::optionalSelectInput(
              ns("s_diff_test"),
              label = "Method for Difference of Proportions Test",
              choices = c("CMH Test" = "cmh"),
              selected = "cmh",
              multiple = FALSE,
              fixed = TRUE
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = paste0("!input['", ns("compare_arms"), "']"),
        shiny::checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total)
      ),
      teal.widgets::panel_item(
        "Additional table settings",
        teal.widgets::optionalSelectInput(
          inputId = ns("prop_ci_method"),
          label = "Method for Proportion CI",
          choices = c(
            "Wald, without correction" = "wald",
            "Wald, with correction" = "waldcc",
            "Clopper-Pearson" = "clopper-pearson",
            "Wilson" = "wilson",
            "Wilson, with correction" = "wilsonc",
            "Jeffreys" = "jeffreys",
            "Agresti-Coull" = "agresti-coull"
          ),
          selected = ifelse(a$rsp_table, "clopper-pearson", "waldcc"),
          multiple = FALSE,
          fixed = FALSE
        ),
        teal.widgets::optionalSelectInput(
          inputId = ns("conf_level"),
          label = "Confidence Level",
          a$conf_level$choices,
          a$conf_level$selected,
          multiple = FALSE,
          fixed = a$conf_level$fixed
        ),
        shiny::tags$label("Show All Response Categories"),
        shinyWidgets::switchInput(
          inputId = ns("show_rsp_cat"),
          value = ifelse(a$rsp_table, TRUE, FALSE),
          size = "mini"
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_binary_outcome <- function(id,
                                 data,
                                 reporter,
                                 filter_panel_api,
                                 dataname,
                                 parentname,
                                 paramcd,
                                 aval_var,
                                 arm_var,
                                 arm_ref_comp,
                                 strata_var,
                                 add_total,
                                 total_label,
                                 label,
                                 default_responses,
                                 rsp_table,
                                 basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_binary_outcome",
      on_off = shiny::reactive(input$compare_arms)
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(arm_var = arm_var, paramcd = paramcd, strata_var = strata_var, aval_var = aval_var),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required")
      ),
      filter_validation_rule = list(paramcd = shinyvalidate::sv_required(message = "Please select a filter."))
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()

      if (isTRUE(input$compare_arms)) {
        iv$add_validator(iv_arm_ref)
      }

      iv$add_rule("responders", shinyvalidate::sv_required("`Responders` field is empty"))
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level between 0 and 1"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(0, 1, message_fmt = "Please choose a confidence level between {left} and {right}")
      )
      teal.transform::compose_and_enable_validators(iv, selector_list, c("arm_var", "aval_var", "paramcd"))
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join",
      join_keys = get_join_keys(data)
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var, strata_var = strata_var),
      join_keys = get_join_keys(data),
      anl_name = "ANL_ADSL"
    )

    anl_q <- shiny::reactive({
      q <- teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data))
      qenv <- teal.code::eval_code(q, as.expression(anl_inputs()$expr))
      teal.code::eval_code(qenv, as.expression(adsl_inputs()$expr))
    })

    shiny::observeEvent(
      c(
        input[[extract_input("aval_var", "ADRS")]],
        input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]]
      ),
      handlerExpr = {
        anl <- anl_q()[["ANL"]]
        aval_var <- anl_inputs()$columns_source$aval_var
        paramcd <- input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]]
        sel_param <- if (is.list(default_responses) && (!is.null(paramcd))) {
          default_responses[[paramcd]]
        } else {
          default_responses
        }
        common_rsp <- if (is.list(sel_param)) {
          sel_param$rsp
        } else {
          sel_param
        }
        responder_choices <- if (length(aval_var) == 0) {
          character(0)
        } else {
          if ("levels" %in% names(sel_param)) {
            if (length(intersect(unique(anl[[aval_var]]), sel_param$levels)) > 1) {
              sel_param$levels
            } else {
              unique(anl[[aval_var]])
            }
          } else {
            unique(anl[[aval_var]])
          }
        }
        shiny::updateSelectInput(
          session, "responders",
          choices = responder_choices,
          selected = intersect(responder_choices, common_rsp)
        )
      }
    )

    validate_check <- shiny::reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      anl_m <- anl_inputs()
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

      if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
        validate_args <- c(validate_args, list(min_n_levels_armvar = NULL))
      }
      if (isTRUE(input$compare_arms)) {
        validate_args <- c(
          validate_args,
          list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
        )
      }

      do.call(what = "validate_standard_inputs", validate_args)

      teal::validate_one_row_per_id(anl, key = c("USUBJID", "STUDYID", input_paramcd))

      shiny::validate(
        if (length(input_strata_var) >= 1L) {
          shiny::need(
            sum(
              vapply(
                anl[input_strata_var],
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

      shiny::validate(
        if (length(input_strata_var) >= 1L) {
          shiny::need(
            sum(
              vapply(
                anl[input_strata_var],
                FUN = function(strata) {
                  tab <- base::table(strata, anl[[input_arm_var]])
                  tab_logic <- tab != 0L
                  sum(apply(tab_logic, 1, sum) == ncol(tab_logic)) >= 2
                },
                FUN.VALUE = logical(1)
              )
            ) > 0,
            "At least one strata variable must have at least two levels with observation(s) in all of the arms."
          )
        }
      )

      if (is.list(default_responses)) {
        shiny::validate(
          shiny::need(
            all(
              grepl("\\.rsp|\\.levels", names(unlist(default_responses))) |
                gsub("[0-9]*", "", names(unlist(default_responses))) %in% names(default_responses)
            ),
            "The lists given for each AVAL in default_responses must be named 'rsp' and 'levels'."
          )
        )
      }

      NULL
    })

    table_q <- shiny::reactive({
      validate_check()

      qenv <- anl_q()
      anl_m <- anl_inputs()
      anl <- qenv[["ANL"]]

      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      shiny::req(input$responders %in% anl[[input_aval_var]])

      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_paramcd <- unlist(anl_m$filter_info$paramcd)["selected"]

      responder_val_levels <- as.character(unique(anl[[input_aval_var]]))
      final_responder <- if (is.list(default_responses)) {
        default_responses[[input_paramcd]][["levels"]]
      } else {
        responder_val_levels
      }
      if (length(final_responder) == 0) final_responder <- input$responders

      my_calls <- template_binary_outcome(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        paramcd = input_paramcd,
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        compare_arm = input$compare_arms,
        combine_comp_arms = input$combine_comp_arms && input$compare_arms,
        aval_var = input_aval_var,
        responder_val = input$responders,
        responder_val_levels = final_responder,
        show_rsp_cat = input$show_rsp_cat,
        control = list(
          global = list(
            method = input$prop_ci_method,
            conf_level = as.numeric(input$conf_level)
          ),
          unstrat = list(
            method_ci = input$u_diff_ci,
            method_test = input$u_diff_test,
            odds = input$u_odds_ratio
          ),
          strat = list(
            method_ci = input$s_diff_ci,
            method_test = input$s_diff_test,
            strat = if (length(input_strata_var) != 0) input_strata_var else NULL
          )
        ),
        add_total = input$add_total,
        total_label = total_label,
        basic_table_args = basic_table_args
      )

      teal.code::eval_code(qenv, as.expression(my_calls))
    })

    # Outputs to render.
    table_r <- shiny::reactive(table_q()[["result"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(table_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(table_q())))
    )

    # Render R code.
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive({
        teal.code::get_code(table_q())
      }),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Binary Outcome Table")
        card$append_text("Binary Outcome Table", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(table_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
