#' Template: Binary Outcome
#'
#' Creates a valid expression to generate a binary outcome analysis.
#'
#' @inheritParams template_arguments
#' @inheritParams tern::s_count_occurrences
#' @param responder_val (`character`)\cr the short label for observations to
#'   translate `AVALC` into responder/non-responder.
#' @param responder_val_levels (`character`)\cr the levels of responses that will be shown in the multinomial
#'   response estimations.
#' @param show_rsp_cat (`logical`)\cr display the multinomial response estimations.
#' @param paramcd (`character`)\cr response parameter value to use in the table title.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_binary_outcome()]
#'
#' @keywords internal
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
                                      global = list(method = "waldcc", conf_level = 0.95),
                                      unstrat = list(method_ci = "waldcc", method_test = "schouten", odds = TRUE),
                                      strat = list(method_ci = "cmh", method_test = "cmh", strat = NULL)
                                    ),
                                    add_total = FALSE,
                                    total_label = default_total_label(),
                                    na_level = tern::default_na_str(),
                                    denom = c("N_col", "n", "N_row"),
                                    basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(aval_var)
  checkmate::assert_flag(compare_arm)
  checkmate::assert_flag(combine_comp_arms)
  checkmate::assert_flag(show_rsp_cat)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(na_level)
  checkmate::assert_string(total_label)

  denom <- match.arg(denom)

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
      expr = dplyr::mutate(is_rsp = dplyr::if_else(!is.na(aval_var), aval_var %in% responder_val, NA)) %>%
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
        ref_arm_val = ref_arm_val,
        compare_arm = compare_arm
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
        show_colcounts = TRUE,
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
          split_fun = rtables::add_overall_level(total_label, first = FALSE)
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
      tern::estimate_proportion(
        vars = "is_rsp",
        conf_level = conf_level,
        method = method,
        table_names = "prop_est",
        denom = denom
      ),
      env = list(
        conf_level = control$global$conf_level,
        method = control$global$method,
        denom = denom
      )
    )
  )

  if (compare_arm) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = conf_level,
          method = method_ci,
          table_names = "u_prop_diff"
        ) %>%
          tern::test_proportion_diff(
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
          expr = tern::estimate_odds_ratio(
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
          expr = tern::estimate_proportion_diff(
            vars = "is_rsp", show_labels = "visible",
            var_labels = "Stratified Analysis",
            variables = list(strata = strata),
            conf_level = conf_level,
            method = method_ci,
            table_names = "s_prop_diff"
          ) %>%
            tern::test_proportion_diff(
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
          expr = tern::estimate_odds_ratio(
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
          expr = tern::estimate_odds_ratio(
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
        tern::estimate_multinomial_response(
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
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parentname)
    },
    env = list(parentname = as.name(parentname))
  )
  y
}

#' teal Module: Binary Outcome Table
#'
#' This module produces a binary outcome response summary table, with the option to match the template for
#' response table `RSPT01` available in the TLG Catalog [here](
#' https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/rspt01.html).
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_binary_outcome
#' @param rsp_table (`logical`)\cr whether the initial set-up of the module should match `RSPT01`. Defaults to `FALSE`.
#' @param control (named `list`)\cr named list containing 3 named lists as follows:
#'   * `global`: a list of settings for overall analysis with 2 named elements `method` and `conf_level`.
#'   * `unstrat`: a list of settings for unstratified analysis with 3 named elements `method_ci` and `method_test`, and
#'     `odds`. See [tern::estimate_proportion_diff()], [tern::test_proportion_diff()], and
#'     [tern::estimate_odds_ratio()], respectively, for options and details on how these settings are implemented in the
#'     analysis.
#'   * `strat`: a list of settings for stratified analysis with elements `method_ci` and `method_test`. See
#'     [tern::estimate_proportion_diff()] and [tern::test_proportion_diff()], respectively, for options and details on
#'     how these settings are implemented in the analysis.
#'
#' @details
#' * The display order of response categories inherits the factor level order of the source data. Use
#'   [base::factor()] and its `levels` argument to manipulate the source data in order to include/exclude
#'   or re-categorize response categories and arrange the display order. If response categories are `"Missing"`,
#'   `"Not Evaluable (NE)"`, or `"Missing or unevaluable"`, 95% confidence interval will not be calculated.
#'
#' * Reference arms are automatically combined if multiple arms selected as reference group.
#'
#' @inherit module_arguments return seealso
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
#' tm_t_binary_outcome(
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
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#'
#' data <- teal_data()
#' data <- within(data, {
#'   library(dplyr)
#'   library(formatters)
#'   ADSL <- tmc_ex_adsl
#'   ADRS <- tmc_ex_adrs %>%
#'     mutate(
#'       AVALC = d_onco_rsp_label(AVALC) %>%
#'         with_label("Character Result/Finding")
#'     ) %>%
#'     filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADRS <- data[["ADRS"]]
#'
#' arm_ref_comp <- list(
#'   ARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
#'   ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
#' )
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_binary_outcome(
#'       label = "Responders",
#'       dataname = "ADRS",
#'       paramcd = choices_selected(
#'         choices = value_choices(ADRS, "PARAMCD", "PARAM"),
#'         selected = "BESRSPI"
#'       ),
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADRS, c("ARM", "ARMCD", "ACTARMCD")),
#'         selected = "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       strata_var = choices_selected(
#'         choices = variable_choices(ADRS, c("SEX", "BMRKR2", "RACE")),
#'         selected = "RACE"
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
#'       ),
#'       denom = "N_col"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
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
                                control = list(
                                  global = list(
                                    method = ifelse(rsp_table, "clopper-pearson", "waldcc"),
                                    conf_level = 0.95
                                  ),
                                  unstrat = list(
                                    method_ci = ifelse(rsp_table, "wald", "waldcc"),
                                    method_test = "schouten",
                                    odds = TRUE
                                  ),
                                  strat = list(method_ci = "cmh", method_test = "cmh")
                                ),
                                add_total = FALSE,
                                total_label = default_total_label(),
                                na_level = tern::default_na_str(),
                                denom = c("N_col", "n", "N_row"),
                                pre_output = NULL,
                                post_output = NULL,
                                basic_table_args = teal.widgets::basic_table_args(),
                                transformators = list(),
                                decorators = list()) {
  message("Initializing tm_t_binary_outcome")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(strata_var, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert(
    checkmate::check_class(default_responses, classes = "list"),
    checkmate::check_class(default_responses, classes = "character"),
    checkmate::check_class(default_responses, classes = "numeric"),
    checkmate::check_class(default_responses, classes = "NULL")
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  # control checks
  checkmate::assert_names(names(control), permutation.of = c("global", "unstrat", "strat"))
  checkmate::assert_names(names(control$global), permutation.of = c("method", "conf_level"))
  checkmate::assert_names(names(control$unstrat), permutation.of = c("method_ci", "method_test", "odds"))
  checkmate::assert_names(names(control$strat), permutation.of = c("method_ci", "method_test"))
  checkmate::assert_subset(
    control$global$method,
    c("wald", "waldcc", "clopper-pearson", "wilson", "wilsonc", "jeffreys", "agresti-coull")
  )
  checkmate::assert_number(control$global$conf_level, lower = 0, upper = 1)
  checkmate::assert_subset(control$unstrat$method_ci, c("wald", "waldcc", "ha", "newcombe", "newcombecc"))
  checkmate::assert_subset(control$unstrat$method_test, c("chisq", "fisher", "schouten"))
  checkmate::assert_logical(control$unstrat$odds)
  checkmate::assert_subset(
    control$strat$method_ci, c("wald", "waldcc", "cmh", "ha", "strat_newcombe", "strat_newcombecc")
  )
  checkmate::assert_subset(control$strat$method_test, c("cmh"))
  assert_decorators(decorators, "table")

  denom <- match.arg(denom)

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
        control = control,
        rsp_table = rsp_table,
        na_level = na_level,
        denom = denom,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_t_binary_outcome <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$paramcd,
    a$arm_var,
    a$aval_var,
    a$strata_var
  )

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(a[c("paramcd", "arm_var", "aval_var", "strata_var")]),
      teal.transform::data_extract_ui(
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
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      tags$div(
        class = "arm-comp-box",
        bslib::input_switch(
          id = ns("compare_arms"),
          label = "Compare Treatments",
          value = !is.null(a$arm_ref_comp)
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
            helpText("Multiple reference groups are automatically combined into a single group."),
            checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Unstratified analysis settings",
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
              selected = a$control$unstrat$method_ci,
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
              selected = a$control$unstrat$method_test,
              multiple = FALSE,
              fixed = FALSE
            ),
            bslib::input_switch(
              id = ns("u_odds_ratio"),
              label = "Odds Ratio Estimation",
              value = a$control$unstrat$odds
            )
          )
        ),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Stratified analysis settings",
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
              selected = a$control$strat$method_ci,
              multiple = FALSE
            ),
            teal.widgets::optionalSelectInput(
              ns("s_diff_test"),
              label = "Method for Difference of Proportions Test",
              choices = c("CMH Test" = "cmh"),
              selected = a$control$strat$method_test,
              multiple = FALSE,
              fixed = TRUE
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("!input['", ns("compare_arms"), "']"),
        checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total)
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion_panel(
        "Additional table settings",
        open = TRUE,
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
          selected = a$control$global$method,
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
        bslib::input_switch(
          id = ns("show_rsp_cat"),
          label = "Show All Response Categories",
          value = ifelse(a$rsp_table, TRUE, FALSE)
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_binary_outcome <- function(id,
                                 data,
                                 dataname,
                                 parentname,
                                 paramcd,
                                 aval_var,
                                 arm_var,
                                 arm_ref_comp,
                                 strata_var,
                                 add_total,
                                 control,
                                 total_label,
                                 label,
                                 default_responses,
                                 rsp_table,
                                 na_level,
                                 denom,
                                 basic_table_args,
                                 decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data()[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_binary_outcome",
      on_off = reactive(input$compare_arms)
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

    iv_r <- reactive({
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
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var, strata_var = strata_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    observeEvent(
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
        updateSelectInput(
          session, "responders",
          choices = responder_choices,
          selected = intersect(responder_choices, common_rsp)
        )
      }
    )

    validate_check <- reactive({
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

      validate(
        if (length(input_strata_var) >= 1L) {
          need(
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

      validate(
        if (length(input_strata_var) >= 1L) {
          need(
            sum(
              vapply(
                anl[input_strata_var],
                FUN = function(strata) {
                  anl_arm <- factor(anl[[input_arm_var]])
                  tab <- base::table(strata, anl_arm)
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
        validate(
          need(
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

    table_q <- reactive({
      validate_check()

      qenv <- anl_q()
      anl_m <- anl_inputs()
      anl <- qenv[["ANL"]]

      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      req(input$responders %in% anl[[input_aval_var]])

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
        na_level = na_level,
        denom = denom,
        basic_table_args = basic_table_args
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })


    decorated_all_q <- srv_decorate_teal_data(
      id = "decorator",
      data = table_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    # Outputs to render.
    table_r <- reactive(decorated_all_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_all_q
  })
}
