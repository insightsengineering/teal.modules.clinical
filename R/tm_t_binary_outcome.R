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
#'       paramcd = variables(choices = "PARAMCD"),
#'       arm_var = variables(
#'         choices = c("ARM", "ARMCD", "ACTARMCD"),
#'         selected = "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       strata_var = variables(
#'         choices = c("SEX", "BMRKR2", "RACE"),
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
                                parentname = "ADSL",
                                arm_var,
                                arm_ref_comp = NULL,
                                paramcd,
                                strata_var,
                                aval_var = variables(
                                  choices = c("AVALC", "SEX"),
                                  selected = "AVALC",
                                  fixed = FALSE
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
  arm_var <- teal.picks::as.picks(arm_var, quiet = FALSE)
  paramcd <- teal.picks::as.picks(paramcd, quiet = FALSE)
  strata_var <- teal.picks::as.picks(strata_var, quiet = FALSE)
  aval_var <- teal.picks::as.picks(aval_var, quiet = FALSE)
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(paramcd, "variables")
  checkmate::assert_class(strata_var, "variables")
  checkmate::assert_class(aval_var, "variables")
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
  teal::assert_decorators(decorators, "table")

  denom <- match.arg(denom)

  arm_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), arm_var)
  paramcd <- teal.picks::picks(teal.picks::datasets(dataname, dataname), paramcd, values())
  aval_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), aval_var)
  strata_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), strata_var)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_binary_outcome,
    ui_args = args[names(args) %in% names(formals(ui_t_binary_outcome))],
    server = srv_t_binary_outcome,
    server_args = args[names(args) %in% names(formals(srv_t_binary_outcome))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}

#' @keywords internal
ui_t_binary_outcome <- function(id,
                                paramcd,
                                arm_var,
                                strata_var,
                                aval_var,
                                arm_ref_comp,
                                add_total,
                                control,
                                conf_level,
                                rsp_table,
                                pre_output,
                                post_output,
                                decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(tags$label("Parameter"), teal.picks::picks_ui(ns("paramcd"), paramcd)),
      selectInput(
        ns("responders"),
        "Responders",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      tags$div(tags$label("Select Treatment Variable"), teal.picks::picks_ui(ns("arm_var"), arm_var)),
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
              selected = control$unstrat$method_ci,
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
              selected = control$unstrat$method_test,
              multiple = FALSE,
              fixed = FALSE
            ),
            bslib::input_switch(
              id = ns("u_odds_ratio"),
              label = "Odds Ratio Estimation",
              value = control$unstrat$odds
            )
          )
        ),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Stratified analysis settings",
            tags$div(tags$label("Stratification Factors"), teal.picks::picks_ui(ns("strata_var"), strata_var)),
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
              selected = control$strat$method_ci,
              multiple = FALSE
            ),
            teal.widgets::optionalSelectInput(
              ns("s_diff_test"),
              label = "Method for Difference of Proportions Test",
              choices = c("CMH Test" = "cmh"),
              selected = control$strat$method_test,
              multiple = FALSE,
              fixed = TRUE
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("!input['", ns("compare_arms"), "']"),
        checkboxInput(ns("add_total"), "Add All Patients column", value = add_total)
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
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
            selected = control$global$method,
            multiple = FALSE,
            fixed = FALSE
          ),
          teal.widgets::optionalSelectInput(
            inputId = ns("conf_level"),
            label = "Confidence Level",
            conf_level$choices,
            conf_level$selected,
            multiple = FALSE,
            fixed = conf_level$fixed
          ),
          bslib::input_switch(
            id = ns("show_rsp_cat"),
            label = "Show All Response Categories",
            value = ifelse(rsp_table, TRUE, FALSE)
          )
        ),
        tags$div(tags$label("Analysis Variable"), teal.picks::picks_ui(ns("aval_var"), aval_var))
      )
    ),
    pre_output = pre_output,
    post_output = post_output
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
    anl_selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        arm_var = arm_var,
        paramcd = paramcd,
        strata_var = strata_var,
        aval_var = aval_var
      ),
      data = data
    )

    arm_var_r <- reactive(anl_selectors$arm_var()$variables$selected)

    arm_ref_comp_iv <- arm_ref_comp_observer_picks(
      session,
      input,
      output,
      id_arm_var = "arm_var-variables-selected",
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_binary_outcome",
      on_off = reactive(input$compare_arms),
      arm_var_r = arm_var_r
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()

      if (isTRUE(input$compare_arms)) {
        iv$add_validator(arm_ref_comp_buckets_validator())
      }

      iv$add_rule("responders", shinyvalidate::sv_required("`Responders` field is empty"))
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level between 0 and 1"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(0, 1, message_fmt = "Please choose a confidence level between {left} and {right}")
      )
      iv$enable()
      iv
    })

    data_with_card <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj
    })
    merged_anl <- merge_srv(
      "merge_anl",
      data = data_with_card, selectors = anl_selectors, output_name = "ANL"
    )
    adsl_selectors <- anl_selectors[c("arm_var", "strata_var")]
    merged_adsl <- merge_srv(
      "merge_adsl_anl",
      data = merged_anl$data,
      selectors = adsl_selectors,
      output_name = "ANL_ADSL"
    )
    anl_q <- merged_adsl$data

    # Keep responders selectInput in sync with merged ANL and all encoding picks.
    # observeEvent(aval, paramcd) alone missed arm/strata/merge updates, leaving
    # responders empty after picks commit and failing shinyvalidate.
    shiny::observe(
      {
        anl <- anl_q()[["ANL"]]
        shiny::req(is.data.frame(anl), nrow(anl) > 0L)

        aval_name <- anl_selectors$aval_var()$variables$selected
        shiny::req(length(aval_name) > 0L)

        pc <- anl_selectors$paramcd()
        paramcd_sel <- if (is.null(pc$values)) character(0) else pc$values$selected
        shiny::req(length(paramcd_sel) > 0L)

        invisible(anl_selectors$arm_var()$variables$selected)
        invisible(anl_selectors$strata_var()$variables$selected)

        sel_param <- if (is.list(default_responses) && length(paramcd_sel) > 0L) {
          default_responses[[paramcd_sel[[1]]]]
        } else {
          default_responses
        }
        common_rsp <- if (is.list(sel_param)) {
          sel_param$rsp
        } else {
          sel_param
        }
        responder_choices <- if (length(aval_name) == 0L) {
          character(0)
        } else {
          av <- aval_name[[1]]
          if (is.list(sel_param) && "levels" %in% names(sel_param)) {
            if (length(intersect(unique(anl[[av]]), sel_param$levels)) > 1) {
              sel_param$levels
            } else {
              unique(anl[[av]])
            }
          } else {
            unique(anl[[av]])
          }
        }
        if (length(responder_choices) == 0L) {
          return(invisible(NULL))
        }
        default_sel <- intersect(responder_choices, common_rsp)
        prev <- as.character(
          unlist(shiny::isolate(input$responders), use.names = FALSE)
        )
        new_sel <- if (
          length(prev) > 0L &&
            all(prev %in% responder_choices)
        ) {
          intersect(prev, responder_choices)
        } else {
          default_sel
        }
        if (length(new_sel) == 0L) {
          new_sel <- default_sel
        }
        if (length(new_sel) == 0L) {
          return(invisible(NULL))
        }
        shiny::updateSelectInput(
          session, "responders",
          choices = responder_choices,
          selected = new_sel
        )
      },
      priority = 1L
    )

    validate_check <- reactive({
      if (isTRUE(input$compare_arms)) {
        arm_ref_comp_iv()
      }
      teal::validate_inputs(iv_r())
      validate(
        need(length(anl_selectors$arm_var()$variables$selected) >= 1L, "A treatment variable is required"),
        need(length(anl_selectors$aval_var()$variables$selected) >= 1L, "An analysis variable is required")
      )
      pc <- anl_selectors$paramcd()
      pc_vals <- if (is.null(pc$values)) character(0) else pc$values$selected
      validate(need(length(pc_vals) >= 1L, "Please select a filter."))

      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      input_arm_var <- as.vector(anl_selectors$arm_var()$variables$selected)
      input_strata_var <- as.vector(anl_selectors$strata_var()$variables$selected)
      input_aval_var <- as.vector(anl_selectors$aval_var()$variables$selected)
      input_paramcd <- as.vector(anl_selectors$paramcd()$variables$selected)

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
      anl <- qenv[["ANL"]]

      input_aval_var <- as.vector(anl_selectors$aval_var()$variables$selected)
      req(input$responders %in% anl[[input_aval_var]])

      input_strata_var <- as.vector(anl_selectors$strata_var()$variables$selected)
      paramcd_col <- as.vector(anl_selectors$paramcd()$variables$selected)[[1]]
      input_paramcd <- unique(anl[[paramcd_col]])[[1]]

      responder_val_levels <- as.character(unique(anl[[input_aval_var]]))
      final_responder <- if (is.list(default_responses)) {
        default_responses[[as.character(input_paramcd)]][["levels"]]
      } else {
        responder_val_levels
      }
      if (length(final_responder) == 0) final_responder <- input$responders

      my_calls <- template_binary_outcome(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_selectors$arm_var()$variables$selected),
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


    decorated_all_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = table_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
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
