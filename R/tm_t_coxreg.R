#' Template: Univariable Cox Regression
#'
#' Creates a valid expression to generate a univariable Cox regression analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis (see [tern::control_coxreg()]).
#' @param at (`list` of `numeric`)\cr when the candidate covariate is a `numeric` type variable, use `at`
#'   to specify the value of the covariate at which the effect should be estimated.
#' @param append (`logical`)\cr whether the result should be appended to the previous one.
#'
#' @inherit template_arguments return
#'
#' @seealso [template_coxreg_m()], [tm_t_coxreg()]
#'
#' @keywords internal
template_coxreg_u <- function(dataname,
                              cov_var,
                              arm_var,
                              cnsr_var,
                              aval_var,
                              ref_arm,
                              comp_arm,
                              paramcd,
                              at = list(),
                              strata_var = NULL,
                              combine_comp_arms = FALSE,
                              control = tern::control_coxreg(),
                              na_level = tern::default_na_str(),
                              append = FALSE,
                              basic_table_args = teal.widgets::basic_table_args()) {
  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  data_pipe <- list()
  data_list <- list()

  data_pipe <- add_expr(
    data_pipe,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  if (combine_comp_arms) {
    data_pipe <- add_expr(
      data_pipe,
      substitute_names(
        expr = dplyr::mutate(arm_var = tern::combine_levels(x = arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
  }

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = dplyr::mutate(event = 1 - cnsr_var),
      env = list(cnsr_var = as.name(cnsr_var))
    )
  )

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = dplyr::mutate(across(where(is.factor) & cov_var, droplevels)),
      env = list(cov_var = cov_var)
    )
  )

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = tern::df_explicit_na(na_level = na_lvl),
      env = list(na_lvl = na_level)
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- data_pipe,
      env = list(data_pipe = pipe_expr(data_pipe))
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = control <- ctrl,
      env = list(ctrl = control)
    )
  )

  variables <- list(time = aval_var, event = "event", arm = arm_var)

  if (!is.null(cov_var)) variables$covariates <- cov_var
  if (!is.null(strata_var)) variables$strata <- strata_var

  y$data <- bracket_expr(data_list)

  layout_list <- list()

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        title = paste("Multi-Variable Cox Regression for", paramcd),
        main_footer = c(
          paste("p-value method for Coxph (Hazard Ratio):", control$pval_method),
          paste("Ties for Coxph (Hazard Ratio):", control$ties)
        )
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::append_topleft(paramcd),
      env = list(paramcd = paramcd)
    )
  )

  stats <- c("n", "hr", "ci", "pval")

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = tern::summarize_coxreg(
        variables = variables,
        control = control,
        at = at,
        multivar = multivariate,
        .stats = stats,
        na_str = na_str
      ),
      env = list(
        multivariate = FALSE,
        variables = variables,
        control = control,
        at = at,
        stats = if (control$interaction) c(stats, "pval_inter") else stats,
        na_str = na_level
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- if (append) {
    quote(table <- c(table, rtables::build_table(lyt = lyt, df = anl)))
  } else {
    quote(table <- rtables::build_table(lyt = lyt, df = anl))
  }

  y
}

#' Template: Multi-Variable Cox Regression
#'
#' Creates a valid expression to generate a multi-variable Cox regression analysis.
#'
#' @inheritParams template_coxreg_u
#' @inheritParams template_arguments
#'
#' @inherit template_arguments return
#'
#' @seealso [template_coxreg_u()], [tm_t_coxreg()]
#'
#' @keywords internal
template_coxreg_m <- function(dataname,
                              cov_var,
                              arm_var,
                              cnsr_var,
                              aval_var,
                              ref_arm,
                              comp_arm,
                              paramcd,
                              at = list(),
                              strata_var = NULL,
                              combine_comp_arms = FALSE,
                              control = tern::control_coxreg(),
                              na_level = tern::default_na_str(),
                              basic_table_args = teal.widgets::basic_table_args()) {
  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  data_pipe <- list()
  data_list <- list()

  data_pipe <- add_expr(
    data_pipe,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  if (combine_comp_arms) {
    data_pipe <- add_expr(
      data_pipe,
      substitute_names(
        expr = dplyr::mutate(arm_var = tern::combine_levels(x = arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
  }

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = dplyr::mutate(event = 1 - cnsr_var),
      env = list(cnsr_var = as.name(cnsr_var))
    )
  )

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = dplyr::mutate(across(where(is.factor) & cov_var, droplevels)),
      env = list(cov_var = cov_var)
    )
  )

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = tern::df_explicit_na(na_level = na_lvl),
      env = list(na_lvl = na_level)
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- data_pipe,
      env = list(data_pipe = pipe_expr(data_pipe))
    )
  )

  variables <- list(time = aval_var, event = "event", arm = arm_var)

  if (!is.null(cov_var)) variables$covariates <- cov_var
  if (!is.null(strata_var)) variables$strata <- strata_var

  y$data <- bracket_expr(data_list)

  layout_list <- list()

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        title = paste("Cox Regression for", paramcd),
        main_footer = c(
          paste("p-value method for Coxph (Hazard Ratio):", control$pval_method),
          paste("Ties for Coxph (Hazard Ratio):", control$ties)
        )
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::append_topleft(paramcd),
      env = list(paramcd = paramcd)
    )
  )

  stats <- c("hr", "ci", "pval")

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = tern::summarize_coxreg(
        variables = variables,
        control = control,
        multivar = multivariate,
        .stats = stats,
        na_str = na_str
      ),
      env = list(
        variables = variables,
        control = control,
        multivariate = TRUE,
        stats = stats,
        na_str = na_level
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- quote({
    table <- rtables::build_table(lyt = lyt, df = anl)
  })

  y
}

#' teal Module: Cox Regression Model
#'
#' This module fits Cox univariable or multi-variable models, consistent with the TLG Catalog
#' templates for Cox regression tables `COXT01` and `COXT02`, respectively. See the TLG Catalog entries
#' for `COXT01` [here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/coxt01.html)
#' and `COXT02` [here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/coxt02.html).
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_coxreg_u
#' @inheritParams template_coxreg_m
#' @param multivariate (`logical`)\cr if `FALSE`, the univariable approach is used instead of the
#'   multi-variable model.
#'
#' @details
#' The Cox Proportional Hazards (PH) model is the most commonly used method to
#' estimate the magnitude of the effect in survival analysis. It assumes proportional
#' hazards: the ratio of the hazards between groups (e.g., two arms) is constant over time.
#' This ratio is referred to as the "hazard ratio" (HR) and is one of the most
#' commonly reported metrics to describe the effect size in survival analysis.
#'
#' This modules expects that the analysis data has the following variables:
#'
#' * `AVAL`: time to event
#' * `CNSR`: 1 if record in `AVAL` is censored, 0 otherwise
#' * `PARAMCD`: variable used to filter for endpoint (e.g. OS). After
#'   filtering for `PARAMCD` one observation per patient is expected
#'
#' The arm variables and stratification/covariate variables are taken from the `ADSL` data.
#'
#' @note
#' * The likelihood ratio test is not supported for models that include strata - the Wald
#'   test will be substituted in these cases.
#' * Multi-variable is the default choice for backward compatibility.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`TableTree` as created from `rtables::build_table`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_coxreg(
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
#' ## First example
#' ## =============
#' ## The example below is based on the usual approach involving creation of
#' ## a random CDISC dataset and then running the application.
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
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADTTE <- tmc_ex_adtte
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADTTE <- data[["ADTTE"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_coxreg(
#'       label = "Cox Reg.",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(c("ARM", "ARMCD", "ACTARMCD"), "ARM"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"
#'       ),
#'       strata_var = choices_selected(
#'         c("COUNTRY", "STRATA1", "STRATA2"), "STRATA1"
#'       ),
#'       cov_var = choices_selected(
#'         c("AGE", "BMRKR1", "BMRKR2", "REGION1"), "AGE"
#'       ),
#'       multivariate = TRUE
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' ## Second example
#' ## ==============
#' ## This time, a synthetic pair of ADTTE/ADSL data is fabricated for Cox regression
#' ## where ties and pval_method matter.
#' library(dplyr)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   ADTTE <- data.frame(
#'     STUDYID = "LUNG",
#'     AVAL = c(4, 3, 1, 1, 2, 2, 3, 1, 2),
#'     CNSR = c(1, 1, 1, 0, 1, 1, 0, 0, 0),
#'     ARMCD = factor(
#'       c(0, 1, 1, 1, 1, 0, 0, 0, 0),
#'       labels = c("ARM A", "ARM B")
#'     ),
#'     SEX = factor(
#'       c(0, 0, 0, 0, 1, 1, 1, 1, 1),
#'       labels = c("F", "M")
#'     ),
#'     INST = factor(c("A", "A", "B", "B", "A", "B", "A", "B", "A")),
#'     stringsAsFactors = FALSE
#'   )
#'   ADTTE <- rbind(ADTTE, ADTTE, ADTTE, ADTTE)
#'   ADTTE <- as_tibble(ADTTE)
#'   set.seed(1)
#'   ADTTE$INST <- sample(ADTTE$INST)
#'   ADTTE$AGE <- sample(seq(5, 75, 5), size = nrow(ADTTE), replace = TRUE)
#'   ADTTE$USUBJID <- paste("sub", 1:nrow(ADTTE), ADTTE$INST, sep = "-")
#'   ADTTE$PARAM <- ADTTE$PARAMCD <- "OS"
#'   ADSL <- subset(
#'     ADTTE,
#'     select = c("USUBJID", "STUDYID", "ARMCD", "SEX", "INST", "AGE")
#'   )
#' })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADTTE <- data[["ADTTE"]]
#'
#' ## `teal` application
#' ## ----------------
#' ## Note that the R code exported by `Show R Code` does not include the data
#' ## pre-processing. You will need to create the dataset as above before
#' ## running the exported R code.
#'
#' arm_ref_comp <- list(ARMCD = list(ref = "ARM A", comp = c("ARM B")))
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_coxreg(
#'       label = "Cox Reg.",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(c("ARMCD"), "ARMCD"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"
#'       ),
#'       strata_var = choices_selected(c("INST"), NULL),
#'       cov_var = choices_selected(c("SEX", "AGE"), "SEX"),
#'       multivariate = TRUE
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_coxreg <- function(label,
                        dataname,
                        parentname = ifelse(
                          inherits(arm_var, "data_extract_spec"),
                          teal.transform::datanames_input(arm_var),
                          "ADSL"
                        ),
                        arm_var,
                        arm_ref_comp = NULL,
                        paramcd,
                        cov_var,
                        strata_var,
                        aval_var = teal.transform::choices_selected(
                          teal.transform::variable_choices(dataname, "AVAL"), "AVAL",
                          fixed = TRUE
                        ),
                        cnsr_var = teal.transform::choices_selected(
                          teal.transform::variable_choices(dataname, "CNSR"), "CNSR",
                          fixed = TRUE
                        ),
                        multivariate = TRUE,
                        na_level = tern::default_na_str(),
                        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.widgets::basic_table_args(),
                        transformators = list(),
                        decorators = list()) {
  message("Initializing tm_t_coxreg")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(cov_var, "choices_selected")
  checkmate::assert_class(strata_var, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(cnsr_var, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname, label = NULL),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = parentname, multiple = TRUE, ordered = TRUE)
  )

  module(
    label = label,
    server = srv_t_coxreg,
    ui = ui_t_coxreg,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        arm_ref_comp = arm_ref_comp,
        dataname = dataname,
        parentname = parentname,
        label = label,
        na_level = na_level,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_t_coxreg <- function(id, ...) {
  a <- list(...) # module args
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$strata_var,
    a$aval_var,
    a$cnsr_var,
    a$cov_var
  )

  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      radioButtons(
        ns("type"),
        label = tags$label("Type of Regression:", class = "text-primary"),
        choices = c(
          "Separate models for comparison groups with one covariate at a time" = "Univariate",
          "One model with all comparison groups and covariates" = "Multivariate"
        ),
        selected = dplyr::if_else(a$multivariate, "Multivariate", "Univariate")
      ),
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(
        a[c("arm_var", "paramcd", "subgroup_var", "strata_var", "aval_var", "cnsr_var", "cov_var")]
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      uiOutput(ns("arms_buckets")),
      conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'Multivariate'"),
        checkboxInput(
          ns("combine_comp_arms"),
          "Combine all comparison groups?"
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("cov_var"),
        label = "Covariates",
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'Univariate'"),
        checkboxInput(
          ns("interactions"),
          "Interaction terms"
        )
      ),
      uiOutput(ns("interaction_input")),
      teal.transform::data_extract_ui(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          conditionalPanel(
            condition = paste0("input['", ns("strata_var"), "'] != ''"),
            radioButtons(
              ns("pval_method"),
              label = tags$p(
                "p-value method for",
                tags$span(class = "text-primary", "Coxph"),
                "(Hazard Ratio)"
              ),
              choices = c("wald", "likelihood"),
              selected = "wald"
            )
          ),
          radioButtons(
            ns("ties"),
            label = tags$p(
              "Ties for ",
              tags$span(class = "text-primary", "Coxph"),
              " (Hazard Ratio)",
              sep = ""
            ),
            choices = c("exact", "breslow", "efron"),
            selected = "exact"
          ),
          teal.widgets::optionalSelectInput(
            inputId = ns("conf_level"),
            label = tags$p(
              "Confidence level for ",
              tags$span(class = "text-primary", "Coxph"),
              " (Hazard Ratio)",
              sep = ""
            ),
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          )
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table"))
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_coxreg <- function(id,
                         data,
                         dataname,
                         parentname,
                         arm_var,
                         paramcd,
                         strata_var,
                         aval_var,
                         cnsr_var,
                         cov_var,
                         arm_ref_comp,
                         label,
                         na_level,
                         basic_table_args,
                         decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    # Observer to update reference and comparison arm input options.
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_coxreg"
    )

    use_interactions <- reactive({
      input$type == "Univariate" && isTRUE(input$interactions)
    })

    overlap_rule <- function(other_var, var_name) {
      function(value) {
        if (length(intersect(value, selector_list()[[other_var]]()$select)) > 0) {
          sprintf("`%s` and `%s` variables should not overlap", var_name[1], var_name[2])
        }
      }
    }

    select_validation_rule <- list(
      aval_var = shinyvalidate::sv_required("An analysis variable is required"),
      cnsr_var = shinyvalidate::sv_required("A censor variable is required"),
      arm_var = shinyvalidate::compose_rules(
        shinyvalidate::sv_required("A treatment variable is required"),
        overlap_rule("strata_var", c("Treatment", "Strata")),
        overlap_rule("cov_var", c("Treatment", "Covariate"))
      ),
      strata_var = shinyvalidate::compose_rules(
        overlap_rule("arm_var", c("Treatment", "Strata")),
        overlap_rule("cov_var", c("Covariate", "Strata"))
      ),
      cov_var = shinyvalidate::compose_rules(
        overlap_rule("arm_var", c("Treatment", "Covariate")),
        overlap_rule("strata_var", c("Covariate", "Strata")),
        ~ if (use_interactions() && length(.) == 0) {
          "If interactions are selected at least one covariate should be specified."
        }
      )
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        strata_var = strata_var,
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        cov_var = cov_var
      ),
      datasets = data,
      select_validation_rule = select_validation_rule,
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("An endpoint is required")
      )
    )


    numeric_level_validation <- function(val) {
      # need to explicitly evaluate 'val' here to ensure
      # the correct label is shown - if this is not done
      # then the last value of "val" is the label for all cases
      v <- val
      ~ if (anyNA(as_numeric_from_comma_sep_str(.))) {
        paste("Numeric interaction level(s) should be specified for", v)
      }
    }


    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(iv_arm_ref)
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(0, 1, message_fmt = "Confidence level must be between 0 and 1")
      )
      iv$add_rule("pval_method", ~ if (length(selector_list()$strata_var()$select) > 0 && . != "wald") {
        "Only Wald tests are supported for models with strata."
      })
      # add rules for interaction_var text inputs

      for (val in interaction_var_r()) {
        iv$add_rule(
          paste0("interact_", val),
          shinyvalidate::sv_required(paste("Interaction level(s) should be specified for", val))
        )
        iv$add_rule(
          paste0("interact_", val), numeric_level_validation(val)
        )
      }
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Cox Regression Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      teal.code::eval_code(obj, as.expression(anl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      anl_q = anl_q
    )

    ## render conditional strata levels input UI  ----
    open_textinput <- function(x, dataset) {
      # For every numeric covariate, the numeric level for the Hazard Ration
      # estimation is proposed only if the covariate is included in the model:
      # for this purpose, a function and a UI-rendered output.
      textInput(
        session$ns(paste0("interact_", x)),
        label = paste("Hazard Ratios for", x, "at (comma delimited):"),
        value = as.character(stats::median(dataset[[x]]))
      )
    }

    interaction_var_r <- reactive({
      # exclude cases when increments are not necessary and
      # finally accessing the UI-rendering function defined above.
      if (use_interactions()) {
        input_cov_var <- as.vector(merged$anl_input_r()$columns_source$cov_var)
        dataset <- merged$anl_q()[[dataname]]
        cov_is_numeric <- vapply(dataset[input_cov_var], is.numeric, logical(1))
        input_cov_var[cov_is_numeric]
      } else {
        NULL
      }
    })

    output$interaction_input <- renderUI({
      if (length(interaction_var_r()) > 0) {
        lapply(interaction_var_r(), open_textinput, dataset = merged$anl_q()[[dataname]])
      }
    })

    ## Prepare the call evaluation environment ----
    validate_checks <- reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_strata_var <- as.vector(merged$anl_input_r()$columns_source$strata_var)
      input_aval_var <- as.vector(merged$anl_input_r()$columns_source$aval_var)
      input_cnsr_var <- as.vector(merged$anl_input_r()$columns_source$cnsr_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]
      input_cov_var <- as.vector(merged$anl_input_r()$columns_source$cov_var)

      cov_is_numeric <- vapply(anl_filtered[input_cov_var], is.numeric, logical(1))
      interaction_var <- input_cov_var[cov_is_numeric]

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var),
        arm_var = input_arm_var,
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        min_nrow = 4
      )

      #  validate arm levels
      if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }

      do.call(what = "validate_standard_inputs", validate_args)

      arm_n <- base::table(anl_filtered[[input_arm_var]])
      anl_arm_n <- if (input$combine_comp_arms) {
        c(sum(arm_n[unlist(input$buckets$Ref)]), sum(arm_n[unlist(input$buckets$Comp)]))
      } else {
        c(sum(arm_n[unlist(input$buckets$Ref)]), arm_n[unlist(input$buckets$Comp)])
      }
      validate(shiny::need(
        all(anl_arm_n >= 2),
        "Each treatment group should have at least 2 records."
      ))

      # validate covariate has at least two levels
      validate(
        need(
          all(vapply(anl_filtered[input_cov_var], FUN = function(x) {
            length(unique(x)) > 1
          }, logical(1))),
          "All covariates needs to have at least two levels"
        )
      )

      NULL
    })

    at <- reactive({
      input_cov_var <- as.vector(merged$anl_input_r()$columns_source$cov_var)
      cov_is_numeric <- vapply(merged$anl_q()[[dataname]][input_cov_var], is.numeric, logical(1))
      interaction_var <- input_cov_var[cov_is_numeric]
      if (length(interaction_var) > 0 && length(input_cov_var) > 0) {
        res <- lapply(
          interaction_var,
          function(x) {
            cov <- input[[paste0("interact_", x)]]
            if (!is.null(cov)) {
              as_numeric_from_comma_sep_str(cov)
            }
          }
        )
        stats::setNames(res, interaction_var)
      }
    })


    call_template <- function(comp_arm, anl, paramcd, multivariate, basic_table_args = NULL) {
      strata_var <- as.vector(anl$columns_source$strata_var)
      strata_var <- if (length(strata_var) != 0) strata_var else NULL
      cov_var <- as.vector(anl$columns_source$cov_var)
      cov_var <- if (length(cov_var) > 0) cov_var else NULL

      at <- if (use_interactions()) at() else list()
      arm_var <- as.vector(anl$columns_source$arm_var)
      cnsr_var <- as.vector(anl$columns_source$cnsr_var)
      aval_var <- as.vector(anl$columns_source$aval_var)
      ref_arm <- unlist(input$buckets$Ref)
      combine_comp_arms <- input$combine_comp_arms
      control <- control_coxreg(
        pval_method = input$pval_method,
        ties = input$ties,
        conf_level = as.numeric(input$conf_level),
        interaction = `if`(!use_interactions(), FALSE, input$interactions)
      )

      if (multivariate) {
        template_coxreg_m(
          dataname = "ANL",
          cov_var = cov_var,
          at = at,
          arm_var = arm_var,
          cnsr_var = cnsr_var,
          aval_var = aval_var,
          ref_arm = ref_arm,
          comp_arm = comp_arm,
          paramcd = paramcd,
          strata_var = strata_var,
          combine_comp_arms = combine_comp_arms,
          control = control,
          na_level = na_level,
          basic_table_args = basic_table_args
        )
      } else {
        template_coxreg_u(
          dataname = "ANL",
          cov_var = cov_var,
          at = at,
          arm_var = arm_var,
          cnsr_var = cnsr_var,
          aval_var = aval_var,
          ref_arm = ref_arm,
          comp_arm = comp_arm,
          paramcd = paramcd,
          strata_var = strata_var,
          combine_comp_arms = combine_comp_arms,
          control = control,
          na_level = na_level,
          append = TRUE,
          basic_table_args = basic_table_args
        )
      }
    }

    ## generate table call with template and render table ----
    all_q <- reactive({
      validate_checks()

      ANL <- merged$anl_q()[["ANL"]]
      paramcd <- as.character(unique(ANL[[unlist(paramcd$filter)["vars_selected"]]]))
      multivariate <- input$type == "Multivariate"
      strata_var <- as.vector(merged$anl_input_r()$columns_source$strata_var)

      if (input$type == "Multivariate") {
        main_title <- paste("Multi-Variable Cox Regression for", paramcd)
        subtitle <- ifelse(length(strata_var) == 0, "", paste("Stratified by", paste(strata_var, collapse = " and ")))
        all_basic_table_args <- teal.widgets::resolve_basic_table_args(
          user_table = basic_table_args,
          module_table = teal.widgets::basic_table_args(
            title = main_title,
            subtitles = subtitle
          )
        )
        expr <- call_template(
          unlist(input$buckets$Comp), merged$anl_input_r(),
          paramcd, multivariate, all_basic_table_args
        )
        obj <- merged$anl_q()
        teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
        teal.code::eval_code(obj, as.expression(expr))
      } else {
        main_title <- paste("Cox Regression for", paramcd)
        subtitle <- ifelse(length(strata_var) == 0, "", paste("Stratified by", paste(strata_var, collapse = " and ")))
        all_basic_table_args <- teal.widgets::resolve_basic_table_args(
          user_table = basic_table_args,
          module_table = teal.widgets::basic_table_args(
            title = main_title,
            subtitles = subtitle
          )
        )

        obj <- merged$anl_q()
        teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
        obj %>%
          teal.code::eval_code(quote(table <- list())) %>%
          teal.code::eval_code(
            as.expression(unlist(lapply(
              unlist(input$buckets$Comp),
              function(x) {
                call_template(x, merged$anl_input_r(), paramcd, multivariate, all_basic_table_args)
              }
            )))
          ) %>%
          teal.code::eval_code(
            substitute(
              expr = {
                table <- lapply(table, function(x) {
                  rtables::col_info(x) <- rtables::col_info(table[[1]])
                  x
                })
                table <- rtables::rbindl_rtables(table, check_headers = TRUE)
                rtables::main_title(table) <- title
                rtables::main_footer(table) <- c(
                  paste("p-value method for Coxph (Hazard Ratio):", control$pval_method),
                  paste("Ties for Coxph (Hazard Ratio):", control$ties)
                )
                rtables::prov_footer(table) <- p_footer
                rtables::subtitles(table) <- subtitle
              },
              env = list(
                title = all_basic_table_args$title,
                p_footer = `if`(is.null(all_basic_table_args$prov_footer), "", all_basic_table_args$prov_footer),
                subtitle = `if`(is.null(all_basic_table_args$subtitles), "", all_basic_table_args$subtitles)
              )
            )
          )
      }
    })



    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    # Outputs to render.
    table_r <- reactive({
      decorated_table_q()[["table"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
