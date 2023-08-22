#' Template: Univariable Cox Regression
#'
#' Creates a valid expression for Cox regression analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis, see [control_coxreg()].
#' @param at (`list` of `numeric`)\cr when the candidate covariate is a `numeric`, use `at`
#' to specify the value of the covariate at which the effect should be estimated.
#' @param append (`logical`)\cr if the result should be appended to the previous one.
#'
#' @seealso [tm_t_coxreg()]
#' @keywords internal
#'
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
                              control = control_coxreg(),
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
        expr = dplyr::mutate(arm_var = combine_levels(x = arm_var, levels = comp_arm)),
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

  data_pipe <- add_expr(data_pipe, quote(df_explicit_na(na_level = "")))

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

  variables <- list(
    time = aval_var, event = "event", arm = arm_var, covariates = cov_var
  )

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
      expr = summarize_coxreg(
        variables = variables,
        control = control,
        at = at,
        multivar = multivariate,
        .stats = stats
      ),
      env = list(
        multivariate = FALSE,
        variables = variables,
        control = control,
        at = at,
        stats = if (control$interaction) c(stats, "pval_inter") else stats
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- if (append) {
    quote(result <- c(result, rtables::build_table(lyt = lyt, df = anl)))
  } else {
    quote(result <- rtables::build_table(lyt = lyt, df = anl))
  }

  y
}

#' Template: Multi-Variable Cox Regression
#'
#' Creates a valid expression for Cox regression analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis,
#'   see [control_coxreg()].
#' @param at (`list` of `numeric`)\cr when the candidate covariate is a
#'  `numeric`, use `at` to specify the value of the covariate at which the
#'  effect should be estimated.
#'
#' @seealso [tm_t_coxreg()]
#' @keywords internal
#'
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
                              control = control_coxreg(),
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
        expr = dplyr::mutate(arm_var = combine_levels(x = arm_var, levels = comp_arm)),
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

  data_pipe <- add_expr(data_pipe, quote(df_explicit_na(na_level = "")))

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- data_pipe,
      env = list(data_pipe = pipe_expr(data_pipe))
    )
  )

  variables <- list(
    time = aval_var, event = "event", arm = arm_var, covariates = cov_var
  )

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
      expr = summarize_coxreg(
        variables = variables,
        control = control,
        multivar = multivariate,
        .stats = stats
      ),
      env = list(
        variables = variables,
        control = control,
        multivariate = TRUE,
        stats = stats
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- quote({
    result <- rtables::build_table(lyt = lyt, df = anl)
    result
  })

  y
}

#' Teal Module: Cox Regression Model
#'
#' Teal module to fit Cox univariable or multi-variable models consistent with
#' `COXT01` and `COXT02` standard outputs, respectively.
#'
#' @inheritParams module_arguments
#' @param multivariate (`logical`)\cr
#'   If `FALSE`, the univariable approach is used
#'   (equivalent to `COXT01` standard) instead of the multi-variable model
#'   (equivalent to `COXT02` standard).
#'
#' @details
#' The Cox Proportional Hazards (PH) model is the most commonly used method to
#' estimate the magnitude of
#' the effect in survival analysis. It assumes proportional hazards: the ratio
#' of the hazards between groups (e.g., two arms) is constant over time.
#' This ratio is referred to as the "hazard ratio" (HR) and is one of the most
#' commonly reported metrics to describe the effect size in survival analysis.
#' For further information about the Cox Proportional Hazards Model, check
#' "Statistical Analysis of Clinical Trials Data with R", NEST team.
#'
#' This modules expects that the analysis data has the following variables:
#'
#' \tabular{ll}{
#'  `AVAL` \tab time to event\cr
#'  `CNSR` \tab boolean or 0,1 is element in `AVAL` censored\cr
#'  `PARAMCD` \tab variable used to filter for endpoint (e.g. OS), after
#'  filtering for `paramcd` one observation per patient is expected
#' }
#'
#' The arm variables, stratification and covariate variables are taken from the
#' `ADSL` data.
#'
#' @section Note:
#' - The likelihood ratio test is not supported for model including strata,
#'   Wald test will be substituted.
#' - Multi-Variable is the default choice for backward compatibility.
#'
#' @export
#'
#' @examples
#' ## First example
#' ## =============
#' ## The example below is based on the usual approach involving creation of
#' ## a random CDISC dataset and then running the application.
#'
#' adsl <- tmc_ex_adsl
#' adtte <- tmc_ex_adtte
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
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADTTE", adtte)
#'   ),
#'   modules = modules(
#'     tm_t_coxreg(
#'       label = "Cox Reg.",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(c("ARM", "ARMCD", "ACTARMCD"), "ARM"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(adtte, "PARAMCD", "PARAM"), "OS"
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
#'
#' ## Second example
#' ## ==============
#' ## This time, a synthetic pair of ADTTE/ADSL is fabricated for a Cox regression
#' ## where ties and pval_method matter.
#'
#' ## Dataset fabrication
#' ## -------------------
#'
#' adtte <- data.frame(
#'   STUDYID = "LUNG",
#'   AVAL = c(4, 3, 1, 1, 2, 2, 3, 1, 2),
#'   CNSR = c(1, 1, 1, 0, 1, 1, 0, 0, 0),
#'   ARMCD = factor(
#'     c(0, 1, 1, 1, 1, 0, 0, 0, 0),
#'     labels = c("ARM A", "ARM B")
#'   ),
#'   SEX = factor(
#'     c(0, 0, 0, 0, 1, 1, 1, 1, 1),
#'     labels = c("F", "M")
#'   ),
#'   INST = factor(c("A", "A", "B", "B", "A", "B", "A", "B", "A")),
#'   stringsAsFactors = FALSE
#' )
#' adtte <- base::rbind(adtte, adtte, adtte, adtte)
#' adtte <- dplyr::as_tibble(adtte)
#' set.seed(1)
#' adtte$INST <- sample(adtte$INST)
#' adtte$AGE <- sample(seq(5, 75, 5), size = nrow(adtte), replace = TRUE)
#' adtte$USUBJID <- paste("sub", 1:nrow(adtte), adtte$INST, sep = "-")
#' adtte$PARAM <- adtte$PARAMCD <- "OS"
#' adsl <- subset(
#'   adtte,
#'   select = c("USUBJID", "STUDYID", "ARMCD", "SEX", "INST", "AGE")
#' )
#'
#' ## Teal application
#' ## ================
#' ## Note that the R code exported by `Show R Code` does not include the data
#' ## preprocessing. You will need to create the dataset as above before
#' ## running the exported R code.
#'
#' arm_ref_comp <- list(ARMCD = list(ref = "ARM A", comp = c("ARM B")))
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset(
#'       dataname = "ADSL",
#'       x = adsl
#'     ),
#'     cdisc_dataset(
#'       dataname = "ADTTE",
#'       x = adtte
#'     )
#'   ),
#'   modules = modules(
#'     tm_t_coxreg(
#'       label = "Cox Reg.",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(c("ARMCD"), "ARMCD"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(adtte, "PARAMCD", "PARAM"), "OS"
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
                        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_coxreg")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

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
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
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

  ns <- shiny::NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::radioButtons(
        ns("type"),
        label = shiny::tags$label("Type of Regression:", class = "text-primary"),
        choices = c(
          "Separate models for comparison groups with one covariate at a time" = "Univariate",
          "One model with all comparison groups and covariates" = "Multivariate"
        ),
        selected = dplyr::if_else(a$multivariate, "Multivariate", "Univariate")
      ),
      shiny::tags$label("Encodings", class = "text-primary"),
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
      shiny::uiOutput(ns("arms_buckets")),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'Multivariate'"),
        shiny::checkboxInput(
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
      shiny::conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'Univariate'"),
        shiny::checkboxInput(
          ns("interactions"),
          "Interaction terms"
        )
      ),
      shiny::uiOutput(ns("interaction_input")),
      teal.transform::data_extract_ui(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional table settings",
          shiny::conditionalPanel(
            condition = paste0("input['", ns("strata_var"), "'] != ''"),
            shiny::radioButtons(
              ns("pval_method"),
              label = shiny::p(
                "p-value method for",
                shiny::span(class = "text-primary", "Coxph"),
                "(Hazard Ratio)"
              ),
              choices = c("wald", "likelihood"),
              selected = "wald"
            )
          ),
          shiny::radioButtons(
            ns("ties"),
            label = shiny::p(
              "Ties for ",
              shiny::span(class = "text-primary", "Coxph"),
              " (Hazard Ratio)",
              sep = ""
            ),
            choices = c("exact", "breslow", "efron"),
            selected = "exact"
          ),
          teal.widgets::optionalSelectInput(
            inputId = ns("conf_level"),
            label = shiny::p(
              "Confidence level for ",
              shiny::span(class = "text-primary", "Coxph"),
              " (Hazard Ratio)",
              sep = ""
            ),
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          )
        )
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
srv_t_coxreg <- function(id,
                         data,
                         reporter,
                         filter_panel_api,
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
                         basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    # Observer to update reference and comparison arm input options.
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_coxreg"
    )

    use_interactions <- shiny::reactive({
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


    iv_r <- shiny::reactive({
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
      join_keys = get_join_keys(data),
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
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
      shiny::textInput(
        session$ns(paste0("interact_", x)),
        label = paste("Hazard Ratios for", x, "at (comma delimited):"),
        value = as.character(stats::median(dataset[[x]]))
      )
    }

    interaction_var_r <- shiny::reactive({
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

    output$interaction_input <- shiny::renderUI({
      if (length(interaction_var_r()) > 0) {
        lapply(interaction_var_r(), open_textinput, dataset = merged$anl_q()[[dataname]])
      }
    })

    ## Prepare the call evaluation environment ----
    validate_checks <- shiny::reactive({
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
      shiny::validate(shiny::need(
        all(anl_arm_n >= 2),
        "Each treatment group should have at least 2 records."
      ))

      # validate covariate has at least two levels
      shiny::validate(
        shiny::need(
          all(vapply(anl_filtered[input_cov_var], FUN = function(x) {
            length(unique(x)) > 1
          }, logical(1))),
          "All covariates needs to have at least two levels"
        )
      )

      NULL
    })

    at <- shiny::reactive({
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
          append = TRUE,
          basic_table_args = basic_table_args
        )
      }
    }

    ## generate table call with template and render table ----
    all_q <- shiny::reactive({
      validate_checks()

      ANL <- merged$anl_q()[["ANL"]] # nolint
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
        teal.code::eval_code(merged$anl_q(), as.expression(expr))
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

        merged$anl_q() %>%
          teal.code::eval_code(quote(result <- list())) %>%
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
                result <- lapply(result, function(x) {
                  rtables::col_info(x) <- rtables::col_info(result[[1]])
                  x
                })
                result <- rtables::rbindl_rtables(result, check_headers = TRUE)
                rtables::main_title(result) <- title
                rtables::main_footer(result) <- c(
                  paste("p-value method for Coxph (Hazard Ratio):", control$pval_method),
                  paste("Ties for Coxph (Hazard Ratio):", control$ties)
                )
                rtables::prov_footer(result) <- p_footer
                rtables::subtitles(result) <- subtitle
                result
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

    table_r <- shiny::reactive(all_q()[["result"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(all_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = "R Code for the Current (Multi-Variable) Cox proportional hazard regression model"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Cox Regression Table")
        card$append_text("Cox Regression Table", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(all_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
