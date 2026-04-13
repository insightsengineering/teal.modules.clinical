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

  if (!is.null(cov_var) && length(cov_var) > 0L) {
    data_pipe <- add_expr(
      data_pipe,
      substitute(
        expr = dplyr::mutate(across(where(is.factor) & cov_var, droplevels)),
        env = list(cov_var = cov_var)
      )
    )
  }

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

  if (!is.null(cov_var) && length(cov_var) > 0L) {
    data_pipe <- add_expr(
      data_pipe,
      substitute(
        expr = dplyr::mutate(across(where(is.factor) & cov_var, droplevels)),
        env = list(cov_var = cov_var)
      )
    )
  }

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
#'       arm_var = variables(
#'         choices = c("ARM", "ARMCD", "ACTARMCD"),
#'         selected = "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = variables(choices = "PARAMCD"),
#'       strata_var = variables(
#'         choices = c("COUNTRY", "STRATA1", "STRATA2"),
#'         selected = "STRATA1"
#'       ),
#'       cov_var = variables(
#'         choices = c("AGE", "BMRKR1", "BMRKR2", "REGION1"),
#'         selected = "AGE",
#'         multiple = TRUE,
#'         ordered = TRUE
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
#'       arm_var = variables(choices = "ARMCD"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = variables(choices = "PARAMCD"),
#'       strata_var = variables(choices = "INST", selected = NULL),
#'       cov_var = variables(
#'         choices = c("SEX", "AGE"),
#'         selected = "SEX",
#'         multiple = TRUE,
#'         ordered = TRUE
#'       ),
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
                        parentname = "ADSL",
                        arm_var,
                        arm_ref_comp = NULL,
                        paramcd,
                        cov_var,
                        strata_var,
                        aval_var = variables(choices = "AVAL"),
                        cnsr_var = variables(choices = "CNSR"),
                        multivariate = TRUE,
                        na_level = tern::default_na_str(),
                        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.widgets::basic_table_args(),
                        transformators = list(),
                        decorators = list()) {
  message("Initializing tm_t_coxreg")
  arm_var <- teal.picks::as.picks(arm_var, quiet = FALSE)
  paramcd <- teal.picks::as.picks(paramcd, quiet = FALSE)
  cov_var <- teal.picks::as.picks(cov_var, quiet = FALSE)
  strata_var <- teal.picks::as.picks(strata_var, quiet = FALSE)
  aval_var <- teal.picks::as.picks(aval_var, quiet = FALSE)
  cnsr_var <- teal.picks::as.picks(cnsr_var, quiet = FALSE)
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(paramcd, "variables")
  checkmate::assert_class(cov_var, "variables")
  checkmate::assert_class(strata_var, "variables")
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(cnsr_var, "variables")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  arm_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), arm_var)
  paramcd <- teal.picks::picks(teal.picks::datasets(dataname, dataname), paramcd, values())
  strata_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), strata_var)
  aval_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), aval_var)
  cnsr_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), cnsr_var)
  cov_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), cov_var)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_coxreg,
    ui = ui_t_coxreg,
    ui_args = args[names(args) %in% names(formals(ui_t_coxreg))],
    server_args = args[names(args) %in% names(formals(srv_t_coxreg))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}

#' @keywords internal
ui_t_coxreg <- function(id,
                        multivariate,
                        paramcd,
                        cnsr_var,
                        aval_var,
                        arm_var,
                        cov_var,
                        strata_var,
                        conf_level,
                        pre_output,
                        post_output,
                        decorators) {
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
        selected = dplyr::if_else(multivariate, "Multivariate", "Univariate")
      ),
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(tags$label("Select Endpoint"), teal.picks::picks_ui(ns("paramcd"), paramcd)),
      tags$div(tags$label("Censor Variable"), teal.picks::picks_ui(ns("cnsr_var"), cnsr_var)),
      tags$div(tags$label("Analysis Variable"), teal.picks::picks_ui(ns("aval_var"), aval_var)),
      tags$div(tags$label("Select Treatment Variable"), teal.picks::picks_ui(ns("arm_var"), arm_var)),
      uiOutput(ns("arms_buckets")),
      conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'Multivariate'"),
        checkboxInput(
          ns("combine_comp_arms"),
          "Combine all comparison groups?"
        )
      ),
      tags$div(tags$label("Covariates"), teal.picks::picks_ui(ns("cov_var"), cov_var)),
      conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'Univariate'"),
        checkboxInput(
          ns("interactions"),
          "Interaction terms"
        )
      ),
      uiOutput(ns("interaction_input")),
      tags$div(tags$label("Stratify by"), teal.picks::picks_ui(ns("strata_var"), strata_var)),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          conditionalPanel(
            condition = paste0(
              "typeof input['", ns("strata_var-variables-selected"), "'] !== 'undefined' && ",
              "input['", ns("strata_var-variables-selected"), "'] != null && ",
              "input['", ns("strata_var-variables-selected"), "'].length > 0"
            ),
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
            conf_level$choices,
            conf_level$selected,
            multiple = FALSE,
            fixed = conf_level$fixed
          )
        )
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table"))
    ),
    pre_output = pre_output,
    post_output = post_output
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

    use_interactions <- reactive({
      input$type == "Univariate" && isTRUE(input$interactions)
    })

    anl_selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        arm_var = arm_var,
        paramcd = paramcd,
        strata_var = strata_var,
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        cov_var = cov_var
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
      module = "tm_t_coxreg",
      arm_var_r = arm_var_r
    )

    merged_input_r <- reactive({
      list(
        columns_source = list(
          arm_var = anl_selectors$arm_var()$variables$selected,
          strata_var = anl_selectors$strata_var()$variables$selected,
          aval_var = anl_selectors$aval_var()$variables$selected,
          cnsr_var = anl_selectors$cnsr_var()$variables$selected,
          cov_var = anl_selectors$cov_var()$variables$selected,
          paramcd = anl_selectors$paramcd()$variables$selected
        )
      )
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
    merged_anl <- merge_srv("merge_anl", data = data_with_card, selectors = anl_selectors, output_name = "ANL")
    anl_q <- merged_anl$data

    numeric_level_validation <- function(val) {
      # need to explicitly evaluate 'val' here to ensure
      # the correct label is shown - if this is not done
      # then the last value of "val" is the label for all cases
      v <- val
      ~ if (anyNA(as_numeric_from_comma_sep_str(.))) {
        paste("Numeric interaction level(s) should be specified for", v)
      }
    }

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
        input_cov_var <- as.vector(merged_input_r()$columns_source$cov_var)
        dataset <- anl_q()[[dataname]]
        cov_is_numeric <- vapply(dataset[input_cov_var], is.numeric, logical(1))
        input_cov_var[cov_is_numeric]
      } else {
        NULL
      }
    })

    output$interaction_input <- renderUI({
      if (length(interaction_var_r()) > 0) {
        lapply(interaction_var_r(), open_textinput, dataset = anl_q()[[dataname]])
      }
    })

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(arm_ref_comp_buckets_validator())
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(0, 1, message_fmt = "Confidence level must be between 0 and 1")
      )
      iv$add_rule("pval_method", function(value) {
        if (length(anl_selectors$strata_var()$variables$selected) > 0L && !identical(value, "wald")) {
          return("Only Wald tests are supported for models with strata.")
        }
        NULL
      })

      for (val in interaction_var_r()) {
        iv$add_rule(
          paste0("interact_", val),
          shinyvalidate::sv_required(paste("Interaction level(s) should be specified for", val))
        )
        iv$add_rule(
          paste0("interact_", val), numeric_level_validation(val)
        )
      }
      iv$enable()
      iv
    })

    ## Prepare the call evaluation environment ----
    validate_checks <- reactive({
      arm_ref_comp_iv()
      teal::validate_inputs(iv_r())

      validate(
        need(length(anl_selectors$arm_var()$variables$selected) >= 1L, "A treatment variable is required"),
        need(length(anl_selectors$aval_var()$variables$selected) >= 1L, "An analysis variable is required"),
        need(length(anl_selectors$cnsr_var()$variables$selected) >= 1L, "A censor variable is required")
      )
      pc <- anl_selectors$paramcd()
      pc_vals <- if (is.null(pc$values)) character(0) else pc$values$selected
      validate(need(length(pc_vals) >= 1L, "An endpoint is required"))

      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- as.vector(merged_input_r()$columns_source$arm_var)
      input_strata_var <- as.vector(merged_input_r()$columns_source$strata_var)
      input_aval_var <- as.vector(merged_input_r()$columns_source$aval_var)
      input_cnsr_var <- as.vector(merged_input_r()$columns_source$cnsr_var)
      input_paramcd <- as.vector(anl_selectors$paramcd()$variables$selected)
      input_cov_var <- as.vector(merged_input_r()$columns_source$cov_var)

      validate(
        need(
          length(intersect(input_arm_var, input_strata_var)) == 0L,
          "`Treatment` and `Strata` variables should not overlap"
        ),
        need(
          length(intersect(input_arm_var, input_cov_var)) == 0L,
          "`Treatment` and `Covariate` variables should not overlap"
        ),
        need(
          length(intersect(input_strata_var, input_cov_var)) == 0L,
          "`Covariate` and `Strata` variables should not overlap"
        )
      )
      validate(
        need(
          !isTRUE(use_interactions()) || length(input_cov_var) > 0L,
          "If interactions are selected at least one covariate should be specified."
        )
      )

      cov_is_numeric <- if (length(input_cov_var) > 0L) {
        vapply(anl_filtered[input_cov_var], is.numeric, logical(1))
      } else {
        logical(0)
      }
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
      if (length(input_cov_var) > 0L) {
        validate(
          need(
            all(vapply(anl_filtered[input_cov_var], FUN = function(x) {
              length(unique(x)) > 1
            }, logical(1))),
            "All covariates needs to have at least two levels"
          )
        )
      }

      NULL
    })

    at <- reactive({
      input_cov_var <- as.vector(merged_input_r()$columns_source$cov_var)
      cov_is_numeric <- vapply(anl_q()[[dataname]][input_cov_var], is.numeric, logical(1))
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

      ANL <- anl_q()[["ANL"]]
      paramcd_col <- as.vector(anl_selectors$paramcd()$variables$selected)[[1]]
      paramcd_vals <- as.character(unique(ANL[[paramcd_col]]))
      multivariate <- input$type == "Multivariate"
      strata_var <- as.vector(merged_input_r()$columns_source$strata_var)

      if (input$type == "Multivariate") {
        main_title <- paste("Multi-Variable Cox Regression for", paramcd_vals)
        subtitle <- ifelse(length(strata_var) == 0, "", paste("Stratified by", paste(strata_var, collapse = " and ")))
        all_basic_table_args <- teal.widgets::resolve_basic_table_args(
          user_table = basic_table_args,
          module_table = teal.widgets::basic_table_args(
            title = main_title,
            subtitles = subtitle
          )
        )
        expr <- call_template(
          unlist(input$buckets$Comp), merged_input_r(),
          paramcd_vals, multivariate, all_basic_table_args
        )
        obj <- anl_q()
        teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
        teal.code::eval_code(obj, as.expression(expr))
      } else {
        main_title <- paste("Cox Regression for", paramcd_vals)
        subtitle <- ifelse(length(strata_var) == 0, "", paste("Stratified by", paste(strata_var, collapse = " and ")))
        all_basic_table_args <- teal.widgets::resolve_basic_table_args(
          user_table = basic_table_args,
          module_table = teal.widgets::basic_table_args(
            title = main_title,
            subtitles = subtitle
          )
        )

        obj <- anl_q()
        teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
        obj %>%
          teal.code::eval_code(quote(table <- list())) %>%
          teal.code::eval_code(
            as.expression(unlist(lapply(
              unlist(input$buckets$Comp),
              function(x) {
                call_template(x, merged_input_r(), paramcd_vals, multivariate, all_basic_table_args)
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


    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
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
