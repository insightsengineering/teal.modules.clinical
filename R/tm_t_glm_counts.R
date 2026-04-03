#' Teal Module: Regression Counts Summary
#'
#' Summarize results of a Poisson negative binomial regression that is result
#' of a generalized linear model of one (e.g. arm) or more covariates.
#'
#' @inheritParams template_arguments
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @param arm_var ([teal.picks::variables()])\cr treatment variable (`parentname`).
#' @param aval_var ([teal.picks::variables()])\cr analysis variable (`dataname`).
#' @param strata_var ([teal.picks::variables()])\cr optional stratification columns (`dataname`).
#' @param offset_var ([teal.picks::variables()])\cr optional offset column (`dataname`).
#' @param cov_var ([teal.picks::variables()])\cr optional covariate columns (`dataname`).
#' @param conf_level ([teal.transform::choices_selected()])\cr object with all available choices and
#'   pre-selected option for confidence level, each within range of (0, 1).
#' @param add_total (`logical`)\cr initial value for the \dQuote{Add All Patients column} checkbox when comparing arms.
#' @param rate_mean_method (`character`) method used to estimate the mean odds ratio. Either "emmeans" or "ppmeans"
#' (as in `summarize_glm_count()`).
#' @param distribution (`character`) value specifying the distribution used in the regression model
#' (Poisson: `"poisson"`,  Quasi-Poisson: `"quasipoisson"`, negative binomial: `"negbin"`).
#' @param offset_var (`character`) a name of the numeric variable to be used as an offset?
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
#' tm_t_glm_counts(
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
#' @details
#' * Teal module for [tern::summarize_glm_count()] analysis, that summarizes results of a
#' Poisson negative binomial regression.
#' * The arm and stratification variables are taken from the `parentname` data.
#' @seealso `summarize_glm_count()`
#' @inherit module_arguments return seealso
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' data <- within(teal_data(), {
#'   ADSL <- tern::tern_ex_adsl
#'   ADTTE <- tern::tern_ex_adtte
#' })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
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
#' ADSL <- data[["ADSL"]]
#' ADTTE <- data[["ADTTE"]]
#' # Initialize the teal app
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_glm_counts(
#'       dataname = "ADTTE",
#'       arm_var = variables(
#'         choices = c("ARM", "ARMCD", "ACTARMCD"),
#'         selected = "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       aval_var = variables(choices = "AVAL"),
#'       strata_var = variables(choices = "SEX", selected = NULL),
#'       offset_var = variables(choices = "AGE", selected = NULL),
#'       cov_var = variables(choices = "SITEID", selected = NULL)
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
tm_t_glm_counts <- function(label = "Counts Module",
                            dataname,
                            parentname = "ADSL",
                            aval_var = variables(choices = "AVAL"),
                            arm_var,
                            strata_var,
                            rate_mean_method = c("emmeans", "ppmeans"),
                            distribution = c("negbin", "quasipoisson", "poisson"),
                            offset_var,
                            cov_var,
                            arm_ref_comp = NULL,
                            conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                            add_total = FALSE,
                            pre_output = NULL,
                            post_output = NULL,
                            basic_table_args = teal.widgets::basic_table_args(),
                            transformators = list(),
                            decorators = list()) {
  message("Initializing tm_t_glm_counts")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  distribution_choices <- c("negbin", "quasipoisson", "poisson")
  rate_mean_method_choices <- c("emmeans", "ppmeans")
  rate_mean_method <- match.arg(rate_mean_method, rate_mean_method_choices)
  distribution <- match.arg(distribution, distribution_choices)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(strata_var, "variables")
  checkmate::assert_class(offset_var, "variables")
  checkmate::assert_class(cov_var, "variables")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  arm_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), arm_var)
  aval_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), aval_var)
  cov_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), cov_var)
  offset_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), offset_var)
  strata_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), strata_var)

  args <- as.list(environment())

  teal::module(
    label = label,
    ui = ui_t_glm_counts,
    server = srv_t_glm_counts,
    ui_args = args[names(args) %in% names(formals(ui_t_glm_counts))],
    server_args = args[names(args) %in% names(formals(srv_t_glm_counts))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}


ui_t_glm_counts <- function(id,
                            arm_var,
                            aval_var,
                            cov_var,
                            offset_var,
                            strata_var,
                            distribution_choices,
                            distribution,
                            rate_mean_method_choices,
                            rate_mean_method,
                            conf_level,
                            add_total,
                            pre_output,
                            post_output,
                            decorators) {
  ns <- NS(id)
  output <- teal.widgets::white_small_well(
    teal.widgets::table_with_settings_ui(ns("table"))
  )

  compare_treatments <- tags$div(
    class = "arm-comp-box",
    tags$label("Compare Treatments"),
    bslib::input_switch(
      id = ns("compare_arms"),
      label = "Compare Treatments",
      value = TRUE
    ),
    conditionalPanel(
      condition = paste0("input['", ns("compare_arms"), "']"),
      tags$div(
        uiOutput(ns("arms_buckets")),
        uiOutput(ns("helptext_ui")),
        checkboxInput(
          ns("combine_comp_arms"),
          "Combine all comparison groups?",
          value = FALSE
        ),
        checkboxInput(ns("add_total"), "Add All Patients column", value = add_total)
      )
    )
  )

  table_settings <- shiny::tagList(
    teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table")),
    bslib::accordion(
      bslib::accordion_panel(
        "Additional table settings",
        teal.widgets::optionalSelectInput(
          inputId = ns("conf_level"),
          label = "Confidence Level",
          choices = conf_level$choices,
          selected = conf_level$selected,
          multiple = FALSE,
          fixed = conf_level$fixed
        ),
      )
    )
  )

  teal.widgets::standard_layout(
    output = output,
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(tags$label("Select Treatment Variable"), teal.picks::picks_ui(ns("arm_var"), arm_var)),
      tags$div(tags$label("Analysis Variable"), teal.picks::picks_ui(ns("aval_var"), aval_var)),
      tags$div(tags$label("Covariate(s)"), teal.picks::picks_ui(ns("cov_var"), cov_var)),
      tags$div(tags$label("Offset variable"), teal.picks::picks_ui(ns("offset_var"), offset_var)),
      tags$div(tags$label("Stratify by"), teal.picks::picks_ui(ns("strata_var"), strata_var)),
      compare_treatments,
      shiny::selectInput(
        ns("distribution"),
        "Distribution",
        choices = distribution_choices,
        selected = distribution
      ),
      shiny::selectInput(
        ns("rate_mean_method"),
        "Rate method",
        choices = rate_mean_method_choices,
        selected = rate_mean_method
      ),
      table_settings
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_t_glm_counts <- function(id,
                             data,
                             dataname,
                             parentname,
                             arm_var,
                             aval_var,
                             offset_var,
                             cov_var,
                             strata_var,
                             arm_ref_comp,
                             label,
                             basic_table_args,
                             decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    anl_selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        arm_var = arm_var,
        aval_var = aval_var,
        strata_var = strata_var,
        cov_var = cov_var,
        offset_var = offset_var
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
      module = "tm_t_counts",
      on_off = reactive(input$compare_arms),
      arm_var_r = arm_var_r
    )

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

    output$helptext_ui <- renderUI({
      req(length(anl_selectors$arm_var()$variables$selected) > 0L)
      helpText("Multiple reference groups are automatically combined into a single group.")
    })

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      if (isTRUE(input$compare_arms)) {
        iv$add_validator(arm_ref_comp_buckets_validator())
      }
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(
          0, 1,
          message_fmt = "Confidence level must be between 0 and 1"
        )
      )
      iv$enable()
      iv
    })

    validate_checks <- reactive({
      if (isTRUE(input$compare_arms)) {
        arm_ref_comp_iv()
      }
      teal::validate_inputs(iv_r())
      validate(
        need(
          length(anl_selectors$arm_var()$variables$selected) >= 1L,
          "A treatment variable is required"
        ),
        need(
          length(anl_selectors$aval_var()$variables$selected) >= 1L,
          "An analysis variable is required"
        )
      )

      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      input_arm_var <- as.vector(anl_selectors$arm_var()$variables$selected)
      input_strata_var <- as.vector(anl_selectors$strata_var()$variables$selected)
      input_aval_var <- as.vector(anl_selectors$aval_var()$variables$selected)

      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_aval_var),
        arm_var = input_arm_var
      )

      if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }
      if (isTRUE(input$compare_arms)) {
        validate_args <- append(
          validate_args,
          list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
        )
      }

      do.call(what = "validate_standard_inputs", validate_args)

      validate(shiny::need(
        !all(is.na(anl[[input_aval_var]])),
        "ANCOVA table cannot be calculated as all values are missing."
      ))

      NULL
    })

    anl <- reactive({
      within(req(anl_q()), {
        ANL <- tern::df_explicit_na(ANL)
      })
    })

    basic_table <- reactive({
      validate_checks()
      req(!is.null(input$buckets$Ref))
      req(anl_q())
      arm_col <- as.vector(anl_selectors$arm_var()$variables$selected)
      within(req(anl()),
        {
          lyt <- rtables::basic_table(show_colcounts = TRUE) %>%
            rtables::split_cols_by(var, ref_group = ref_group, split_fun = tern::ref_group_position("first"))
        },
        ref_group = unlist(input$buckets$Ref),
        var = arm_col
      )
    })

    summarize_counts <- reactive({
      validate_checks()
      offset_var <- as.vector(anl_selectors$offset_var()$variables$selected)
      cov_var <- as.vector(anl_selectors$cov_var()$variables$selected)
      arm_var <- as.vector(anl_selectors$arm_var()$variables$selected)
      variables <- if (length(offset_var) && length(cov_var)) {
        within(req(basic_table()),
          {
            variables <- list(arm = var, covariates = cov_var, offset_var = offset_var)
          },
          var = arm_var,
          cov_var = cov_var,
          offset_var = offset_var
        )
      } else if (!length(offset_var) && length(cov_var)) {
        within(req(basic_table()),
          {
            variables <- list(arm = var, covariates = cov_var)
          },
          var = arm_var,
          cov_var = cov_var
        )
      } else if (length(offset_var) && !length(cov_var)) {
        within(req(basic_table()),
          {
            variables <- list(arm = var, offset_var = offset_var)
          },
          var = arm_var,
          offset_var = offset_var
        )
      } else {
        within(req(basic_table()),
          {
            variables <- list(arm = var)
          },
          var = arm_var
        )
      }
      w <- within(variables,
        {
          lyt <- tern::summarize_glm_count(
            lyt,
            vars = var,
            variables = variables,
            conf_level = conf_level,
            distribution = distribution,
            rate_mean_method = rate_mean_method,
            var_labels = "Adjusted (NB) exacerbation rate (per year)",
            table_names = "adj-nb",
            .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
            .labels = c(
              rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
              rate_ratio_ci = "Rate Ratio CI", pval = "p-value"
            )
          )
        },
        rate_mean_method = input$rate_mean_method,
        var = as.vector(anl_selectors$aval_var()$variables$selected),
        conf_level = as.numeric(input$conf_level),
        distribution = input$distribution
      )
    })

    # Create output table
    table_out <- reactive({
      req(summarize_counts())
      table <- within(req(summarize_counts()), {
        table <- rtables::build_table(
          lyt = lyt,
          df = ANL
        )
      })

      validate(need(!inherits(table, "try-error"), "Model couldn't be fitted."))
      table
    })

    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = table_out,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(id = "table", table_r = table_r)

    decorated_table_q
  })
}
