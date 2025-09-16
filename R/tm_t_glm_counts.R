#' Teal Module: Regression Counts Summary
#'
#' Summarize results of a Poisson negative binomial regression that is result
#' of a generalized linear model of one (e.g. arm) or more covariates.
#'
#' @inheritParams template_arguments
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @param conf_level ([teal.transform::choices_selected()])\cr object with all available choices and
#'   pre-selected option for confidence level, each within range of (0, 1).
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
#'       arm_var = choices_selected(
#'         variable_choices(ADTTE, c("ARM", "ARMCD", "ACTARMCD")),
#'         "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       aval_var = choices_selected(
#'         variable_choices(ADTTE, "AVAL"),
#'         "AVAL"
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, "SEX"),
#'         NULL
#'       ),
#'       offset_var = choices_selected(
#'         variable_choices(ADSL, "AGE"),
#'         NULL
#'       ),
#'       cov_var = choices_selected(
#'         variable_choices(ADTTE, "SITEID"),
#'         NULL
#'       )
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(ui = app$ui, server = app$server)
#' }
#' @export
tm_t_glm_counts <- function(label = "Counts Module",
                            dataname,
                            parentname = ifelse(
                              inherits(arm_var, "data_extract_spec"),
                              teal.transform::datanames_input(arm_var),
                              "ADSL"
                            ),
                            aval_var = teal.transform::choices_selected(
                              teal.transform::variable_choices(dataname, "AVAL"), "AVAL",
                              fixed = TRUE
                            ),
                            arm_var,
                            strata_var,
                            rate_mean_method = c("emmeans", "ppmeans"),
                            distribution = c("negbin", "quasipoisson", "poisson"),
                            offset_var,
                            cov_var,
                            arm_ref_comp = NULL,
                            conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                            pre_output = NULL,
                            post_output = NULL,
                            basic_table_args = teal.widgets::basic_table_args(),
                            transformators = list(),
                            decorators = list()) {
  message("Initializing tm_t_glm_counts")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  rate_mean_method <- match.arg(rate_mean_method)
  distribution <- match.arg(distribution)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = dataname),
    offset_var = cs_to_des_select(offset_var, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = dataname)
  )

  teal::module(
    label = label,
    ui = ui_t_glm_counts,
    server = srv_t_glm_counts,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}


ui_t_glm_counts <- function(id, ...) {
  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$offset_var,
    a$cov_var,
    a$aval_var
  )
  output <- teal.widgets::white_small_well(
    teal.widgets::table_with_settings_ui(ns("table"))
  )
  forms <- tagList(
    teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
  )

  compare_treatments <- tags$div(
    class = "arm-comp-box",
    tags$label("Compare Treatments"),
    bslib::input_switch(
      id = ns("compare_arms"),
      label = "Compare Treatments",
      value = !is.null(a$arm_var)
    ),
    conditionalPanel(
      condition = paste0("input['", ns("compare_arms"), "']"),
      tags$div(
        uiOutput(ns("arms_buckets")), # from arm_ref_comp_observer
        uiOutput(ns("helptext_ui")), # For feedback on comparisons
        checkboxInput(
          ns("combine_comp_arms"),
          "Combine all comparison groups?",
          value = FALSE
        ),
        teal.transform::data_extract_ui(
          id = ns("strata_var"),
          label = "Stratify by",
          data_extract_spec = a$strata_var,
          is_single_dataset = is_single_dataset_value
        ),
        checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total)
      )
    )
  )

  table_settings <- bslib::accordion_panel(
    "Additional table settings",
    teal.widgets::optionalSelectInput(
      inputId = ns("conf_level"),
      label = "Confidence Level",
      choices = c(0.8, 0.9, 0.95),
      selected = 0.95,
      multiple = FALSE,
      fixed = FALSE
    ),
    ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
  )

  teal.widgets::standard_layout(
    output = output,
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::data_extract_ui(
        ns("arm_var"),
        "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        ns("cov_var"),
        "Covariate(s)",
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        ns("offset_var"),
        "Offset variable",
        data_extract_spec = a$offset_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      compare_treatments,
      shiny::selectInput(
        ns("distribution"),
        "Distribution",
        choices = a$distribution
      ),
      shiny::selectInput(
        ns("rate_mean_method"),
        "Rate method",
        choices = a$rate_mean_method
      ),
      table_settings,
    ),
    forms = forms,
    pre_output = a$pre_output,
    post_output = a$post_output
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

    # Input validation
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_counts",
      on_off = reactive(input$compare_arms)
    )

    list_data_extract <- list(
      arm_var = arm_var,
      aval_var = aval_var,
      strata_var = strata_var,
      cov_var = cov_var,
      offset_var = offset_var
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list_data_extract,
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required")
      )
    )

    ## Data source merging
    adsl_merge_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list_data_extract,
      anl_name = "ANL"
    )

    output$helptext_ui <- renderUI({
      req(selector_list()$arm_var()$select)
      helpText("Multiple reference groups are automatically combined into a single group.")
    })

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()

      if (isTRUE(input$compare_arms)) {
        iv$add_validator(iv_arm_ref)
      }

      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level", shinyvalidate::sv_between(
          0, 1,
          message_fmt = "Confidence level must be between 0 and 1"
        )
      )
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    ## Merge data
    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Time To Count Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(as.expression(adsl_merge_inputs()$expr))
    })

    validate_checks <- reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      anl_m <- adsl_merge_inputs()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_aval_var # , input_paramcd
        ),
        arm_var = input_arm_var
      )

      # validate arm levels
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

      # check that there is at least one record with no missing data
      validate(shiny::need(
        !all(is.na(anl[[input_aval_var]])),
        "ANCOVA table cannot be calculated as all values are missing."
      ))

      NULL
    })

    ##  Preprocessing the data: user specified
    anl <- reactive({
      within(req(anl_q()), {
        ANL <- tern::df_explicit_na(ANL)
      })
    })
    ## Add basic specification for the table
    basic_table <- reactive({
      req(!is.null(input$buckets$Ref))
      ami <- req(adsl_merge_inputs())
      within(req(anl()),
        {
          lyt <- rtables::basic_table(show_colcounts = TRUE) %>%
            rtables::split_cols_by(var, ref_group = ref_group, split_fun = tern::ref_group_position("first"))
        },
        ref_group = unlist(input$buckets$Ref),
        var = as.vector(ami$columns_source$arm_var)
      )
    })

    ## Create covariates for the table
    ## Create tables
    summarize_counts <- reactive({
      ami <- req(adsl_merge_inputs())
      offset_var <- as.vector(ami$columns_source$offset_var)
      cov_var <- as.vector(ami$columns_source$cov_var)
      arm_var <- as.vector(ami$columns_source$arm_var)
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
        var = as.vector(ami$columns_source$aval_var),
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

    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = table_out,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(id = "table", table_r = table_r)

    # Render R code
    source_code_r <- reactive(teal.code::get_code(req(decorated_table_q())))
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = label
    )

    decorated_table_q
  })
}
