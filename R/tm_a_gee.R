#' Template for Generalized Estimating Equations (GEE) analysis module
#'
#' Creates a valid expression to generate an analysis table using Generalized Estimating Equations (GEE).
#'
#' @inheritParams template_arguments
#' @param output_table (`character`)\cr type of output table (`"t_gee_cov", "t_gee_coef", "t_gee_lsmeans"`).
#' @param data_model_fit (`character`)\cr dataset used to fit the model by `tern.gee::fit_gee()`.
#' @param dataname_lsmeans (`character`)\cr dataset used for `alt_counts_df` argument of `rtables::build_table()`.
#' @param split_covariates (`character`)\cr vector of names of variables to use as covariates in
#'   `tern.gee::vars_gee()`.
#' @param cor_struct (`character`)\cr assumed correlation structure in `tern.gee::fit_gee`.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_a_gee()]
#'
#' @keywords internal
template_a_gee <- function(output_table,
                           data_model_fit = "ANL",
                           dataname_lsmeans = "ANL_ADSL",
                           input_arm_var = "ARM",
                           ref_group = "A: Drug X",
                           aval_var,
                           id_var,
                           arm_var,
                           visit_var,
                           split_covariates,
                           cor_struct,
                           conf_level = 0.95,
                           basic_table_args = teal.widgets::basic_table_args()) {
  y <- list()
  y$model <- list()
  y$table <- list()

  all_basic_table_args <- teal.widgets::resolve_basic_table_args(basic_table_args)

  model_list <- add_expr(
    list(),
    substitute(
      expr = {
        model_fit <- tern.gee::fit_gee(
          vars = tern.gee::vars_gee(
            response = as.vector(aval_var),
            covariates = as.vector(split_covariates),
            id = as.vector(id_var),
            arm = as.vector(arm_var),
            visit = as.vector(visit_var)
          ),
          data = data_model_fit,
          regression = "logistic",
          cor_struct = cor_struct
        )
      },
      env = list(
        data_model_fit = as.name(data_model_fit),
        aval_var = aval_var,
        split_covariates = split_covariates,
        id_var = id_var,
        arm_var = arm_var,
        visit_var = visit_var,
        cor_struct = cor_struct
      )
    )
  )

  table_list <-
    add_expr(
      list(),
      if (output_table == "t_gee_cov") {
        substitute(
          expr = {
            table <- tern.gee::as.rtable(model_fit, type = "cov")
            rtables::subtitles(table) <- st
            rtables::main_footer(table) <- mf
          },
          env = list(
            st = basic_table_args$subtitles,
            mf = basic_table_args$main_footer
          )
        )
      } else if (output_table == "t_gee_coef") {
        substitute(
          expr = {
            table <- tern.gee::as.rtable(data.frame(Coefficient = model_fit$coefficients))
            rtables::subtitles(table) <- st
            rtables::main_footer(table) <- mf
          },
          env = list(
            conf_level = conf_level,
            st = basic_table_args$subtitles,
            mf = basic_table_args$main_footer
          )
        )
      } else if (output_table == "t_gee_lsmeans") {
        substitute(
          expr = {
            lsmeans_fit_model <- tern.gee::lsmeans(model_fit, conf_level)
            table <- rtables::basic_table(show_colcounts = TRUE) %>%
              rtables::split_cols_by(var = input_arm_var, ref_group = model_fit$ref_level) %>%
              tern.gee::summarize_gee_logistic() %>%
              rtables::build_table(
                df = lsmeans_fit_model,
                alt_counts_df = dataname_lsmeans
              )

            rtables::subtitles(table) <- st
            rtables::main_footer(table) <- mf
          },
          env = list(
            dataname_lsmeans = as.name(dataname_lsmeans),
            conf_level = conf_level,
            input_arm_var = input_arm_var,
            st = basic_table_args$subtitles,
            mf = basic_table_args$main_footer
          )
        )
      }
    )
  # Note: l_html_concomitant_adcm is still not included since one column is available out of 9

  y$model <- bracket_expr(model_list)
  y$table <- bracket_expr(table_list)

  y
}

#' teal Module: Generalized Estimating Equations (GEE) analysis
#'
#' This module produces an analysis table using Generalized Estimating Equations (GEE).
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_arguments
#' @inheritParams template_a_gee
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`ElementaryTable` - output of `rtables::build_table()`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_a_gee(
#'    ..., # arguments for module
#'    decorators = list(
#'      table = teal_transform_module(...) # applied to the `table` output
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
#'   library(teal.modules.clinical)
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl
#'   ADQS <- tmc_ex_adqs %>%
#'     filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
#'     mutate(
#'       AVISIT = as.factor(AVISIT),
#'       AVISITN = rank(AVISITN) %>%
#'         as.factor() %>%
#'         as.numeric() %>%
#'         as.factor(),
#'       AVALBIN = AVAL < 50 # Just as an example to get a binary endpoint.
#'     ) %>%
#'     droplevels()
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_a_gee(
#'       label = "GEE",
#'       dataname = "ADQS",
#'       aval_var = teal.picks::variables(choices = "AVALBIN"),
#'       id_var = teal.picks::variables(choices = c("USUBJID", "SUBJID"), selected = "USUBJID"),
#'       arm_var = teal.picks::variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
#'       visit_var = teal.picks::variables(choices = c("AVISIT", "AVISITN"), selected = "AVISIT"),
#'       paramcd = teal.picks::variables(choices = c("PARAMCD", "PARAM")),
#'       cov_var = teal.picks::variables(
#'         choices = c("BASE", "AGE", "SEX", "BASE:AVISIT"),
#'         selected = NULL
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_a_gee <- function(label,
                     dataname,
                     parentname = "ADSL",
                     aval_var = teal.picks::variables(choices = "AVALBIN"),
                     id_var = teal.picks::variables(choices = c("USUBJID", "SUBJID")),
                     arm_var = teal.picks::variables(choices = c("ARM", "ARMCD")),
                     visit_var = teal.picks::variables(choices = c("AVISIT", "AVISITN")),
                     cov_var = teal.picks::variables(choices = c("BASE", "AGE", "SEX", "BASE:AVISIT")),
                     arm_ref_comp = NULL,
                     paramcd = teal.picks::variables(choices = c("PARAMCD", "PARAM")),
                     conf_level = teal.picks::values(c(0.95, 0.9, 0.8), 0.95, multiple = FALSE),
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.widgets::basic_table_args(),
                     transformators = list(),
                     decorators = list()) {
  message("Initializing tm_a_gee (prototype)")

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  aval_var <- deprecate_pick_variables_arg(aval_var, "aval_var")
  id_var <- deprecate_pick_variables_arg(id_var, "id_var")
  arm_var <- deprecate_pick_variables_arg(arm_var, "arm_var")
  visit_var <- deprecate_pick_variables_arg(visit_var, "visit_var")
  cov_var <- deprecate_pick_variables_arg(cov_var, "cov_var")
  paramcd <- deprecate_pick_variables_arg(paramcd, "paramcd")
  conf_level <- deprecate_pick_values_arg(conf_level, "conf_level")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  aval_var <- teal.picks::picks(teal.picks::datasets(dataname), aval_var)
  id_var <- teal.picks::picks(teal.picks::datasets(dataname), id_var)
  arm_var <- teal.picks::picks(teal.picks::datasets(parentname), arm_var)
  visit_var <- teal.picks::picks(teal.picks::datasets(dataname), visit_var)
  split_covariates <- teal.picks::picks(
    teal.picks::datasets(dataname),
    split_choices_variables(cov_var)
  )
  cov_var <- teal.picks::picks(teal.picks::datasets(dataname), cov_var)
  paramcd <- teal.picks::picks(
    teal.picks::datasets(dataname),
    paramcd,
    values(all_values, first_value)
  )

  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_gee,
    ui = ui_gee,
    ui_args = args[names(args) %in% names(formals(ui_gee))],
    server_args = args[names(args) %in% names(formals(srv_gee))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}

ui_gee <- function(id,
                   arm_var,
                   aval_var,
                   id_var,
                   visit_var,
                   cov_var,
                   split_covariates,
                   paramcd,
                   conf_level,
                   pre_output,
                   post_output,
                   decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tags$h3(textOutput(ns("gee_title"))),
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(tags$label("Analysis Variable"), teal.picks::picks_ui(ns("aval_var"), aval_var)),
      tags$div(tags$label("Select Endpoint"), teal.picks::picks_ui(ns("paramcd"), paramcd)),
      tags$div(tags$label("Visit Variable"), teal.picks::picks_ui(ns("visit_var"), visit_var)),
      tags$div(tags$label("Covariates"), teal.picks::picks_ui(ns("cov_var"), cov_var)),
      shinyjs::hidden(
        tags$div(tags$label("Split Covariates"), teal.picks::picks_ui(ns("split_covariates"), split_covariates))
      ),
      tags$div(tags$label("Select Treatment Variable"), teal.picks::picks_ui(ns("arm_var"), arm_var)),
      shinyjs::hidden(
        uiOutput(ns("arms_buckets")),
        helpText(
          id = ns("help_text"), "Multiple reference groups are automatically combined into a single group."
        ),
        checkboxInput(
          ns("combine_comp_arms"),
          "Combine all comparison groups?",
          value = FALSE
        )
      ),
      tags$div(tags$label("Subject Identifier"), teal.picks::picks_ui(ns("id_var"), id_var)),
      selectInput(
        ns("cor_struct"),
        "Correlation Structure",
        choices = c(
          "unstructured",
          "toeplitz", # needs the fix of https://github.com/insightsengineering/tern.gee/issues/3
          "auto-regressive",
          "compound symmetry"
        ),
        selected = "unstructured",
        multiple = FALSE
      ),
      teal.widgets::optionalSelectInput(
        ns("conf_level"),
        "Confidence Level",
        conf_level$choices,
        conf_level$selected,
        multiple = FALSE,
        fixed = teal.picks::is_pick_fixed(conf_level)
      ),
      radioButtons(
        ns("output_table"),
        "Output Type",
        choices = c(
          "LS means" = "t_gee_lsmeans",
          "Covariance" = "t_gee_cov",
          "Coefficients" = "t_gee_coef"
        ),
        selected = "t_gee_lsmeans"
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table"))
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_gee <- function(id,
                    data,
                    dataname,
                    parentname,
                    arm_var,
                    paramcd,
                    id_var,
                    visit_var,
                    cov_var,
                    split_covariates,
                    aval_var,
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
        paramcd = paramcd,
        id_var = id_var,
        visit_var = visit_var,
        cov_var = cov_var,
        split_covariates = split_covariates,
        aval_var = aval_var
      ),
      data = data
    )

    ## split_covariates ----
    # Derive split_covariates reactively from cov_var, excluding arm_var if present
    split_covariates_r <- reactive({
      cov_selected <- anl_selectors$cov_var()$variables$selected
      arm_selected <- anl_selectors$arm_var()$variables$selected
      split_vals <- split_interactions(cov_selected)
      if (length(intersect(split_vals, arm_selected)) >= 1L) {
        setdiff(split_vals, arm_selected)
      } else {
        split_vals
      }
    })

    arm_var_r <- reactive(anl_selectors$arm_var()$variables$selected)

    ## arm_ref_comp_observer ----
    arm_ref_comp_observer_picks(
      session,
      input,
      output,
      id_arm_var = "arm_var-variables-selected",
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_a_gee",
      on_off = reactive(TRUE),
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

    # Create ANL_ADSL for use as alt_counts_df in rtables::build_table()
    anl_q_with_adsl <- reactive({
      teal.code::eval_code(
        anl_q(),
        bquote(ANL_ADSL <- .(as.name(parentname)))
      )
    })

    shinyjs::show("gee_title")

    validate_checks <- reactive({
      validate(
        need(
          length(anl_selectors$arm_var()$variables$selected) >= 1L,
          "A treatment variable is required"
        ),
        need(
          length(anl_selectors$aval_var()$variables$selected) >= 1L,
          "An analysis variable is required"
        ),
        need(
          length(anl_selectors$id_var()$variables$selected) >= 1L,
          "A subject identifier is required"
        ),
        need(
          length(anl_selectors$visit_var()$variables$selected) >= 1L,
          "A visit variable is required"
        ),
        need(
          length(anl_selectors$paramcd()$variables$selected) >= 1L,
          "An endpoint is required"
        ),
        need(
          !is.null(input$conf_level) && nzchar(input$conf_level),
          "Please choose a confidence level"
        ),
        need(
          is.na(suppressWarnings(as.numeric(input$conf_level))) ||
            (as.numeric(input$conf_level) > 0 && as.numeric(input$conf_level) < 1),
          "Confidence level must be between 0 and 1"
        ),
        need(
          !is.null(input$cor_struct) && nzchar(input$cor_struct),
          "Please choose a correlation structure"
        )
      )
      NULL
    })

    ## table_r ----
    table_q <- reactive({
      validate_checks()
      output_table <- input$output_table
      conf_level <- as.numeric(input$conf_level)

      req(output_table)

      input_aval_var <- as.vector(anl_selectors$aval_var()$variables$selected)
      input_arm_var <- as.vector(anl_selectors$arm_var()$variables$selected)
      input_id_var <- as.vector(anl_selectors$id_var()$variables$selected)
      input_visit_var <- as.vector(anl_selectors$visit_var()$variables$selected)
      input_paramcd <- as.vector(anl_selectors$paramcd()$variables$selected)
      input_values_paramcd <- as.vector(anl_selectors$paramcd()$values$selected)
      input_split_covariates <- split_covariates_r()

      basic_table_args$subtitles <- paste0(
        "Analysis Variable: ", input_aval_var,
        ",  Endpoint:", paste(input_paramcd, input_values_paramcd, collapse = ", "),
        ifelse(length(input_split_covariates) == 0, "",
          paste(",  Covariates:", paste(input_split_covariates, collapse = ", "))
        )
      )
      basic_table_args$main_footer <- c(paste("Correlation Structure:", input$cor_struct))

      my_calls <- template_a_gee(
        output_table = output_table,
        data_model_fit = "ANL",
        dataname_lsmeans = "ANL_ADSL",
        input_arm_var = input_arm_var,
        conf_level = conf_level,
        aval_var = input_aval_var,
        split_covariates = input_split_covariates,
        id_var = input_id_var,
        arm_var = input_arm_var,
        visit_var = input_visit_var,
        cor_struct = input$cor_struct,
        basic_table_args = basic_table_args
      )

      table_type <- switch(input$output_table,
        "t_gee_cov" = "Residual Covariance Matrix Estimate",
        "t_gee_coef" = "Model Coefficients",
        "t_gee_lsmeans" = "LS Means Estimates"
      )

      obj <- anl_q_with_adsl()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), paste("### ", table_type, "Table"))
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    output$gee_title <- renderText({
      output_title <- switch(input$output_table,
        "t_gee_cov" = "Residual Covariance Matrix Estimate",
        "t_gee_coef" = "Model Coefficients",
        "t_gee_lsmeans" = "LS Means Estimates"
      )
      output_title
    })

    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = table_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    # Outputs to render.
    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
