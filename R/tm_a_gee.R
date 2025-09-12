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
#'       aval_var = choices_selected("AVALBIN", fixed = TRUE),
#'       id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       visit_var = choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
#'       paramcd = choices_selected(
#'         choices = value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
#'         selected = "FKSI-FWB"
#'       ),
#'       cov_var = choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL)
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
                     parentname = ifelse(
                       inherits(arm_var, "data_extract_spec"),
                       teal.transform::datanames_input(arm_var),
                       "ADSL"
                     ),
                     aval_var,
                     id_var,
                     arm_var,
                     visit_var,
                     cov_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.widgets::basic_table_args(),
                     transformators = list(),
                     decorators = list()) {
  message("Initializing tm_a_gee (prototype)")

  cov_var <- teal.transform::add_no_selected_choices(cov_var, multiple = TRUE)

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(visit_var, "choices_selected")
  checkmate::assert_class(cov_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    visit_var = cs_to_des_select(visit_var, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE),
    split_covariates = cs_to_des_select(split_choices(cov_var), dataname = dataname, multiple = TRUE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname)
  )

  teal::module(
    label = label,
    server = srv_gee,
    ui = ui_gee,
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

ui_gee <- function(id, ...) {
  a <- list(...) # module args

  ns <- NS(id)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$id_var,
    a$visit_var,
    a$cov_var,
    a$aval_var
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tags$h3(textOutput(ns("gee_title"))),
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = tags$div(
      ### Reporter
      teal.reporter::add_card_button_ui(ns("add_reporter"), label = "Add Report Card"),
      tags$br(), tags$br(),
      ###
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "id_var", "visit_var", "cov_var", "aval_var")]),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("visit_var"),
        label = "Visit Variable",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cov_var"),
        label = "Covariates",
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      shinyjs::hidden(
        teal.transform::data_extract_ui(
          id = ns("split_covariates"),
          label = "Split Covariates",
          data_extract_spec = a$split_covariates,
          is_single_dataset = is_single_dataset_value
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
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
      teal.transform::data_extract_ui(
        id = ns("id_var"),
        label = "Subject Identifier",
        data_extract_spec = a$id_var,
        is_single_dataset = is_single_dataset_value
      ),
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
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
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
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table"))
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_gee <- function(id,
                    data,
                    filter_panel_api,
                    reporter,
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
                    plot_height,
                    plot_width,
                    basic_table_args,
                    decorators) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    ## split_covariates ----
    observeEvent(input[[extract_input("cov_var", dataname)]],
      ignoreNULL = FALSE,
      {
        # update covariates as actual variables
        split_interactions_values <- split_interactions(
          input[[extract_input("cov_var", dataname)]]
        )
        arm_var_value <- input[[extract_input("arm_var", parentname)]]
        arm_in_cov <- length(intersect(split_interactions_values, arm_var_value)) >= 1L
        if (arm_in_cov) {
          split_covariates_selected <- setdiff(split_interactions_values, arm_var_value)
        } else {
          split_covariates_selected <- split_interactions_values
        }
        teal.widgets::updateOptionalSelectInput(
          session,
          inputId = extract_input("split_covariates", dataname),
          selected = split_covariates_selected
        )
      }
    )

    ## arm_ref_comp_observer ----
    arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_a_gee"
    )

    ## data_merge_modules ----
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        id_var = id_var,
        visit_var = visit_var,
        split_covariates = split_covariates,
        aval_var = aval_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required"),
        id_var = shinyvalidate::sv_required("A Subject identifier is required"),
        visit_var = shinyvalidate::sv_required("A visit variable is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("An endpoint is required")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(
          0, 1,
          inclusive = c(FALSE, FALSE),
          message_fmt = "Confidence level must be between 0 and 1"
        )
      )
      iv$add_rule("cor_struct", shinyvalidate::sv_required("Please choose a correlation structure"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      data() %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    # Initially hide the output title because there is no output yet.
    shinyjs::show("gee_title")

    validate_checks <- reactive({
      teal::validate_inputs(iv_r())

      # To do in production: add validations.
      NULL
    })

    ## table_r ----
    table_q <- reactive({
      validate_checks()
      output_table <- input$output_table
      conf_level <- as.numeric(input$conf_level)
      col_source <- merged$anl_input_r()$columns_source
      filter_info <- merged$anl_input_r()$filter_info

      req(output_table)

      basic_table_args$subtitles <- paste0(
        "Analysis Variable: ", col_source$aval_var,
        ",  Endpoint: ", filter_info$paramcd[[1]]$selected[[1]],
        ifelse(length(col_source$split_covariates) == 0, "",
          paste(",  Covariates:", paste(col_source$split_covariates, collapse = ", "))
        )
      )
      basic_table_args$main_footer <- c(paste("Correlation Structure:", input$cor_struct))

      my_calls <- template_a_gee(
        output_table = output_table,
        data_model_fit = "ANL",
        dataname_lsmeans = "ANL_ADSL",
        input_arm_var = as.vector(col_source$arm_var),
        conf_level = conf_level,
        aval_var = col_source$aval_var,
        split_covariates = col_source$split_covariates,
        id_var = col_source$id_var,
        arm_var = col_source$arm_var,
        visit_var = col_source$visit_var,
        cor_struct = input$cor_struct,
        basic_table_args = basic_table_args
      )
      teal.code::eval_code(merged$anl_q(), as.expression(unlist(my_calls)))
    })

    output$gee_title <- renderText({
      # Input on output type.
      output_table <- input$output_table

      output_title <- switch(output_table,
        "t_gee_cov" = "Residual Covariance Matrix Estimate",
        "t_gee_coef" = "Model Coefficients",
        "t_gee_lsmeans" = "LS Means Estimates"
      )
      output_title
    })

    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = table_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    # Outputs to render.
    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    # Render R code
    source_code_r <- reactive(teal.code::get_code(req(decorated_table_q())))
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Generalized Estimating Equations (GEE) Analysis Table",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        table_type <- switch(input$output_table,
          "t_gee_cov" = "Residual Covariance Matrix Estimate",
          "t_gee_coef" = "Model Coefficients",
          "t_gee_lsmeans" = "LS Means Estimates"
        )
        card$append_text(paste(table_type, "Table"), "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(source_code_r())
        card
      }
      teal.reporter::add_card_button_srv("add_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}
