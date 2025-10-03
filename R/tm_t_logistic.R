#' Template: Logistic Regression
#'
#' Creates a valid expression to generate a logistic regression table.
#'
#' @inheritParams template_arguments
#' @inheritParams tern::tidy.glm
#' @param arm_var (`character`)\cr variable names that can be used as `arm_var`. To fit a logistic model with no
#'   arm/treatment variable, set to `NULL`.
#' @param topleft (`character`)\cr text to use as top-left annotation in the table.
#' @param interaction_var (`character`)\cr names of the variables that can be used for interaction variable selection.
#' @param responder_val (`character`)\cr values of the responder variable corresponding with a successful response.
#' @param label_paramcd (`character`)\cr label of response parameter value to print in the table title.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_logistic()]
#'
#' @keywords internal
template_logistic <- function(dataname,
                              arm_var,
                              aval_var,
                              label_paramcd,
                              cov_var,
                              interaction_var,
                              ref_arm,
                              comp_arm,
                              topleft = "Logistic Regression",
                              conf_level = 0.95,
                              combine_comp_arms = FALSE,
                              responder_val = c("CR", "PR"),
                              at = NULL,
                              basic_table_args = teal.widgets::basic_table_args()) {

  # Common assertion no matter if arm_var is NULL or not.
  checkmate::assert_string(dataname)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(label_paramcd, null.ok = TRUE)
  checkmate::assert_string(topleft, null.ok = TRUE)
  checkmate::assert_character(cov_var, null.ok = TRUE)
  checkmate::assert_string(interaction_var, null.ok = TRUE)

  y <- list()

  data_pipe <- list()
  data_list <- list()

  # Conditional assertion depends on if arm_var isn't NULL.
  if (!is.null(arm_var)) {
    checkmate::assert_string(arm_var)
    checkmate::assert_flag(combine_comp_arms)

    ref_arm_val <- paste(ref_arm, collapse = "/")

    y$arm_lab <- substitute(
      expr = arm_var_lab <- teal.data::col_labels(anl[arm_var], fill = FALSE),
      env = list(anl = as.name(dataname), arm_var = arm_var)
    )

    # Start to build data when arm_var is not NULL.
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

    data_list <- add_expr(
      data_list,
      substitute(
        expr = ANL <- data_pipe,
        env = list(data_pipe = pipe_expr(data_pipe))
      )
    )
  }

  data_list <- add_expr(
    data_list,
    substitute(
      expr = ANL <- df %>%
        dplyr::mutate(Response = aval_var %in% responder_val) %>%
        tern::df_explicit_na(na_level = "_NA_"),
      env = list(df = as.name("ANL"), aval_var = as.name(aval_var), responder_val = responder_val)
    )
  )

  y$data <- bracket_expr(data_list)

  if (!is.null(arm_var)) {
    y$relabel <- substitute(
      expr = teal.data::col_labels(ANL[arm_var]) <- arm_var_lab,
      env = list(arm_var = arm_var)
    )
  }

  model_list <- list()
  model_list <- if (is.null(interaction_var)) {
    add_expr(
      model_list,
      substitute(
        expr = tern::fit_logistic(
          ANL,
          variables = list(response = "Response", arm = arm_var, covariates = cov_var)
        ),
        env = list(arm_var = arm_var, cov_var = cov_var)
      )
    )
  } else {
    add_expr(
      model_list,
      substitute(
        expr = tern::fit_logistic(
          ANL,
          variables = list(
            response = "Response", arm = arm_var, covariates = cov_var,
            interaction = interaction_var
          )
        ),
        env = list(arm_var = arm_var, cov_var = cov_var, interaction_var = interaction_var)
      )
    )
  }

  model_list <- if (is.null(interaction_var)) {
    add_expr(
      model_list,
      substitute(
        expr = broom::tidy(conf_level = conf_level),
        env = list(conf_level = conf_level)
      )
    )
  } else {
    add_expr(
      model_list,
      substitute(
        expr = broom::tidy(conf_level = conf_level, at = at),
        env = list(conf_level = conf_level, at = at)
      )
    )
  }

  model_list <- add_expr(model_list, quote(tern::df_explicit_na(na_level = "_NA_")))

  y$model <- substitute(
    expr = mod <- model_pipe,
    env = list(model_pipe = pipe_expr(model_list))
  )

  layout_list <- list()

  basic_title <- if (length(responder_val) > 1) {
    paste(
      "Summary of Logistic Regression Analysis for", label_paramcd, "for",
      paste(utils::head(responder_val, -1), collapse = ", "),
      "and", utils::tail(responder_val, 1), "Responders"
    )
  } else {
    paste("Summary of Logistic Regression Analysis for", label_paramcd, "for", responder_val, "Responders")
  }

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(title = basic_title)
    )
  )

  y$table <- substitute(
    expr = {
      table <- expr_basic_table_args %>%
        tern::summarize_logistic(
          conf_level = conf_level,
          drop_and_remove_str = "_NA_"
        ) %>%
        rtables::append_topleft(topleft) %>%
        rtables::build_table(df = mod)
    },
    env = list(
      expr_basic_table_args = parsed_basic_table_args,
      conf_level = conf_level,
      topleft = topleft
    )
  )

  y
}

#' teal Module: Logistic Regression
#'
#' This module produces a multi-variable logistic regression table consistent with the TLG Catalog template
#' `LGRT02` available [here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/lgrt02.html).
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_logistic
#' @param arm_var ([teal.transform::choices_selected()] or `NULL`)\cr object
#'   with all available choices and preselected option for variable names that can be used as `arm_var`. This defines
#'   the grouping variable(s) in the results table. If there are two elements selected for `arm_var`, the second
#'   variable will be nested under the first variable. If `NULL`, no arm/treatment variable is included in the
#'   logistic model.
#' @param avalc_var ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the analysis variable (categorical).
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
#' tm_t_logistic(
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
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' library(dplyr)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADRS <- tmc_ex_adrs %>%
#'     filter(PARAMCD %in% c("BESRSPI", "INVET"))
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADRS <- data[["ADRS"]]
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
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_logistic(
#'       label = "Logistic Regression",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADRS, c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         choices = value_choices(ADRS, "PARAMCD", "PARAM"),
#'         selected = "BESRSPI"
#'       ),
#'       cov_var = choices_selected(
#'         choices = c("SEX", "AGE", "BMRKR1", "BMRKR2"),
#'         selected = "SEX"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_logistic <- function(label,
                          dataname,
                          parentname = ifelse(
                            inherits(arm_var, "data_extract_spec"),
                            teal.transform::datanames_input(arm_var),
                            "ADSL"
                          ),
                          arm_var = NULL,
                          arm_ref_comp = NULL,
                          paramcd,
                          cov_var = NULL,
                          avalc_var = teal.transform::choices_selected(
                            teal.transform::variable_choices(dataname, "AVALC"), "AVALC",
                            fixed = TRUE
                          ),
                          conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                          pre_output = NULL,
                          post_output = NULL,
                          basic_table_args = teal.widgets::basic_table_args(),
                          transformators = list(),
                          decorators = list()) {
  message("Initializing tm_t_logistic")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(cov_var, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(avalc_var, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_list(arm_ref_comp, names = "named", null.ok = TRUE)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = `if`(is.null(arm_var), NULL, cs_to_des_select(arm_var, dataname = parentname)),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE),
    avalc_var = cs_to_des_select(avalc_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_t_logistic,
    ui = ui_t_logistic,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        arm_ref_comp = arm_ref_comp,
        label = label,
        dataname = dataname,
        parentname = parentname,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_t_logistic <- function(id, ...) {
  a <- list(...)
  if (!is.null(a$arm_var)) {
    is_single_dataset_value <- teal.transform::is_single_dataset(
      a$arm_var,
      a$paramcd,
      a$avalc_var,
      a$cov_var
    )
  } else {
    is_single_dataset_value <- teal.transform::is_single_dataset(
      a$paramcd,
      a$avalc_var,
      a$cov_var
    )
  }

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::table_with_settings_ui(ns("table")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "avalc_var", "cov_var")]),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("avalc_var"),
        label = "Analysis Variable",
        data_extract_spec = a$avalc_var,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        ns("responders"),
        "Responders",
        choices = c("CR", "PR"),
        selected = c("CR", "PR"),
        multiple = TRUE
      ),
      if (!is.null(a$arm_var)) {
        tags$div(
          teal.transform::data_extract_ui(
            id = ns("arm_var"),
            label = "Select Treatment Variable",
            data_extract_spec = a$arm_var,
            is_single_dataset = is_single_dataset_value
          ),
          uiOutput(ns("arms_buckets")),
          checkboxInput(
            ns("combine_comp_arms"),
            "Combine all comparison groups?",
            value = FALSE
          )
        )
      },
      teal.transform::data_extract_ui(
        id = ns("cov_var"),
        label = "Covariates",
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      uiOutput(ns("interaction_variable")),
      uiOutput(ns("interaction_input")),
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
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table"))
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_logistic <- function(id,
                           data,
                           dataname,
                           parentname,
                           arm_var,
                           arm_ref_comp,
                           paramcd,
                           avalc_var,
                           cov_var,
                           label,
                           basic_table_args,
                           decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    # Observer to update reference and comparison arm input options.
    iv_arco <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_logistic"
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        avalc_var = avalc_var,
        cov_var = cov_var
      ),
      datasets = data,
      select_validation_rule = list(
        arm_var = shinyvalidate::sv_required("Treatment Variable is empty"),
        avalc_var = shinyvalidate::sv_required("Analysis variable is empty"),
        cov_var = shinyvalidate::sv_required("`Covariates` field is empty")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("`Select Endpoint` field is empty")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("responders", shinyvalidate::sv_required("`Responders` field is empty"))
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level."))
      iv$add_rule("conf_level", shinyvalidate::sv_between(
        0, 1,
        message_fmt = "Confdence level must be between {left} and {right}."
      ))
      iv$add_validator(iv_arco)
      # Conditional validator for interaction values.
      iv_int <- shinyvalidate::InputValidator$new()
      iv_int$condition(
        ~ length(input$interaction_var) > 0L &&
          is.numeric(merged$anl_q()[["ANL"]][[input$interaction_var]])
      )
      iv_int$add_rule("interaction_values", shinyvalidate::sv_required(
        "If interaction is specified the level should be entered."
      ))
      iv_int$add_rule(
        "interaction_values",
        ~ if (anyNA(as_numeric_from_comma_sep_str(.))) {
          "Interaction levels are invalid."
        }
      )
      iv_int$add_rule(
        "interaction_values",
        ~ if (any(duplicated(as_numeric_from_comma_sep_str(.)))) {
          "Interaction levels must be unique."
        }
      )
      iv$add_validator(iv_int)
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Logistic Regression"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    # Because the AVALC values depends on the selected PARAMCD.
    observeEvent(merged$anl_input_r(), {
      avalc_var <- merged$anl_input_r()$columns_source$avalc_var
      if (nrow(merged$anl_q()[["ANL"]]) == 0) {
        responder_choices <- c("CR", "PR")
        responder_sel <- c("CR", "PR")
      } else {
        if (length(avalc_var) == 0) {
          return(NULL)
        }
        responder_choices <- unique(merged$anl_q()[["ANL"]][[avalc_var]])
        responder_sel <- intersect(responder_choices, isolate(input$responders))
      }
      updateSelectInput(
        session, "responders",
        choices = responder_choices,
        selected = responder_sel
      )
    })

    output$interaction_variable <- renderUI({
      cov_var <- as.vector(merged$anl_input_r()$columns_source$cov_var)
      if (length(cov_var) > 0) {
        teal.widgets::optionalSelectInput(
          session$ns("interaction_var"),
          label = "Interaction",
          choices = cov_var,
          selected = NULL,
          multiple = FALSE
        )
      } else {
        NULL
      }
    })

    output$interaction_input <- renderUI({
      interaction_var <- input$interaction_var
      if (length(interaction_var) > 0) {
        if (is.numeric(merged$anl_q()[["ANL"]][[interaction_var]])) {
          tagList(
            textInput(
              session$ns("interaction_values"),
              label = sprintf("Specify %s values (comma delimited) for treatment ORs calculation:", interaction_var),
              value = as.character(stats::median(merged$anl_q()[["ANL"]][[interaction_var]]))
            )
          )
        }
      } else {
        NULL
      }
    })

    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      validate_inputs(iv_r())

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_avalc_var <- as.vector(merged$anl_input_r()$columns_source$avalc_var)
      input_cov_var <- as.vector(merged$anl_input_r()$columns_source$cov_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]
      input_interaction_var <- input$interaction_var

      input_interaction_at <- input_interaction_var[input_interaction_var %in% input_cov_var]

      at_values <- as_numeric_from_comma_sep_str(input$interaction_values)

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_avalc_var, input_cov_var),
        arm_var = input_arm_var,
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        min_nrow = 4
      )

      # validate arm levels
      if (!is.null(arm_var)) {
        if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
          validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
        }

        do.call(what = "validate_standard_inputs", validate_args)

        arm_n <- base::table(merged$anl_q()[["ANL"]][[input_arm_var]])
        anl_arm_n <- if (input$combine_comp_arms) {
          c(sum(arm_n[unlist(input$buckets$Ref)]), sum(arm_n[unlist(input$buckets$Comp)]))
        } else {
          c(sum(arm_n[unlist(input$buckets$Ref)]), arm_n[unlist(input$buckets$Comp)])
        }
        validate(shiny::need(
          all(anl_arm_n >= 2),
          "Each treatment group should have at least 2 records."
        ))
      }

      # validate covariate has at least two levels
      validate(
        need(
          all(
            vapply(
              merged$anl_q()[["ANL"]][input_cov_var],
              FUN = function(x) {
                length(unique(x)) > 1
              },
              logical(1)
            )
          ),
          "All covariates need to have at least two levels"
        )
      )
    })

    # Generate r code for the analysis.
    all_q <- reactive({
      validate_checks()

      ANL <- merged$anl_q()[["ANL"]]

      label_paramcd <- get_paramcd_label(ANL, paramcd)

      paramcd <- as.character(unique(ANL[[unlist(paramcd$filter)["vars_selected"]]]))

      interaction_var <- input$interaction_var
      interaction_flag <- length(interaction_var) != 0

      at_values <- as_numeric_from_comma_sep_str(input$interaction_values)
      at_flag <- interaction_flag && is.numeric(ANL[[interaction_var]])

      cov_var <- names(merged$anl_input_r()$columns_source$cov_var)

      calls <- template_logistic(
        dataname = "ANL",
        arm_var = names(merged$anl_input_r()$columns_source$arm_var),
        aval_var = names(merged$anl_input_r()$columns_source$avalc_var),
        label_paramcd = label_paramcd,
        cov_var = if (length(cov_var) > 0) cov_var else NULL,
        interaction_var = if (interaction_flag) interaction_var else NULL,
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        combine_comp_arms = input$combine_comp_arms,
        topleft = paramcd,
        conf_level = as.numeric(input$conf_level),
        at = if (at_flag) at_values else NULL,
        responder_val = input$responders,
        basic_table_args = basic_table_args
      )

      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
      teal.code::eval_code(obj, as.expression(calls))
    })

    # Decoration of table output.
    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
