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
#' @param arm_var ([teal.picks::variables] or [teal.transform::choices_selected()] or `NULL`)\cr object
#'   with all available choices and preselected option for variable names that can be used as `arm_var`. This defines
#'   the grouping variable(s) in the results table. If there are two elements selected for `arm_var`, the second
#'   variable will be nested under the first variable. If `NULL`, no arm/treatment variable is included in the
#'   logistic model.
#' @param avalc_var ([teal.picks::variables] or [teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the analysis variable (categorical).
#' @param paramcd_var ([teal.picks::variables()])\cr object with all available choices and
#' preselected option for the parameter code variable.
#' @param paramcd_values ([teal.picks::values()])\cr object with all available choices and preselected
#' option for the parameter code values.
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
#' @inheritSection teal::example_module Reporting
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
#'       paramcd_var = teal.picks::variables("PARAMCD", "PARAMCD"),
#'       paramcd_values = teal.picks::values(multiple = FALSE),
#'       cov_var = teal.picks::variables(selected = NULL),
#'       avalc_var = teal.picks::variables("AVALC", fixed = TRUE),
#'       conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95", keep_order = TRUE)
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
                          parentname = "ADSL",
                          arm_var = NULL,
                          arm_ref_comp = NULL,
                          paramcd = lifecycle::deprecated(),
                          paramcd_var = teal.picks::variables("PARAMCD", "PARAMCD"),
                          paramcd_values = teal.picks::values(multiple = FALSE),
                          cov_var = teal.picks::variables(selected = NULL),
                          avalc_var = teal.picks::variables("AVALC", fixed = TRUE),
                          conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95", keep_order = TRUE),
                          pre_output = NULL,
                          post_output = NULL,
                          basic_table_args = teal.widgets::basic_table_args(),
                          transformators = list(),
                          decorators = list()) {
  message("Initializing tm_t_logistic")

  arm_var <- deprecate_pick_variables_arg(arm_var, "arm_var")
  cov_var <- deprecate_pick_variables_arg(cov_var, "cov_var")
  avalc_var <- deprecate_pick_variables_arg(avalc_var, "avalc_var")
  conf_level <- deprecate_pick_values_arg(conf_level, "conf_level")
  if (!missing(paramcd)) {
    lifecycle::deprecate_warn(
      when = "0.13.0",
      what = "tm_a_mmrm(paramcd)",
      details = "Use of paramcd was removed in `tm_a_mmrm` module usage with teal.picks"
    )
    if (!missing(paramcd_var) || !missing(paramcd_values)) {
      stop("Please provide either `paramcd` or `paramcd_var` with `paramcd_values`, not both.")
    }
    paramcd_values <- deprecate_pick_values_arg(paramcd, "paramcd")
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "variables", null.ok = TRUE)
  checkmate::assert_class(cov_var, "variables", null.ok = TRUE)
  checkmate::assert_class(avalc_var, "variables")
  checkmate::assert_class(conf_level, "values")
  checkmate::assert_list(arm_ref_comp, names = "named", null.ok = TRUE)
  checkmate::assert_class(paramcd_var, "variables")
  checkmate::assert_class(paramcd_values, "values")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  for (var in c("arm_var", "paramcd_var", "paramcd_values")) {
    if (isTRUE(attr(get(var), "multiple", exact = TRUE))) {
      message(
        sprintf("Multiple variables are not allowed for %s. Forcing single selection.", var),
        domain = "force_single_selection"
      )
      eval(substitute(attr(var, "multiple") <- FALSE, list(var = as.name(var))))
    }
  }

  if (!is.null(arm_var)) {
    arm_var <- teal.picks::picks(datasets(parentname, parentname), arm_var)
  }
  cov_var <- teal.picks::picks(datasets(dataname, dataname), cov_var)
  avalc_var <- teal.picks::picks(datasets(dataname, dataname), avalc_var)
  paramcd <- teal.picks::picks(datasets(dataname, dataname), variables = paramcd_var, values = paramcd_values)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_logistic,
    ui = ui_t_logistic,
    ui_args = args[names(args) %in% names(formals(ui_t_logistic))],
    server_args = args[names(args) %in% names(formals(srv_t_logistic))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_t_logistic <- function(id,
                          arm_var,
                          paramcd,
                          avalc_var,
                          cov_var,
                          conf_level,
                          pre_output,
                          post_output,
                          decorators) {
  ns <- NS(id)
  conf_level$fixed <- conf_level$fixed %||% FALSE
  teal.widgets::standard_layout(
    output = teal.widgets::table_with_settings_ui(ns("table")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Endpoint:"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Analysis Variable:"),
        teal.picks::picks_ui(ns("avalc_var"), paramcd)
      ),
      selectInput(
        ns("responders"),
        "Responders",
        choices = c("CR", "PR"),
        selected = c("CR", "PR"),
        multiple = TRUE
      ),
      if (!is.null(arm_var)) {
        tags$div(
          tags$div(
            tags$label("Select Treatment Variable:"),
            teal.picks::picks_ui(ns("arm_var"), arm_var),
          ),
          uiOutput(ns("arms_buckets")),
          checkboxInput(
            ns("combine_comp_arms"),
            "Combine all comparison groups?",
            value = FALSE
          )
        )
      },
      tags$div(
        tags$label("Covariates:"),
        teal.picks::picks_ui(ns("cov_var"), cov_var)
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
        choices = conf_level$choices,
        selected = conf_level$selected,
        multiple = FALSE,
        fixed = conf_level$fixed
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table"))
    ),
    pre_output = pre_output,
    post_output = post_output
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

    selectors <- teal.picks::picks_srv(
      picks = list(
        arm_var = arm_var,
        paramcd = paramcd,
        avalc_var = avalc_var,
        cov_var = cov_var
      ),
      data = data
    )

    arm_var_r <- reactive(selectors$arm_var()$variables$selected)

    iv_arco <- arm_ref_comp_observer_picks(
      session,
      input,
      output,
      id_arm_var = "arm_var-variables-selected",
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_logistic",
      arm_var_r = arm_var_r
    )

    validated_q <- reactive({
      obj <- req(data())
      validate_input(
        inputId = "arm_var-variables-selected",
        condition = !is.null(selectors$arm_var()$variables$selected),
        message = "Treatment Variable is empty."
      )
      validate_input(
        inputId = "avalc_var-variables-selected",
        condition = !is.null(selectors$avalc_var()$variables$selected),
        message = "Analysis variable is empty"
      )
      validate_input(
        inputId = "cov_var-variables-selected",
        condition = !is.null(selectors$cov_var()$variables$selected),
        message = "`Covariates` field is empty"
      )
      validate_input(
        inputId = "paramcd-values-selected",
        condition = !is.null(selectors$paramcd()$values$selected),
        message = "`Select Endpoint` field is empty"
      )
      validate_input(
        inputId = "conf_level",
        condition = !is.null(input$conf_level),
        message = "Please choose a confidence level."
      )
      validate_input(
        inputId = "conf_level",
        condition = as.numeric(input$conf_level) > 0 && as.numeric(input$conf_level) < 1,
        message = "Confidence level must be a number between 0 and 1."
      )
      validate_input(
        inputId = "responders",
        condition = !is.null(input$responders) && length(input$responders) > 0,
        message = "`Responders` field is empty"
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Logistics table"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      obj
    })

    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data = validated_q,
      selectors = selectors,
      join_fun = "dplyr::inner_join",
      output_name = "ANL"
    )

    adsl_inputs <- teal.picks::merge_srv(
      "adsl_inputs",
      data = validated_q,
      selectors = if (is.null(arm_var)) list() else selectors["arm_var"],
      output_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      anl <- c(anl_inputs$data(), adsl_inputs$data())
      if (length(input$interaction_var) > 0L && is.numeric(anl[[input$interaction_var]])) {
        validate_input(
          inputId = "interaction_values",
          condition = !is.null(input$interaction_values),
          message = "If interaction is specified the level should be entered."
        )

        validate_input(
          inputId = "interaction_values",
          condition = !anyNA(as_numeric_from_comma_sep_str(input$interaction_values)),
          message = "Interaction levels are invalid."
        )

        validate_input(
          inputId = "interaction_values",
          condition = !any(duplicated(as_numeric_from_comma_sep_str(input$interaction_values))),
          message = "Interaction levels must be unique."
        )
      }
      anl
    })

    # Because the AVALC values depends on the selected PARAMCD.
    observeEvent(anl_inputs$data(), {
      avalc_var <- anl_inputs$variables()$avalc_var
      anl <- anl_q()[["ANL"]]
      if (nrow(anl) == 0) {
        responder_choices <- c("CR", "PR")
        responder_sel <- c("CR", "PR")
      } else {
        if (length(avalc_var) == 0) {
          return(NULL)
        }
        responder_choices <- unique(anl[[avalc_var]])
        responder_sel <- intersect(responder_choices, isolate(input$responders))
      }
      updateSelectInput(
        session, "responders",
        choices = responder_choices,
        selected = responder_sel
      )
    })

    output$interaction_variable <- renderUI({
      cov_var <- anl_inputs$variables()$cov_var
      if (length(cov_var) > 0L) {
        teal.widgets::optionalSelectInput(
          session$ns("interaction_var"),
          label = "Interaction",
          choices = cov_var,
          selected = NULL,
          multiple = FALSE
        )
      }
    })

    output$interaction_input <- renderUI({
      interaction_var <- input$interaction_var
      if (length(interaction_var) > 0) {
        if (is.numeric(anl_q()[["ANL"]][[interaction_var]])) {
          tagList(
            textInput(
              session$ns("interaction_values"),
              label = sprintf("Specify %s values (comma delimited) for treatment ORs calculation:", interaction_var),
              value = as.character(stats::median(anl_q()[["ANL"]][[interaction_var]]))
            )
          )
        }
      }
    })

    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- anl_inputs$variables()$arm_var
      input_avalc_var <- anl_inputs$variables()$avalc_var
      input_cov_var <- anl_inputs$variables()$cov_var
      input_paramcd <- anl_inputs$variables()$paramcd
      input_interaction_var <- input$interaction_var

      input_interaction_at <- input_interaction_var[input_interaction_var %in% input_cov_var]

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

        arm_n <- base::table(anl_q()[["ANL"]][[input_arm_var]])
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
              anl_q()[["ANL"]][input_cov_var],
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

      obj <- anl_q()
      ANL <- obj[["ANL"]]

      label_paramcd <- selectors$paramcd()$values$selected

      paramcd <- as.character(unique(ANL[[anl_inputs$variables()$paramcd]]))

      interaction_var <- input$interaction_var
      interaction_flag <- length(interaction_var) != 0

      at_values <- as_numeric_from_comma_sep_str(input$interaction_values)
      at_flag <- interaction_flag && is.numeric(ANL[[interaction_var]])
      cov_var <- anl_inputs$variables()$cov_var

      calls <- template_logistic(
        dataname = "ANL",
        arm_var = anl_inputs$variables()$arm_var,
        aval_var = anl_inputs$variables()$avalc_var,
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
      teal.code::eval_code(obj, as.expression(calls))
    })

    # Decoration of table output.
    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
