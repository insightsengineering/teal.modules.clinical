#' Template: Logistic Regression
#'
#' Creates an expression for logistic regressions.
#'
#' @inheritParams template_arguments
#' @param arm_var (`character`)\cr
#'   variable names that can be used as `arm_var`. No arm or treatment variable is included in the logistic model is
#'   being `NULL`.
#' @param topleft (`character`)\cr
#'  the top-left annotation in the table.
#' @param at optional, (`NULL` or `numeric`)\cr
#'  values for the interaction variable. Otherwise the median is used.
#' @param interaction_var (`character`)\cr
#'  names of the variables that can be used for interaction variable selection.
#' @param responder_val (`character`)\cr
#'  values of the responder variable corresponding with a successful response.
#' @param paramcd (`character`)\cr response parameter value to use in the table title.
#' @param label_paramcd (`character`)\cr Label of response parameter value to use in the table title.
#'
#' @seealso [tm_t_logistic()]
#' @keywords internal
#'
template_logistic <- function(dataname,
                              arm_var,
                              aval_var,
                              paramcd,
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
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(aval_var),
    assertthat::is.string(paramcd),
    assertthat::is.string(label_paramcd) || is.null(label_paramcd),
    assertthat::is.string(topleft) || is.null(topleft),
    is.character(cov_var) || is.null(cov_var),
    assertthat::is.string(interaction_var) || is.null(interaction_var)
  )

  y <- list()

  data_pipe <- list()
  data_list <- list()

  # Conditional assertion depends on if arm_var isn't NULL.
  if (!is.null(arm_var)) {
    assertthat::assert_that(
      assertthat::is.string(arm_var),
      assertthat::is.flag(combine_comp_arms)
    )

    ref_arm_val <- paste(ref_arm, collapse = "/")

    y$arm_lab <- substitute(
      expr = arm_var_lab <- formatters::var_labels(anl[arm_var], fill = FALSE),
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
          expr = dplyr::mutate(arm_var = combine_levels(x = arm_var, levels = comp_arm)),
          names = list(arm_var = as.name(arm_var)),
          others = list(comp_arm = comp_arm)
        )
      )
    }

    data_list <- add_expr(
      data_list,
      substitute(
        expr = ANL <- data_pipe, # nolint
        env = list(data_pipe = pipe_expr(data_pipe))
      )
    )
  }

  data_list <- add_expr(
    data_list,
    substitute(
      expr = ANL <- df %>% # nolint
        dplyr::mutate(Response = aval_var %in% responder_val) %>%
        df_explicit_na(na_level = "_NA_"),
      env = list(df = as.name("ANL"), aval_var = as.name(aval_var), responder_val = responder_val)
    )
  )

  y$data <- bracket_expr(data_list)

  if (!is.null(arm_var)) {
    y$relabel <- substitute(
      expr = formatters::var_labels(ANL[arm_var]) <- arm_var_lab, # nolint
      env = list(arm_var = arm_var)
    )
  }

  model_list <- list()
  model_list <- if (is.null(interaction_var)) {
    add_expr(
      model_list,
      substitute(
        expr = fit_logistic(
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
        expr = fit_logistic(
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

  model_list <- add_expr(model_list, quote(df_explicit_na(na_level = "_NA_")))

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
      result <- expr_basic_table_args %>%
        summarize_logistic(
          conf_level = conf_level,
          drop_and_remove_str = "_NA_"
        ) %>%
        rtables::append_topleft(topleft) %>%
        rtables::build_table(df = mod)
      result
    },
    env = list(
      expr_basic_table_args = parsed_basic_table_args,
      conf_level = conf_level,
      topleft = topleft
    )
  )

  y
}


#' Teal Module: Logistic Regression
#'
#' @description This module produces a multi-variable logistic regression table that matches `LGRT02`.
#'
#' @inheritParams module_arguments
#' @param arm_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()]) or `NULL`\cr
#'   object with all available choices
#'   and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table. If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#'   arm_var is optional, when being NULL, no arm or treatment variable is included in the logistic model.
#' @param avalc_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'  object with all available choices and preselected option for the analysis variable (categorical).
#'
#' @export
#' @examples
#' adsl <- tmc_ex_adsl
#' adrs <- tmc_ex_adrs %>%
#'   dplyr::filter(PARAMCD %in% c("BESRSPI", "INVET"))
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
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADRS", adrs)
#'   ),
#'   modules = modules(
#'     tm_t_logistic(
#'       label = "Logistic Regression",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adrs, c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         choices = value_choices(adrs, "PARAMCD", "PARAM"),
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
#'   shinyApp(ui = app$ui, server = app$server)
#' }
#'
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
                          basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_logistic")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_multi_class(arm_var, c("choices_selected", "data_extract_spec"), null.ok = TRUE)
  checkmate::assert_list(arm_ref_comp, names = "named", null.ok = TRUE)
  checkmate::assert_multi_class(paramcd, c("choices_selected", "data_extract_spec"))
  checkmate::assert_multi_class(cov_var, c("choices_selected", "data_extract_spec"))
  checkmate::assert_multi_class(avalc_var, c("choices_selected", "data_extract_spec"))
  checkmate::assert_class(conf_level, classes = "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

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
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' User Interface for `tm_t_logistic`
#' @noRd
#'
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


  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
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
      shiny::selectInput(
        ns("responders"),
        "Responders",
        choices = c("CR", "PR"),
        selected = c("CR", "PR"),
        multiple = TRUE
      ),
      if (!is.null(a$arm_var)) {
        shiny::div(
          teal.transform::data_extract_ui(
            id = ns("arm_var"),
            label = "Select Treatment Variable",
            data_extract_spec = a$arm_var,
            is_single_dataset = is_single_dataset_value
          ),
          shiny::uiOutput(ns("arms_buckets")),
          shiny::checkboxInput(
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
      shiny::uiOutput(ns("interaction_var")),
      shiny::uiOutput(ns("interaction_input")),
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
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' Server Function for `tm_t_logistic`
#' @noRd
#'
srv_t_logistic <- function(id,
                           data,
                           reporter,
                           filter_panel_api,
                           dataname,
                           parentname,
                           arm_var,
                           arm_ref_comp,
                           paramcd,
                           avalc_var,
                           cov_var,
                           label,
                           basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    # Observer to update reference and comparison arm input options.
    iv_arco <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data[[parentname]],
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

    iv_r <- shiny::reactive({
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
      iv_int$condition(~ length(input$interaction_var) > 0L &&
        is.numeric(merged$anl_q()[["ANL"]][[input$interaction_var]]))
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
      join_keys = get_join_keys(data),
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      join_keys = get_join_keys(data),
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    # Because the AVALC values depends on the selected PARAMCD.
    shiny::observeEvent(merged$anl_input_r(), {
      avalc_var <- merged$anl_input_r()$columns_source$avalc_var
      if (nrow(merged$anl_q()[["ANL"]]) == 0) {
        responder_choices <- c("CR", "PR")
        responder_sel <- c("CR", "PR")
      } else {
        responder_choices <- unique(merged$anl_q()[["ANL"]][[avalc_var]])
        responder_sel <- intersect(responder_choices, shiny::isolate(input$responders))
      }
      shiny::updateSelectInput(
        session, "responders",
        choices = responder_choices,
        selected = responder_sel
      )
    })

    output$interaction_var <- shiny::renderUI({
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

    output$interaction_input <- shiny::renderUI({
      interaction_var <- input$interaction_var
      if (length(interaction_var) > 0) {
        if (is.numeric(merged$anl_q()[["ANL"]][[interaction_var]])) {
          shiny::tagList(
            shiny::textInput(
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

    validate_checks <- shiny::reactive({
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
        shiny::validate(shiny::need(
          all(anl_arm_n >= 2),
          "Each treatment group should have at least 2 records."
        ))
      }

      # validate covariate has at least two levels
      shiny::validate(
        shiny::need(
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

    all_q <- shiny::reactive({
      validate_checks()

      ANL <- merged$anl_q()[["ANL"]] # nolint

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
        paramcd = paramcd,
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

      teal.code::eval_code(merged$anl_q(), as.expression(calls))
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
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Logistic Regression Table")
        card$append_text("Logistic Regression Table", "header2")
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
