#' Template: Logistic Regression
#'
#' Creates an expression for logistic regressions.
#'
#' @inheritParams template_arguments
#' @param topleft (`character`)\cr
#'  the top-left annotation in the table.
#' @param at optional, (`NULL` or `numeric`)\cr
#'  values for the interaction variable. Otherwise the median is used.
#' @param interaction_var (`character`)\cr
#'  names of the variables that can be used for interaction variable selection.
#' @param responder_val (`character`)\cr
#'  values of the responder variable corresponding with a successful response.
#' @param paramcd (`character`)\cr response parameter value to use in the table title.
#'
#' @seealso [tm_t_logistic()]
#'
#' @importFrom broom tidy
template_logistic <- function(dataname,
                              arm_var,
                              aval_var,
                              paramcd,
                              cov_var,
                              interaction_var,
                              ref_arm,
                              comp_arm,
                              topleft = "Logistic Regression",
                              conf_level = 0.95,
                              combine_comp_arms = FALSE,
                              responder_val = c("CR", "PR"),
                              at = NULL) {

  assert_that(
    is.string(dataname),
    is.string(arm_var) || is.null(arm_var),
    is.string(aval_var),
    is.string(paramcd),
    is.string(topleft) || is.null(topleft),
    is.character(cov_var) || is.null(cov_var),
    is.string(interaction_var) || is.null(interaction_var),
    is.flag(combine_comp_arms)
  )

  ref_arm_val <- paste(ref_arm, collapse = "/")
  y <- list()

  if (!is.null(arm_var)) {
    y$arm_lab <- substitute(
      expr = arm_var_lab <- var_labels(anl[arm_var]),
      env = list(anl = as.name(dataname), arm_var = arm_var)
    )
  }

  data_list <- list()

  if (!is.null(arm_var)) {
    data_list <- add_expr(
      data_list,
      prepare_arm(
        dataname = dataname,
        arm_var = arm_var,
        ref_arm = ref_arm,
        comp_arm = comp_arm,
        ref_arm_val = ref_arm_val
      )
    )
  }

  if (combine_comp_arms) {
    data_list <- add_expr(
      data_list,
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
      expr = dplyr::mutate(Response = aval_var %in% responder_val),
      env = list(aval_var = as.name(aval_var), responder_val = responder_val)
    )
  )

  data_list <- add_expr(data_list, quote(df_explicit_na(na_level = "")))

  y$data <- substitute(
    expr = anl <- data_pipe,
    env = list(data_pipe = pipe_expr(data_list))
  )

  if (!is.null(arm_var)) {
    y$relabel <- substitute(
      expr = rtables::var_labels(anl[arm_var]) <- arm_var_lab,
      env = list(arm_var = arm_var)
    )
  }

  model_list <- list()
  model_list <- if (is.null(interaction_var) || is.null(arm_var)) {
    add_expr(
      model_list,
      substitute(
        expr = fit_logistic(
          anl,
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
          anl,
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

  model_list <- add_expr(model_list, quote(df_explicit_na(na_level = "")))

  y$model <- substitute(
    expr = mod <- model_pipe,
    env = list(model_pipe = pipe_expr(model_list))
  )

  y$table <- substitute(
    expr = {
      result <- basic_table(
        title = paste(
          "Table of", paramcd, "for", paste(head(responder_val, -1), collapse = ", "),
          ifelse(length(responder_val) > 1, "and", ""),
          tail(responder_val, 1), "Responders"
          )
      ) %>%
        summarize_logistic(conf_level = conf_level) %>%
        append_topleft(topleft) %>%
        build_table(df = mod)
      result
    },
    env = list(
      conf_level = conf_level,
      topleft = topleft,
      paramcd = paramcd,
      responder_val = responder_val
      )
    )

  y
}


#' Teal Module: Logistic Regression
#'
#' @description This module produces a multi-variable logistic regression table that matches the
#'   STREAM template `lgrt02`.
#'
#' @inheritParams module_arguments
#' @param avalc_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'  object with all available choices and preselected option for the analysis variable (categorical).
#'
#' @export
#' @examples
#'
#' library(scda)
#' library(dplyr)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADRS <- synthetic_cdisc_data("latest")$adrs %>%
#'   filter(PARAMCD %in% c("BESRSPI", "INVET"))
#'
#' arm_ref_comp = list(
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
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADRS", ADRS, code = 'ADRS <- synthetic_cdisc_data("latest")$adrs %>%
#'       filter(PARAMCD %in% c("BESRSPI", "INVET"))'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
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
#'
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_t_logistic <- function(label,
                          dataname,
                          parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                          arm_var,
                          arm_ref_comp = NULL,
                          paramcd,
                          cov_var = NULL,
                          avalc_var = choices_selected(variable_choices(dataname, "AVALC"), "AVALC", fixed = TRUE),
                          conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                          pre_output = NULL,
                          post_output = NULL,
                          no_arm_var = FALSE) {
  logger::log_info("Initializing tm_t_logistic")
  stopifnot(
    length(dataname) == 1,
    is.choices_selected(conf_level)
  )

  args <- as.list(environment())

  if (no_arm_var) {
    data_extract_list <- list(
      arm_var = NULL,
      paramcd = cs_to_des_filter(paramcd, dataname = dataname),
      avalc_var = cs_to_des_select(avalc_var, dataname = dataname),
      cov_var = cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE)
    )
  } else {
    data_extract_list <- list(
      arm_var = cs_to_des_select(arm_var, dataname = parentname),
      paramcd = cs_to_des_filter(paramcd, dataname = dataname),
      avalc_var = cs_to_des_select(avalc_var, dataname = dataname),
      cov_var = cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE)
    )
  }

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
        parentname = parentname
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' User Interface for `tm_t_logistic`
#' @noRd
#'
ui_t_logistic <- function(id, ...) {

  a <- list(...)
  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$avalc_var,
    a$cov_var
  )

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      table_with_settings_ui(ns("table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "paramcd", "avalc_var", "cov_var")]),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
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
      if (a$no_arm_var == FALSE) {
        div(
          data_extract_input(
            id = ns("arm_var"),
            label = "Select Treatment Variable",
            data_extract_spec = a$arm_var,
            is_single_dataset = is_single_dataset_value
          ),
          selectInput(
            ns("ref_arm"),
            "Reference Group",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            ns("comp_arm"),
            "Comparison Group",
            choices = NULL,
            multiple = TRUE
          ),
          checkboxInput(
            ns("combine_comp_arms"),
            "Combine all comparison groups?",
            value = FALSE
          )
        )
      },
      data_extract_input(
        id = ns("cov_var"),
        label = "Covariates",
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      uiOutput(ns("interaction_var")),
      uiOutput(ns("interaction_input")),
      optionalSelectInput(
        inputId = ns("conf_level"),
        label = p(
          "Confidence level for ",
          span(style = "color:darkblue", "Coxph"),
          " (Hazard Ratio)",
          sep = ""
        ),
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' Server Function for `tm_t_logistic`
#' @noRd
#' @importFrom stats median
#'
srv_t_logistic <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           parentname,
                           arm_var,
                           arm_ref_comp,
                           paramcd,
                           avalc_var,
                           cov_var,
                           label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  # Observer to update reference and comparison arm input options.
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    dataname = parentname,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_logistic"
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, paramcd, avalc_var, cov_var),
    input_id = c("arm_var", "paramcd", "avalc_var", "cov_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  # Because the AVALC values depends on the selected PARAMCD.
  observeEvent(anl_merged(), {
    avalc_var <- anl_merged()$columns_source$avalc_var
    if (nrow(anl_merged()$data()) == 0) {
      responder_choices <- c("CR", "PR")
      responder_sel <- c("CR", "PR")
    } else {
      responder_choices <- unique(anl_merged()$data()[[avalc_var]])
      responder_sel <- intersect(responder_choices, isolate(input$responders))
    }
    updateSelectInput(
      session, "responders",
      choices = responder_choices,
      selected = responder_sel
    )
  })

  output$interaction_var <- renderUI({
    anl_m <- anl_merged()
    cov_var <- as.vector(anl_m$columns_source$cov_var)
    if (length(cov_var) > 0) {
      optionalSelectInput(
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
    anl_m <- anl_merged()
    interaction_var <- input$interaction_var
    if (length(interaction_var) > 0) {
      if (is.numeric(anl_m$data()[[interaction_var]])) {
        tagList(
          textInput(
            session$ns("interaction_values"),
            label = sprintf("Specify %s values (comma delimited) for treatment ORs calculation:", interaction_var),
            value = as.character(median(anl_m$data()[[interaction_var]]))
          )
        )
      }
    } else {
      NULL
    }
  })

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_avalc_var <- as.vector(anl_m$columns_source$avalc_var)
    input_cov_var <- as.vector(anl_m$columns_source$cov_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]
    input_interaction_var <- input$interaction_var

    input_interaction_at <- input_interaction_var[input_interaction_var %in% input_cov_var]
    interaction_flag <- length(input_interaction_at) != 0

    at_values <- if (is.null(input$interaction_values)) {
      NA
    } else {
      unlist(utils.nest::as_num(input$interaction_values))
    }

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_avalc_var, input_cov_var),
      arm_var = input_arm_var,
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      min_nrow = 4
    )

    # validate arm levels
    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
    }

    validate(need(
      input$conf_level >= 0 && input$conf_level <= 1,
      "Please choose a confidence level between 0 and 1"
    ))

    do.call(what = "validate_standard_inputs", validate_args)

    arm_n <- base::table(anl_m$data()[[input_arm_var]])
    anl_arm_n <- if (input$combine_comp_arms) {
      c(sum(arm_n[input$ref_arm]), sum(arm_n[input$comp_arm]))
    } else {
      c(sum(arm_n[input$ref_arm]), arm_n[input$comp_arm])
    }
    validate(need(
      all(anl_arm_n >= 2),
      "Each treatment group should have at least 2 records."
    ))

    validate(
      need(is_character_single(input_avalc_var), "Analysis variable should be a single column."),
      need(input$responders, "`Responders` field is empty")
    )

    # validate interaction values
    if (interaction_flag && (is.numeric(anl_m$data()[[input_interaction_at]]))) {
      validate(need(
        !is.na(at_values),
        "If interaction is specified the level should be entered."
      ))
    }

    # validate covariate has at least two levels
    validate(
      need(
        all(
          vapply(
            anl_m$data()[input_cov_var],
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

  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    ANL <- chunks_get_var("ANL") # nolint
    paramcd <- as.character(unique(ANL[[unlist(paramcd$filter)["vars_selected"]]]))

    interaction_var <- input$interaction_var
    interaction_flag <- length(interaction_var) != 0

    at_values <- if (is.null(input$interaction_values)) {
      NA
    } else {
      unlist(utils.nest::as_num(input$interaction_values))
    }
    at_flag <- interaction_flag && is.numeric(anl_m$data()[[interaction_var]])

    cov_var <- as.vector(anl_m$columns_source$cov_var)

    calls <- template_logistic(
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      aval_var = as.vector(anl_m$columns_source$avalc_var),
      paramcd = paramcd,
      cov_var = if (length(cov_var) > 0) cov_var else NULL,
      interaction_var = if (interaction_flag) interaction_var else NULL,
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      combine_comp_arms = input$combine_comp_arms,
      topleft = paramcd,
      conf_level = as.numeric(input$conf_level),
      at = if (at_flag) at_values else NULL,
      responder_val = input$responders
    )

    mapply(expression = calls, chunks_push)
  })

  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("result")
  })

  callModule(
    table_with_settings_srv,
    id = "table",
    table_r = table
  )

  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm_var, paramcd, avalc_var, cov_var)
    ),
    modal_title = "R Code for the Current Logistic Regression",
    code_header = label
  )
}
