#' Template: `ANCOVA` summary
#'
#' Creates a valid expression for analysis of variance summary table.
#' @inheritParams template_arguments
#' @param paramcd_levels (`character`)\cr
#'   variable levels for the studied parameter.
#' @param paramcd_var (`character`)\cr
#'   variable name for the studied parameter.
#' @param visit_levels (`character`)\cr
#'   variable levels for studied visits.
#' @param label_aval (`character`)\cr
#'   label of value variable used for title rendering.
#' @param label_paramcd (`character`)\cr
#'   variable label used for title rendering.
#'
#' @seealso [tm_t_ancova()]
#' @keywords internal
#'
template_ancova <- function(dataname = "ANL",
                            parentname = "ADSL",
                            arm_var,
                            ref_arm = NULL,
                            comp_arm = NULL,
                            combine_comp_arms = FALSE,
                            aval_var,
                            label_aval = NULL,
                            cov_var,
                            include_interact = FALSE,
                            interact_var = NULL,
                            interact_y = FALSE,
                            paramcd_levels = "",
                            paramcd_var = "PARAMCD",
                            label_paramcd = NULL,
                            visit_levels = "",
                            visit_var = "AVISIT",
                            conf_level = 0.95,
                            basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(label_aval) || is.null(label_aval),
    assertthat::is.flag(combine_comp_arms),
    assertthat::is.string(aval_var),
    is.character(cov_var),
    assertthat::is.flag(include_interact),
    all(sapply(interact_y, assertthat::is.string)) || isFALSE(interact_y),
    assertthat::is.string(interact_var) || is.null(interact_var)
  )

  y <- list()

  if (include_interact && !any(interact_y == "") && !is.null(interact_var)) {
    cov_var <- c(cov_var, paste0(arm_var, "*", interact_var))
  }

  if (length(cov_var) == 0) {
    cov_var <- NULL
  }

  # Data processing.
  data_list <- list()
  anl_list <- list()
  parent_list <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  anl_list <- add_expr(
    anl_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val,
      drop = FALSE
    )
  )
  anl_list <- add_expr(anl_list, quote(droplevels()))

  parent_list <- add_expr(
    parent_list,
    prepare_arm(
      dataname = parentname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val,
      drop = FALSE
    )
  )
  parent_list <- add_expr(parent_list, quote(droplevels()))

  if (combine_comp_arms) {
    anl_list <- add_expr(
      anl_list,
      substitute_names(
        expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
    parent_list <- add_expr(
      parent_list,
      substitute_names(
        expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
  }

  anl_list <- add_expr(anl_list, quote(df_explicit_na(na_level = "")))
  parent_list <- add_expr(parent_list, quote(df_explicit_na(na_level = "")))

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- anl_list,
      env = list(
        anl = as.name(dataname),
        anl_list = pipe_expr(anl_list)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parent <- parent_list,
      env = list(
        parent = as.name(parentname),
        parent_list = pipe_expr(parent_list)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Build layout.
  visits_title <- if (length(visit_levels) > 1) {
    paste(
      paste(utils::head(visit_levels, -1), collapse = ", "),
      "and", utils::tail(visit_levels, 1)
    )
  } else if (length(visit_levels) == 1) {
    visit_levels
  } else {
    ""
  }

  table_title <- if (length(label_paramcd) > 1) {
    paste(
      "Summary of Analysis of Variance for", paste(label_paramcd, collapse = " and "),
      "at", visits_title, "for", label_aval
    )
  } else if (length(label_paramcd == 1)) {
    paste("Summary of Analysis of Variance for", label_paramcd, "at", visits_title, "for", label_aval)
  } else {
    ""
  }

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(title = table_title)
    )
  )

  y$layout_prep <- quote(split_fun <- drop_split_levels)
  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_cols_by(var = arm_var, ref_group = ref_group) %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          visit_var,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = formatters::var_labels(dataname[visit_var], fill = TRUE)
        ),
      env = list(
        arm_var = arm_var,
        ref_group = paste(ref_arm, collapse = "/"),
        visit_var = visit_var,
        dataname = as.name(dataname)
      )
    )
  )

  if (length(paramcd_levels) > 1) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::split_rows_by(
          paramcd_var,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = formatters::var_labels(dataname[paramcd_var], fill = TRUE)
        ),
        env = list(
          paramcd_var = paramcd_var,
          dataname = as.name(dataname)
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::append_topleft(paste0("  ", paramcd_levels)),
        env = list(
          paramcd_levels = paramcd_levels
        )
      )
    )
  }

  if (!include_interact) {
    if (length(paramcd_levels) > 1) {
      if (length(cov_var) == 0) {
        ls_lbls <- c(lsmean = "Unadjusted Mean", lsmean_diff = "Difference in Unadjusted Means")
        var_lbls <- "Unadjusted mean"
      } else {
        ls_lbls <- NULL
        var_lbls <- "Adjusted mean"
      }
      layout_list <- add_expr(
        layout_list,
        substitute(
          summarize_ancova(
            vars = aval_var,
            variables = list(arm = arm_var, covariates = cov_var),
            conf_level = conf_level,
            var_labels = var_labels,
            show_labels = "hidden",
            .labels = ls_labels
          ),
          env = list(
            aval_var = aval_var,
            arm_var = arm_var,
            cov_var = cov_var,
            conf_level = conf_level,
            var_labels = var_lbls,
            ls_labels = ls_lbls
          )
        )
      )
    } else {
      # Only one entry in `paramcd_levels` here.
      layout_list <- add_expr(
        layout_list,
        substitute(
          summarize_ancova(
            vars = aval_var,
            variables = list(arm = arm_var, covariates = NULL),
            conf_level = conf_level,
            var_labels = "Unadjusted comparison",
            .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
            table_names = "unadjusted_comparison"
          ),
          env = list(
            aval_var = aval_var,
            arm_var = arm_var,
            conf_level = conf_level
          )
        )
      )
      if (length(cov_var) > 0) {
        layout_list <- add_expr(
          layout_list,
          substitute(
            summarize_ancova(
              vars = aval_var,
              variables = list(arm = arm_var, covariates = cov_var),
              conf_level = conf_level,
              var_labels = paste0(
                "Adjusted comparison (", paste(cov_var, collapse = " + "), ")"
              ),
              table_names = "adjusted_comparison"
            ),
            env = list(
              aval_var = aval_var,
              arm_var = arm_var,
              cov_var = cov_var,
              conf_level = conf_level
            )
          )
        )
      }
    }
  } else {
    cts_interact <- all(interact_y == FALSE)
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::append_topleft(paste0("    Interaction Variable: ", interact_var)),
        env = list(
          interact_var = interact_var
        )
      )
    )
    for (int_y in interact_y) {
      if (length(paramcd_levels) > 1) {
        layout_list <- add_expr(
          layout_list,
          substitute(
            summarize_ancova(
              vars = aval_var,
              variables = list(arm = arm_var, covariates = cov_var),
              conf_level = conf_level,
              var_labels = paste("Interaction Level:", interact_y),
              show_labels = if (cts_interact) "hidden" else "visible",
              interaction_y = interact_y,
              interaction_item = interact_var
            ),
            env = list(
              aval_var = aval_var,
              arm_var = arm_var,
              cov_var = cov_var,
              conf_level = conf_level,
              interact_y = int_y,
              interact_var = interact_var,
              cts_interact = cts_interact
            )
          )
        )
      } else {
        # Only one entry in `paramcd_levels` here.
        if (int_y == interact_y[1]) {
          layout_list <- add_expr(
            layout_list,
            substitute(
              summarize_ancova(
                vars = aval_var,
                variables = list(arm = arm_var, covariates = NULL),
                conf_level = conf_level,
                var_labels = "Unadjusted comparison",
                .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
                table_names = "unadjusted_comparison"
              ),
              env = list(
                aval_var = aval_var,
                arm_var = arm_var,
                cov_var = cov_var,
                conf_level = conf_level
              )
            )
          )
        }
        if (length(cov_var) > 0) {
          layout_list <- add_expr(
            layout_list,
            substitute(
              summarize_ancova(
                vars = aval_var,
                variables = list(arm = arm_var, covariates = cov_var),
                conf_level = conf_level,
                var_labels = if (cts_interact) {
                  paste0("Adjusted comparison (", paste(cov_var, collapse = " + "), ")")
                } else {
                  paste0(
                    "Adjusted comparison (", paste(cov_var, collapse = " + "),
                    "), Interaction Level: ", interact_y
                  )
                },
                table_names = "adjusted_comparison",
                interaction_y = interact_y,
                interaction_item = interact_var
              ),
              env = list(
                aval_var = aval_var,
                arm_var = arm_var,
                cov_var = cov_var,
                conf_level = conf_level,
                interact_y = int_y,
                interact_var = interact_var,
                cts_interact = cts_interact
              )
            )
          )
        }
      }
    }
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Build table.
  y$table <- substitute(
    expr = {
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      result
    },
    env = list(
      anl = as.name(dataname),
      parent = as.name(parentname)
    )
  )

  y
}

#' Teal Module: `ANCOVA` Teal Module
#'
#' @inheritParams module_arguments
#'
#' @details This module produces an analysis of variance summary table that is
#' similar to `AOVT01` when multiple endpoints are selected.
#' When a single endpoint is selected, both unadjusted and adjusted comparison
#' would be provided. This modules expects that the analysis data has the
#' following variables:
#'
#' \tabular{ll}{
#'  `AVISIT` \tab variable used to filter for analysis visits.\cr
#'  `PARAMCD` \tab variable used to filter for endpoints, after filtering for
#'  `paramcd` and `avisit`, one observation per patient is expected for the analysis
#'  to be meaningful.
#' }
#'
#' @export
#'
#' @examples
#' adsl <- tmc_ex_adsl
#' adqs <- tmc_ex_adqs
#'
#' arm_ref_comp <- list(
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   ),
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADQS", adqs)
#'   ),
#'   modules = modules(
#'     tm_t_ancova(
#'       label = "ANCOVA Table",
#'       dataname = "ADQS",
#'       avisit = choices_selected(
#'         choices = value_choices(adqs, "AVISIT"),
#'         selected = "WEEK 1 DAY 8"
#'       ),
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, c("ARM", "ACTARMCD", "ARMCD")),
#'         selected = "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       aval_var = choices_selected(
#'         choices = variable_choices(adqs, c("CHG", "AVAL")),
#'         selected = "CHG"
#'       ),
#'       cov_var = choices_selected(
#'         choices = variable_choices(adqs, c("BASE", "STRATA1", "SEX")),
#'         selected = "STRATA1"
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(adqs, "PARAMCD", "PARAM"),
#'         selected = "FKSI-FWB"
#'       ),
#'       interact_var = choices_selected(
#'         choices = variable_choices(adqs, c("BASE", "STRATA1", "SEX")),
#'         selected = "STRATA1"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_ancova <- function(label,
                        dataname,
                        parentname = ifelse(
                          inherits(arm_var, "data_extract_spec"),
                          teal.transform::datanames_input(arm_var),
                          "ADSL"
                        ),
                        arm_var,
                        arm_ref_comp = NULL,
                        aval_var,
                        cov_var,
                        include_interact = FALSE,
                        interact_var = NULL,
                        interact_y = FALSE,
                        avisit,
                        paramcd,
                        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_ancova")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- c(as.list(environment()))

  if (is.null(interact_var)) {
    interact_var <- teal.transform::choices_selected(
      choices = cov_var$choices,
      selected = NULL
    )
  }

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE),
    avisit = cs_to_des_filter(avisit, dataname = dataname, multiple = TRUE, include_vars = TRUE),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE),
    interact_var = cs_to_des_select(interact_var, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_ancova,
    ui_args = c(data_extract_list, args),
    server = srv_ancova,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        include_interact = include_interact,
        label = label,
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_ancova <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var, a$aval_var, a$cov_var, a$avisit, a$paramcd, a$interact_var
  )

  ns <- shiny::NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "aval_var", "cov_var", "avisit", "paramcd", "interact_var")]),
      teal.transform::data_extract_ui(
        id = ns("avisit"),
        label = "Analysis Visit",
        data_extract_spec = a$avisit,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
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
      shiny::uiOutput(
        ns("arms_buckets"),
        title = paste(
          "Multiple reference groups are automatically combined into a single group",
          "when more than one value is selected."
        )
      ),
      shiny::uiOutput(ns("helptext_ui")),
      shiny::checkboxInput(
        ns("combine_comp_arms"),
        "Combine all comparison groups?",
        value = FALSE
      ),
      teal.transform::data_extract_ui(
        id = ns("cov_var"),
        label = "Covariates",
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = shiny::HTML(paste("Confidence Level")),
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      ),
      shiny::div(
        shiny::tags$label("Include Interaction Term"),
        shinyWidgets::switchInput(
          inputId = ns("include_interact"),
          value = FALSE,
          size = "mini"
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("include_interact"), "']"),
          shiny::div(
            teal.transform::data_extract_ui(
              id = ns("interact_var"),
              label = "Select Interaction Variable",
              data_extract_spec = a$interact_var,
              is_single_dataset = is_single_dataset_value
            ),
            teal.widgets::optionalSelectInput(
              ns("interact_y"),
              label = "Select Interaction y",
              choices = "",
              selected = "",
              multiple = TRUE,
              fixed = FALSE
            )
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
srv_ancova <- function(id,
                       data,
                       reporter,
                       filter_panel_api,
                       dataname,
                       parentname,
                       arm_var,
                       arm_ref_comp,
                       aval_var,
                       cov_var,
                       include_interact,
                       interact_var,
                       paramcd,
                       avisit,
                       label,
                       basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel.
    iv_arco <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_ancova"
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        aval_var = aval_var,
        cov_var = cov_var,
        avisit = avisit,
        paramcd = paramcd,
        interact_var = interact_var
      ),
      datasets = data,
      select_validation_rule = list(
        arm_var = shinyvalidate::sv_required("Arm variable cannot be empty."),
        aval_var = shinyvalidate::sv_required("Analysis variable cannot be empty."),
        cov_var = shinyvalidate::sv_optional(),
        interact_var = shinyvalidate::sv_optional()
      ),
      filter_validation_rule = list(
        avisit = shinyvalidate::sv_required("`Analysis Visit` field cannot be empty."),
        paramcd = shinyvalidate::sv_required("`Select Endpoint` is not selected.")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level."))
      iv$add_rule("conf_level", shinyvalidate::sv_between(
        0, 1,
        message_fmt = "Confdence level must be between {left} and {right}."
      ))
      iv$add_validator(iv_arco)
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
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL",
      join_keys = get_join_keys(data)
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

    output$helptext_ui <- shiny::renderUI({
      if (length(selector_list()$arm_var()$select) != 0) {
        shiny::helpText("Multiple reference groups are automatically combined into a single group.")
      }
    })

    # Event handler:
    # Update interact_y choices to all levels of selected interact_var
    shiny::observeEvent(
      {
        input$include_interact
        input$`interact_var-dataset_ADQS_singleextract-select`
      },
      {
        interact_var <- input$`interact_var-dataset_ADQS_singleextract-select`
        if (isTRUE(input$include_interact) && length(interact_var) > 0) {
          interact_choices <- sort(as.vector(unique(merged$anl_q()[[dataname]][[interact_var]])))
          if (all(is.numeric(interact_choices))) {
            shinyjs::hide("interact_y")
          } else {
            interact_select <- if (!all(input$interact_y %in% interact_choices)) {
              interact_choices[1]
            } else {
              input$interact_y
            }
            shinyjs::show("interact_y")
            teal.widgets::updateOptionalSelectInput(
              session,
              "interact_y",
              selected = interact_select,
              choices = interact_choices
            )
          }
        }
      }
    )

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- shiny::reactive({
      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      teal::validate_inputs(iv_r())

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_aval_var <- as.vector(merged$anl_input_r()$columns_source$aval_var)
      input_cov_var <- as.vector(merged$anl_input_r()$columns_source$cov_var)
      input_interact_var <- as.vector(merged$anl_input_r()$columns_source$interact_var)
      input_avisit <- unlist(avisit$filter)["vars_selected"]
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # Validate inputs.
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_paramcd, input_avisit, input_aval_var, input_cov_var, input_interact_var
        ),
        arm_var = input_arm_var
      )
      validate_args <- append(
        validate_args,
        list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
      )
      do.call(what = "validate_standard_inputs", validate_args)

      # Other validations.
      shiny::validate(shiny::need(
        length(unique(adsl_filtered[[input_arm_var]])) > 1,
        "ANCOVA table needs at least 2 arm groups to make comparisons."
      ))
      # check that there is at least one record with no missing data
      shiny::validate(shiny::need(
        !all(is.na(merged$anl_q()[["ANL"]][[input_aval_var]])),
        "ANCOVA table cannot be calculated as all values are missing."
      ))
      # check that for each visit there is at least one record with no missing data
      all_NA_dataset <- merged$anl_q()[["ANL"]] %>% # nolint
        dplyr::group_by(dplyr::across(dplyr::all_of(c(input_avisit, input_arm_var)))) %>%
        dplyr::summarize(all_NA = all(is.na(.data[[input_aval_var]])))
      shiny::validate(shiny::need(
        !any(all_NA_dataset$all_NA),
        "ANCOVA table cannot be calculated as all values are missing for one visit for (at least) one arm."
      ))

      if (input$include_interact) {
        if (!is.null(input_interact_var) && length(input_interact_var) > 0) {
          shiny::validate(shiny::need(
            !input_interact_var %in% c(input_avisit, input_paramcd) &&
              length(as.vector(unique(anl_filtered[[input_interact_var]]))) > 1,
            paste(
              "Interaction variable cannot be a filter variable and must have more than one level.",
              "Please select a different interaction variable."
            )
          ))
          if (!all(is.numeric(as.vector(unique(anl_filtered[[input_interact_var]]))))) {
            shiny::validate(shiny::need(
              !is.null(input$interact_y),
              paste(
                "Interaction y must be selected when a discrete variable is chosen for interact variable.",
                "Please select an interaction y, change the interaction variable, or turn off interactions."
              )
            ))
          }
        }
      }

      if (length(input_cov_var >= 1L)) {
        input_cov_var_dataset <- anl_filtered[input_cov_var]
        shiny::validate(
          shiny::need(
            all(vapply(input_cov_var_dataset, function(col) length(unique(col)) > 1L, logical(1))),
            "Selected covariates should have more than one level for showing the adjusted analysis."
          )
        )
      }
    })

    # The R-code corresponding to the analysis.
    table_q <- shiny::reactive({
      validate_checks()
      ANL <- merged$anl_q()[["ANL"]] # nolint

      label_paramcd <- get_paramcd_label(ANL, paramcd)
      input_aval <- as.vector(merged$anl_input_r()$columns_source$aval_var)
      label_aval <- if (length(input_aval) != 0) attributes(ANL[[input_aval]])$label else NULL
      paramcd_levels <- unique(ANL[[unlist(paramcd$filter)["vars_selected"]]])
      visit_levels <- unique(ANL[[unlist(avisit$filter)["vars_selected"]]])

      interact_var <- as.vector(merged$anl_input_r()$columns_source$interact_var)
      if (length(interact_var) > 0) {
        if (is.numeric(ANL[[interact_var]])) {
          interact_y <- FALSE
        } else if (!all(input$interact_y %in% levels(ANL[[interact_var]]))) {
          interact_y <- levels(ANL[[interact_var]])[1]
        } else {
          interact_y <- input$interact_y
        }
      } else {
        interact_var <- NULL
        if (length(input$interact_y) == 0 || all(input$interact_y == "")) {
          interact_y <- FALSE
        }
      }

      my_calls <- template_ancova(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = as.vector(merged$anl_input_r()$columns_source$arm_var),
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        combine_comp_arms = input$combine_comp_arms,
        aval_var = as.vector(merged$anl_input_r()$columns_source$aval_var),
        label_aval = label_aval,
        cov_var = as.vector(merged$anl_input_r()$columns_source$cov_var),
        include_interact = input$include_interact,
        interact_var = interact_var,
        interact_y = interact_y,
        paramcd_levels = paramcd_levels,
        paramcd_var = unlist(paramcd$filter)["vars_selected"],
        label_paramcd = label_paramcd,
        visit_levels = visit_levels,
        visit_var = unlist(avisit$filter)["vars_selected"],
        conf_level = as.numeric(input$conf_level),
        basic_table_args = basic_table_args
      )
      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
    })

    # Output to render.
    table_r <- shiny::reactive({
      table_q()[["result"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(table_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(table_q())))
    )

    # Render R code.
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(table_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("ANCOVA")
        card$append_text("ANCOVA", "header2")
        card$append_text("Analysis of Covariance", "header3")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(table_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
