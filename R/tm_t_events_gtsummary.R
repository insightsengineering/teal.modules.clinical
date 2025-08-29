#' teal Module: Adverse Events Summary
#'
#' This module produces an adverse events summary table.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_arguments
#' @inheritParams template_events_summary
#' @param arm_var ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table.
#'   If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param dthfl_var ([teal.transform::choices_selected()])\cr object
#'   with all available choices and preselected option for variable names that can be used as death flag variable.
#'   Records with `"Y"` are summarized in the table row for "Total number of deaths".
#' @param dcsreas_var ([teal.transform::choices_selected()])\cr object
#'   with all available choices and preselected option for variable names that can be used as study discontinuation
#'   reason variable. Records with `"ADVERSE EVENTS"` are summarized in the table row for
#'   "Total number of patients withdrawn from study due to an AE".
#' @param flag_var_anl ([teal.transform::choices_selected()] or `NULL`)\cr
#'   vector with names of flag variables from `dataset` used to count adverse event sub-groups (e.g. Serious events,
#'   Related events, etc.). Variable labels are used as table row names if they exist.
#' @param flag_var_aesi ([teal.transform::choices_selected()] or `NULL`)\cr
#'   vector with names of flag variables from `dataset` used to count adverse event special interest groups. All flag
#'   variables must be of type `logical`. Variable labels are used as table row names if they exist.
#' @param aeseq_var ([teal.transform::choices_selected()])\cr variable for
#'   adverse events sequence number from `dataset`. Used for counting total number of events.
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
#' tm_t_events_summary(
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
#'   ADSL <- tmc_ex_adsl %>%
#'     mutate(
#'       DTHFL = case_when(
#'         !is.na(DTHDT) ~ "Y",
#'         TRUE ~ ""
#'       ) %>% with_label("Subject Death Flag")
#'     )
#'   ADAE <- tmc_ex_adae
#'
#'   .add_event_flags <- function(dat) {
#'     dat <- dat %>%
#'       mutate(
#'         TMPFL_SER = AESER == "Y",
#'         TMPFL_REL = AEREL == "Y",
#'         TMPFL_GR5 = AETOXGR == "5",
#'         TMP_SMQ01 = !is.na(SMQ01NAM),
#'         TMP_SMQ02 = !is.na(SMQ02NAM),
#'         TMP_CQ01 = !is.na(CQ01NAM)
#'       )
#'     column_labels <- list(
#'       TMPFL_SER = "Serious AE",
#'       TMPFL_REL = "Related AE",
#'       TMPFL_GR5 = "Grade 5 AE",
#'       TMP_SMQ01 = aesi_label(dat[["SMQ01NAM"]], dat[["SMQ01SC"]]),
#'       TMP_SMQ02 = aesi_label("Y.9.9.9.9/Z.9.9.9.9 AESI"),
#'       TMP_CQ01 = aesi_label(dat[["CQ01NAM"]])
#'     )
#'     col_labels(dat)[names(column_labels)] <- as.character(column_labels)
#'     dat
#'   }
#'
#'   #' Generating user-defined event flags.
#'   ADAE <- ADAE %>% .add_event_flags()
#'
#'   .ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
#'   .aesi_vars <- names(ADAE)[startsWith(names(ADAE), "TMP_")]
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_events_gtsummary(
#'       label = "Adverse Events Summary",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(
#'         choices = variable_choices("ADSL", c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       flag_var_anl = choices_selected(
#'         choices = variable_choices("ADAE", data[[".ae_anl_vars"]]),
#'         selected = data[[".ae_anl_vars"]][1],
#'         keep_order = TRUE,
#'         fixed = FALSE
#'       ),
#'       flag_var_aesi = choices_selected(
#'         choices = variable_choices("ADAE", data[[".aesi_vars"]]),
#'         selected = data[[".aesi_vars"]][1],
#'         keep_order = TRUE,
#'         fixed = FALSE
#'       ),
#'       add_total = TRUE
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_events_gtsummary <- function(label,
                                  dataname,
                                  parentname = ifelse(
                                    inherits(arm_var, "data_extract_spec"),
                                    teal.transform::datanames_input(arm_var),
                                    "ADSL"
                                  ),
                                  arm_var,
                                  flag_var_anl = NULL,
                                  flag_var_aesi = NULL,
                                  dthfl_var = teal.transform::choices_selected(
                                    teal.transform::variable_choices(parentname, "DTHFL"), "DTHFL",
                                    fixed = TRUE
                                  ),
                                  dcsreas_var = teal.transform::choices_selected(
                                    teal.transform::variable_choices(parentname, "DCSREAS"), "DCSREAS",
                                    fixed = TRUE
                                  ),
                                  llt = teal.transform::choices_selected(
                                    teal.transform::variable_choices(dataname, "AEDECOD"), "AEDECOD",
                                    fixed = TRUE
                                  ),
                                  aeseq_var = teal.transform::choices_selected(
                                    teal.transform::variable_choices(dataname, "AESEQ"), "AESEQ",
                                    fixed = TRUE
                                  ),
                                  add_total = TRUE,
                                  total_label = default_total_label(),
                                  na_level = tern::default_na_str(),
                                  count_dth = TRUE,
                                  count_wd = TRUE,
                                  count_subj = TRUE,
                                  count_pt = TRUE,
                                  count_events = TRUE,
                                  pre_output = NULL,
                                  post_output = NULL,
                                  basic_table_args = teal.widgets::basic_table_args(),
                                  transformators = list(),
                                  decorators = list()) {
  message("Initializing tm_t_events_gtsummary")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(flag_var_anl, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(flag_var_aesi, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(dthfl_var, "choices_selected")
  checkmate::assert_class(dcsreas_var, "choices_selected")
  checkmate::assert_class(llt, "choices_selected")
  checkmate::assert_class(aeseq_var, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(count_dth)
  checkmate::assert_flag(count_wd)
  checkmate::assert_flag(count_subj)
  checkmate::assert_flag(count_pt)
  checkmate::assert_flag(count_events)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname, multiple = TRUE, ordered = TRUE),
    dthfl_var = cs_to_des_select(dthfl_var, dataname = parentname),
    dcsreas_var = cs_to_des_select(dcsreas_var, dataname = parentname),
    flag_var_anl = `if`(
      is.null(flag_var_anl),
      NULL,
      cs_to_des_select(flag_var_anl, dataname = dataname, multiple = TRUE, ordered = TRUE)
    ),
    flag_var_aesi = `if`(
      is.null(flag_var_aesi),
      NULL,
      cs_to_des_select(flag_var_aesi, dataname = dataname, multiple = TRUE, ordered = TRUE)
    ),
    aeseq_var = cs_to_des_select(aeseq_var, dataname = dataname),
    llt = cs_to_des_select(llt, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_t_events_gtsummary,
    ui_args = c(data_extract_list, args),
    server = srv_t_events_gtsummary,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        total_label = total_label,
        na_level = na_level,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_t_events_gtsummary <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$dthfl_var,
    a$dcsreas_var,
    a$flag_var_anl,
    a$flag_var_aesi,
    a$aeseq_var,
    a$llt
  )

  teal.widgets::standard_layout(
    output = gt::gt_output(ns("table")),
    encoding = tags$div(
      ### Reporter
      teal.reporter::add_card_button_ui(ns("add_reporter"), label = "Add Report Card"),
      tags$br(), tags$br(),
      ###
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(
        a[c("arm_var", "dthfl_var", "dcsreas_var", "flag_var_anl", "flag_var_aesi", "aeseq_var", "llt")]
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      `if`(
        is.null(a$flag_var_anl),
        NULL,
        teal.transform::data_extract_ui(
          id = ns("flag_var_anl"),
          label = "Event Flag Variables",
          data_extract_spec = a$flag_var_anl,
          is_single_dataset = is_single_dataset_value
        )
      ),
      `if`(
        is.null(a$flag_var_aesi),
        NULL,
        teal.transform::data_extract_ui(
          id = ns("flag_var_aesi"),
          label = "AE Basket Flag Variables",
          data_extract_spec = a$flag_var_aesi,
          is_single_dataset = is_single_dataset_value
        )
      ),
      checkboxInput(
        ns("add_total"),
        "Add All Patients column",
        value = a$add_total
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion_panel(
        "Table Settings",
        open = TRUE,
        checkboxInput(
          ns("count_dth"),
          "Count deaths",
          value = a$count_dth
        ),
        checkboxInput(
          ns("count_wd"),
          "Count withdrawals due to AE",
          value = a$count_wd
        ),
        checkboxInput(
          ns("count_subj"),
          "Count patients",
          value = a$count_subj
        ),
        checkboxInput(
          ns("count_pt"),
          "Count preferred terms",
          value = a$count_pt
        ),
        checkboxInput(
          ns("count_events"),
          "Count events",
          value = a$count_events
        )
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          teal.transform::data_extract_ui(
            id = ns("dthfl_var"),
            label = "Death Flag Variable",
            data_extract_spec = a$dthfl_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.transform::data_extract_ui(
            id = ns("dcsreas_var"),
            label = "Study Discontinuation Reason Variable",
            data_extract_spec = a$dcsreas_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.transform::data_extract_ui(
            id = ns("aeseq_var"),
            label = "AE Sequence Variable",
            data_extract_spec = a$aeseq_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.transform::data_extract_ui(
            id = ns("llt"),
            label = "AE Term Variable",
            data_extract_spec = a$llt,
            is_single_dataset = is_single_dataset_value
          )
        )
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


#' @keywords internal
srv_t_events_gtsummary <- function(id,
                                   data,
                                   reporter,
                                   filter_panel_api,
                                   dataname,
                                   parentname,
                                   arm_var,
                                   dthfl_var,
                                   dcsreas_var,
                                   flag_var_anl,
                                   flag_var_aesi,
                                   aeseq_var,
                                   llt,
                                   label,
                                   total_label,
                                   na_level,
                                   basic_table_args,
                                   decorators) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    data_extract_vars <- list(
      arm_var = arm_var, dthfl_var = dthfl_var, dcsreas_var = dcsreas_var,
      aeseq_var = aeseq_var, llt = llt
    )

    if (!is.null(flag_var_anl)) {
      data_extract_vars[["flag_var_anl"]] <- flag_var_anl
    }

    if (!is.null(flag_var_aesi)) {
      data_extract_vars[["flag_var_aesi"]] <- flag_var_aesi
    }

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = data_extract_vars,
      datasets = data,
      select_validation_rule = list(
        arm_var = ~ if (length(.) != 1 && length(.) != 2) "Please select exactly 1 or 2 treatment variables",
        dthfl_var = shinyvalidate::sv_required("Death Flag Variable is requried"),
        dcsreas_var = shinyvalidate::sv_required("Study Discontinuation Reason Variable is required"),
        aeseq_var = shinyvalidate::sv_required("AE Sequence Variable is required"),
        llt = shinyvalidate::sv_required("AE Term Variable is required")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = Filter(Negate(is.null), list(arm_var = arm_var, dthfl_var = dthfl_var, dcsreas_var = dcsreas_var)),
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

    validate_checks <- reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_dthfl_var <- as.vector(merged$anl_input_r()$columns_source$dthfl_var)
      input_dcsreas_var <- as.vector(merged$anl_input_r()$columns_source$dcsreas_var)
      input_flag_var_anl <- if (!is.null(flag_var_anl)) {
        as.vector(merged$anl_input_r()$columns_source$flag_var_anl)
      } else {
        NULL
      }
      input_flag_var_aesi <- if (!is.null(flag_var_anl)) {
        as.vector(merged$anl_input_r()$columns_source$flag_var_aesi)
      } else {
        NULL
      }
      input_aeseq_var <- as.vector(merged$anl_input_r()$columns_source$aeseq_var)
      input_llt <- as.vector(merged$anl_input_r()$columns_source$llt)

      validate(
        need(
          is.factor(adsl_filtered[[input_arm_var[[1]]]]) && is.factor(anl_filtered[[input_arm_var[[1]]]]),
          "The treatment variable selected must be a factor variable in all datasets used."
        ),
        if (length(input_arm_var) == 2) {
          need(
            is.factor(adsl_filtered[[input_arm_var[[2]]]]) && all(!adsl_filtered[[input_arm_var[[2]]]] %in% c(
              "", NA
            )),
            "Please check nested treatment variable which needs to be a factor without NA or empty strings."
          )
        },
        need(
          identical(levels(adsl_filtered[[input_arm_var[[1]]]]), levels(anl_filtered[[input_arm_var[[1]]]])),
          "The treatment variable selected must have the same levels across all datasets used."
        )
      )

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_dthfl_var, input_dcsreas_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_flag_var_anl, input_flag_var_aesi, input_aeseq_var, input_llt),
        arm_var = input_arm_var[[1]]
      )
    })

    # The R-code corresponding to the analysis.
    table_pre_q <- reactive({

      #' count_dth (`logical`)\cr whether to show count of total deaths (based on `dthfl_var`). Defaults to `TRUE`.
      #' count_wd (`logical`)\cr whether to show count of patients withdrawn from study due to an adverse event
      #'   (based on `dcsreas_var`). Defaults to `TRUE`.

      # If events variable:
      #' count_subj (`logical`)\cr whether to show count of unique subjects (based on `USUBJID`).
      #' count_pt (`logical`)\cr whether to show count of unique preferred terms (based on `llt`).
      #' count_events (`logical`)\cr whether to show count of events (based on `aeseq_var`).

      # If withdrawal:
      # "AE leading to withdrawal from study"
      # "AE leading to withdrawal from treatment"

      # TODO: Is this the right ADAM variable to check withdraw from study?
      flag_withdrawl_study <- any(grepl("WITHDRAWN", data()$ADAE$DCSREAS))

      tdc <- within(
        data(),
        {
          # Assuming the data we receive is already filtered
          library("rlang")
          library("gtsummary")
          library("dplyr")
          selection_AEACN <- c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED")
          vars <- c("DTHFL") # , "AEWITHFL"
          # add variable labels, which will be used in the table below
          labels <- list(
            # Those that must be (DTHFL and AEWITHFL are given more descriptive titles)
            ae_count = "Total number of AEs",
            DTHFL = "Total number of deaths",
            # AEWITHFL = "Total number of participants withdrawn from study due to an AE",
            # Those that are calculated
            ae_any = "Total number of participants with at least one AE",
            # ae_death = "AE with fatal outcome",
            ae_serious = "Serious AE",
            ae_ser_withdraw = "Serious AE leading to withdrawal from treatment",
            ae_ser_acn = "Serious AE leading to dose modification/interruption",
            ae_sae_rel = "Related Serious AE",
            ae_withdraw = "AE leading to withdrawal from treatment",
            ae_acn = "AE leading to modification/interruption",
            ae_rel = "Related AE",
            ae_rel_withdraw = "Related AE leading to withdrawal from treatment",
            ae_rel_acn = "Related AE leading to dose modication/interruption",
            ae_sev = "Severe AE (at greatest intensity)"
          )

          # This one is dynamic and should be available
          df_table <- ADSL %>%
            select("USUBJID", by, vars) %>%
            # recode Y/N/NA to TRUE/FALSE to summarize dichotomously below
            mutate(
              across(!!!vars, ~ case_match(., "Y" ~ TRUE, .default = FALSE))
            ) %>%
            # create subject-level flags from ADAE data set
            dunlin::subject_level_flag(
              data_long = ADAE,
              # Any AE
              ae_any = TRUE,
              # Serious AE leading to withdrawal from treatment
              ae_ser_withdraw = AESER == "Y" & AEACN == "DRUG WITHDRAWN",
              # Serious AE leading to dose modification/interruption
              ae_ser_acn = AESER == "Y" & AEACN %in% selection_AEACN,
              # Related Serious AE
              ae_sae_rel = AESER == "Y" & AEREL == "Y",
              # AE Leading to withdrawal from treatment
              ae_withdraw = AEACN == "DRUG WITHDRAWN",
              # AE leading to modification/interruption
              ae_acn = AEACN %in% selection_AEACN,
              # Related AE
              ae_rel = AEREL == "Y",
              # Related AE leading to withdrawal from treatment
              ae_rel_withdraw = AEREL == "Y" & AEACN == "DRUG WITHDRAWN",
              # Related AE leading to dose modification/interruption
              ae_rel_acn = AEREL == "Y" & AEACN %in% selection_AEACN,
              # Severe AE (at greatest intensity)
              ae_sev = AESEV == "SEVERE",
              # AE with fatal outcome
              # ae_death = AESDTH == "Y",
              # Serious AE
              ae_serious = AESER == "Y"
            ) %>%
            # add number of AEs
            left_join(
              summarise(ADAE, .by = USUBJID, ae_count = n()),
              by = "USUBJID",
              relationship = "one-to-one"
            )


          table <-  tbl_summary(
            df_table,
            label = labels,
            include = names(labels),
            by = by, # split table by group
            missing = "no" # don't list missing data separately
          ) %>%
            gtsummary::modify_header(label = "**Variable**") %>%
            bold_labels()
        },
        by = data_extract_vars$arm_var$select$selected

      )

      tdc
    })

    if (input$count_pt) {
      table_q <- reactive({
        req(table_pre_q())
        within(table_pre_q(), {
          table <- add_n(table, col_label = "All patients", last = TRUE)
        })
      })
    } else {
      table_q <- table_pre_q
    }

    # Outputs to render.
    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = table_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    output$table <- gt::render_gt({
      req(is(table_r(), "tbl_summary"))
      gtsummary::as_gt(table_r())
    })
    # teal.widgets::table_with_settings_srv(
    #   id = "table",
    #   table_r = table_r
    # )

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
          title = "Adverse Events Summary Table",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Table", "header3")
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
    ###
  })
}
