#' @describeIn tm_t_tte [`teal.picks::picks()`]-based `event_desc_var`.
#' @export
tm_t_tte.picks <- function(label,
                           dataname,
                           parentname = "ADSL",
                           arm_var = variables(choices = c("ARM", "ARMCD", "ACTARMCD")),
                           arm_ref_comp = NULL,
                           paramcd = variables(choices = "PARAMCD"),
                           strata_var = variables(
                             choices = c("SEX", "BMRKR2"),
                             selected = "SEX"
                           ),
                           aval_var = variables(choices = "AVAL", fixed = TRUE),
                           cnsr_var = variables(choices = "CNSR", fixed = TRUE),
                           conf_level_coxph = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                           conf_level_survfit = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                           time_points = teal.transform::choices_selected(c(182, 243), 182),
                           time_unit_var = variables(choices = "AVALU", fixed = TRUE),
                           event_desc_var = variables(choices = "EVNTDESC", fixed = TRUE),
                           add_total = FALSE,
                           total_label = default_total_label(),
                           na_level = tern::default_na_str(),
                           pre_output = NULL,
                           post_output = NULL,
                           basic_table_args = teal.widgets::basic_table_args(),
                           transformators = list(),
                           decorators = list()) {
  arm_var <- migrate_choices_selected_to_variables(arm_var, arg_name = "arm_var")
  paramcd <- migrate_value_choices_to_picks(paramcd, multiple = FALSE, arg_name = "paramcd")
  strata_var <- migrate_choices_selected_to_variables(strata_var, arg_name = "strata_var")
  aval_var <- migrate_choices_selected_to_variables(aval_var, arg_name = "aval_var")
  cnsr_var <- migrate_choices_selected_to_variables(cnsr_var, arg_name = "cnsr_var")
  time_unit_var <- migrate_choices_selected_to_variables(time_unit_var, arg_name = "time_unit_var")
  event_desc_var <- migrate_choices_selected_to_variables(event_desc_var, arg_name = "event_desc_var")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(conf_level_coxph, "choices_selected")
  checkmate::assert_class(conf_level_survfit, "choices_selected")
  checkmate::assert_class(time_points, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  arm_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), arm_var)
  strata_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), strata_var)
  paramcd <- create_picks_helper(teal.picks::datasets(dataname, dataname), paramcd)
  aval_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), aval_var)
  cnsr_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), cnsr_var)
  time_unit_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), time_unit_var)
  event_desc_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), event_desc_var)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_tte,
    ui = ui_t_tte,
    ui_args = args[names(args) %in% names(formals(ui_t_tte))],
    server_args = args[names(args) %in% names(formals(srv_t_tte))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @describeIn tm_t_tte [`teal.picks::variables()`] for `event_desc_var` (same as [`tm_t_tte.picks()`]).
#' @export
tm_t_tte.variables <- tm_t_tte.picks

#' @keywords internal
#' @noRd
tm_t_tte_legacy_event_desc <- function(label,
                                       dataname,
                                       parentname = "ADSL",
                                       arm_var = variables(choices = c("ARM", "ARMCD", "ACTARMCD")),
                                       arm_ref_comp = NULL,
                                       paramcd = variables(choices = "PARAMCD"),
                                       strata_var = variables(
                                         choices = c("SEX", "BMRKR2"),
                                         selected = "SEX"
                                       ),
                                       aval_var = variables(choices = "AVAL", fixed = TRUE),
                                       cnsr_var = variables(choices = "CNSR", fixed = TRUE),
                                       conf_level_coxph = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                                       conf_level_survfit = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                                       time_points = teal.transform::choices_selected(c(182, 243), 182),
                                       time_unit_var = variables(choices = "AVALU", fixed = TRUE),
                                       event_desc_var,
                                       add_total = FALSE,
                                       total_label = default_total_label(),
                                       na_level = tern::default_na_str(),
                                       pre_output = NULL,
                                       post_output = NULL,
                                       basic_table_args = teal.widgets::basic_table_args(),
                                       transformators = list(),
                                       decorators = list()) {
  event_desc_var <- migrate_list_extract_spec_to_picks(event_desc_var, arg_name = "event_desc_var")
  tm_t_tte.picks(
    label = label,
    dataname = dataname,
    parentname = parentname,
    arm_var = arm_var,
    arm_ref_comp = arm_ref_comp,
    paramcd = paramcd,
    strata_var = strata_var,
    aval_var = aval_var,
    cnsr_var = cnsr_var,
    conf_level_coxph = conf_level_coxph,
    conf_level_survfit = conf_level_survfit,
    time_points = time_points,
    time_unit_var = time_unit_var,
    event_desc_var = event_desc_var,
    add_total = add_total,
    total_label = total_label,
    na_level = na_level,
    pre_output = pre_output,
    post_output = post_output,
    basic_table_args = basic_table_args,
    transformators = transformators,
    decorators = decorators
  )
}

#' @describeIn tm_t_tte Legacy [`teal.transform::data_extract_spec()`] for `event_desc_var`.
#' @export
tm_t_tte.default <- tm_t_tte_legacy_event_desc

#' @describeIn tm_t_tte Legacy `list` of [`teal.transform::data_extract_spec()`] for `event_desc_var`.
#' @export
tm_t_tte.list <- tm_t_tte_legacy_event_desc

#' @keywords internal
ui_t_tte <- function(id,
                     arm_var,
                     arm_ref_comp,
                     paramcd,
                     aval_var,
                     cnsr_var,
                     strata_var,
                     event_desc_var,
                     time_unit_var,
                     time_points,
                     conf_level_coxph,
                     conf_level_survfit,
                     add_total,
                     pre_output,
                     post_output,
                     decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Endpoint"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Analysis Variable"),
        teal.picks::picks_ui(ns("aval_var"), aval_var)
      ),
      tags$div(
        tags$label("Censor Variable"),
        teal.picks::picks_ui(ns("cnsr_var"), cnsr_var)
      ),
      tags$div(
        tags$label("Select Treatment Variable"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      ),
      tags$div(
        class = "arm-comp-box",
        bslib::input_switch(
          id = ns("compare_arms"),
          label = "Compare Treatments",
          value = !is.null(arm_ref_comp)
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
            tags$div(
              tags$label("Stratify by"),
              teal.picks::picks_ui(ns("strata_var"), strata_var)
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("!input['", ns("compare_arms"), "']"),
        checkboxInput(ns("add_total"), "Add All Patients column", value = add_total)
      ),
      teal.widgets::optionalSelectInput(ns("time_points"),
        "Time Points",
        time_points$choices,
        time_points$selected,
        multiple = TRUE,
        fixed = time_points$fixed
      ),
      tags$div(
        tags$label("Event Description Variable"),
        teal.picks::picks_ui(ns("event_desc_var"), event_desc_var)
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        bslib::accordion_panel(
          "Comparison settings",
          radioButtons(
            ns("pval_method_coxph"),
            label = HTML(
              paste(
                "p-value method for ",
                tags$span(class = "text-primary", "Coxph"),
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            choices = c("wald", "log-rank", "likelihood"),
            selected = "log-rank"
          ),
          radioButtons(
            ns("ties_coxph"),
            label = HTML(
              paste(
                "Ties for ",
                tags$span(class = "text-primary", "Coxph"),
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            choices = c("exact", "breslow", "efron"),
            selected = "exact"
          ),
          teal.widgets::optionalSelectInput(
            inputId = ns("conf_level_coxph"),
            label = HTML(
              paste(
                "Confidence Level for ",
                tags$span(class = "text-primary", "Coxph"),
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            conf_level_coxph$choices,
            conf_level_coxph$selected,
            multiple = FALSE,
            fixed = conf_level_coxph$fixed
          )
        )
      ),
      bslib::accordion_panel(
        "Additional table settings",
        open = TRUE,
        teal.widgets::optionalSelectInput(
          inputId = ns("conf_level_survfit"),
          label = HTML(
            paste(
              "Confidence Level for ",
              tags$span(class = "text-primary", "Survfit"),
              " (KM Median Estimate & Event Free Rate)",
              sep = ""
            )
          ),
          conf_level_survfit$choices,
          conf_level_survfit$selected,
          multiple = FALSE,
          fixed = conf_level_survfit$fixed
        ),
        radioButtons(
          ns("conf_type_survfit"),
          "Confidence Level Type for Survfit",
          choices = c("plain", "log", "log-log"),
          selected = "plain"
        ),
        sliderInput(
          inputId = ns("probs_survfit"),
          label = "KM Estimate Percentiles",
          min = 0.01,
          max = 0.99,
          value = c(0.25, 0.75),
          width = "100%"
        ),
        tags$div(
          tags$label("Time Unit Variable"),
          teal.picks::picks_ui(ns("time_unit_var"), time_unit_var)
        )
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table")),
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_t_tte <- function(id,
                      data,
                      arm_var,
                      paramcd,
                      aval_var,
                      cnsr_var,
                      strata_var,
                      event_desc_var,
                      dataname,
                      parentname,
                      arm_ref_comp,
                      time_unit_var,
                      add_total,
                      total_label,
                      label,
                      na_level,
                      basic_table_args,
                      decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        arm_var = arm_var,
        paramcd = paramcd,
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        strata_var = strata_var,
        event_desc_var = event_desc_var,
        time_unit_var = time_unit_var
      ),
      data = data
    )

    arm_var_r <- reactive(selectors$arm_var()$variables$selected)

    arm_ref_comp_iv <- arm_ref_comp_observer_picks(
      session,
      input,
      output,
      id_arm_var = "arm_var-variables-selected",
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_tte",
      on_off = reactive(input$compare_arms),
      arm_var_r = arm_var_r
    )

    output$helptext_ui <- renderUI({
      req(selectors$arm_var()$variables$selected)
      helpText("Multiple reference groups are automatically combined into a single group.")
    })

    anl_selectors <- selectors
    adsl_selectors <- selectors[c("arm_var", "strata_var")]

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
    merged_adsl_anl <- merge_srv(
      "merge_adsl_anl",
      data = merged_anl$data,
      selectors = adsl_selectors,
      output_name = "ANL_ADSL"
    )
    anl_q <- merged_adsl_anl$data

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- reactive({
      if (isTRUE(input$compare_arms)) {
        arm_ref_comp_iv()
      }
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      input_arm_var <- anl_selectors$arm_var()$variables$selected
      input_strata_var <- anl_selectors$strata_var()$variables$selected
      input_aval_var <- anl_selectors$aval_var()$variables$selected
      input_cnsr_var <- anl_selectors$cnsr_var()$variables$selected
      input_event_desc <- anl_selectors$event_desc_var()$variables$selected
      input_time_unit_var <- anl_selectors$time_unit_var()$variables$selected
      input_paramcd_var <- anl_selectors$paramcd()$variables$selected
      input_paramcd_values <- anl_selectors$paramcd()$values$selected
      validate(shiny::need(
        length(input_paramcd_values) >= 1L,
        "Please select at least one parameter value."
      ))

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_paramcd_var, input_aval_var,
          input_cnsr_var, input_event_desc, input_time_unit_var
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
          list(
            ref_arm = arm_bucket_values(input$buckets, "Ref"),
            comp_arm = arm_bucket_values(input$buckets, "Comp")
          )
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

    # The R-code corresponding to the analysis.
    all_q <- reactive({
      validate_checks()

      input_strata_var <- anl_selectors$strata_var()$variables$selected

      my_calls <- template_tte(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = anl_selectors$arm_var()$variables$selected,
        paramcd = anl_selectors$paramcd()$variables$selected,
        ref_arm = arm_bucket_values(input$buckets, "Ref"),
        comp_arm = arm_bucket_values(input$buckets, "Comp"),
        compare_arm = input$compare_arms,
        combine_comp_arms = input$combine_comp_arms && input$compare_arms,
        aval_var = anl_selectors$aval_var()$variables$selected,
        cnsr_var = anl_selectors$cnsr_var()$variables$selected,
        strata_var = if (length(input_strata_var) != 0) input_strata_var else NULL,
        time_points = as.numeric(input$time_points),
        time_unit_var = anl_selectors$time_unit_var()$variables$selected,
        event_desc_var = anl_selectors$event_desc_var()$variables$selected,
        control = control_tte(
          coxph = tern::control_coxph(
            pval_method = input$pval_method_coxph,
            ties = input$ties_coxph,
            conf_level = as.numeric(input$conf_level_coxph)
          ),
          surv_time = control_surv_time(
            conf_level = as.numeric(input$conf_level_survfit),
            conf_type = input$conf_type_survfit,
            quantiles = input$probs_survfit
          ),
          surv_timepoint = tern::control_surv_timepoint(
            conf_level = as.numeric(input$conf_level_survfit),
            conf_type = input$conf_type_survfit
          )
        ),
        add_total = input$add_total,
        total_label = total_label,
        na_level = na_level,
        basic_table_args = basic_table_args
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(id = "table", table_r = table_r)

    decorated_table_q
  })
}
