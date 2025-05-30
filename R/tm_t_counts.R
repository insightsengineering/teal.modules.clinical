tm_t_counts <- function(label = "Counts Module",
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
                        # id_var,
                        arm_var,
                        strata_var,
                        rate_mean_method = c("emmeans", "ppmeans"),
                        distribution = c("quasipoisson", "negbin", "poisson"),
                        offset_var,
                        cov_var,
                        arm_ref_comp = NULL,
                        paramcd,
                        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.widgets::basic_table_args(),
                        transformators = list(),
                        decorators = list()) {

  message("Initializing tm_t_counts")
  # checkmate::assert_string(label)
  # checkmate::assert_string(dataname)
  # rate_mean_method <- match.arg(rate_mean_method)
  # distribution <- match.arg(distribution)
  # checkmate::assert_string(parentname)
  # checkmate::assert_class(arm_var, "choices_selected")
  # checkmate::assert_class(paramcd, "choices_selected")
  # checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  # checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  # checkmate::assert_class(basic_table_args, "basic_table_args")
  # assert_decorators(decorators, "table")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = dataname),
    paramcd = cs_to_des_select(paramcd, dataname = dataname),
    offset_var = cs_to_des_select(offset_var, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = dataname)
  #   hlt = cs_to_des_select(hlt, dataname = dataname, multiple = TRUE, ordered = TRUE),
  #   llt = cs_to_des_select(llt, dataname = dataname)
  )

  teal::module(
    label = label,
    ui = ui_counts,
    server = srv_counts,
    ui_args = c(data_extract_list, args),
    server_args = c(data_extract_list,
                    list(
                      dataname = dataname,
                      parentname = parentname,
                      arm_ref_comp = arm_ref_comp,
                      # Arguments on data_extract_list:
                      # paramcd = paramcd,
                      # cov_var = cov_var,
                      # aval_var = aval_var,
                      # arm_var = arm_var,
                      # strata_var = strata_var,
                      label = label,
                      basic_table_args = basic_table_args,
                      decorators = decorators
                    )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}


ui_counts <-  function(id, ...) {
  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$offset_var,
    a$cov_var,
    a$aval_var
  )
  output <- teal.widgets::white_small_well(
    teal.widgets::table_with_settings_ui(ns("table")
    )
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
      value = FALSE,
      width = "100%"
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
        # TODO replace by data_extract_ui as in tm_t_tte.R L646
      )
    )
  )

  table_settings <- bslib::accordion_panel(
    "Additional table settings",
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
      inputId = ns("conf_level"),
      label = "Confidence Level",
      choices = c(0.8, 0.9, 0.95, 0.99),
      selected = 0.95,
      multiple = FALSE,
      fixed = FALSE
    ),
    ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
  )

  teal.widgets::standard_layout(
    output = output,
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"), tags$br(),
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
        ns("arm_var"),
        "Group variable",
        data_extract_spec = a$arm_var,
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
    forms = forms
  )
}

srv_counts <- function(id,
                       data,
                       filter_panel_api,
                       reporter,
                       dataname,
                       parentname,
                       arm_var,
                       paramcd,
                       aval_var,
                       offset_var,
                       # paramcd,
                       # id_var,
                       # visit_var,
                       cov_var,
                       strata_var,
                       # split_covariates,
                       arm_ref_comp,
                       label,
                       # plot_height,
                       # plot_width,
                       basic_table_args,
                       decorators) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {

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

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        aval_var = aval_var,
        strata_var = strata_var,
        offset_var = offset_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("An endpoint is required")
      )
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

      iv$add_rule("conf_level_coxph", shinyvalidate::sv_required("Please choose a hazard ratio confidence level"))
      iv$add_rule(
        "conf_level_coxph", shinyvalidate::sv_between(
          0, 1,
          message_fmt = "Hazard ratio confidence level must between 0 and 1"
        )
      )
      iv$add_rule("conf_level_survfit", shinyvalidate::sv_required("Please choose a KM confidence level"))
      iv$add_rule(
        "conf_level_survfit", shinyvalidate::sv_between(
          0, 1,
          message_fmt = "KM confidence level must between 0 and 1"
        )
      )
      iv$add_rule(
        "probs_survfit",
        ~ if (!is.null(.) && .[1] == .[2]) "KM Estimate Percentiles cannot have a range of size 0"
      )
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })
    validate_checks <- reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      anl_m <- anl_merge_inputs()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
      input_event_desc <- as.vector(anl_m$columns_source$event_desc_var)
      input_time_unit_var <- as.vector(anl_m$columns_source$time_unit_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_paramcd, input_aval_var,
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

    # Processing
    ## Data source merging
    anl_merge_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )
    adsl_merge_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      join_keys = teal.data::join_keys(data),
      data_extract = list(arm_var = arm_var, strata_var = strata_var),
      anl_name = "ANL_ADSL"
    )

    ## Add library calls
    add_pkg_loads <- reactive({
      within(data(), {
        library("dplyr")
        library("tern")
        library("rtables")
      })
    })
    ## Merge data
    anl_q <- reactive({
      add_pkg_loads() %>%
        teal.code::eval_code(as.expression(anl_merge_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_merge_inputs()$expr))
    })

    ##  Preprocessing the data
    anl <- reactive({
      within(add_pkg_loads(), {
        anl <- dplyr::filter(ADTTE, PARAMCD == "TNE")
        anl$AVAL_f <- as.factor(anl$AVAL)
        anl <- df_explicit_na(anl)
      })
    })
    # TODO fix the reference and var group iff needed
    browser()
    ## Add basic specification for the table
    basic_table <- reactive({
      within(anl(), {
        lyt <- rtables::basic_table(show_colcounts = TRUE) %>%
          rtables::split_cols_by(var, ref_group = ref_group, split_fun = tern::ref_group_position("first"))
      },
      ref_group = input$arm_var,
      var = input$vars_vars
      )
    })

    ## Create covariates for the table
    variables <- list(arm = input$arm_var, covariates = input$cov_var, offset = input$offset_var)
    ## Create tables
    summarize_counts <- reactive({
      logger::log_info("counts")
      within(basic_table(), {
        summarized_counts <- tern::summarize_glm_count(lyt,
                            vars = var,
                            variables = variables,
                            conf_level = conf_level,
                            distribution = distribution,
                            rate_mean_method = method,
                            var_labels = "Adjusted (NB) exacerbation rate (per year)",
                            table_names = "adj-nb",
                            .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
                            .labels = c(
                              rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
                              rate_ratio_ci = "Rate Ratio CI", pval = "p-value"
                            ))
      },
      var = input$vars,
      variables = variables,
      conf_level = as.numeric(input$conf_level),
      method = input$rate_mean_method,
      distribution = input$distribution)
    })
    # Add unstratified rows
    unstratified <- reactive({
      logger::log_info("Unstratified")
      within(summarize_counts(), {

      })
    })
    # Add stratified rows
    stratified <- reactive({
      logger::log_info("stratified")
      within(unstratified(), {

      })
    })

    # Add/fix p-value
    pvalues <- reactive({
      logger::log_info("pvalues")
      within(stratified(), {

      })
    })

    # Create output table
    table <- reactive({
      within(pvalues(), {
        table <- build_table(
          lyt = lyt,
          df = anl
        )
        table
      })
    })

    # output$out <- shiny::renderText({get_code(table())})

    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = table(),
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

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Time To Count Table",
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
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}

