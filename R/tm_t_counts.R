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
                        # visit_var,
                        # cov_var,
                        arm_ref_comp = NULL,
                        paramcd,
                        strata_var,
                        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                        # pre_output = NULL,
                        # post_output = NULL,
                        # basic_table_args = teal.widgets::basic_table_args(),
                        transformators = list(),
                        decorators = list()) {

  message("Initializing tm_t_counts")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  # checkmate::assert_string(parentname)
  # checkmate::assert_class(arm_var, "choices_selected")
  # checkmate::assert_class(seq_var, "choices_selected")
  # checkmate::assert_class(hlt, "choices_selected")
  # checkmate::assert_class(llt, "choices_selected")
  # checkmate::assert_string(event_type)
  # checkmate::assert_string(title_text)
  # checkmate::assert_flag(add_total)
  # checkmate::assert_string(total_label)
  # checkmate::assert_string(na_level)
  # checkmate::assert_flag(drop_arm_levels)
  # checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  # checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  # checkmate::assert_class(basic_table_args, "basic_table_args")

  # assert_decorators(decorators, "table")

  args <- as.list(environment())

  # data_extract_list <- list(
  #   arm_var = cs_to_des_select(arm_var, dataname = parentname),
  #   seq_var = cs_to_des_select(seq_var, dataname = dataname),
  #   hlt = cs_to_des_select(hlt, dataname = dataname, multiple = TRUE, ordered = TRUE),
  #   llt = cs_to_des_select(llt, dataname = dataname)
  # )
  data_extract_list <- list()

  teal::module(
    label = label,
    datanames = "all",
    ui = ui_counts,
    server = srv_counts,
    ui_args = c(data_extract_list, args),
    server_args = c(data_extract_list,
                    list(
                      dataname = dataname,
                      # parentname = parentname,
                      # arm_ref_comp = arm_ref_comp,
                      # label = label,
                      # basic_table_args = basic_table_args,
                      decorators = decorators
                    )
    ),
    transformators = transformators
    # datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}


ui_counts <-  function(id, ...) {
  a <- list(...) # module args

  # is_single_dataset_value <- teal.transform::is_single_dataset(
  #   a$arm_var,
  #   a$paramcd,
  #   a$id_var,
  #   a$visit_var,
  #   a$cov_var,
  #   a$aval_var
  # )

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::table_with_settings_ui(ns("table")
      )
    ),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"), tags$br(),
      shiny::selectInput(ns("dataset"),
                         "Select dataset",
                         choices = c("ADSL", "ADTTE"),
                         selected = "ADTTE"
      ),
      shiny::selectInput(
        ns("vars"),
        "Analysis Variable",
        choices = c("ARM", "ARMCD")
      ),
      shiny::selectInput(
        ns("covariates"),
        "Covariates",
        choices = c("var1", "var2")
      ),
      shiny::selectInput(
        ns("offset"),
        "Offset",
        choices = c("var1", "var2")
      ),
      shiny::selectInput(
        ns("strata"),
        "Stratify",
        choices = c("SEX", "SITE", "AGE")
      ),

      tags$div(
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
            shiny::selectInput(
              ns("arm"),
              "Group variable",
              choices = c("SITE", "SEX")
            ),
            shiny::selectInput(
              ns("group_ref"),
              "Ref",
              choices = c("A", "B", "C")
            ),
            shiny::selectInput(
              ns("group_ref"),
              "Comp",
              choices = c("A", "B", "C"),
              selected = c("B", "C"),
              multiple = TRUE
            )
          )
        )
      ),
      shiny::selectInput(
        ns("distribution"),
        "Distribution",
        choices = c("quasipoisson", "negbin", "poisson")
      ),
      shiny::selectInput(
        ns("rate_mean_method"),
        "Rate method",
        choices = c("emmeans", "ppmeans")
      ),
      bslib::accordion_panel(
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
      ),
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = tags$div(
      tags$h2("Output of the code"),
      tags$a("note: UI is not used yet to modify the server part of the module"),
    ),
    post_output = NULL
  )
}

srv_counts <- function(id,
                       data,
                       # filter_panel_api,
                       # reporter,
                       dataname,
                       parentname,
                       arm_var,
                       # paramcd,
                       # id_var,
                       # visit_var,
                       # cov_var,
                       # split_covariates,
                       # aval_var,
                       # arm_ref_comp,
                       # label,
                       # plot_height,
                       # plot_width,
                       # basic_table_args,
                       decorators) {
  moduleServer(id, function(input, output, session) {

    # with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
    # with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
    # checkmate::assert_class(data, "reactive")
    # checkmate::assert_class(isolate(data()), "teal_data")


    # Add library calls
    add_pkg_loads <- reactive({
      within(data(), {
        library("dplyr")
        library("tern")
        library("rtables")
      })
    })

    # Preprocessing of the data
    anl <- reactive({
      within(add_pkg_loads(), {
        anl <- dplyr::filter(ADTTE, PARAMCD == "TNE")
        anl$AVAL_f <- as.factor(anl$AVAL)
        anl <- df_explicit_na(anl)
      })
    })

    # Add basic specification for the table
    basic_table <- reactive({
      logger::log_info("basic table")
      within(anl(), {
        lyt <- rtables::basic_table(show_colcounts = TRUE) %>%
          rtables::split_cols_by(var, ref_group = ref_group, split_fun = tern::ref_group_position("first"))
      },
      ref_group = input$group_ref,
      var = input$vars
      )
    })

    # Create covariates for the table
    variables <- list(arm = input$var, covariates = input$covariates, offset = input$offset)
    # Create tables
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

