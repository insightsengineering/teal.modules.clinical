#' @export
tm_a_mmrm.default <- function(label,
                              dataname,
                              parentname = "ADSL",
                              aval_var = teal.picks::variables(c("AVAL", "CHG")),
                              id_var = teal.picks::variables(c("USUBJID", "SUBJID")),
                              arm_var = teal.picks::variables(c("ARM", "ARMCD")),
                              visit_var = teal.picks::variables(c("AVISIT", "AVISITN")),
                              cov_var = teal.picks::variables(selected = NULL),
                              arm_ref_comp = NULL,
                              paramcd = lifecycle::deprecated(),
                              paramcd_var = teal.picks::variables(c("PARAMCD", "PARAM")),
                              paramcd_values = teal.picks::values(tidyselect::everything()),
                              method = teal.picks::values(
                                c("Satterthwaite", "Kenward-Roger", "Kenward-Roger-Linear"),
                                "Satterthwaite"
                              ),
                              conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95"),
                              plot_height = c(700L, 200L, 2000L),
                              plot_width = NULL,
                              total_label = default_total_label(),
                              pre_output = NULL,
                              post_output = NULL,
                              basic_table_args = teal.widgets::basic_table_args(),
                              ggplot2_args = teal.widgets::ggplot2_args(),
                              transformators = list(),
                              decorators = list(),
                              ...) {
  message("Initializing tm_a_mmrm")
  checkmate::assert_string(label)
  checkmate::assert_string(total_label)
  checkmate::assert_string(dataname)
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(id_var, "variables")
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(visit_var, "variables")
  checkmate::assert_class(cov_var, "variables")
  if (!missing(paramcd)) {
    lifecycle::deprecate_warn(
      when = "0.13.0",
      what = "tm_a_mmrm(paramcd)",
      details = "Use of paramcd was removed in `tm_a_mmrm` module usage with teal.picks"
    )
  }
  checkmate::assert_class(paramcd_var, "variables")
  checkmate::assert_class(paramcd_values, "values")
  checkmate::assert_class(method, "values")
  checkmate::assert_class(conf_level, "values")
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(
    plot_height[1],
    lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height"
  )
  checkmate::assert_numeric(
    plot_width,
    len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE
  )
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  if (inherits(ggplot2_args, "ggplot2_args")) {
    ggplot2_args <- list(default = ggplot2_args)
  }
  plot_choices <- c("lsmeans", "diagnostic")
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  assert_decorators(
    decorators,
    c(
      "lsmeans_table",
      "lsmeans_plot",
      "covariance_table",
      "fixed_effects_table",
      "diagnostic_table",
      "diagnostic_plot"
    )
  )

  aval_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), aval_var)
  id_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), id_var)
  arm_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), arm_var)
  paramcd <- create_picks_helper(
    teal.picks::datasets(dataname, dataname),
    teal.picks::picks(paramcd_var, paramcd_values, check_dataset = FALSE)
  )
  visit_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), visit_var)
  split_covariates <- create_picks_helper(
    teal.picks::datasets(dataname, dataname),
    split_choices_variables(cov_var)
  )
  cov_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), cov_var)

  args <- as.list(environment())
  module(
    label = label,
    server = srv_mmrm.picks,
    ui = ui_mmrm.picks,
    ui_args = args[names(args) %in% names(formals(ui_mmrm.picks))],
    server_args = args[names(args) %in% names(formals(srv_mmrm.picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_mmrm.picks <- function(id, # nolint: object_name.
                          aval_var,
                          paramcd,
                          visit_var,
                          cov_var,
                          split_covariates,
                          arm_var,
                          id_var,
                          method,
                          conf_level,
                          decorators,
                          pre_output,
                          post_output) {
  ns <- NS(id)
  fixed <- attr(method, "fixed", exact = TRUE)

  tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        textOutput(ns("null_input_msg")),
        tags$h3(textOutput(ns("mmrm_title"))),
        teal.widgets::table_with_settings_ui(ns("mmrm_table")),
        teal.widgets::plot_with_settings_ui(id = ns("mmrm_plot"))
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"),
        tags$br(),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Model Settings",
            tags$div(
              tags$label("Analysis Variable"),
              teal.picks::picks_ui(ns("aval_var"), aval_var),
            ),
            tags$div(
              tags$label("Select Endpoint"),
              teal.picks::picks_ui(ns("paramcd"), paramcd)
            ),
            tags$div(
              tags$label("Visit Variable"),
              teal.picks::picks_ui(ns("visit_var"), visit_var)
            ),
            tags$div(
              tags$label("Covariates"),
              teal.picks::picks_ui(ns("cov_var"), cov_var)
            ),
            shinyjs::hidden(
              tagList(
                tags$label("Split Covariates"),
                teal.picks::picks_ui(ns("split_covariates"), split_covariates)
              )
            ),
            tags$div(
              tags$label("Select Treatment Variable"),
              teal.picks::picks_ui(ns("arm_var"), arm_var)
            ),
            #
            shinyjs::hidden(uiOutput(ns("arms_buckets"))),
            shinyjs::hidden(
              helpText(
                id = ns("help_text"),
                "Multiple reference groups are automatically combined into a single group."
              )
            ),
            shinyjs::hidden(
              checkboxInput(
                ns("combine_comp_arms"),
                "Combine all comparison groups?",
                value = FALSE
              )
            ),
            tags$div(
              tags$label("Subject Identifier"),
              teal.picks::picks_ui(ns("id_var"), id_var)
            ),
            selectInput(
              ns("weights_emmeans"),
              "Weights for LS means",
              choices = c("proportional", "equal"),
              selected = "proportional",
              multiple = FALSE
            ),
            selectInput(
              ns("cor_struct"),
              "Correlation Structure",
              choices = eval(formals(tern.mmrm::build_formula)$cor_struct),
              multiple = FALSE
            ),
            teal.widgets::optionalSelectInput(
              ns("method"),
              "Adjustment Method",
              method$choices,
              method$selected,
              multiple = FALSE,
              fixed = attr(method, "fixed", exact = TRUE)
            ),
            teal.widgets::optionalSelectInput(
              ns("conf_level"),
              "Confidence Level",
              conf_level$choices,
              conf_level$selected,
              multiple = FALSE,
              fixed = attr(conf_level, "fixed", exact = TRUE)
            ),
            checkboxInput(
              ns("parallel"),
              "Parallel Computing",
              value = TRUE
            )
          )
        ),
        actionButton(
          ns("button_start"),
          "Fit Model",
          icon = icon("calculator"),
          width = "100%",
          class = "btn action-button",
          style = "background-color: orange; color: black; margin-bottom: 1rem;"
        ),
        radioButtons(
          ns("output_function"),
          "Output Type",
          choices = c(
            "LS means table" = "t_mmrm_lsmeans",
            "LS means plots" = "g_mmrm_lsmeans",
            "Covariance estimate" = "t_mmrm_cov",
            "Fixed effects" = "t_mmrm_fixed",
            "Fit statistics" = "t_mmrm_diagnostic",
            "Diagnostic plots" = "g_mmrm_diagnostic"
          ),
          selected = "t_mmrm_lsmeans"
        ),
        # Decorators ---
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == '%s'",
            ns("output_function"),
            "t_mmrm_lsmeans"
          ),
          teal::ui_transform_teal_data(
            ns("d_lsmeans_table"),
            select_decorators(decorators, "lsmeans_table")
          )
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == '%s'",
            ns("output_function"),
            "g_mmrm_lsmeans"
          ),
          teal::ui_transform_teal_data(
            ns("d_lsmeans_plot"),
            select_decorators(decorators, "lsmeans_plot")
          )
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == '%s'",
            ns("output_function"),
            "t_mmrm_cov"
          ),
          teal::ui_transform_teal_data(
            ns("d_covariance_table"),
            select_decorators(decorators, "covariance_table")
          )
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == '%s'",
            ns("output_function"),
            "t_mmrm_fixed"
          ),
          teal::ui_transform_teal_data(
            ns("d_fixed_effects_table"),
            select_decorators(decorators, "fixed_effects_table")
          )
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == '%s'",
            ns("output_function"),
            "t_mmrm_diagnostic"
          ),
          teal::ui_transform_teal_data(
            ns("d_diagnostic_table"),
            select_decorators(decorators, "diagnostic_table")
          )
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == '%s'",
            ns("output_function"),
            "g_mmrm_diagnostic"
          ),
          teal::ui_transform_teal_data(
            ns("d_diagnostic_plot"),
            select_decorators(decorators, "diagnostic_plot")
          )
        ),
        # End of Decorators ---
        conditionalPanel(
          condition = paste0(
            "input['",
            ns("output_function"),
            "'] == 't_mmrm_lsmeans'",
            " || ",
            "input['",
            ns("output_function"),
            "'] == 'g_mmrm_lsmeans'",
            " || ",
            "input['",
            ns("output_function"),
            "'] == 'g_mmrm_diagnostic'"
          ),
          bslib::accordion(
            open = TRUE,
            bslib::accordion_panel(
              title = "Output Settings",

              # Additional option for LS means table.
              selectInput(
                ns("t_mmrm_lsmeans_show_relative"),
                "Show Relative Change",
                choices = c("reduction", "increase", "none"),
                selected = "reduction",
                multiple = FALSE
              ),
              checkboxGroupInput(
                ns("g_mmrm_lsmeans_select"),
                "LS means plots",
                choices = c(
                  "Estimates" = "estimates",
                  "Contrasts" = "contrasts"
                ),
                selected = c("estimates", "contrasts"),
                inline = TRUE
              ),
              sliderInput(
                ns("g_mmrm_lsmeans_width"),
                "CI bar width",
                min = 0.1,
                max = 1,
                value = 0.6
              ),
              checkboxInput(
                ns("g_mmrm_lsmeans_contrasts_show_pval"),
                "Show contrasts p-values",
                value = FALSE
              ),
              # Additional options for diagnostic plots.
              radioButtons(
                ns("g_mmrm_diagnostic_type"),
                "Diagnostic plot type",
                choices = c(
                  "Fitted vs. Residuals" = "fit-residual",
                  "Normal Q-Q Plot of Residuals" = "q-q-residual"
                ),
                selected = NULL
              ),
              sliderInput(
                ns("g_mmrm_diagnostic_z_threshold"),
                "Label observations above this threshold",
                min = 0.1,
                max = 10,
                value = 3
              )
            )
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_mmrm.picks <- function(id, # nolint: object_name.
                           data,
                           reporter,
                           filter_panel_api,
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
                           total_label,
                           plot_height,
                           plot_width,
                           basic_table_args,
                           ggplot2_args,
                           decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- teal.picks::picks_srv(
      picks = list(
        arm_var = arm_var,
        paramcd = paramcd,
        id_var = id_var,
        visit_var = visit_var,
        cov_var = cov_var,
        aval_var = aval_var,
        split_covariates = split_covariates
      ),
      data = data
    )

    arm_var_r <- reactive(selectors$arm_var()$variables$selected)

    arm_ref_comp_iv <- arm_ref_comp_observer_picks(
      session,
      input,
      output,
      id_arm_var = "arm_var-variables-selected", # From UI.
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_mmrm",
      arm_var_r = arm_var_r
    )

    observeEvent(selectors$cov_var()$variables$selected,
      {
        # update covariates as actual variables
        split_interactions_values <- split_interactions(selectors$cov_var()$variables$selected)
        arm_var_value <- selectors$arm_var()$variables$selected
        if (length(intersect(split_interactions_values, arm_var_value)) >= 1L) {
          split_covariates_selected <- setdiff(split_interactions_values, arm_var_value)
        } else {
          split_covariates_selected <- split_interactions_values
        }
        rv_value <- selectors$split_covariates()
        rv_value$variables$selected <- split_covariates_selected
        selectors$split_covariates(rv_value)
      },
      ignoreNULL = FALSE
    )

    validated_q <- reactive({
      obj <- req(data())
      validate_input(
        inputId = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message = "A analysis variable must be selected."
      )
      validate_input(
        inputId = "visit_var-variables-selected",
        condition = !is.null(selectors$visit_var()$variables$selected),
        message = "A visit variables must be selected."
      )
      validate_input(
        inputId = "arm_var-variables-selected",
        condition = !is.null(selectors$arm_var()$variables$selected),
        message = "A treatment variables must be selected."
      )
      validate_input(
        inputId = "id_var-variables-selected",
        condition = !is.null(selectors$id_var()$variables$selected),
        message = "A subject identifier must be selected."
      )
      selected_visit <- selectors$visit_var()$variables$selected
      if (selected_visit > 0) { # Interactive covariates and visit variable must include same visit variable
        selected_covr <- selectors$cov_var()$variables$selected
        checks <- list(
          list(cov = "BASE:AVISIT", invalid_visit = "AVISITN", valid_visit = "AVISIT"),
          list(cov = "BASE:AVISITN", invalid_visit = "AVISIT", valid_visit = "AVISITN")
        )
        vapply(checks, FUN.VALUE = logical(1L), function(x) {
          validate_input(
            inputId = "cov_var-variables-selected",
            condition = !(x$cov %in% selected_covr) || !(x$invalid_visit %in% selected_visit),
            message = paste0(
              "'", x$cov, "' is not a valid covariate when '", x$invalid_visit,
              "' is selected as visit variable. Please deselect '", x$cov,
              "' as a covariate or change visit variable to '", x$valid_visit, "'."
            )
          )
          TRUE
        })
      }
      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# MMR Plot"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      obj |>
        within(
          tern::set_default_na_str(default_na_str),
          default_na_str = getOption(
            "tern_default_na_str",
            default = "<Missing>"
          )
        )
    })

    anl_inputs <- teal.picks::merge_srv(
      id = "merge",
      data = validated_q,
      selectors = selectors[names(selectors) != "cov_var"],
      output_name = "ANL"
    )

    adsl_merge_inputs <- teal.picks::merge_srv(
      "merge_adsl",
      selectors = selectors["arm_var"],
      data = anl_inputs$data,
      output_name = "ANL_ADSL"
    )

    anl_q <- reactive(adsl_merge_inputs$data())

    # Initially hide the output title because there is no output yet.
    shinyjs::hide("mmrm_title")

    # reactiveVal used to send a signal to plot_with_settings module to hide the UI
    show_plot_rv <- reactiveVal(FALSE)

    # this will store the current/last state of inputs and data that generated a model-fit
    # its purpose is to allow any input change to be checked whether it resulted in an out of sync state
    state <- reactiveValues(input = NULL, button_start = 0)

    # Note:
    # input$parallel does not get us out of sync (it just takes longer to get to same result)
    sync_inputs <- c(
      "aval_var-variables-selected",
      "arm_var-variables-selected",
      "Ref",
      "Comp",
      "combine_comp_arms",
      "visit_var-variables-selected",
      "cov_var-variables-selected",
      "id_var-variables-selected",
      "weights_emmeans",
      "cor_struct",
      "method",
      "conf_level"
    )

    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel.

    observeEvent(selectors$arm_var()$variables$selected, {
      arm_var <- as.vector(selectors$arm_var()$variables$selected)
      if (length(arm_var) == 0) {
        shinyjs::hide("arms_buckets")
        shinyjs::hide("help_text")
        shinyjs::hide("combine_comp_arms")
      } else {
        shinyjs::show("arms_buckets")
        shinyjs::show("help_text")
        shinyjs::show("combine_comp_arms")
      }
    })

    # Event handler:
    observeEvent(input$output_function, {
      # Show either the plot or the table output.
      output_function <- input$output_function
      if (isTRUE(grepl("^t_", output_function))) {
        show_plot_rv(FALSE)
        shinyjs::show("mmrm_table")
      } else if (isTRUE(grepl("^g_", output_function))) {
        shinyjs::hide("mmrm_table")
        show_plot_rv(TRUE)
      } else {
        stop("unknown output type")
      }

      # Show or hide LS means table option.
      if (isTRUE(output_function == "t_mmrm_lsmeans")) {
        shinyjs::show("t_mmrm_lsmeans_show_relative")
      } else {
        shinyjs::hide("t_mmrm_lsmeans_show_relative")
      }
    })

    # Event handler:
    # Show or hide the LS means plot options.
    observeEvent(list(input$output_function, input$g_mmrm_lsmeans_select), {
      output_function <- input$output_function
      g_mmrm_lsmeans_select <- input$g_mmrm_lsmeans_select
      if (isTRUE(output_function == "g_mmrm_lsmeans")) {
        shinyjs::show("g_mmrm_lsmeans_select")
        shinyjs::show("g_mmrm_lsmeans_width")
        if (isTRUE("contrasts" %in% g_mmrm_lsmeans_select)) {
          shinyjs::show("g_mmrm_lsmeans_contrasts_show_pval")
        } else {
          shinyjs::hide("g_mmrm_lsmeans_contrasts_show_pval")
        }
      } else {
        shinyjs::hide("g_mmrm_lsmeans_select")
        shinyjs::hide("g_mmrm_lsmeans_width")
        shinyjs::hide("g_mmrm_lsmeans_contrasts_show_pval")
      }
    })

    # Event handler:
    # Show or hide the diagnostic plot type option.
    observeEvent(list(input$output_function, input$g_mmrm_diagnostic_type), {
      output_function <- input$output_function
      g_mmrm_diagnostic_type <- input$g_mmrm_diagnostic_type
      if (isTRUE(output_function == "g_mmrm_diagnostic")) {
        shinyjs::show("g_mmrm_diagnostic_type")
        if (isTRUE(g_mmrm_diagnostic_type == "q-q-residual")) {
          shinyjs::show("g_mmrm_diagnostic_z_threshold")
        } else {
          shinyjs::hide("g_mmrm_diagnostic_z_threshold")
        }
      } else {
        shinyjs::hide("g_mmrm_diagnostic_type")
        shinyjs::hide("g_mmrm_diagnostic_z_threshold")
      }
    })

    # Event handler:
    # When the "Fit Model" button is clicked, hide initial message, show title, disable model fit button.
    shinyjs::onclick("button_start", {
      state$input <- mmrm_inputs_reactive()
      shinyjs::hide("null_input_msg")
      shinyjs::disable("button_start")
      success <- try(mmrm_fit(), silent = TRUE)
      if (!inherits(success, "try-error")) {
        shinyjs::show("mmrm_title")
      } else {
        shinyjs::hide("mmrm_title")
      }
    })

    # all the inputs and data that can be out of sync with the fitted model
    mmrm_inputs_reactive <- reactive({
      shinyjs::disable("button_start")
      arm_ref_comp_iv() # make sure the arm_ref_comp reactive values are up to date
      encoding_inputs <- lapply(
        sync_inputs,
        function(x) {
          if (x %in% c("Ref", "Comp")) {
            unlist(input$buckets[[x]])
          } else {
            input[[x]]
          }
        }
      )
      names(encoding_inputs) <- sync_inputs

      adsl_filtered <- anl_q()[["ADSL"]]
      anl_filtered <- anl_q()[[dataname]]

      teal::validate_has_data(adsl_filtered, min_nrow = 1)
      teal::validate_has_data(anl_filtered, min_nrow = 1)

      validate_checks()
      c(
        list(adsl_filtered = adsl_filtered, anl_filtered = anl_filtered),
        encoding_inputs
      )
    })

    output$null_input_msg <- renderText({
      mmrm_inputs_reactive()
      paste(
        "Please first specify 'Model Settings' and press 'Fit Model'.",
        "Afterwards choose 'Output Type' and optional 'Output Settings'.",
        "If changes to the 'Model Settings' or dataset (by filtering) are made,",
        "then the 'Fit Model' button must be pressed again to update the MMRM model.",
        "Note that the 'Show R Code' button can only be clicked if the model fit is up to date."
      )
    })

    # compares the mmrm_inputs_reactive values with the values stored in 'state'
    state_has_changed <- reactive({
      req(state$input)
      displayed_state <- mmrm_inputs_reactive()
      equal_ADSL <- all.equal(state$input$adsl_filtered, displayed_state$adsl_filtered) # nolint: object_name.
      equal_dataname <- all.equal(state$input$anl_filtered, displayed_state$anl_filtered)
      true_means_change <- vapply(
        sync_inputs,
        FUN = function(x) {
          if (is.null(state$input[[x]])) {
            if (is.null(displayed_state[[x]])) {
              return(FALSE)
            } else {
              return(TRUE)
            }
          } else if (is.null(displayed_state[[x]])) {
            return(TRUE)
          }
          if (length(state$input[[x]]) != length(displayed_state[[x]])) {
            return(TRUE)
          }
          any(sort(state$input[[x]]) != sort(displayed_state[[x]]))
        },
        FUN.VALUE = logical(1)
      )

      # all.equal function either returns TRUE or a character scalar to describe where there is inequality
      any(c(is.character(equal_ADSL), is.character(equal_dataname), true_means_change))
    })

    # Event handler:
    # These trigger when we are out of sync and then enable the start button and
    # disable the show R code button and show warning message
    observeEvent(mmrm_inputs_reactive(), {
      shinyjs::enable("button_start")
      if (!state_has_changed()) {
        shinyjs::disable("button_start")
      }
    })

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl_data <- anl_q()[["ANL"]]

      anl_m_inputs <- anl_inputs$variables()
      input_arm_var <- anl_m_inputs$arm_var
      input_visit_var <- anl_m_inputs$visit_var

      input_aval_var <- anl_m_inputs$aval_var
      input_id_var <- anl_m_inputs$id_var
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # Split the existing covariate strings in their variable parts, to allow "A*B" and "A:B" notations.
      input_cov_var <- anl_m_inputs$split_covariates
      covariate_parts <- split_interactions(input_cov_var)

      all_x_vars <- c(input_arm_var, input_visit_var, covariate_parts)

      all_x_vars_in_adsl <- intersect(all_x_vars, colnames(adsl_filtered))
      all_x_vars_in_anl <- setdiff(all_x_vars, all_x_vars_in_adsl)

      adslvars <- unique(c("USUBJID", "STUDYID", input_arm_var, input_id_var, all_x_vars_in_adsl))
      anlvars <- unique(c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_visit_var, all_x_vars_in_anl))

      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = adslvars,
        anl = anl_filtered,
        anlvars = anlvars,
        arm_var = input_arm_var,
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        min_nrow = 10,
        need_arm = FALSE
      )

      Map(
        function(visit_df, visit_name) {
          dup <- any(duplicated(visit_df[[input_id_var]]))
          validate(need(!dup, paste("Duplicated subject ID found at", visit_name)))
        },
        split(anl_data, anl_data[[input_visit_var]]),
        visit_name = levels(anl_data[[input_visit_var]])
      )
    })

    # Connector:
    # Fit the MMRM, once the user clicks on the start button.
    mmrm_fit <- eventReactive(input$button_start, {
      qenv <- anl_q()
      anl_m_inputs <- anl_inputs$variables()

      my_calls <- template_fit_mmrm(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        aval_var = anl_m_inputs$aval_var,
        arm_var = anl_m_inputs$arm_var,
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        combine_comp_arms = input$combine_comp_arms,
        id_var = anl_m_inputs$id_var,
        visit_var = anl_m_inputs$visit_var,
        cov_var = selectors$cov_var()$variables$selected,
        conf_level = as.numeric(input$conf_level),
        method = as.character(input$method),
        cor_struct = input$cor_struct,
        weights_emmeans = input$weights_emmeans,
        parallel = input$parallel
      )
      teal.code::eval_code(qenv, as.expression(unlist(my_calls)))
    })

    output$mmrm_title <- renderText({
      new_inputs <- try(state_has_changed(), silent = TRUE)
      # No message needed here because it will be displayed by either plots or tables output
      validate(need(!inherits(new_inputs, "try-error") && !new_inputs, character(0)))

      # Input on output type.
      output_function <- input$output_function
      g_mmrm_diagnostic_type <- input$g_mmrm_diagnostic_type
      g_mmrm_lsmeans_select <- input$g_mmrm_lsmeans_select

      output_title <- switch(output_function,
        "t_mmrm_cov" = "Residual Covariance Matrix Estimate",
        "t_mmrm_diagnostic" = "Model Fit Statistics",
        "t_mmrm_fixed" = "Fixed Effects Estimates",
        "t_mmrm_lsmeans" = "LS Means and Contrasts Estimates",
        "g_mmrm_diagnostic" = switch(g_mmrm_diagnostic_type,
          "fit-residual" = "Marginal Fitted Values vs. Residuals",
          "q-q-residual" = "Q-Q Normal Plot for Standardized Residuals"
        ),
        "g_mmrm_lsmeans" = if (setequal(g_mmrm_lsmeans_select, c("estimates", "contrasts"))) {
          "LS Means Estimates and Contrasts"
        } else if (identical(g_mmrm_lsmeans_select, "estimates")) {
          "LS Means Estimates"
        } else {
          "LS Means Contrasts"
        }
      )
      output_title
    })

    table_q <- reactive({
      validate(
        need(
          !state_has_changed(),
          "Inputs changed and no longer reflect the fitted model. Press `Fit Model` button again to re-fit model."
        )
      )
      # Input on output type.
      output_function <- input$output_function

      # If the output is not a table, stop here.
      if (!isTRUE(grepl("^t_", output_function))) {
        return(NULL)
      }
      # Get the fit stack while evaluating the fit code at the same time.
      qenv <- mmrm_fit()
      fit <- qenv[["fit"]]

      anl_m_inputs <- anl_inputs$variables()

      ANL <- qenv[["ANL"]]
      ANL_ADSL <- qenv[["ANL_ADSL"]]
      paramcd <- anl_m_inputs$paramcd

      basic_table_args$subtitles <- paste0(
        "Analysis Variable: ", anl_m_inputs$aval_var,
        ",  Endpoint: ", paste(selectors$paramcd()$values$selected, collapse = ", "),
        ifelse(is.null(fit$vars$covariates), "", paste(",  Covariates:", paste(fit$vars$covariates, collapse = ", ")))
      )
      basic_table_args$main_footer <- c(
        paste("Weights for LS Means:", input$weights_emmeans),
        paste("Correlation Structure:", input$cor_struct),
        paste("Adjustment Method:", input$method)
      )

      mmrm_table <- template_mmrm_tables(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        fit_name = "fit",
        arm_var = anl_m_inputs$arm_var,
        ref_arm = unlist(input$buckets$Ref),
        visit_var = anl_m_inputs$visit_var,
        paramcd = paramcd,
        show_relative = input$t_mmrm_lsmeans_show_relative,
        table_type = output_function,
        total_label = total_label,
        basic_table_args = basic_table_args
      )

      obj <- qenv
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(mmrm_table))
    })

    # Endpoint:
    # Plot outputs.
    plot_q <- reactive({
      validate(
        need(
          !state_has_changed(),
          "Inputs changed and no longer reflect the fitted model. Press `Fit Model` button again to re-fit model."
        )
      )
      # Input on output type.
      output_function <- input$output_function

      # Stop here if the output is not a plot.
      if (!isTRUE(grepl("^g_", output_function))) {
        return(NULL)
      }

      qenv <- mmrm_fit()
      fit <- qenv[["fit"]]

      ggplot2_args[["lsmeans"]] <- teal.widgets::ggplot2_args(
        labs <- list(
          subtitle = paste0(
            "Endpoint: ", fit$fit$data$PARAMCD[1],
            ifelse(is.null(fit$vars$covariates), "",
              paste(",  Covariates:", paste(fit$vars$covariates, collapse = ", "))
            )
          ),
          caption = paste(
            paste("Weights for LS Means:", input$weights_emmeans),
            paste("Correlation Structure:", input$cor_struct),
            paste("Adjustment Method:", input$method),
            sep = "\n"
          )
        )
      )

      ggplot2_args[["default"]] <- teal.widgets::ggplot2_args(
        labs <- list(
          subtitle = paste0(
            "Analysis Variable: ", fit$vars$response,
            ",  Endpoint: ", fit$fit$data$PARAMCD[1]
          )
        )
      )

      lsmeans_args <- if (output_function == "g_mmrm_lsmeans") {
        list(
          select = input$g_mmrm_lsmeans_select,
          width = input$g_mmrm_lsmeans_width,
          show_pval = input$g_mmrm_lsmeans_contrasts_show_pval
        )
      }

      diagnostic_args <- if (output_function == "g_mmrm_diagnostic") {
        list(
          type = input$g_mmrm_diagnostic_type,
          z_threshold = input$g_mmrm_diagnostic_z_threshold
        )
      }

      mmrm_plot_expr <- template_mmrm_plots(
        fit_name = "fit",
        lsmeans_plot = lsmeans_args,
        diagnostic_plot = diagnostic_args,
        ggplot2_args = ggplot2_args
      )
      obj <- qenv
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, as.expression(mmrm_plot_expr))
    })

    decorated_tables_q <- lapply(
      stats::setNames(
        nm = c("lsmeans_table", "diagnostic_table", "fixed_effects_table", "covariance_table")
      ),
      function(output_function) {
        teal::srv_transform_teal_data(
          id = sprintf("d_%s", output_function),
          data = table_q,
          transformators = select_decorators(decorators, output_function),
          expr = reactive(bquote(.(as.name(output_function))))
        )
      }
    )

    decorated_objs_q <- c(
      decorated_tables_q,
      lapply(
        stats::setNames(nm = c("lsmeans_plot", "diagnostic_plot")),
        function(output_function) {
          teal::srv_transform_teal_data(
            id = sprintf("d_%s", output_function),
            data = plot_q,
            transformators = select_decorators(decorators, output_function),
            expr = reactive(bquote(.(as.name(output_function))))
          )
        }
      )
    )

    obj_ix_r <- reactive({
      switch(input$output_function,
        t_mmrm_lsmeans = "lsmeans_table",
        t_mmrm_diagnostic = "diagnostic_table",
        t_mmrm_fixed = "fixed_effects_table",
        t_mmrm_cov = "covariance_table",
        g_mmrm_lsmeans = "lsmeans_plot",
        g_mmrm_diagnostic = "diagnostic_plot"
      )
    })

    plot_r <- reactive({
      req(!is.null(plot_q()))
      decorated_objs_q[[obj_ix_r()]]()[[obj_ix_r()]]
    })

    table_r <- reactive({
      req(!is.null(table_q()))
      decorated_objs_q[[obj_ix_r()]]()[[obj_ix_r()]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "mmrm_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      show_hide_signal = reactive(show_plot_rv())
    )

    teal.widgets::table_with_settings_srv(
      id = "mmrm_table",
      table_r = table_r,
      show_hide_signal = reactive(!show_plot_rv())
    )

    set_chunk_dims(pws, reactive({
      decorated_objs_q[[obj_ix_r()]]()
    }))
  })
}
