#' @describeIn tm_g_km [`teal.picks`] implementation when `paramcd` is [`teal.picks::picks()`].
#'
#' @keywords internal
tm_g_km.picks <- function(label,
                          dataname,
                          parentname = NULL,
                          arm_var = teal.picks::variables(
                            choices = c("ARM", "ARMCD", "ACTARMCD"),
                            selected = "ARM",
                            multiple = FALSE
                          ),
                          arm_ref_comp = NULL,
                          paramcd = teal.picks::picks(
                            teal.picks::variables("PARAMCD", fixed = TRUE),
                            teal.picks::values(
                              choices = c("OS", "PFS", "EFS"),
                              selected = "OS",
                              multiple = FALSE
                            ),
                            check_dataset = FALSE
                          ),
                          strata_var = teal.picks::variables(
                            choices = c("SEX", "BMRKR2"),
                            selected = "SEX",
                            multiple = TRUE
                          ),
                          facet_var = NULL,
                          time_unit_var = teal.picks::variables("AVALU", fixed = TRUE),
                          aval_var = teal.picks::variables("AVAL", fixed = TRUE),
                          cnsr_var = teal.picks::variables("CNSR", fixed = TRUE),
                          conf_level = teal.picks::values(
                            c("0.95", "0.9", "0.8"),
                            selected = "0.95",
                            keep_order = TRUE,
                            multiple = FALSE
                          ),
                          conf_type = teal.picks::values(
                            c("plain", "log", "log-log"),
                            selected = "plain",
                            keep_order = TRUE,
                            multiple = FALSE
                          ),
                          font_size = c(11L, 1L, 30L),
                          xticks = NULL,
                          control_annot_surv_med = tern::control_surv_med_annot(),
                          control_annot_coxph = tern::control_coxph_annot(x = 0.27, y = 0.35, w = 0.3),
                          legend_pos = c(0.9, 0.5),
                          rel_height_plot = c(80L, 0L, 100L),
                          plot_height = c(800L, 400L, 5000L),
                          plot_width = NULL,
                          pre_output = NULL,
                          post_output = NULL,
                          transformators = list(),
                          decorators = list()) {
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)

  if (is.null(parentname)) {
    parentname <- if (inherits(arm_var, "data_extract_spec")) {
      teal.transform::datanames_input(arm_var)
    } else {
      "ADSL"
    }
  }

  if (is.null(facet_var)) {
    facet_var <- teal.transform::add_no_selected_choices(
      teal.transform::choices_selected(
        teal.transform::variable_choices(parentname, c("SEX", "BMRKR2")),
        selected = NULL
      ),
      multiple = FALSE
    )
  }

  arm_var <- migrate_choices_selected_to_variables(arm_var, arg_name = "arm_var")
  strata_var <- migrate_choices_selected_to_variables(strata_var, arg_name = "strata_var", multiple = TRUE)
  facet_var <- migrate_choices_selected_to_variables(facet_var, arg_name = "facet_var")
  aval_var <- migrate_choices_selected_to_variables(aval_var, arg_name = "aval_var")
  cnsr_var <- migrate_choices_selected_to_variables(cnsr_var, arg_name = "cnsr_var")
  time_unit_var <- migrate_choices_selected_to_variables(time_unit_var, arg_name = "time_unit_var")
  paramcd <- migrate_value_choices_to_picks(paramcd, multiple = FALSE, arg_name = "paramcd")
  conf_level <- migrate_choices_selected_to_values(conf_level, arg_name = "conf_level", multiple = FALSE)
  conf_type <- migrate_choices_selected_to_values(conf_type, arg_name = "conf_type", multiple = FALSE)

  checkmate::assert_string(parentname)
  checkmate::assert_numeric(xticks, null.ok = TRUE)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  teal::assert_decorators(decorators, "plot")

  arm_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), arm_var)
  strata_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), strata_var)
  facet_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), facet_var)
  aval_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), aval_var)
  cnsr_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), cnsr_var)
  time_unit_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), time_unit_var)
  paramcd <- create_picks_helper(teal.picks::datasets(dataname, dataname), paramcd)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_km_picks,
    ui = ui_g_km_picks,
    ui_args = args[names(args) %in% names(formals(ui_g_km_picks))],
    server_args = args[names(args) %in% names(formals(srv_g_km_picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' Drop `-- no selection --` facet tokens for the template and validation.
#' @keywords internal
#' @noRd
tm_g_km_resolve_facet_cols <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character(0))
  }
  out <- character()
  for (z in x) {
    nz <- teal.transform::no_selected_as_NULL(z)
    if (!is.null(nz)) {
      out <- c(out, nz)
    }
  }
  unique(out)
}

#' @keywords internal
ui_g_km_picks <- function(id,
                          arm_var,
                          paramcd,
                          strata_var,
                          facet_var,
                          aval_var,
                          cnsr_var,
                          time_unit_var,
                          conf_level,
                          conf_type,
                          arm_ref_comp,
                          font_size,
                          rel_height_plot,
                          xticks,
                          pre_output,
                          post_output,
                          decorators) {
  ns <- NS(id)
  conf_level$fixed <- conf_level$fixed %||% FALSE
  conf_type$fixed <- conf_type$fixed %||% FALSE

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      verbatimTextOutput(outputId = ns("text")),
      teal.widgets::plot_with_settings_ui(
        id = ns("myplot")
      )
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Endpoint:"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Analysis Variable:"),
        teal.picks::picks_ui(ns("aval_var"), aval_var)
      ),
      tags$div(
        tags$label("Censor Variable:"),
        teal.picks::picks_ui(ns("cnsr_var"), cnsr_var)
      ),
      tags$div(
        tags$label("Facet Plots by:"),
        teal.picks::picks_ui(ns("facet_var"), facet_var)
      ),
      tags$div(
        tags$label("Select Treatment Variable:"),
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
            uiOutput(
              ns("arms_buckets"),
              title = paste(
                "Multiple reference groups are automatically combined into a single group when more than one",
                "value is selected."
              )
            ),
            checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            ),
            tags$div(
              tags$label("Stratify by:"),
              teal.picks::picks_ui(ns("strata_var"), strata_var)
            )
          )
        )
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "plot")),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Comparison settings",
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
            )
          )
        )
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title = "Additional plot settings",
          textInput(
            inputId = ns("xticks"),
            label = "Specify break intervals for x-axis e.g. 0 ; 500",
            value = if (!is.null(xticks)) {
              paste(xticks, collapse = " ; ")
            }
          ),
          radioButtons(
            ns("yval"),
            tags$label("Value on y-axis", class = "text-primary"),
            choices = c("Survival probability", "Failure probability"),
            selected = c("Survival probability"),
          ),
          teal.widgets::optionalSliderInput(
            ns("ylim"),
            tags$label("y-axis limits", class = "text-primary"),
            value = c(0, 1),
            min = 0, max = 1
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"),
            "Table Font Size",
            font_size,
            ticks = FALSE, step = 1
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("rel_height_plot"),
            "Relative Height of Plot (%)",
            rel_height_plot,
            ticks = FALSE, step = 1
          ),
          checkboxInput(
            inputId = ns("show_ci_ribbon"),
            label = "Show CI ribbon",
            value = FALSE,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns("show_km_table"),
            label = "Show KM table",
            value = TRUE,
            width = "100%"
          ),
          teal.widgets::optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            conf_level$choices,
            conf_level$selected,
            multiple = FALSE,
            fixed = conf_level$fixed
          ),
          teal.widgets::optionalSelectInput(
            ns("conf_type"),
            "Confidence Interval Type",
            conf_type$choices,
            conf_type$selected,
            multiple = FALSE,
            fixed = conf_type$fixed
          ),
          textInput(ns("xlab"), "X-axis label", "Time"),
          tags$div(
            tags$label("Time Unit Variable:"),
            teal.picks::picks_ui(ns("time_unit_var"), time_unit_var)
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_km_picks <- function(id,
                           data,
                           dataname,
                           parentname,
                           paramcd,
                           arm_var,
                           arm_ref_comp,
                           strata_var,
                           facet_var,
                           aval_var,
                           cnsr_var,
                           label,
                           time_unit_var,
                           plot_height,
                           plot_width,
                           xticks,
                           control_annot_surv_med,
                           control_annot_coxph,
                           legend_pos,
                           decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- teal.picks::picks_srv(
      picks = list(
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        arm_var = arm_var,
        paramcd = paramcd,
        strata_var = strata_var,
        facet_var = facet_var,
        time_unit_var = time_unit_var
      ),
      data = data
    )

    arm_var_r <- reactive(selectors$arm_var()$variables$selected)

    arm_ref_iv <- arm_ref_comp_observer_picks(
      session,
      input,
      output,
      id_arm_var = "arm_var-variables-selected",
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_g_km",
      on_off = reactive(input$compare_arms),
      arm_var_r = arm_var_r
    )

    iv_plot <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("font_size", shinyvalidate::sv_required("Plot tables font size must be greater than or equal to 5"))
      iv$add_rule("font_size", shinyvalidate::sv_gte(5, "Plot tables font size must be greater than or equal to 5"))
      iv$add_rule("ylim", shinyvalidate::sv_required("Please choose a range for y-axis limits"))
      iv$add_rule("xticks", shinyvalidate::sv_optional())
      iv$add_rule(
        "xticks",
        function(value) {
          val <- as_numeric_from_comma_sep_str(value, sep = ";")
          if (anyNA(val) || any(val < 0)) {
            "All break intervals for x-axis must be non-negative numbers separated by semicolons"
          } else if (all(val == 0)) {
            "At least one break interval for x-axis must be > 0"
          }
        }
      )
      iv
    })

    validated_q <- reactive({
      obj <- req(data())

      validate_input(
        inputId = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message = "Please select an analysis variable."
      )
      validate_input(
        inputId = "cnsr_var-variables-selected",
        condition = !is.null(selectors$cnsr_var()$variables$selected),
        message = "Please select a censor variable."
      )
      validate_input(
        inputId = "arm_var-variables-selected",
        condition = !is.null(selectors$arm_var()$variables$selected),
        message = "Please select a treatment variable."
      )
      validate_input(
        inputId = "paramcd-values-selected",
        condition = !is.null(selectors$paramcd()$values$selected),
        message = "Please select an endpoint."
      )
      validate_input(
        inputId = "time_unit_var-variables-selected",
        condition = !is.null(selectors$time_unit_var()$variables$selected),
        message = "Please select a time unit variable."
      )
      validate_input(
        inputId = "conf_level",
        condition = !is.null(input$conf_level),
        message = "Please choose a confidence level."
      )
      validate_input(
        inputId = "conf_level",
        condition = as.numeric(input$conf_level) > 0 && as.numeric(input$conf_level) < 1,
        message = "Confidence level must be between 0 and 1."
      )
      validate_input(
        inputId = "conf_type",
        condition = !is.null(input$conf_type) && input$conf_type %in% c("plain", "log", "log-log"),
        message = "Please choose a confidence interval type."
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

    validate_checks <- reactive({
      if (isTRUE(input$compare_arms)) {
        arm_ref_iv()
      }
      teal::validate_inputs(iv_plot())

      anl_q <- anl_inputs$data()
      ANL <- anl_q[["ANL"]]
      teal::validate_has_data(ANL, 2)

      adsl_filtered <- anl_q[[parentname]]
      anl_filtered <- anl_q[[dataname]]

      vm <- anl_inputs$variables()
      input_arm_var <- vm$arm_var[[1L]]
      input_strata_var <- vm$strata_var[[1L]]
      input_facet_var <- tm_g_km_resolve_facet_cols(vm$facet_var[[1L]])
      input_aval_var <- vm$aval_var[[1L]]
      input_cnsr_var <- vm$cnsr_var[[1L]]
      input_paramcd_col <- vm$paramcd[[1L]]
      input_time_unit_var <- vm$time_unit_var[[1L]]

      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var, input_facet_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd_col, input_aval_var, input_cnsr_var, input_time_unit_var),
        arm_var = input_arm_var
      )

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

      NULL
    })

    all_q <- reactive({
      validate_checks()

      obj <- anl_inputs$data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )

      ANL <- obj[["ANL"]]
      teal::validate_has_data(ANL, 2)

      vm <- anl_inputs$variables()

      input_xticks <- if (!is.null(input$xticks)) {
        as_numeric_from_comma_sep_str(input$xticks, sep = ";")
      }

      param_col <- vm$paramcd[[1L]]
      param_vals <- as.character(unique(ANL[[param_col]]))
      title <- paste("KM Plot of", paste(param_vals, collapse = ", "))

      my_calls <- template_g_km(
        dataname = "ANL",
        arm_var = vm$arm_var[[1L]],
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        compare_arm = input$compare_arms,
        combine_comp_arms = input$combine_comp_arms,
        aval_var = vm$aval_var[[1L]],
        cnsr_var = vm$cnsr_var[[1L]],
        strata_var = vm$strata_var[[1L]],
        time_points = NULL,
        time_unit_var = vm$time_unit_var[[1L]],
        facet_var = tm_g_km_resolve_facet_cols(vm$facet_var[[1L]]),
        annot_surv_med = input$show_km_table,
        annot_coxph = input$compare_arms,
        control_annot_surv_med = control_annot_surv_med,
        control_annot_coxph = control_annot_coxph,
        legend_pos = legend_pos,
        xticks = input_xticks,
        font_size = input$font_size,
        pval_method = input$pval_method_coxph,
        conf_level = as.numeric(input$conf_level),
        conf_type = input$conf_type,
        ties = input$ties_coxph,
        xlab = input$xlab,
        yval = ifelse(input$yval == "Survival probability", "Survival", "Failure"),
        ylim = input$ylim,
        rel_height_plot = input$rel_height_plot / 100,
        ci_ribbon = input$show_ci_ribbon,
        title = title
      )
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_all_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "plot"),
      expr = quote(plot)
    )
    plot_r <- reactive(decorated_all_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
