#' Template: `Kaplan-Meier`
#'
#' @inheritParams template_arguments
#' @inheritParams tern::g_km
#' @inheritParams tern::control_coxreg
#' @param facet_var (`character`)\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used for facet plotting.
#'
#' @seealso [tm_g_km()]
#' @keywords internal
#'
template_g_km <- function(dataname = "ANL",
                          arm_var = "ARM",
                          ref_arm = NULL,
                          comp_arm = NULL,
                          compare_arm = FALSE,
                          combine_comp_arms = FALSE,
                          aval_var = "AVAL",
                          cnsr_var = "CNSR",
                          xticks = NULL,
                          strata_var = NULL,
                          time_points = NULL,
                          facet_var = "SEX",
                          font_size = 10,
                          conf_level = 0.95,
                          ties = "efron",
                          xlab = "Survival time",
                          time_unit_var = "AVALU",
                          yval = "Survival",
                          pval_method = "log-rank",
                          annot_surv_med = TRUE,
                          annot_coxph = TRUE,
                          position_coxph = c(-0.04, 0.02),
                          width_annots = list(surv_med = grid::unit(0.45, "npc"), coxph = grid::unit(0.6, "npc")),
                          ci_ribbon = FALSE,
                          title = "KM Plot") {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(arm_var),
    assertthat::is.string(aval_var),
    assertthat::is.string(cnsr_var),
    assertthat::is.string(time_unit_var),
    assertthat::is.flag(compare_arm),
    assertthat::is.flag(combine_comp_arms),
    is.null(xticks) | is.numeric(xticks),
    assertthat::is.string(title)
  )

  ref_arm_val <- paste(ref_arm, collapse = "/")
  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      compare_arm = compare_arm,
      ref_arm_val = ref_arm_val
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = dplyr::mutate(
        is_event = cnsr_var == 0
      ),
      env = list(
        anl = as.name(dataname),
        cnsr_var = as.name(cnsr_var)
      )
    )
  )

  if (compare_arm && combine_comp_arms) {
    comp_arm_val <- paste(comp_arm, collapse = "/")
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm, new_level = comp_arm_val)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm, comp_arm_val = comp_arm_val)
      )
    )
  }

  y$data <- substitute(
    expr = {
      anl <- data_pipe
    },
    env = list(
      data_pipe = pipe_expr(data_list)
    )
  )

  y$variables <- if (length(strata_var) != 0) {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm, strat = strata_var),
      env = list(tte = aval_var, arm = arm_var, strata_var = strata_var)
    )
  } else {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm),
      env = list(tte = aval_var, arm = arm_var)
    )
  }
  graph_list <- list()

  if (length(facet_var) != 0L) {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = {
          facets <- droplevels(anl$facet_var)
          anl <- split(anl, f = facets)
        },
        env = list(
          facet_var = as.name(facet_var)
        )
      )
    )
  } else {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = {
          facets <- NULL
          anl <- list(anl)
        }
      )
    )
  }

  graph_list <- add_expr(
    graph_list,
    substitute(
      expr = {
        grid::grid.newpage()
        lyt <- grid::grid.layout(nrow = length(anl), ncol = 1) %>%
          grid::viewport(layout = .) %>%
          grid::pushViewport()

        g_km_counter_generator <- function() {
          plot_number <- 0L
          function(x) {
            plot_number <<- plot_number + 1L
            g_km(
              x,
              variables = variables,
              control_surv = control_surv_timepoint(conf_level = conf_level),
              xticks = xticks,
              xlab = sprintf(
                "%s (%s)",
                xlab,
                gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(x$time_unit_var[1]), perl = TRUE)
              ),
              yval = yval,
              title = sprintf(
                "%s%s",
                sprintf(
                  "%s%s",
                  title,
                  if (!is.null(facets)) {
                    sprintf(", %s = %s", as.character(quote(facet_var)), unique(x[[as.character(quote(facet_var))]]))
                  } else {
                    ""
                  }
                ),
                if (length(strata_var) != 0) {
                  sprintf("\nStratified by %s", toString(strata_var))
                } else {
                  ""
                }
              ),
              footnotes = if (annot_coxph) {
                paste(
                  "Ties for Coxph (Hazard Ratio):", ties, "\n",
                  "p-value Method for Coxph (Hazard Ratio):", pval_method
                )
              },
              newpage = FALSE,
              vp = grid::viewport(layout.pos.row = plot_number, layout.pos.col = 1),
              font_size = font_size,
              ci_ribbon = ci_ribbon,
              ggtheme = ggplot2::theme_minimal(),
              annot_surv_med = annot_surv_med,
              annot_coxph = annot_coxph,
              control_coxph_pw = control_coxph(conf_level = conf_level, pval_method = pval_method, ties = ties),
              position_coxph = position_coxph,
              width_annots = width_annots
            )
          }
        }

        g_km_counter <- g_km_counter_generator()

        plot_list <- lapply(
          anl,
          g_km_counter
        )

        plot <- tern::stack_grobs(grobs = plot_list)
        plot
      },
      env = list(
        facet_var = if (length(facet_var) != 0L) as.name(facet_var),
        font_size = font_size,
        strata_var = strata_var,
        xticks = xticks,
        xlab = xlab,
        time_unit_var = as.name(time_unit_var),
        yval = yval,
        conf_level = conf_level,
        pval_method = pval_method,
        annot_surv_med = annot_surv_med,
        annot_coxph = annot_coxph,
        position_coxph = position_coxph,
        width_annots = width_annots,
        ties = ties,
        ci_ribbon = ci_ribbon,
        title = title
      )
    )
  )

  y$graph <- bracket_expr(graph_list)
  y
}


#' Teal Module: `Kaplan-Meier`
#'
#' This teal module produces a grid style `Kaplan-Meier` plot for data with
#' `ADaM` structure.
#'
#' @inheritParams module_arguments
#' @param facet_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used for facet plotting.
#'
#' @export
#'
#' @examples
#' library(nestcolor)
#'
#' adsl <- tmc_ex_adsl
#' adtte <- tmc_ex_adtte
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
#'     cdisc_dataset("ADTTE", adtte)
#'   ),
#'   modules = modules(
#'     tm_g_km(
#'       label = "Kaplan-Meier Plot",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(
#'         variable_choices(adsl, c("ARM", "ARMCD", "ACTARMCD")),
#'         "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(adtte, "PARAMCD", "PARAM"),
#'         "OS"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       strata_var = choices_selected(
#'         variable_choices(adsl, c("SEX", "BMRKR2")),
#'         "SEX"
#'       ),
#'       facet_var = choices_selected(
#'         variable_choices(adsl, c("SEX", "BMRKR2")),
#'         NULL
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_km <- function(label,
                    dataname,
                    parentname = ifelse(
                      inherits(arm_var, "data_extract_spec"),
                      teal.transform::datanames_input(arm_var),
                      "ADSL"
                    ),
                    arm_var,
                    arm_ref_comp = NULL,
                    paramcd,
                    strata_var,
                    facet_var,
                    time_unit_var = teal.transform::choices_selected(
                      teal.transform::variable_choices(dataname, "AVALU"), "AVALU",
                      fixed = TRUE
                    ),
                    aval_var = teal.transform::choices_selected(
                      teal.transform::variable_choices(dataname, "AVAL"), "AVAL",
                      fixed = TRUE
                    ),
                    cnsr_var = teal.transform::choices_selected(
                      teal.transform::variable_choices(dataname, "CNSR"), "CNSR",
                      fixed = TRUE
                    ),
                    conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                    plot_height = c(1200L, 400L, 5000L),
                    plot_width = NULL,
                    pre_output = NULL,
                    post_output = NULL) {
  logger::log_info("Initializing tm_g_km")

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  args <- as.list(environment())
  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    facet_var = cs_to_des_select(facet_var, dataname = parentname, multiple = FALSE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    time_unit_var = cs_to_des_select(time_unit_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_g_km,
    ui = ui_g_km,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        label = label,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        plot_height = plot_height,
        plot_width = plot_width
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}


#' User Interface for KM Module
#' @noRd
ui_g_km <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$strata_var,
    a$facet_var,
    a$aval_var,
    a$cnsr_var,
    a$time_unit_var
  )

  ns <- shiny::NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      shiny::verbatimTextOutput(outputId = ns("text")),
      teal.widgets::plot_with_settings_ui(
        id = ns("myplot")
      )
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "strata_var", "facet_var", "aval_var", "cnsr_var")]),
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
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("facet_var"),
        label = "Facet Plots by",
        data_extract_spec = a$facet_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::div(
        class = "arm-comp-box",
        shiny::tags$label("Compare Treatments"),
        shinyWidgets::switchInput(
          inputId = ns("compare_arms"),
          value = !is.null(a$arm_ref_comp),
          size = "mini"
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          shiny::div(
            shiny::uiOutput(
              ns("arms_buckets"),
              title = paste(
                "Multiple reference groups are automatically combined into a single group when more than one",
                "value is selected."
              )
            ),
            shiny::checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            ),
            teal.transform::data_extract_ui(
              id = ns("strata_var"),
              label = "Stratify by",
              data_extract_spec = a$strata_var,
              is_single_dataset = is_single_dataset_value
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        teal.widgets::panel_group(
          teal.widgets::panel_item(
            "Comparison settings",
            shiny::radioButtons(
              ns("pval_method_coxph"),
              label = shiny::HTML(
                paste(
                  "p-value method for ",
                  shiny::span(class = "text-primary", "Coxph"), # nolint
                  " (Hazard Ratio)",
                  sep = ""
                )
              ),
              choices = c("wald", "log-rank", "likelihood"),
              selected = "log-rank"
            ),
            shiny::radioButtons(
              ns("ties_coxph"),
              label = shiny::HTML(
                paste(
                  "Ties for ",
                  shiny::span(class = "text-primary", "Coxph"), # nolint
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
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional plot settings",
          shiny::textInput(
            inputId = ns("xticks"),
            label = "Specify break intervals for x-axis e.g. 0 ; 500"
          ),
          shiny::radioButtons(
            ns("yval"),
            shiny::tags$label("Value on y-axis", class = "text-primary"),
            choices = c("Survival probability", "Failure probability"),
            selected = c("Survival probability"),
          ),
          shiny::numericInput(
            inputId = ns("font_size"),
            label = "Plot tables font size",
            value = 10,
            min = 5,
            max = 15,
            step = 1,
            width = "100%"
          ),
          shiny::checkboxInput(
            inputId = ns("show_ci_ribbon"),
            label = "Show CI ribbon",
            value = FALSE,
            width = "100%"
          ),
          shiny::checkboxInput(
            inputId = ns("show_km_table"),
            label = "Show KM table",
            value = TRUE,
            width = "100%"
          ),
          teal.widgets::optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          shiny::textInput(ns("xlab"), "X-axis label", "Time"),
          teal.transform::data_extract_ui(
            id = ns("time_unit_var"),
            label = "Time Unit Variable",
            data_extract_spec = a$time_unit_var,
            is_single_dataset = is_single_dataset_value
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


#' Server for KM Module
#' @noRd
#'
srv_g_km <- function(id,
                     data,
                     reporter,
                     filter_panel_api,
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
                     plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    # Setup arm variable selection, default reference arms and default
    # comparison arms for encoding panel
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_tte",
      on_off = shiny::reactive(input$compare_arms)
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        arm_var = arm_var,
        paramcd = paramcd,
        strata_var = strata_var,
        facet_var = facet_var,
        time_unit_var = time_unit_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        cnsr_var = shinyvalidate::sv_required("A censor variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("An endpoint is required")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()

      if (isTRUE(input$compare_arms)) {
        iv$add_validator(iv_arm_ref)
      }

      iv$add_rule("font_size", shinyvalidate::sv_required("Plot tables font size must be greater than or equal to 5"))
      iv$add_rule("font_size", shinyvalidate::sv_gte(5, "Plot tables font size must be greater than or equal to 5"))
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(
          0, 1,
          inclusive = c(FALSE, FALSE),
          message_fmt = "Confidence level must be between 0 and 1"
        )
      )
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
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      join_keys = get_join_keys(data),
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    anl_q <- shiny::reactive({
      teal.code::eval_code(
        teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)),
        code = as.expression(anl_inputs()$expr)
      )
    })

    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())

      qenv <- anl_q()
      adsl_filtered <- qenv[[parentname]]
      anl_filtered <- qenv[[dataname]]

      anl_m <- anl_inputs()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_facet_var <- as.vector(anl_m$columns_source$facet_var)
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]
      input_time_unit_var <- as.vector(anl_m$columns_source$time_unit_var)

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var, input_facet_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var, input_time_unit_var),
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

      NULL
    })

    all_q <- shiny::reactive({
      validate_checks()

      qenv <- anl_q()
      anl_m <- anl_inputs()

      anl <- qenv[["ANL"]] # nolint
      teal::validate_has_data(anl, 2)

      input_xticks <- if (!is.null(input$xticks)) {
        as_numeric_from_comma_sep_str(input$xticks, sep = ";")
      }

      input_paramcd <- as.character(unique(anl[[as.vector(anl_m$columns_source$paramcd)]]))
      title <- paste("KM Plot of", input_paramcd)

      my_calls <- template_g_km(
        dataname = "ANL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        compare_arm = input$compare_arms,
        combine_comp_arms = input$combine_comp_arms,
        aval_var = as.vector(anl_m$columns_source$aval_var),
        cnsr_var = as.vector(anl_m$columns_source$cnsr_var),
        strata_var = as.vector(anl_m$columns_source$strata_var),
        time_points = NULL,
        time_unit_var = as.vector(anl_m$columns_source$time_unit_var),
        facet_var = as.vector(anl_m$columns_source$facet_var),
        annot_surv_med = input$show_km_table,
        annot_coxph = input$compare_arms,
        xticks = input_xticks,
        font_size = input$font_size,
        pval_method = input$pval_method_coxph,
        conf_level = as.numeric(input$conf_level),
        ties = input$ties_coxph,
        xlab = input$xlab,
        yval = ifelse(input$yval == "Survival probability", "Survival", "Failure"),
        ci_ribbon = input$show_ci_ribbon,
        title = title
      )
      teal.code::eval_code(qenv, as.expression(my_calls))
    })

    plot_r <- shiny::reactive(all_q()[["plot"]])

    # Insert the plot into a plot with settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
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
        card$set_name("Kaplan Meier Plot")
        card$append_text("Kaplan Meier Plot", "header2")
        card$append_text("Non-parametric method used to estimate the survival function from lifetime data", "header3")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
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
