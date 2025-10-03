#' Template: Survival Forest Plot
#'
#' Creates a valid expression to generate a survival forest plot.
#'
#' @inheritParams template_arguments
#' @inheritParams template_forest_rsp
#' @param stats (`character`)\cr the names of statistics to be reported among:
#'   * `n_tot_events`: Total number of events per group.
#'   * `n_events`: Number of events per group.
#'   * `n_tot`: Total number of observations per group.
#'   * `n`: Number of observations per group.
#'   * `median`: Median survival time.
#'   * `hr`: Hazard ratio.
#'   * `ci`: Confidence interval of hazard ratio.
#'   * `pval`: p-value of the effect.
#'   Note, one of the statistics `n_tot` and `n_tot_events`, as well as both `hr` and `ci`
#'   are required.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_forest_tte()]
#'
#' @keywords internal
template_forest_tte <- function(dataname = "ANL",
                                parentname = "ANL_ADSL",
                                arm_var,
                                ref_arm = NULL,
                                comp_arm = NULL,
                                obj_var_name = "",
                                aval_var = "AVAL",
                                cnsr_var = "CNSR",
                                subgroup_var,
                                strata_var = NULL,
                                stats = c("n_tot_events", "n_events", "median", "hr", "ci"),
                                riskdiff = NULL,
                                conf_level = 0.95,
                                col_symbol_size = NULL,
                                time_unit_var = "AVALU",
                                font_size = 15,
                                ggplot2_args = teal.widgets::ggplot2_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(obj_var_name)
  checkmate::assert_character(subgroup_var, null.ok = TRUE)
  checkmate::assert_character(stats, min.len = 3)
  checkmate::assert_true(any(c("n_tot", "n_tot_events") %in% stats))
  checkmate::assert_true(all(c("hr", "ci") %in% stats))
  checkmate::assert_list(riskdiff, null.ok = TRUE)
  checkmate::assert_number(font_size)

  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  # Data processing.
  data_list <- list()
  anl_list <- list()
  parent_list <- list()

  anl_list <- add_expr(
    anl_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  anl_list <- add_expr(
    anl_list,
    substitute_names(
      expr = {
        dplyr::mutate(arm_var = tern::combine_levels(arm_var, comp_arm)) %>%
          dplyr::mutate(is_event = cnsr_var == 0)
      },
      names = list(arm_var = as.name(arm_var)),
      others = list(
        comp_arm = comp_arm,
        cnsr_var = as.name(cnsr_var)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- anl_list,
      env = list(
        anl_list = pipe_expr(anl_list)
      )
    )
  )

  parent_list <- add_expr(
    parent_list,
    prepare_arm(
      dataname = parentname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  parent_list <- add_expr(
    parent_list,
    substitute_names(
      expr = dplyr::mutate(arm_var = tern::combine_levels(arm_var, comp_arm)),
      names = list(arm_var = as.name(arm_var)),
      others = list(
        ref_arm = ref_arm,
        comp_arm = comp_arm
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parent <- parent_list,
      env = list(
        parent_list = pipe_expr(parent_list)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Tabulate subgroup analysis of response.
  summary_list <- list()

  summary_list <- add_expr(
    summary_list,
    substitute(
      expr = df <- tern::extract_survival_subgroups(
        variables = list(
          tte = aval_var,
          is_event = "is_event",
          arm = arm_var,
          subgroups = subgroup_var,
          strata = strata_var
        ),
        control = tern::control_coxph(conf_level = conf_level),
        data = anl
      ),
      env = list(
        aval_var = aval_var,
        arm_var = arm_var,
        subgroup_var = subgroup_var,
        strata_var = strata_var,
        conf_level = conf_level
      )
    )
  )

  y$summary <- bracket_expr(summary_list)

  # Table output.
  y$table <- substitute(
    expr = {
      result <- rtables::basic_table() %>%
        tern::tabulate_survival_subgroups(
          df,
          vars = stats,
          time_unit = as.character(anl$time_unit_var[1]),
          riskdiff = riskdiff
        )
    },
    env = list(stats = stats, time_unit_var = as.name(time_unit_var), riskdiff = riskdiff)
  )

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = teal.widgets::ggplot2_args(
      labs = list(
        title = paste(
          paste("Forest Plot of Survival Duration for", obj_var_name),
          ifelse(is.null(strata_var), "", paste("Stratified by", paste(strata_var, collapse = " and "))),
          sep = "\n"
        ),
        caption = ""
      )
    )
  )

  plot_list <- list()

  plot_list <- add_expr(
    plot_list,
    substitute(
      expr = {
        f <- tern::g_forest(
          tbl = result,
          col_symbol_size = col_s_size,
          font_size = font_size,
          as_list = TRUE
        )
      },
      env = list(
        col_s_size = col_symbol_size,
        font_size = font_size
      )
    )
  )

  plot_list <- add_expr(
    plot_list,
    substitute(
      expr = {
        table <- f[["table"]] +
          ggplot2::labs(title = ggplot2_args_title, subtitle = ggplot2_args_subtitle)
        plot <- f[["plot"]] + ggplot2::labs(caption = ggplot2_args_caption)
      },
      env = list(
        ggplot2_args_title = all_ggplot2_args$labs$title,
        ggplot2_args_subtitle = all_ggplot2_args$labs$subtitle,
        ggplot2_args_caption = all_ggplot2_args$labs$caption
      )
    )
  )

  # Plot output.
  y$plot <- plot_list

  y
}

#' teal Module: Forest Survival Plot
#'
#' This module produces a grid-style forest plot for time-to-event data with ADaM structure.
#'
#' @inheritParams tern::g_forest
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_forest_tte
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_forest_tte(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied only to `plot` output
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
#' library(nestcolor)
#' library(dplyr)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADTTE <- tmc_ex_adtte
#'   ADSL$RACE <- droplevels(ADSL$RACE) %>% with_label("Race")
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADTTE <- data[["ADTTE"]]
#'
#' arm_ref_comp <- list(
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   ),
#'   ARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_forest_tte(
#'       label = "Forest Survival",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD")),
#'         "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"),
#'         "OS"
#'       ),
#'       subgroup_var = choices_selected(
#'         variable_choices(ADSL, names(ADSL)),
#'         c("BMRKR2", "SEX")
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("STRATA1", "STRATA2")),
#'         "STRATA2"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_forest_tte <- function(label,
                            dataname,
                            parentname = ifelse(
                              inherits(arm_var, "data_extract_spec"),
                              teal.transform::datanames_input(arm_var),
                              "ADSL"
                            ),
                            arm_var,
                            arm_ref_comp = NULL,
                            subgroup_var,
                            paramcd,
                            strata_var,
                            aval_var = teal.transform::choices_selected(
                              teal.transform::variable_choices(dataname, "AVAL"), "AVAL",
                              fixed = TRUE
                            ),
                            cnsr_var = teal.transform::choices_selected(
                              teal.transform::variable_choices(dataname, "CNSR"), "CNSR",
                              fixed = TRUE
                            ),
                            stats = c("n_tot_events", "n_events", "median", "hr", "ci"),
                            riskdiff = NULL,
                            conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                            time_unit_var = teal.transform::choices_selected(
                              teal.transform::variable_choices(dataname, "AVALU"), "AVALU",
                              fixed = TRUE
                            ),
                            fixed_symbol_size = TRUE,
                            plot_height = c(500L, 200L, 2000L),
                            plot_width = c(1500L, 800L, 3000L),
                            rel_width_forest = c(25L, 0L, 100L),
                            font_size = c(15L, 1L, 30L),
                            pre_output = NULL,
                            post_output = NULL,
                            ggplot2_args = teal.widgets::ggplot2_args(),
                            transformators = list(),
                            decorators = list()) {
  message("Initializing tm_g_forest_tte")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(subgroup_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(strata_var, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(cnsr_var, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_class(time_unit_var, "choices_selected")
  checkmate::assert_character(stats, min.len = 3)
  checkmate::assert_true(any(c("n_tot", "n_tot_events") %in% stats))
  checkmate::assert_true(all(c("hr", "ci") %in% stats))
  checkmate::assert_list(riskdiff, null.ok = TRUE)
  checkmate::assert_flag(fixed_symbol_size)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")
  assert_decorators(decorators, "plot")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    subgroup_var = cs_to_des_select(subgroup_var, dataname = parentname, multiple = TRUE, ordered = TRUE),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    time_unit_var = cs_to_des_select(time_unit_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_g_forest_tte,
    ui = ui_g_forest_tte,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        arm_ref_comp = arm_ref_comp,
        parentname = parentname,
        stats = stats,
        riskdiff = riskdiff,
        label = label,
        plot_height = plot_height,
        plot_width = plot_width,
        ggplot2_args = ggplot2_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_g_forest_tte <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$subgroup_var,
    a$strata_var,
    a$aval_var,
    a$cnsr_var,
    a$time_unit_var
  )

  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "subgroup_var", "strata_var", "aval_var", "cnsr_var")]),
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
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      uiOutput(
        ns("arms_buckets"),
        title = paste(
          "Multiple reference groups are automatically combined into a single group when more than one",
          "value is selected."
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("subgroup_var"),
        label = "Subgroup Variables",
        data_extract_spec = a$subgroup_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional plot settings",
          teal.widgets::optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          checkboxInput(ns("fixed_symbol_size"), "Fixed symbol size", value = TRUE),
          teal.transform::data_extract_ui(
            id = ns("time_unit_var"),
            label = "Time Unit Variable",
            data_extract_spec = a$time_unit_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("rel_width_forest"),
            "Relative Width of Forest Plot (%)",
            a$rel_width_forest,
            ticks = FALSE, step = 1
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"),
            "Table Font Size",
            a$font_size,
            ticks = FALSE, step = 1
          )
        )
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_g_forest_tte <- function(id,
                             data,
                             dataname,
                             parentname,
                             arm_var,
                             arm_ref_comp,
                             paramcd,
                             subgroup_var,
                             strata_var,
                             aval_var,
                             cnsr_var,
                             time_unit_var,
                             stats,
                             riskdiff,
                             label,
                             plot_height,
                             plot_width,
                             ggplot2_args,
                             decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data()[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_g_forest_tte"
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        subgroup_var = subgroup_var,
        strata_var = strata_var,
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        time_unit_var = time_unit_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        cnsr_var = shinyvalidate::sv_required("A censor variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required(message = "Please select Endpoint filter.")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(0, 1, message_fmt = "Confidence level must be between 0 and 1")
      )
      iv$add_validator(iv_arm_ref)
      teal.transform::compose_and_enable_validators(iv, selector_list, c("arm_var", "aval_var", "paramcd"))
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var, subgroup_var = subgroup_var, strata_var = strata_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Forest Survival Plot"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(code = as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(code = as.expression(adsl_inputs()$expr))
    })

    validate_checks <- reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      anl_m <- anl_inputs()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
      input_subgroup_var <- as.vector(anl_m$columns_source$subgroup_var)
      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_time_unit_var <- as.vector(anl_m$columns_source$time_unit_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_subgroup_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var, input_time_unit_var),
        arm_var = input_arm_var
      )

      # validate arm levels
      if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }
      validate_args <- append(
        validate_args, list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
      )

      if (length(input_subgroup_var) > 0) {
        validate(
          need(
            all(vapply(adsl_filtered[, input_subgroup_var], is.factor, logical(1))),
            "Not all subgroup variables are factors."
          )
        )
      }

      if (length(input_strata_var) > 0) {
        validate(
          need(
            all(vapply(adsl_filtered[, input_strata_var], is.factor, logical(1))),
            "Not all stratification variables are factors."
          )
        )
      }

      do.call(what = "validate_standard_inputs", validate_args)

      validate(need(
        length(anl[[input_paramcd]]) > 0,
        "Value of the endpoint variable should not be empty."
      ))

      NULL
    })

    # The R-code corresponding to the analysis.
    all_q <- reactive({
      validate_checks()

      anl_m <- anl_inputs()

      strata_var <- as.vector(anl_m$columns_source$strata_var)
      subgroup_var <- as.vector(anl_m$columns_source$subgroup_var)
      resolved_paramcd <- teal.transform::resolve_delayed(paramcd, as.list(data()))
      obj_var_name <- get_g_forest_obj_var_name(resolved_paramcd, input)

      my_calls <- template_forest_tte(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        obj_var_name = obj_var_name,
        aval_var = as.vector(anl_m$columns_source$aval_var),
        cnsr_var = as.vector(anl_m$columns_source$cnsr_var),
        subgroup_var = if (length(subgroup_var) != 0) subgroup_var else NULL,
        strata_var = if (length(strata_var) != 0) strata_var else NULL,
        stats = stats,
        riskdiff = riskdiff,
        conf_level = as.numeric(input$conf_level),
        col_symbol_size = if (!input$fixed_symbol_size) 1,
        time_unit_var = as.vector(anl_m$columns_source$time_unit_var),
        font_size = input$font_size,
        ggplot2_args = ggplot2_args
      )
      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table and Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Outputs to render.
    decorated_all_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr = {
        table
        plot
      }
    )
    plot_r <- reactive({
      cowplot::plot_grid(
        decorated_all_q()[["table"]],
        decorated_all_q()[["plot"]],
        align = "h",
        axis = "tblr",
        rel_widths = c(1 - input$rel_width_forest / 100, input$rel_width_forest / 100)
      )
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
