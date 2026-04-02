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
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' library(nestcolor)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   library(teal.modules.clinical)
#'   library(formatters)
#'   library(dplyr)
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
                            parentname = "ADSL",
                            arm_var = teal.picks::variables(
                              choices  = c("ARM", "ARMCD"),
                              selected = "ARMCD",
                              multiple = FALSE
                            ),
                            arm_ref_comp = NULL,
                            paramcd_var = teal.picks::variables("PARAMCD"),
                            paramcd_value = teal.picks::values(multiple = FALSE),
                            aval_var = teal.picks::variables("AVAL", fixed = TRUE),
                            cnsr_var = teal.picks::variables("CNSR", fixed = TRUE),
                            time_unit_var = teal.picks::variables("AVALU", fixed = TRUE),
                            subgroup_var = teal.picks::variables(selected = NULL, multiple = TRUE),
                            strata_var = teal.picks::variables(selected = NULL, multiple = TRUE),
                            stats = c("n_tot_events", "n_events", "median", "hr", "ci"),
                            riskdiff = NULL,
                            fixed_symbol_size = TRUE,
                            conf_level = teal.picks::values(
                              c("0.95", "0.9", "0.8"),
                              selected   = "0.95",
                              keep_order = TRUE
                            ),
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
  checkmate::assert_class(arm_var, "variables", null.ok = TRUE)
  checkmate::assert_class(paramcd_var, "variables")
  checkmate::assert_class(paramcd_value, "values")
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(cnsr_var, "variables")
  checkmate::assert_class(time_unit_var, "variables")
  checkmate::assert_class(subgroup_var, "variables")
  checkmate::assert_class(strata_var, "variables")
  checkmate::assert_class(conf_level, "values")
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

  # Build picks from specs -------------------------------------------------------

  # arm_var: treatment variable from parentname (ADSL)
  arm_var <- if (!is.null(arm_var)) {
    teal.picks::picks(teal.picks::datasets(parentname), arm_var)
  } else {
    NULL
  }

  # paramcd: filter picks on dataname by PARAMCD value
  paramcd <- teal.picks::picks(
    teal.picks::datasets(dataname),
    variables = paramcd_var,
    values    = paramcd_value
  )

  # aval_var: analysis variable from dataname (time-to-event)
  aval_var <- teal.picks::picks(teal.picks::datasets(dataname), aval_var)

  # cnsr_var: censoring variable from dataname
  cnsr_var <- teal.picks::picks(teal.picks::datasets(dataname), cnsr_var)

  # time_unit_var: time unit variable from dataname
  time_unit_var <- teal.picks::picks(teal.picks::datasets(dataname), time_unit_var)

  # subgroup_var: subgroup variables from parentname (multiple, ordered)
  subgroup_var <- teal.picks::picks(teal.picks::datasets(parentname), subgroup_var)

  # strata_var: stratification variables from parentname
  strata_var <- teal.picks::picks(teal.picks::datasets(parentname), strata_var)

  args <- as.list(environment())

  module(
    label       = label,
    server      = srv_g_forest_tte,
    ui          = ui_g_forest_tte,
    ui_args     = args[names(args) %in% names(formals(ui_g_forest_tte))],
    server_args = args[names(args) %in% names(formals(srv_g_forest_tte))],
    transformators = transformators,
    datanames   = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_forest_tte <- function(id,
                            arm_var,
                            paramcd,
                            aval_var,
                            cnsr_var,
                            time_unit_var,
                            subgroup_var,
                            strata_var,
                            conf_level,
                            fixed_symbol_size,
                            rel_width_forest,
                            font_size,
                            pre_output,
                            post_output,
                            decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
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
      if (!is.null(arm_var)) {
        tags$div(
          tags$div(
            tags$label("Select Treatment Variable:"),
            teal.picks::picks_ui(ns("arm_var"), arm_var)
          ),
          uiOutput(
            ns("arms_buckets"),
            title = paste(
              "Multiple reference groups are automatically combined into a single group",
              "when more than one value is selected."
            )
          )
        )
      },
      tags$div(
        tags$label("Subgroup Variables:"),
        teal.picks::picks_ui(ns("subgroup_var"), subgroup_var)
      ),
      tags$div(
        tags$label("Stratify by:"),
        teal.picks::picks_ui(ns("strata_var"), strata_var)
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional plot settings",
          teal.widgets::optionalSelectInput(
            inputId  = ns("conf_level"),
            label    = "Level of Confidence",
            choices  = conf_level$choices,
            selected = conf_level$selected,
            multiple = FALSE,
            fixed    = conf_level$fixed %||% FALSE
          ),
          checkboxInput(ns("fixed_symbol_size"), "Fixed symbol size", value = fixed_symbol_size),
          tags$div(
            tags$label("Time Unit Variable:"),
            teal.picks::picks_ui(ns("time_unit_var"), time_unit_var)
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("rel_width_forest"),
            "Relative Width of Forest Plot (%)",
            rel_width_forest,
            ticks = FALSE, step = 1
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"),
            "Table Font Size",
            font_size,
            ticks = FALSE, step = 1
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
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
                             aval_var,
                             cnsr_var,
                             time_unit_var,
                             subgroup_var,
                             strata_var,
                             stats,
                             riskdiff,
                             label,
                             plot_height,
                             plot_width,
                             ggplot2_args,
                             decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    # Setup arm ref/comp buckets UI and validator
    if (!is.null(arm_var)) {
      iv_arm_ref <- arm_ref_comp_observer(
        session,
        input,
        output,
        id_arm_var   = "arm_var-variables-selected",
        data         = reactive(data()[[parentname]]),
        arm_ref_comp = arm_ref_comp,
        module       = "tm_g_forest_tte"
      )
    }

    picks_list <- Filter(Negate(is.null), list(
      arm_var      = arm_var,
      paramcd      = paramcd,
      aval_var     = aval_var,
      cnsr_var     = cnsr_var,
      time_unit_var = time_unit_var,
      subgroup_var  = subgroup_var,
      strata_var    = strata_var
    ))

    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data  = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId   = "paramcd-values-selected",
        condition = !is.null(selectors$paramcd()$values$selected),
        message   = "Please select an endpoint (PARAMCD)."
      )
      teal:::validate_input(
        inputId   = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message   = "An analysis variable is required."
      )
      teal:::validate_input(
        inputId   = "cnsr_var-variables-selected",
        condition = !is.null(selectors$cnsr_var()$variables$selected),
        message   = "A censor variable is required."
      )
      if (!is.null(arm_var)) {
        teal:::validate_input(
          inputId   = "arm_var-variables-selected",
          condition = !is.null(selectors$arm_var()$variables$selected),
          message   = "A treatment variable is required."
        )
      }
      teal:::validate_input(
        inputId   = "conf_level",
        condition = !is.null(input$conf_level),
        message   = "Please choose a confidence level."
      )
      teal:::validate_input(
        inputId   = "conf_level",
        condition = {
          cv <- suppressWarnings(as.numeric(input$conf_level))
          !is.na(cv) && cv > 0 && cv < 1
        },
        message   = "Confidence level must be between 0 and 1."
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Forest Survival Plot"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      obj
    })

    # Merge ADTTE selectors: PARAMCD filter + aval_var + cnsr_var + time_unit_var → ANL
    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data        = validated_q,
      selectors   = selectors[c("paramcd", "aval_var", "cnsr_var", "time_unit_var")],
      join_fun    = "dplyr::inner_join",
      output_name = "ANL"
    )

    # Merge ADSL selectors: arm_var + subgroup_var + strata_var → ANL_ADSL
    adsl_selector_names <- intersect(
      c("arm_var", "subgroup_var", "strata_var"),
      names(selectors)
    )
    adsl_inputs <- teal.picks::merge_srv(
      "adsl_inputs",
      data        = validated_q,
      selectors   = selectors[adsl_selector_names],
      output_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      c(anl_inputs$data(), adsl_inputs$data())
    })

    validate_checks <- reactive({
      req(anl_q())
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered  <- anl_q()[[dataname]]
      anl           <- anl_q()[["ANL"]]

      input_arm_var       <- selectors$arm_var()$variables$selected
      input_aval_var      <- selectors$aval_var()$variables$selected
      input_cnsr_var      <- selectors$cnsr_var()$variables$selected
      input_subgroup_var  <- selectors$subgroup_var()$variables$selected
      input_strata_var    <- selectors$strata_var()$variables$selected
      input_paramcd       <- selectors$paramcd()$variables$selected
      input_time_unit_var <- selectors$time_unit_var()$variables$selected

      validate_args <- list(
        adsl     = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_subgroup_var, input_strata_var),
        anl      = anl_filtered,
        anlvars  = c(
          "USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var, input_time_unit_var
        ),
        arm_var  = input_arm_var
      )

      if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }
      validate_args <- append(
        validate_args,
        list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
      )

      if (length(input_subgroup_var) > 0) {
        validate(
          need(
            all(vapply(adsl_filtered[, input_subgroup_var, drop = FALSE], is.factor, logical(1))),
            "Not all subgroup variables are factors."
          )
        )
      }
      if (length(input_strata_var) > 0) {
        validate(
          need(
            all(vapply(adsl_filtered[, input_strata_var, drop = FALSE], is.factor, logical(1))),
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

    all_q <- reactive({
      validate_checks()

      input_arm_var       <- selectors$arm_var()$variables$selected
      input_aval_var      <- selectors$aval_var()$variables$selected
      input_cnsr_var      <- selectors$cnsr_var()$variables$selected
      input_subgroup_var  <- selectors$subgroup_var()$variables$selected
      input_strata_var    <- selectors$strata_var()$variables$selected
      input_time_unit_var <- selectors$time_unit_var()$variables$selected

      # obj_var_name: label for the selected PARAMCD level, read directly from selector
      obj_var_name <- selectors$paramcd()$values$selected %||% ""

      my_calls <- template_forest_tte(
        dataname        = "ANL",
        parentname      = "ANL_ADSL",
        arm_var         = input_arm_var,
        ref_arm         = unlist(input$buckets$Ref),
        comp_arm        = unlist(input$buckets$Comp),
        obj_var_name    = obj_var_name,
        aval_var        = input_aval_var,
        cnsr_var        = input_cnsr_var,
        subgroup_var    = if (length(input_subgroup_var) != 0) input_subgroup_var else NULL,
        strata_var      = if (length(input_strata_var) != 0) input_strata_var else NULL,
        stats           = stats,
        riskdiff        = riskdiff,
        conf_level      = as.numeric(input$conf_level),
        col_symbol_size = if (!input$fixed_symbol_size) 1,
        time_unit_var   = input_time_unit_var,
        font_size       = input$font_size,
        ggplot2_args    = ggplot2_args
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table and Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_all_q <- srv_decorate_teal_data(
      id         = "decorator",
      data       = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr       = reactive({
        substitute(
          cowplot::plot_grid(
            table,
            plot,
            align      = "h",
            axis       = "tblr",
            rel_widths = c(1 - input_rel_width_forest / 100, input_rel_width_forest / 100)
          ),
          env = list(input_rel_width_forest = input$rel_width_forest)
        )
      }),
      expr_is_reactive = TRUE
    )

    plot_r <- reactive({
      cowplot::plot_grid(
        decorated_all_q()[["table"]],
        decorated_all_q()[["plot"]],
        align      = "h",
        axis       = "tblr",
        rel_widths = c(1 - input$rel_width_forest / 100, input$rel_width_forest / 100)
      )
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id     = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width  = plot_width
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
