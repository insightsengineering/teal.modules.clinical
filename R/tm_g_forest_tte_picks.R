#' teal Module: Forest Survival Plot (teal.picks)
#'
#' This module produces a grid-style forest plot for time-to-event data with ADaM structure.
#' This is the `teal.picks` implementation of [tm_g_forest_tte()].
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_forest_tte
#' @param dataname (`character`)\cr name of the analysis dataset (e.g. `"ADTTE"`).
#' @param parentname (`character`)\cr name of the parent dataset (e.g. `"ADSL"`).
#' @param arm_var (`variables` or `NULL`)\cr variable spec for the treatment arm variable.
#'   Resolved against `parentname`.
#' @param paramcd_var (`variables`)\cr variable spec pointing at the `PARAMCD` column.
#'   Combined with `paramcd_value` to build a filter picks.
#' @param paramcd_value (`values`)\cr value filter spec to select the endpoint (PARAMCD level).
#' @param aval_var (`variables`)\cr variable spec for the analysis (time-to-event) variable (e.g. `AVAL`).
#'   Resolved against `dataname`. Fixed by default.
#' @param cnsr_var (`variables`)\cr variable spec for the censoring variable (e.g. `CNSR`).
#'   Resolved against `dataname`. Fixed by default.
#' @param time_unit_var (`variables`)\cr variable spec for the time unit variable (e.g. `AVALU`).
#'   Resolved against `dataname`. Fixed by default.
#' @param subgroup_var (`variables`)\cr variable spec for subgroup variables (multiple, ordered).
#'   Resolved against `parentname`.
#' @param strata_var (`variables`)\cr variable spec for stratification variables (multiple).
#'   Resolved against `parentname`.
#' @param conf_level (`values`)\cr available confidence levels.
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
#' tm_g_forest_tte.picks(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied only to `plot` output
#'    )
#' )
#' ```
#'
#' @export
tm_g_forest_tte.picks <- function(label,
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
  arm_var_picks <- if (!is.null(arm_var)) {
    teal.picks::picks(teal.picks::datasets(parentname), arm_var)
  } else {
    NULL
  }

  # paramcd: filter picks on dataname by PARAMCD value
  paramcd_picks <- teal.picks::picks(
    teal.picks::datasets(dataname),
    variables = paramcd_var,
    values    = paramcd_value
  )

  # aval_var: analysis variable from dataname (time-to-event)
  aval_var_picks <- teal.picks::picks(teal.picks::datasets(dataname), aval_var)

  # cnsr_var: censoring variable from dataname
  cnsr_var_picks <- teal.picks::picks(teal.picks::datasets(dataname), cnsr_var)

  # time_unit_var: time unit variable from dataname
  time_unit_var_picks <- teal.picks::picks(teal.picks::datasets(dataname), time_unit_var)

  # subgroup_var: subgroup variables from parentname (multiple, ordered)
  subgroup_var_picks <- teal.picks::picks(teal.picks::datasets(parentname), subgroup_var)

  # strata_var: stratification variables from parentname
  strata_var_picks <- teal.picks::picks(teal.picks::datasets(parentname), strata_var)

  args <- as.list(environment())

  module(
    label       = label,
    server      = srv_g_forest_tte.picks,
    ui          = ui_g_forest_tte.picks,
    ui_args     = args[names(args) %in% names(formals(ui_g_forest_tte.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_forest_tte.picks))],
    transformators = transformators,
    datanames   = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_forest_tte.picks <- function(id,
                                  arm_var_picks,
                                  paramcd_picks,
                                  aval_var_picks,
                                  cnsr_var_picks,
                                  time_unit_var_picks,
                                  subgroup_var_picks,
                                  strata_var_picks,
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
        teal.picks::picks_ui(ns("paramcd_picks"), paramcd_picks)
      ),
      tags$div(
        tags$label("Analysis Variable:"),
        teal.picks::picks_ui(ns("aval_var_picks"), aval_var_picks)
      ),
      tags$div(
        tags$label("Censor Variable:"),
        teal.picks::picks_ui(ns("cnsr_var_picks"), cnsr_var_picks)
      ),
      if (!is.null(arm_var_picks)) {
        tags$div(
          tags$div(
            tags$label("Select Treatment Variable:"),
            teal.picks::picks_ui(ns("arm_var_picks"), arm_var_picks)
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
        teal.picks::picks_ui(ns("subgroup_var_picks"), subgroup_var_picks)
      ),
      tags$div(
        tags$label("Stratify by:"),
        teal.picks::picks_ui(ns("strata_var_picks"), strata_var_picks)
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
            teal.picks::picks_ui(ns("time_unit_var_picks"), time_unit_var_picks)
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
srv_g_forest_tte.picks <- function(id,
                                   data,
                                   dataname,
                                   parentname,
                                   arm_var_picks,
                                   arm_ref_comp,
                                   paramcd_picks,
                                   aval_var_picks,
                                   cnsr_var_picks,
                                   time_unit_var_picks,
                                   subgroup_var_picks,
                                   strata_var_picks,
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
    if (!is.null(arm_var_picks)) {
      iv_arm_ref <- arm_ref_comp_observer(
        session,
        input,
        output,
        id_arm_var   = "arm_var_picks-variables-selected",
        data         = reactive(data()[[parentname]]),
        arm_ref_comp = arm_ref_comp,
        module       = "tm_g_forest_tte"
      )
    }

    picks_list <- Filter(Negate(is.null), list(
      arm_var_picks      = arm_var_picks,
      paramcd_picks      = paramcd_picks,
      aval_var_picks     = aval_var_picks,
      cnsr_var_picks     = cnsr_var_picks,
      time_unit_var_picks = time_unit_var_picks,
      subgroup_var_picks  = subgroup_var_picks,
      strata_var_picks    = strata_var_picks
    ))

    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data  = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId   = "paramcd_picks-values-selected",
        condition = !is.null(selectors$paramcd_picks()$values$selected),
        message   = "Please select an endpoint (PARAMCD)."
      )
      teal:::validate_input(
        inputId   = "aval_var_picks-variables-selected",
        condition = !is.null(selectors$aval_var_picks()$variables$selected),
        message   = "An analysis variable is required."
      )
      teal:::validate_input(
        inputId   = "cnsr_var_picks-variables-selected",
        condition = !is.null(selectors$cnsr_var_picks()$variables$selected),
        message   = "A censor variable is required."
      )
      if (!is.null(arm_var_picks)) {
        teal:::validate_input(
          inputId   = "arm_var_picks-variables-selected",
          condition = !is.null(selectors$arm_var_picks()$variables$selected),
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
      selectors   = selectors[c("paramcd_picks", "aval_var_picks", "cnsr_var_picks", "time_unit_var_picks")],
      join_fun    = "dplyr::inner_join",
      output_name = "ANL"
    )

    # Merge ADSL selectors: arm_var + subgroup_var + strata_var → ANL_ADSL
    adsl_selector_names <- intersect(
      c("arm_var_picks", "subgroup_var_picks", "strata_var_picks"),
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

      input_arm_var       <- selectors$arm_var_picks()$variables$selected
      input_aval_var      <- selectors$aval_var_picks()$variables$selected
      input_cnsr_var      <- selectors$cnsr_var_picks()$variables$selected
      input_subgroup_var  <- selectors$subgroup_var_picks()$variables$selected
      input_strata_var    <- selectors$strata_var_picks()$variables$selected
      input_paramcd       <- selectors$paramcd_picks()$variables$selected
      input_time_unit_var <- selectors$time_unit_var_picks()$variables$selected

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

      input_arm_var       <- selectors$arm_var_picks()$variables$selected
      input_aval_var      <- selectors$aval_var_picks()$variables$selected
      input_cnsr_var      <- selectors$cnsr_var_picks()$variables$selected
      input_subgroup_var  <- selectors$subgroup_var_picks()$variables$selected
      input_strata_var    <- selectors$strata_var_picks()$variables$selected
      input_time_unit_var <- selectors$time_unit_var_picks()$variables$selected

      # obj_var_name: label for the selected PARAMCD level, read directly from selector
      obj_var_name <- selectors$paramcd_picks()$values$selected %||% ""

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
