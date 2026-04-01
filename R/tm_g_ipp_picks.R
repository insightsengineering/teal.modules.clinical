#' teal Module: Individual Patient Plots (teal.picks)
#'
#' This module produces [ggplot2::ggplot()] type individual patient plots that display trends in parameter
#' values over time for each patient, using data with ADaM structure.
#'
#' This is a `teal.picks` implementation of [tm_g_ipp()].
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_g_ipp
#' @inheritParams template_arguments
#' @param arm_var ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   specification for the arm variable (filter-style).
#' @param paramcd ([teal.picks::values()] or [teal.transform::choices_selected()])\cr
#'   specification for the parameter code filter.
#' @param aval_var ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   specification for the analysis value variable.
#' @param avalu_var ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   specification for the analysis value unit variable.
#' @param id_var ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   specification for the patient identifier variable.
#' @param visit_var ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   specification for the visit/timepoint variable.
#' @param baseline_var ([teal.picks::variables()] or [teal.transform::choices_selected()])\cr
#'   specification for the baseline value variable.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot`)
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.clinical")`.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @export
tm_g_ipp.picks <- function(label,
                           dataname,
                           parentname = "ADSL",
                           arm_var = teal.picks::variables("ARMCD"),
                           paramcd_var = teal.picks::variables("PARAMCD"),
                           paramcd_value = teal.picks::values(multiple = FALSE),
                           aval_var = teal.picks::variables("AVAL", fixed = TRUE),
                           avalu_var = teal.picks::variables("AVALU", fixed = TRUE),
                           id_var = teal.picks::variables("USUBJID", fixed = TRUE),
                           visit_var = teal.picks::variables("AVISIT"),
                           baseline_var = teal.picks::variables("BASE", fixed = TRUE),
                           add_baseline_hline = FALSE,
                           separate_by_obs = FALSE,
                           suppress_legend = FALSE,
                           add_avalu = TRUE,
                           plot_height = c(1200L, 400L, 5000L),
                           plot_width = NULL,
                           pre_output = NULL,
                           post_output = NULL,
                           ggplot2_args = teal.widgets::ggplot2_args(),
                           transformators = list(),
                           decorators = list(),
                           # legacy choices_selected arguments kept for back-compat
                           paramcd) {
  message("Initializing tm_g_ipp")

  # Compatibility layer: convert choices_selected -> teal.picks
  for (arg in c("arm_var", "aval_var", "avalu_var", "id_var", "visit_var", "baseline_var")) {
    if (inherits(get(arg), "choices_selected")) {
      assign(arg, teal.picks::as.picks(get(arg)))
    }
  }

  if (missing(paramcd)) {
    checkmate::assert_class(paramcd_var, "variables")
    checkmate::assert_class(paramcd_value, "values")
    paramcd <- teal.picks::picks(
      datasets(dataname), variables = paramcd_var, values = paramcd_value
    )
  } else {
    if (!missing(paramcd_var) || !missing(paramcd_value)) {
      stop("Please provide either `paramcd` or `paramcd_var` with `paramcd_value`, not both.")
    }
    checkmate::assert_class(paramcd, "choices_selected")
    paramcd <- teal.picks::as.picks(paramcd)
  }
  # End of compatibility

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(avalu_var, "variables")
  checkmate::assert_class(id_var, "variables")
  checkmate::assert_class(visit_var, "variables")
  checkmate::assert_class(baseline_var, "variables")
  checkmate::assert_flag(add_baseline_hline)
  checkmate::assert_flag(separate_by_obs)
  checkmate::assert_flag(suppress_legend)
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

  # Build picks objects bound to datasets
  arm_var   <- teal.picks::picks(datasets(parentname), arm_var)
  aval_var  <- teal.picks::picks(datasets(dataname), aval_var)
  avalu_var <- teal.picks::picks(datasets(dataname), avalu_var)
  id_var    <- teal.picks::picks(datasets(dataname), id_var)
  visit_var <- teal.picks::picks(datasets(dataname), visit_var)
  baseline_var <- teal.picks::picks(datasets(dataname), baseline_var)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_ipp.picks,
    ui = ui_g_ipp.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_ipp.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_ipp.picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_ipp.picks <- function(id,
                           arm_var,
                           paramcd,
                           aval_var,
                           avalu_var,
                           id_var,
                           visit_var,
                           baseline_var,
                           add_baseline_hline,
                           separate_by_obs,
                           suppress_legend,
                           add_avalu,
                           pre_output,
                           post_output,
                           decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Arm:"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      ),
      tags$div(
        tags$label("Select Parameter:"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Timepoint Variable:"),
        teal.picks::picks_ui(ns("visit_var"), visit_var)
      ),
      tags$div(
        tags$label("Parameter Values over Time:"),
        teal.picks::picks_ui(ns("aval_var"), aval_var)
      ),
      tags$div(
        tags$label("Patient ID:"),
        teal.picks::picks_ui(ns("id_var"), id_var)
      ),
      tags$div(
        tags$label("Analysis Variable Unit:"),
        teal.picks::picks_ui(ns("avalu_var"), avalu_var)
      ),
      tags$div(
        tags$label("Baseline Parameter Values:"),
        teal.picks::picks_ui(ns("baseline_var"), baseline_var)
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional plot settings",
          checkboxInput(
            ns("add_baseline_hline"),
            "Add reference lines at baseline value",
            value = add_baseline_hline
          ),
          checkboxInput(
            ns("separate_by_obs"),
            "Separate plots by ID",
            value = separate_by_obs
          ),
          checkboxInput(
            ns("suppress_legend"),
            "Suppress legend",
            value = suppress_legend
          ),
          checkboxInput(
            ns("add_avalu"),
            "Add unit value in title/y axis",
            value = add_avalu
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_ipp.picks <- function(id,
                            data,
                            dataname,
                            parentname,
                            arm_var,
                            paramcd,
                            aval_var,
                            avalu_var,
                            id_var,
                            visit_var,
                            baseline_var,
                            plot_height,
                            plot_width,
                            label,
                            ggplot2_args,
                            decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- teal.picks::picks_srv(
      picks = list(
        arm_var   = arm_var,
        paramcd   = paramcd,
        aval_var  = aval_var,
        avalu_var = avalu_var,
        id_var    = id_var,
        visit_var = visit_var,
        baseline_var = baseline_var
      ),
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())
      validate_input(
        inputId = "arm_var-variables-selected",
        condition = !is.null(selectors$arm_var()$variables$selected),
        message = "Arm variable is empty."
      )
      validate_input(
        inputId = "paramcd-values-selected",
        condition = !is.null(selectors$paramcd()$values$selected),
        message = "`Select Parameter` field is empty"
      )
      validate_input(
        inputId = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message = "A Parameter values over Time must be selected"
      )
      validate_input(
        inputId = "avalu_var-variables-selected",
        condition = !is.null(selectors$avalu_var()$variables$selected),
        message = "An Analysis Variable Unit must be selected"
      )
      validate_input(
        inputId = "id_var-variables-selected",
        condition = !is.null(selectors$id_var()$variables$selected),
        message = "A Patient ID must be selected"
      )
      validate_input(
        inputId = "visit_var-variables-selected",
        condition = !is.null(selectors$visit_var()$variables$selected),
        message = "A Timepoint Variable must be selected"
      )
      validate_input(
        inputId = "baseline_var-variables-selected",
        condition = !is.null(selectors$baseline_var()$variables$selected),
        message = "Baseline Parameter Values must be selected"
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

    adsl_inputs <- teal.picks::merge_srv(
      "adsl_inputs",
      data = validated_q,
      selectors = selectors["arm_var"],
      output_name = "ANL_ADSL"
    )

    validate_checks <- reactive({
      adsl_filtered <- adsl_inputs$data()[[parentname]]
      anl_filtered  <- anl_inputs$data()[[dataname]]

      input_arm_var    <- anl_inputs$variables()$arm_var
      input_aval_var   <- anl_inputs$variables()$aval_var
      input_avalu_var  <- anl_inputs$variables()$avalu_var
      input_id_var     <- anl_inputs$variables()$id_var
      input_visit_var  <- anl_inputs$variables()$visit_var
      input_baseline_var <- anl_inputs$variables()$baseline_var
      input_paramcd    <- anl_inputs$variables()$paramcd

      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("STUDYID", input_id_var, input_arm_var),
        anl = anl_filtered,
        anlvars = c(
          "STUDYID",
          input_id_var,
          input_arm_var,
          input_aval_var,
          input_avalu_var,
          input_paramcd,
          input_visit_var,
          input_baseline_var
        ),
        arm_var = input_arm_var
      )

      do.call(what = "validate_standard_inputs", validate_args)
      NULL
    })

    all_q <- reactive({
      validate_checks()

      ANL <- anl_inputs$data()[["ANL"]]
      teal::validate_has_data(ANL, 2)

      input_arm_var    <- anl_inputs$variables()$arm_var
      input_avalu_var  <- anl_inputs$variables()$avalu_var
      input_paramcd    <- anl_inputs$variables()$paramcd

      avalu_first   <- as.character(ANL[[input_avalu_var]][1])
      paramcd_first <- as.character(ANL[[input_paramcd]][1])
      arm_levels    <- levels(droplevels(ANL[[input_arm_var]]))

      my_calls <- template_g_ipp(
        dataname     = "ANL",
        aval_var     = anl_inputs$variables()$aval_var,
        avalu_var    = input_avalu_var,
        avalu_first  = avalu_first,
        id_var       = anl_inputs$variables()$id_var,
        visit_var    = anl_inputs$variables()$visit_var,
        baseline_var = anl_inputs$variables()$baseline_var,
        add_baseline_hline = input$add_baseline_hline,
        separate_by_obs    = input$separate_by_obs,
        suppress_legend    = input$suppress_legend,
        paramcd      = input_paramcd,
        paramcd_first = paramcd_first,
        arm_var      = input_arm_var,
        arm_levels   = arm_levels,
        ggplot2_args = ggplot2_args,
        add_avalu    = input$add_avalu
      )

      obj <- c(anl_inputs$data(), adsl_inputs$data())
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_all_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "plot")
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
