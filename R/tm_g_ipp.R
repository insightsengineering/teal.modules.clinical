#' Template: Individual Patient Plots
#'
#' Creates a valid expression to generate [ggplot2::ggplot()] plots of individual patients.
#'
#' @inheritParams template_arguments
#' @param visit_var (`character`)\cr name of the variable for visit timepoints.
#' @param add_baseline_hline (`logical`)\cr whether a horizontal line should be added to the plot at baseline y-value.
#' @param separate_by_obs (`logical`)\cr whether to create multi-panel plots.
#' @param suppress_legend (`logical`)\cr whether to suppress the plot legend.
#' @param arm_levels (`character`)\cr vector of all levels of `arm_var`.
#' @param avalu_first (`character`)\cr `avalu_var` text to append to the plot title and y-axis label if `add_avalu` is
#'   `TRUE`.
#' @param paramcd_first (`character`)\cr `paramcd` text to append to the plot title and y-axis label.
#' @param add_avalu (`logical`)\cr whether `avalu_first` text should be appended to the plot title and y-axis label.
#' @param ggplot2_args (`ggplot2_args`) optional\cr object created by [teal.widgets::ggplot2_args()] with settings
#'   for the module plot. For this module, this argument will only accept `ggplot2_args` object with `labs` list of
#'   the following child elements: `title`, `subtitle`, `x`, `y`. No other elements are taken into account. The
#'   argument is merged with option `teal.ggplot2_args` and with default module arguments (hard coded in the module
#'   body).
#'
#'   For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_ipp()]
#'
#' @keywords internal
template_g_ipp <- function(dataname = "ANL",
                           paramcd,
                           arm_var,
                           arm_levels,
                           avalu_first,
                           paramcd_first,
                           aval_var = "AVAL",
                           avalu_var = "AVALU",
                           id_var = "USUBJID",
                           visit_var = "AVISIT",
                           baseline_var = "BASE",
                           add_baseline_hline = FALSE,
                           separate_by_obs = FALSE,
                           ggplot2_args = teal.widgets::ggplot2_args(),
                           suppress_legend = FALSE,
                           add_avalu = TRUE) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(paramcd)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(avalu_var)
  checkmate::assert_string(id_var)
  checkmate::assert_string(visit_var)
  checkmate::assert_string(baseline_var)
  checkmate::assert_flag(add_baseline_hline)
  checkmate::assert_flag(separate_by_obs)
  checkmate::assert_flag(suppress_legend)
  checkmate::assert_flag(add_avalu)

  y <- list()
  # Data preprocessing

  y$data <- substitute(
    expr = anl <- df %>% droplevels(),
    env = list(df = as.name(dataname))
  )

  title <- ifelse(
    add_avalu,
    sprintf("Individual Patient Plot for %s Values (%s) over Time", paramcd_first, avalu_first),
    sprintf("Individual Patient Plot for %s Values over Time", paramcd_first)
  )
  y_axis <- ifelse(
    add_avalu,
    sprintf("%s (%s)", paramcd_first, avalu_first),
    paramcd_first
  )

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = teal.widgets::ggplot2_args(
      labs = list(
        title = title,
        x = "Visit",
        y = y_axis,
        subtitle = paste(arm_levels, collapse = ", ")
      )
    )
  )

  graph_list <- list()
  graph_list <- add_expr(
    graph_list,
    substitute(
      expr = {
        plot <- tern::h_g_ipp(
          df = anl,
          xvar = visit,
          yvar = aval,
          xlab = xlab_val,
          ylab = ylab_val,
          title = title_val,
          subtitle = subtitle_val,
          id_var = id,
          add_baseline_hline = add_baseline_hline,
          yvar_baseline = base
        )
      },
      env = list(
        xlab_val = all_ggplot2_args$labs$x,
        ylab_val = all_ggplot2_args$labs$y,
        title_val = all_ggplot2_args$labs$title,
        subtitle_val = all_ggplot2_args$labs$subtitle,
        paramcd = paramcd,
        visit = visit_var,
        aval = aval_var,
        id = id_var,
        add_baseline_hline = add_baseline_hline,
        base = baseline_var,
        avalu = avalu_var,
        arm = arm_var
      )
    )
  )

  if (separate_by_obs) {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(id)),
        env = list(id = as.name(id_var))
      )
    )
  }

  if (suppress_legend) {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = {
          plot <- plot + ggplot2::theme(legend.position = "none")
        },
        env = list(id = as.name(id_var))
      )
    )
  }

  y$graph <- bracket_expr(graph_list)

  y
}

#' teal Module: Individual Patient Plots
#'
#' This module produces [ggplot2::ggplot()] type individual patient plots that display trends in parameter
#' values over time for each patient, using data with ADaM structure.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_g_ipp
#' @inheritParams template_arguments
#' @param arm_var ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for variable values that can be used as arm variable.
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
#' tm_g_ipp(
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
#'   library(tern)
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl %>%
#'     slice(1:20) %>%
#'     df_explicit_na()
#'   ADLB <- tmc_ex_adlb %>%
#'     filter(USUBJID %in% ADSL$USUBJID) %>%
#'     df_explicit_na() %>%
#'     filter(AVISIT != "SCREENING")
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_ipp(
#'       label = "Individual Patient Plot",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         value_choices(ADLB, "ARMCD"),
#'         "ARM A"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(ADLB, "PARAMCD"),
#'         "ALT"
#'       ),
#'       aval_var = choices_selected(
#'         variable_choices(ADLB, c("AVAL", "CHG")),
#'         "AVAL"
#'       ),
#'       avalu_var = choices_selected(
#'         variable_choices(ADLB, c("AVALU")),
#'         "AVALU",
#'         fixed = TRUE
#'       ),
#'       id_var = choices_selected(
#'         variable_choices(ADLB, c("USUBJID")),
#'         "USUBJID",
#'         fixed = TRUE
#'       ),
#'       visit_var = choices_selected(
#'         variable_choices(ADLB, c("AVISIT")),
#'         "AVISIT"
#'       ),
#'       baseline_var = choices_selected(
#'         variable_choices(ADLB, c("BASE")),
#'         "BASE",
#'         fixed = TRUE
#'       ),
#'       add_baseline_hline = FALSE,
#'       separate_by_obs = FALSE
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_ipp <- function(label,
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
    server = srv_g_ipp,
    ui = ui_g_ipp,
    ui_args = args[names(args) %in% names(formals(ui_g_ipp))],
    server_args = args[names(args) %in% names(formals(srv_g_ipp))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_ipp <- function(id,
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
srv_g_ipp <- function(id,
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
