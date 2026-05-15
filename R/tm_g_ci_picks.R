#' @describeIn tm_g_ci teal.picks encodings via \code{picks} objects for \code{x_var}, \code{y_var}, and \code{color}
#' (use [`tm_g_ci.variables()`] to pass [`teal.picks::variables()`] objects; they are wrapped into \code{picks}).
#' @export
tm_g_ci.picks <- function(label,
                          x_var = teal.picks::picks(
                            teal.picks::datasets("ADSL"),
                            teal.picks::variables(
                              choices = c("ARMCD", "BMRKR2"),
                              selected = "ARMCD",
                              multiple = FALSE
                            )
                          ),
                          y_var = teal.picks::picks(
                            teal.picks::datasets("ADLB"),
                            teal.picks::variables(
                              choices = c("AVAL", "CHG", "CHG2"),
                              selected = "AVAL",
                              multiple = FALSE
                            )
                          ),
                          color = teal.picks::picks(
                            teal.picks::datasets("ADSL"),
                            teal.picks::variables(
                              choices = c("SEX", "STRATA1", "STRATA2"),
                              selected = "STRATA1",
                              multiple = FALSE
                            )
                          ),
                          stat = c("mean", "median"),
                          paramcd = teal.picks::values(
                            choices = c("ALT", "CRP", "IGA"),
                            selected = "ALT",
                            multiple = FALSE
                          ),
                          avisit = teal.picks::values(
                            choices = c(
                              "SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15",
                              "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"
                            ),
                            selected = "SCREENING",
                            multiple = FALSE
                          ),
                          conf_level = teal.picks::values(c(0.95, 0.9, 0.8), 0.95),
                          plot_height = c(700L, 200L, 2000L),
                          plot_width = NULL,
                          pre_output = NULL,
                          post_output = NULL,
                          ggplot2_args = teal.widgets::ggplot2_args(),
                          transformators = list(),
                          decorators = list()) {
  stat <- match.arg(stat)

  checkmate::assert_string(label)
  x_dataname <- "ADSL"
  y_dataname <- "ADLB"

  checkmate::assert_class(x_var, "picks")
  checkmate::assert_class(y_var, "picks")
  checkmate::assert_class(color, "picks")
  checkmate::assert_class(paramcd, "values")
  checkmate::assert_class(avisit, "values")
  checkmate::assert_class(conf_level, "values")
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
  teal::assert_decorators(decorators, "plot")

  x_var_picks <- x_var

  paramcd_picks <- teal.picks::picks(
    teal.picks::datasets(y_dataname, y_dataname),
    teal.picks::variables("PARAMCD"),
    paramcd
  )
  avisit_picks <- teal.picks::picks(
    teal.picks::datasets(y_dataname, y_dataname),
    teal.picks::variables("AVISIT"),
    avisit
  )

  y_var_picks <- y_var

  color_picks <- color

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_ci.picks,
    ui = ui_g_ci.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_ci.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_ci.picks))],
    transformators = transformators,
    datanames = c(x_dataname, y_dataname)
  )
}

#' @describeIn tm_g_ci teal.picks encodings via \code{variables} (recommended entry point).
#' @export
tm_g_ci.variables <- function(label,
                              x_var = teal.picks::variables(
                                choices = c("ARMCD", "BMRKR2"),
                                selected = "ARMCD",
                                multiple = FALSE
                              ),
                              y_var = teal.picks::variables(
                                choices = c("AVAL", "CHG", "CHG2"),
                                selected = "AVAL",
                                multiple = FALSE
                              ),
                              color = teal.picks::variables(
                                choices = c("SEX", "STRATA1", "STRATA2"),
                                selected = "STRATA1",
                                multiple = FALSE
                              ),
                              stat = c("mean", "median"),
                              paramcd = teal.picks::values(
                                choices = c("ALT", "CRP", "IGA"),
                                selected = "ALT",
                                multiple = FALSE
                              ),
                              avisit = teal.picks::values(
                                choices = c(
                                  "SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15",
                                  "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"
                                ),
                                selected = "SCREENING",
                                multiple = FALSE
                              ),
                              conf_level = teal.picks::values(c(0.95, 0.9, 0.8), 0.95),
                              plot_height = c(700L, 200L, 2000L),
                              plot_width = NULL,
                              pre_output = NULL,
                              post_output = NULL,
                              ggplot2_args = teal.widgets::ggplot2_args(),
                              transformators = list(),
                              decorators = list()) {
  tm_g_ci.picks(
    label = label,
    x_var = teal.picks::picks(teal.picks::datasets("ADSL"), x_var),
    y_var = teal.picks::picks(teal.picks::datasets("ADLB"), y_var),
    color = teal.picks::picks(teal.picks::datasets("ADSL"), color),
    stat = stat,
    paramcd = paramcd,
    avisit = avisit,
    conf_level = conf_level,
    plot_height = plot_height,
    plot_width = plot_width,
    pre_output = pre_output,
    post_output = post_output,
    ggplot2_args = ggplot2_args,
    transformators = transformators,
    decorators = decorators
  )
}

#' @keywords internal
ui_g_ci.picks <- function(id, # nolint: object_name.
                          x_var_picks,
                          y_var_picks,
                          paramcd_picks,
                          avisit_picks,
                          color_picks,
                          conf_level,
                          stat,
                          pre_output,
                          post_output,
                          decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Treatment (x axis):"),
        teal.picks::picks_ui(ns("x_var_picks"), x_var_picks)
      ),
      tags$div(
        tags$label("Select lab (PARAMCD):"),
        teal.picks::picks_ui(ns("paramcd_picks"), paramcd_picks)
      ),
      tags$div(
        tags$label("Select visit (AVISIT):"),
        teal.picks::picks_ui(ns("avisit_picks"), avisit_picks)
      ),
      tags$div(
        tags$label("Analysis Value (y axis):"),
        teal.picks::picks_ui(ns("y_var_picks"), y_var_picks)
      ),
      tags$div(
        tags$label("Groups (color):"),
        teal.picks::picks_ui(ns("color_picks"), color_picks)
      ),
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        choices = conf_level$choices,
        selected = conf_level$selected,
        multiple = FALSE,
        fixed = teal.picks::is_pick_fixed(conf_level)
      ),
      radioButtons(
        inputId = ns("stat"),
        label = "Statistic to use",
        choices = c("mean", "median"),
        selected = stat
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "plot"))
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_ci.picks <- function(id, # nolint: object_name.
                           data,
                           x_dataname,
                           y_dataname,
                           x_var_picks,
                           y_var_picks,
                           paramcd_picks,
                           avisit_picks,
                           color_picks,
                           label,
                           plot_height,
                           plot_width,
                           ggplot2_args,
                           decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    picks_list <- list(
      x_var_picks = x_var_picks,
      y_var_picks = y_var_picks,
      paramcd_picks = paramcd_picks,
      avisit_picks = avisit_picks,
      color_picks = color_picks
    )

    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data  = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId = "x_var_picks-variables-selected",
        condition = length(selectors$x_var_picks()$variables$selected) > 0L,
        message = "Please select a treatment variable (x axis)."
      )
      teal:::validate_input(
        inputId = "paramcd_picks-values-selected",
        condition = length(selectors$paramcd_picks()$values$selected) > 0L,
        message = "Please select a lab parameter (PARAMCD)."
      )
      teal:::validate_input(
        inputId = "avisit_picks-values-selected",
        condition = length(selectors$avisit_picks()$values$selected) > 0L,
        message = "Please select a visit (AVISIT)."
      )
      teal:::validate_input(
        inputId = "y_var_picks-variables-selected",
        condition = length(selectors$y_var_picks()$variables$selected) > 0L,
        message = "Please select an analysis value variable (y axis)."
      )
      teal:::validate_input(
        inputId = "color_picks-variables-selected",
        condition = length(selectors$color_picks()$variables$selected) > 0L,
        message = "Please select a grouping variable (color)."
      )
      teal:::validate_input(
        inputId = "conf_level",
        condition = !is.null(input$conf_level),
        message = "Please choose a confidence level."
      )
      teal:::validate_input(
        inputId = "conf_level",
        condition = {
          cv <- suppressWarnings(as.numeric(input$conf_level))
          !is.na(cv) && cv > 0 && cv < 1
        },
        message = "Confidence level must be between 0 and 1."
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Confidence Interval Plot"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      obj
    })

    # Merge y_dataname selectors (paramcd filter + avisit filter + y variable)
    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data = validated_q,
      selectors = selectors[c("paramcd_picks", "avisit_picks", "y_var_picks")],
      output_name = "ANL_Y"
    )

    # Merge x_dataname selectors (x variable + optional color variable)
    x_selectors <- selectors[c("x_var_picks", "color_picks")]
    adsl_inputs <- teal.picks::merge_srv(
      "adsl_inputs",
      data = validated_q,
      selectors = x_selectors,
      output_name = "ANL_X"
    )

    anl_q <- reactive({
      anl <- c(anl_inputs$data(), adsl_inputs$data())

      x <- selectors$x_var_picks()$variables$selected
      y <- selectors$y_var_picks()$variables$selected

      teal::validate_has_data(anl[["ANL_Y"]], min_nrow = 2)

      validate(
        need(
          !all(is.na(anl[["ANL_Y"]][[y]])),
          "No valid data. Please check the filtering options for analysis value (y axis)."
        )
      )
      anl
    })

    all_q <- reactive({
      obj <- anl_q()

      x <- selectors$x_var_picks()$variables$selected
      y <- selectors$y_var_picks()$variables$selected
      color <- selectors$color_picks()$variables$selected

      paramcd_sel <- selectors$paramcd_picks()$values$selected
      avisit_sel <- selectors$avisit_picks()$values$selected

      x_label <- teal.modules.clinical::column_annotation_label(obj[[x_dataname]], x)
      y_label <- teal.modules.clinical::column_annotation_label(obj[[y_dataname]], y)
      color_label <- if (!is.null(color) && length(color) > 0) {
        teal.modules.clinical::column_annotation_label(obj[[x_dataname]], color)
      } else {
        NULL
      }

      gg_args <- ggplot2_args
      gg_args$labs$title <- paste("Confidence Interval Plot by", x_label)
      gg_args$labs$x <- x_label
      gg_args$labs$subtitle <- paste("Visit:", avisit_sel)
      gg_args$labs$y <- paste(paramcd_sel, y_label)
      gg_args$labs$color <- color_label
      gg_args$labs$lty <- color_label
      gg_args$labs$shape <- color_label

      # Build ANL by joining ANL_X and ANL_Y
      join_call <- quote(ANL <- dplyr::inner_join(ANL_X, ANL_Y))

      list_calls <- template_g_ci(
        dataname = "ANL",
        x_var = x,
        y_var = y,
        grp_var = if (length(color) == 0 || is.null(color)) NULL else color,
        stat = input$stat,
        conf_level = as.numeric(input$conf_level),
        ggplot2_args = gg_args
      )

      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, join_call) |>
        teal.code::eval_code(list_calls)
    })

    decorated_plot_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "plot"),
      expr = quote(plot)
    )

    plot_r <- reactive(decorated_plot_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_plot_q)
  })
}
