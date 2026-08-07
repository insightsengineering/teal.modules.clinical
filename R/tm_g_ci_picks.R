#' @describeIn tm_g_ci teal.picks encodings via \code{picks} objects for \code{x_var}, \code{y_var}, and \code{color}
#' (use [`tm_g_ci()`] to pass [`teal.picks::variables()`] objects; they are wrapped into \code{picks}).
#' @export
tm_g_ci.default <- function(label,
                            x_var,
                            y_var,
                            paramcd,
                            avisit,
                            color,
                            stat = c("mean", "median"),
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
  checkmate::assert_multi_class(x_var, c("picks"))
  checkmate::assert_multi_class(y_var, c("picks"))
  checkmate::assert_multi_class(color, c("picks"))
  checkmate::assert_multi_class(paramcd, c("picks"))
  checkmate::assert_multi_class(avisit, c("picks"))
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

  paramcd <- create_picks_helper(y_var$datasets, paramcd)
  avisit <- create_picks_helper(y_var$datasets, avisit)
  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_ci,
    ui = ui_g_ci,
    ui_args = args[names(args) %in% names(formals(ui_g_ci))],
    server_args = args[names(args) %in% names(formals(srv_g_ci))],
    transformators = transformators
  )
}

#' @keywords internal
ui_g_ci <- function(id, # nolint: object_name.
                    x_var,
                    y_var,
                    paramcd,
                    avisit,
                    color,
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
        teal.picks::picks_ui(ns("x_var"), x_var)
      ),
      tags$div(
        tags$label("Select lab (PARAMCD):"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Select visit (AVISIT):"),
        teal.picks::picks_ui(ns("avisit"), avisit)
      ),
      tags$div(
        tags$label("Analysis Value (y axis):"),
        teal.picks::picks_ui(ns("y_var"), y_var)
      ),
      tags$div(
        tags$label("Groups (color):"),
        teal.picks::picks_ui(ns("color"), color)
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
        choices = formals(tm_g_ci)$stat,
        selected = stat
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "plot"))
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_ci <- function(id, # nolint: object_name.
                     data,
                     x_var,
                     y_var,
                     paramcd,
                     avisit,
                     color,
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
      x_var = x_var,
      y_var = y_var,
      paramcd = paramcd,
      avisit = avisit,
      color = color
    )

    selectors <- teal.picks::picks_srv(picks = picks_list, data = data)

    validated_q <- reactive({
      obj <- req(data())

      teal::validate_input(
        inputId = "x_var-variables-selected",
        condition = length(selectors$x_var()$variables$selected) > 0L,
        message = "Please select a treatment variable (x axis)."
      )
      teal::validate_input(
        inputId = "paramcd-values-selected",
        condition = length(selectors$paramcd()$values$selected) > 0L,
        message = "Please select a lab parameter (PARAMCD)."
      )
      teal::validate_input(
        inputId = "avisit-values-selected",
        condition = length(selectors$avisit()$values$selected) > 0L,
        message = "Please select a visit (AVISIT)."
      )
      teal::validate_input(
        inputId = "y_var-variables-selected",
        condition = length(selectors$y_var()$variables$selected) > 0L,
        message = "Please select an analysis value variable (y axis)."
      )
      teal::validate_input(
        inputId = "color-variables-selected",
        condition = length(selectors$color()$variables$selected) > 0L,
        message = "Please select a grouping variable (color)."
      )
      teal::validate_input(
        inputId = "conf_level",
        condition = !is.null(input$conf_level),
        message = "Please choose a confidence level."
      )
      teal::validate_input(
        inputId = "conf_level",
        condition = {
          cv <- suppressWarnings(as.numeric(input$conf_level))
          !is.na(cv) && cv > 0 && cv < 1
        },
        message = "Confidence level must be between 0 and 1."
      )

      validate(
        teal::need_input(
          inputId = c("y_var-datasets-selected", "paramcd-datasets-selected"),
          condition = identical(
            selectors$y_var()$datasets$selected,
            selectors$paramcd()$datasets$selected,
          ),
          message = "Analysis and Treatment variables must be from the same dataset."
        ),
        teal::need_input(
          inputId = c("y_var-datasets-selected", "avisit-datasets-selected"),
          condition = identical(
            selectors$y_var()$datasets$selected,
            selectors$avisit()$datasets$selected,
          ),
          message = "Analysis and visit variables must be from the same dataset."
        )
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Confidence Interval Plot"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      obj
    })

    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data = validated_q,
      selectors = selectors,
      output_name = "ANL"
    )

    all_q <- reactive({
      obj <- anl_inputs$data()

      x_sel <- selectors$x_var()$variables$selected
      y_sel <- selectors$y_var()$variables$selected

      teal::validate_has_data(obj[["ANL"]], min_nrow = 2)

      validate(
        need(
          !all(is.na(obj[["ANL"]][[y_sel]])),
          "No valid data. Please check the filtering options for analysis value (y axis)."
        )
      )

      x_sel <- selectors$x_var()$variables$selected
      y_sel <- selectors$y_var()$variables$selected
      color_sel <- selectors$color()$variables$selected
      paramcd_sel <- selectors$paramcd()$values$selected
      avisit_sel <- selectors$avisit()$values$selected

      x_label <- teal.modules.clinical::column_annotation_label(obj[["ANL"]], x_sel)
      y_label <- teal.modules.clinical::column_annotation_label(obj[["ANL"]], y_sel)
      color_label <- if (!is.null(color_sel) && length(color_sel) > 0) {
        teal.modules.clinical::column_annotation_label(obj[["ANL"]], color_sel)
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

      list_calls <- template_g_ci(
        dataname = "ANL",
        x_var = x_sel,
        y_var = y_sel,
        grp_var = if (length(color_sel) == 0 || is.null(color_sel)) NULL else color_sel,
        stat = input$stat,
        conf_level = as.numeric(input$conf_level),
        ggplot2_args = gg_args
      )

      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, list_calls)
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
