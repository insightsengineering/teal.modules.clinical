#' Template: Confidence Interval Plot
#'
#' Creates a valid expression to generate a [ggplot2::ggplot()] confidence interval plot.
#'
#' @inheritParams template_arguments
#' @param x_var (`character`)\cr name of the treatment variable to put on the x-axis.
#' @param y_var (`character`)\cr name of the response variable to put on the y-axis.
#' @param grp_var (`character`)\cr name of the group variable used to determine the plot colors, point shapes,
#'   and line types.
#' @param stat (`character`)\cr statistic to plot. Options are `"mean"` and `"median"`.
#' @param unit_var (`character`)\cr name of the unit variable.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_ci()]
#'
#' @keywords internal
template_g_ci <- function(dataname,
                          x_var,
                          y_var,
                          grp_var = NULL,
                          stat = c("mean", "median"),
                          conf_level = 0.95,
                          unit_var = "AVALU",
                          ggplot2_args = teal.widgets::ggplot2_args()) {
  stat <- match.arg(stat)

  graph_list <- list()
  graph_list <- if (is.null(grp_var)) {
    add_expr(
      expr_ls = graph_list,
      new_expr = {
        substitute(
          expr = ggplot2::ggplot(
            data = ANL,
            mapping = ggplot2::aes(
              x = x_var,
              y = y_var
            )
          ),
          env = list(
            x_var = as.name(x_var),
            y_var = as.name(y_var)
          )
        )
      }
    )
  } else {
    add_expr(
      expr_ls = graph_list,
      new_expr = {
        substitute(
          expr = ggplot2::ggplot(
            data = ANL,
            mapping = ggplot2::aes(
              x = x_var,
              y = y_var,
              color = grp_var,
              lty = grp_var,
              shape = grp_var
            )
          ),
          env = list(
            x_var = as.name(x_var),
            y_var = as.name(y_var),
            grp_var = as.name(grp_var)
          )
        )
      }
    )
  }

  graph_list <- if (conf_level == 0.95) {
    add_expr(
      expr_ls = graph_list,
      new_expr = substitute(
        expr = ggplot2::stat_summary(
          fun.data = fun,
          geom = "errorbar",
          width = .1,
          position = ggplot2::position_dodge(width = .5)
        ),
        env = list(
          fun = switch(stat,
            mean = substitute(tern::stat_mean_ci),
            median = substitute(tern::stat_median_ci)
          )
        )
      )
    )
  } else {
    add_expr(
      expr_ls = graph_list,
      new_expr = substitute(
        expr = ggplot2::stat_summary(
          fun.data = fun,
          geom = "errorbar",
          width = .1,
          position = ggplot2::position_dodge(width = .5)
        ),
        env = list(
          fun = switch(stat,
            mean = substitute(
              expr = function(x) tern::stat_mean_ci(x, conf_level = conf_level),
              env = list(conf_level = conf_level)
            ),
            median = substitute(
              expr = function(x) tern::stat_median_ci(x, conf_level = conf_level),
              env = list(conf_level = conf_level)
            )
          )
        )
      )
    )
  }

  graph_list <- add_expr(
    expr_ls = graph_list,
    new_expr = substitute(
      expr = ggplot2::stat_summary(
        fun = fun,
        geom = "point",
        position = ggplot2::position_dodge(width = .5)
      ),
      env = list(
        fun = switch(stat,
          mean = quote(mean),
          median = quote(median)
        )
      )
    )
  )

  parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
    teal.widgets::resolve_ggplot2_args(
      user_plot = ggplot2_args,
      module_plot = teal.widgets::ggplot2_args(
        labs = list(
          title = "Confidence Interval Plot by Treatment Group",
          caption = sprintf(
            "%s and %i%% CIs for %s are displayed.",
            switch(stat,
              mean = "Mean",
              median = "Median"
            ),
            100 * conf_level,
            stat
          ),
          x = "Treatment Group",
          y = "Value",
          color = "",
          lty = "",
          shape = ""
        ),
        theme = list()
      )
    )
  )

  graph_list <- add_expr(
    expr_ls = graph_list,
    new_expr = parsed_ggplot2_args$labs
  )

  if (!is.null(parsed_ggplot2_args$theme)) {
    graph_list <- add_expr(
      expr_ls = graph_list,
      new_expr = parsed_ggplot2_args$theme
    )
  }

  substitute(
    expr = {
      plot <- graph_expr
    },
    env = list(graph_expr = pipe_expr(graph_list, pipe_str = "+"))
  )
}

#' teal Module: Confidence Interval Plot
#'
#' This module produces a [ggplot2::ggplot()] type confidence interval plot consistent with the TLG Catalog template
#' `CIG01` available [here](https://insightsengineering.github.io/tlg-catalog/stable/graphs/other/cig01.html).
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_g_ci
#' @param color (`data_extract_spec`)\cr the group variable used to determine the plot colors, shapes, and line types.
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
#' tm_g_ci(
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
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl
#'   ADLB <- tmc_ex_adlb
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_ci(
#'       label = "Confidence Interval Plot",
#'       x_var = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = c("ARMCD", "BMRKR2"),
#'           selected = c("ARMCD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y_var = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           label = "Analyzed Value",
#'           choices = c("AVAL", "CHG"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       color = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Color by variable",
#'           choices = c("SEX", "STRATA1", "STRATA2"),
#'           selected = c("STRATA1"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_ci <- function(label,
                    x_var,
                    y_var,
                    color,
                    stat = c("mean", "median"),
                    conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                    plot_height = c(700L, 200L, 2000L),
                    plot_width = NULL,
                    pre_output = NULL,
                    post_output = NULL,
                    ggplot2_args = teal.widgets::ggplot2_args(),
                    transformators = list(),
                    decorators = list()) {
  message("Initializing tm_g_ci")
  checkmate::assert_string(label)
  stat <- match.arg(stat)
  checkmate::assert_class(y_var, classes = "data_extract_spec")
  checkmate::assert_class(x_var, classes = "data_extract_spec")
  checkmate::assert_class(color, classes = "data_extract_spec")
  x_var <- teal.transform::list_extract_spec(x_var, allow_null = TRUE)
  y_var <- teal.transform::list_extract_spec(y_var, allow_null = TRUE)
  color <- teal.transform::list_extract_spec(color, allow_null = TRUE)
  teal.transform::check_no_multiple_selection(x_var)
  teal.transform::check_no_multiple_selection(y_var)
  teal.transform::check_no_multiple_selection(color)

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
  checkmate::assert_class(ggplot2_args, "ggplot2_args")
  assert_decorators(decorators, "plot")

  args <- as.list(environment())

  data_extract_list <- list(x_var = x_var, y_var = y_var, color = color)

  module(
    label = label,
    server = srv_g_ci,
    server_args = list(
      x_var = x_var,
      y_var = y_var,
      color = color,
      label = label,
      plot_height = plot_height,
      plot_width = plot_width,
      ggplot2_args = ggplot2_args,
      decorators = decorators
    ),
    transformators = transformators,
    ui = ui_g_ci,
    ui_args = args,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_g_ci <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(args[c("x_var", "y_var", "color")]),
      teal.transform::data_extract_ui(
        id = ns("x_var"),
        label = "Treatment (x axis)",
        data_extract_spec = args$x_var
      ),
      teal.transform::data_extract_ui(
        id = ns("y_var"),
        label = "Analysis Value (y axis)",
        data_extract_spec = args$y_var
      ),
      teal.transform::data_extract_ui(
        id = ns("color"),
        label = "Groups (color)",
        data_extract_spec = args$color
      ),
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        choices = args$conf_level$choices,
        selected = args$conf_level$selected,
        multiple = FALSE,
        fixed = args$conf_level$fixed
      ),
      radioButtons(
        inputId = ns("stat"),
        label = "Statistic to use",
        choices = c("mean", "median"),
        selected = args$stat
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "plot"))
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @keywords internal
srv_g_ci <- function(id,
                     data,
                     x_var,
                     y_var,
                     color,
                     label,
                     plot_height,
                     plot_width,
                     ggplot2_args,
                     decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(x_var = x_var, y_var = y_var, color = color),
      datasets = data,
      select_validation_rule = list(
        x_var = shinyvalidate::sv_required("Select a treatment (x axis)"),
        y_var = shinyvalidate::sv_required("Select an analysis value (y axis)")
      ),
      filter_validation_rule = list(
        y_var = shinyvalidate::sv_required(message = "Please select the filters.")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(0, 1, message_fmt = "Please choose a confidence level between 0 and 1")
      )
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      join_keys = teal.data::join_keys(data),
      selector_list = selector_list
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# CI Plot"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>% teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    all_q <- reactive({
      teal::validate_inputs(iv_r())
      teal::validate_has_data(anl_q()[["ANL"]], min_nrow = 2)

      x <- anl_inputs()$columns_source$x_var
      y <- anl_inputs()$columns_source$y_var
      color <- anl_inputs()$columns_source$color

      validate(
        need(
          !all(is.na(anl_q()[["ANL"]][[y]])),
          "No valid data. Please check the filtering option for analysis value (y axis)"
        )
      )

      x_label <- teal.modules.clinical::column_annotation_label(data()[[attr(x, "dataname")]], x)
      y_label <- teal.modules.clinical::column_annotation_label(data()[[attr(y, "dataname")]], y)
      color_label <- if (length(color)) {
        teal.modules.clinical::column_annotation_label(data()[[attr(color, "dataname")]], color)
      } else {
        NULL
      }

      ggplot2_args$labs$title <- paste("Confidence Interval Plot by", x_label)
      ggplot2_args$labs$x <- x_label
      ggplot2_args$labs$subtitle <- paste("Visit:", anl_inputs()$filter_info$y_var[[2]]$selected[[1]])
      ggplot2_args$labs$y <- paste(
        anl_inputs()$filter_info$y_var[[1]]$selected[[1]],
        y_label
      )
      ggplot2_args$labs$color <- color_label
      ggplot2_args$labs$lty <- color_label
      ggplot2_args$labs$shape <- color_label
      list_calls <- template_g_ci(
        dataname = "ANL",
        x_var = x,
        y_var = y,
        grp_var = if (length(color) == 0) {
          NULL
        } else {
          color
        },
        stat = input$stat,
        conf_level = as.numeric(input$conf_level),
        ggplot2_args = ggplot2_args
      )
      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Plot")
      teal.code::eval_code(obj, list_calls)
    })

    decorated_plot_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr = plot
    )
    # Outputs to render.
    plot_r <- reactive(decorated_plot_q()[["plot"]])

    # Render R code
    source_code_r <- reactive(teal.code::get_code(req(decorated_plot_q())))
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = label
    )

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_plot_q)
  })
}
