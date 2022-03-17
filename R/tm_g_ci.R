#' Template: Confidence Interval Plot
#'
#' Writes the expressions to filter data and draw confidence interval
#' estimation.
#'
#' @inheritParams template_arguments
#' @param x_var (`character`)\cr treatment variable corresponding to the x axis.
#' @param y_var (`character`)\cr response variable corresponding to the y axis.
#' @param grp_var (`character`)\cr group variable corresponding to the colors
#'  point shape and line type.
#' @param stat (`character`)\cr either `mean` or `median`.
#' @param unit_var (`character`)\cr variable name in `dataname` where the unit is
#'  read.
#'
#' @seealso [tm_g_ci()]
#' @keywords internal
#'
template_g_ci <- function(dataname, # nolint
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
          expr = ggplot(
            data = ANL,
            mapping = aes(
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
          expr = ggplot(
            data = ANL,
            mapping = aes(
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
        expr = stat_summary(
          fun.data = fun,
          geom = "errorbar",
          width = .1,
          position = position_dodge(width = .5)
        ),
        env = list(
          fun = switch(stat,
            mean = substitute(stat_mean_ci),
            median = substitute(stat_median_ci)
          )
        )
      )
    )
  } else {
    add_expr(
      expr_ls = graph_list,
      new_expr = substitute(
        expr = stat_summary(
          fun.data = fun,
          geom = "errorbar",
          width = .1,
          position = position_dodge(width = .5)
        ),
        env = list(
          fun = switch(stat,
            mean = substitute(
              expr = function(x) stat_mean_ci(x, conf_level = conf_level),
              env = list(conf_level = conf_level)
            ),
            median = substitute(
              expr = function(x) stat_median_ci(x, conf_level = conf_level),
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
      expr = stat_summary(
        fun = fun,
        geom = "point",
        position = position_dodge(width = .5)
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
          x = "Treatment Group"
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
      gg <- graph_expr
      print(gg)
    },
    env = list(graph_expr = pipe_expr(graph_list, pipe_str = "+"))
  )
}

#' Teal Module: Confidence Interval Plot (`CIG01`)
#'
#' The module generates the R code and returns the corresponding output.
#'
#' @inheritParams module_arguments
#' @inheritParams template_g_ci
#' @param color (`data_extract_spec`)\cr the group variable (color, line type
#'   and point shape).
#'
#' @export
#' @examples
#'
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- synthetic_cdisc_data('latest')$adsl
#'     ADLB <- synthetic_cdisc_data('latest')$adlb"
#'   ),
#'   modules = modules(
#'     tm_g_ci(
#'       label = "Confidence Interval Plot",
#'       x_var = teal.transform::data_extract_spec(
#'         dataname = "ADSL",
#'         select = teal.transform::select_spec(
#'           choices = c("ARMCD", "BMRKR2"),
#'           selected = c("ARMCD"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y_var = teal.transform::data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           teal.transform::filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           teal.transform::filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = teal.transform::select_spec(
#'           label = "Analyzed Value",
#'           choices = c("AVAL", "CHG"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       color = teal.transform::data_extract_spec(
#'         dataname = "ADSL",
#'         select = teal.transform::select_spec(
#'           label = "Color by variable",
#'           choices = c("SEX", "STRATA1", "STRATA2"),
#'           selected = c("STRATA1"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   ),
#'   header = "Example of Confidence Interval Plot",
#'   footer = tags$p(
#'     class = "text-muted", "Source: `teal.modules.clinical::tm_g_ci`"
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
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
                    ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_ci")
  checkmate::assert_string(label)
  stat <- match.arg(stat)
  checkmate::assert_class(y_var, classes = "data_extract_spec")
  checkmate::assert_class(x_var, classes = "data_extract_spec")
  checkmate::assert_class(color, classes = "data_extract_spec")
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

  args <- as.list(environment())

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
      ggplot2_args = ggplot2_args
    ),
    ui = ui_g_ci,
    ui_args = args,
    filters = "all"
  )
}

ui_g_ci <- function(id, ...) { # nolint
  ns <- NS(id)
  args <- list(...)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(args[c("x_var", "y_var", "color")]),
      teal.transform::data_extract_ui(
        id = ns("x_var"),
        label = "Treatment (x axis)",
        data_extract_spec = args$x_var
      ),
      teal.transform::data_extract_ui(
        id = ns("y_var"),
        label = "Analyzed Value (y axis)",
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
      )
    ),
    forms = teal::get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_g_ci <- function(id, # nolint
                     datasets,
                     x_var,
                     y_var,
                     color,
                     label,
                     plot_height,
                     plot_width,
                     ggplot2_args) {
  stopifnot(is_cdisc_data(datasets))
  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()

    merged_data <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(x_var = x_var, y_var = y_var, color = color)
    )

    validate_data <- reactive({
      validate(
        need(
          length(merged_data()$columns_source$x_var) > 0,
          "Select a treatment (x axis)."
        )
      )
      validate(
        need(
          length(merged_data()$columns_source$y_var) > 0,
          "Select an analyzed value (y axis)."
        )
      )
      teal::validate_has_data(merged_data()$data(), min_nrow = 2)

      validate(need(
        input$conf_level >= 0 && input$conf_level <= 1,
        "Please choose a confidence level between 0 and 1"
      ))
    })

    list_calls <- reactive(
      template_g_ci(
        dataname = "ANL",
        x_var = merged_data()$columns_source$x_var,
        y_var = merged_data()$columns_source$y_var,
        grp_var = if (length(merged_data()$columns_source$color) == 0) {
          NULL
        } else {
          merged_data()$columns_source$color
        },
        stat = input$stat,
        conf_level = as.numeric(input$conf_level),
        ggplot2_args = ggplot2_args
      )
    )

    eval_call <- reactive({
      validate_data()
      teal.code::chunks_reset()
      teal.code::chunks_push_data_merge(x = merged_data())
      teal.code::chunks_push(list_calls())
    })

    plot_r <- reactive({
      eval_call()
      teal.code::chunks_safe_eval()
      teal.code::chunks_get_var("gg")
    })

    teal::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.transform::get_extract_datanames(list(x_var, y_var, color)),
      modal_title = label
    )

    teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )
  })
}
