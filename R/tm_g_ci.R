library(teal.devel)
library(dplyr)
library(ggplot2)


#' Template: Confidence Interval Plot
#'
#' Writes the expressions to filter data and draw confidence interval
#' estimation.
#'
#' @inheritParams argument_convention
#' @param x_var (`string`)\cr treatment variable corresponding to the x axis.
#' @param y_var (`string`)\cr response variable corresponding to the y axis.
#' @param grp_var (`string`)\cr group variable corresponding to the colors
#'  point shape and line type.
#' @param stat (`string`)\cr either `mean` or `median`.
#' @param unit_var (`string`)\cr variable name in `dataname` where the unit is
#'  read.
#'
template_g_ci <- function(dataname, # nousage # nolint
                          x_var,
                          y_var,
                          grp_var = NULL,
                          stat = c("mean", "median"),
                          conf_level = 0.95,
                          unit_var = "AVALU") {
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
          fun = switch(
            stat,
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
          fun = switch(
            stat,
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
      ) + labs(
        title = title,
        caption = caption,
        x = "Treatment Group"
      ),
      env = list(
        fun = switch(
          stat,
          mean = substitute(mean),
          median = substitute(median)
        ),
        title = paste("Confidence Interval Plot by Treatment Group"),
        caption = paste0(
          switch(stat, mean = "Mean", median = "Median"),
          " and ", 100 * conf_level, "% CIs for ",
          stat,
          " are displayed."
        )
      )
    )
  )

  substitute(
    expr = gg <- graph_expr,
    env = list(graph_expr = pipe_expr(graph_list, pipe_str = "+"))
  )

}


ui_g_ci <- function(id, ...) { # nousage # nolint
  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = plotOutput(ns("plot")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x_var", "y_var", "color")]),
      data_extract_input(
        id = ns("x_var"),
        label = "Treatment (x axis)",
        data_extract_spec = args$x_var
      ),
      data_extract_input(
        id = ns("y_var"),
        label = "Analyzed Value (y axis)",
        data_extract_spec = args$y_var
      ),
      data_extract_input(
        id = ns("color"),
        label = "Groups (color)",
        data_extract_spec = args$color
      ),
      numericInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        value = 0.95,
        min = 0.01,
        max = 0.99,
        step = 0.01,
        width = "100%"
      ),
      radioButtons(
        inputId = ns("stat"),
        label = "Statistic to use",
        choices = c("mean", "median"),
        selected = args$stat
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
}

srv_g_ci <- function(input, # nousage # nolint
                     output,
                     session,
                     datasets,
                     x_var,
                     y_var,
                     color) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(x_var, y_var, color),
    input_id = c("x_var", "y_var", "color")
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
    validate_has_data(merged_data()$data(), min_nrow = 15)

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
      conf_level = input$conf_level
    )
  )

  eval_call <- reactive({
    validate_data()
    chunks_reset()
    chunks_push_data_merge(x = merged_data())
    chunks_push(list_calls())
  })

  output$plot <- renderPlot({
    eval_call()
    chunks_safe_eval()
    chunks_get_var("gg")
  })

  # Show the R code when user clicks show_rcode
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R code for custom plot",
      rcode = get_rcode(
        datasets = datasets,
        title = "R code for custom plot"
      )
    )
  })
}

#' Teal Module: Confidence Interval Plot (`CIG01`)
#'
#' The module generates the R code and returns the corresponding output.
#'
#' @inheritParams template_g_ci
#' @inheritParams argument_convention
#' @param x_var (`data_extract_spec`)\cr the candidate treatment variable
#'   (x axis).
#' @param y_var (`data_extract_spec`)\cr the candidate analyzed variable
#'   (y axis).
#' @param color (`data_extract_spec`)\cr the group variable (color, line type
#'   and point shape).
#'
#' @seealso [teal::data_extract_spec()]
#'
#' @export
#' @examples
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- radsl(cached = TRUE)
#'     ADLB <- radlb(cached = TRUE)"
#'   ),
#'   modules = root_modules(
#'     tm_g_ci(
#'       label = "Confidence Interval Plot",
#'       x_var = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = c("ARMCD", "BMRKR2"),
#'           selected = c("ARMCD" ),
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
#'   ),
#'   header = "Example of Confidence Interval Plot",
#'   footer = tags$p(
#'     class = "text-muted", "Source: `teal.modules.clinical::tm_g_ci`"
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_ci <- function(label,
                    x_var,
                    y_var,
                    color,
                    stat = c("mean", "median")) {

  stat <- match.arg(stat)
  stopifnot(
    is.character(label),
    is_class_list("data_extract_spec")(list(y_var, x_var, color))
  )
  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_ci,
    server_args = list(
      x_var = x_var,
      y_var = y_var,
      color = color
    ),
    ui = ui_g_ci,
    ui_args = args,
    filters = "all"
  )
}
