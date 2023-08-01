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
        expr = ggplot2::stat_summary(
          fun.data = fun,
          geom = "errorbar",
          width = .1,
          position = ggplot2::position_dodge(width = .5)
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
#'
#' @examples
#' library(nestcolor)
#'
#' adsl <- tmc_ex_adsl
#' adlb <- tmc_ex_adlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADLB", adlb)
#'   ),
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
#'             choices = levels(adlb$PARAMCD),
#'             selected = levels(adlb$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(adlb$AVISIT),
#'             selected = levels(adlb$AVISIT)[1],
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
#'   footer = shiny::tags$p(
#'     class = "text-muted", "Source: `teal.modules.clinical::tm_g_ci`"
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
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
    datanames = "all"
  )
}

ui_g_ci <- function(id, ...) { # nolint
  ns <- shiny::NS(id)
  args <- list(...)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
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
      shiny::radioButtons(
        inputId = ns("stat"),
        label = "Statistic to use",
        choices = c("mean", "median"),
        selected = args$stat
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_g_ci <- function(id, # nolint
                     data,
                     reporter,
                     filter_panel_api,
                     x_var,
                     y_var,
                     color,
                     label,
                     plot_height,
                     plot_width,
                     ggplot2_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
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

    iv_r <- shiny::reactive({
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
      join_keys = get_join_keys(data),
      selector_list = selector_list
    )

    anl_q <- shiny::reactive(
      teal.code::eval_code(
        object = teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)),
        code = as.expression(anl_inputs()$expr)
      )
    )

    all_q <- shiny::reactive({
      teal::validate_inputs(iv_r())
      teal::validate_has_data(anl_q()[["ANL"]], min_nrow = 2)

      x <- anl_inputs()$columns_source$x_var
      y <- anl_inputs()$columns_source$y_var
      color <- anl_inputs()$columns_source$color

      shiny::validate(
        shiny::need(
          !all(is.na(anl_q()[["ANL"]][[y]])),
          "No valid data. Please check the filtering option for analysis value (y axis)"
        )
      )

      x_label <- column_annotation_label(data[[attr(x, "dataname")]](), x)
      y_label <- column_annotation_label(data[[attr(y, "dataname")]](), y)
      color_label <- if (length(color)) {
        column_annotation_label(data[[attr(color, "dataname")]](), color)
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
      teal.code::eval_code(anl_q(), list_calls)
    })

    plot_r <- shiny::reactive(all_q()[["gg"]])

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(all_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = label
    )

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("CI Plot")
        card$append_text("CI Plot", "header2")
        card$append_text("Confidence Interval Plot", "header3")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(all_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
