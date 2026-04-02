#' @param x (`picks` or `NULL`)\cr variable on the x-axis.
#' @param fill (`picks` or `NULL`)\cr grouping variable to determine bar colors.
#' @param x_facet (`picks` or `NULL`)\cr row-wise faceting groups.
#' @param y_facet (`picks` or `NULL`)\cr column-wise faceting groups.
#' @export
tm_g_barchart_simple.picks <- function(x = teal.picks::picks(
                                         teal.picks::datasets(),
                                         teal.picks::variables(selected = NULL, multiple = FALSE)
                                       ),
                                       fill = teal.picks::picks(
                                         teal.picks::datasets(),
                                         teal.picks::variables(selected = NULL, multiple = FALSE)
                                       ),
                                       x_facet = teal.picks::picks(
                                         teal.picks::datasets(),
                                         teal.picks::variables(selected = NULL, multiple = FALSE)
                                       ),
                                       y_facet = teal.picks::picks(
                                         teal.picks::datasets(),
                                         teal.picks::variables(selected = NULL, multiple = FALSE)
                                       ),
                                       label = "Count Barchart",
                                       plot_options = NULL,
                                       plot_height = c(600L, 200L, 2000L),
                                       plot_width = NULL,
                                       pre_output = NULL,
                                       post_output = NULL,
                                       ggplot2_args = teal.widgets::ggplot2_args(),
                                       transformators = list(),
                                       decorators = list()) {
  message("Initializing tm_g_barchart_simple")
  checkmate::assert_string(label)
  checkmate::assert_list(plot_options, null.ok = TRUE)
  checkmate::assert_class(x, "picks", null.ok = TRUE)
  checkmate::assert_class(fill, "picks", null.ok = TRUE)
  checkmate::assert_class(x_facet, "picks", null.ok = TRUE)
  checkmate::assert_class(y_facet, "picks", null.ok = TRUE)

  if (length(c(x, fill, x_facet, y_facet)) == 0) {
    stop("at least one must be specified: 'x', 'fill', 'x_facet', 'y_facet'")
  }

  # Force single-variable selection for facet variables
  if (!is.null(x_facet) && isTRUE(attr(x_facet$variables, "multiple"))) {
    warning("`x_facet` accepts only a single variable selection. Forcing multiple to FALSE.")
    attr(x_facet$variables, "multiple") <- FALSE
  }
  if (!is.null(y_facet) && isTRUE(attr(y_facet$variables, "multiple"))) {
    warning("`y_facet` accepts only a single variable selection. Forcing multiple to FALSE.")
    attr(y_facet$variables, "multiple") <- FALSE
  }
  if (!is.null(x) && isTRUE(attr(x$variables, "multiple"))) {
    warning("`x` accepts only a single variable selection. Forcing multiple to FALSE.")
    attr(x$variables, "multiple") <- FALSE
  }
  if (!is.null(fill) && isTRUE(attr(fill$variables, "multiple"))) {
    warning("`fill` accepts only a single variable selection. Forcing multiple to FALSE.")
    attr(fill$variables, "multiple") <- FALSE
  }

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
  teal::assert_decorators(decorators, names = "plot")

  plot_options <- utils::modifyList(
    list(stacked = FALSE), # default
    `if`(is.null(plot_options), list(), plot_options)
  )

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_barchart_simple.picks,
    ui = ui_g_barchart_simple.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_barchart_simple.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_barchart_simple.picks))],
    transformators = transformators,
    datanames = teal.modules.general::.picks_datanames(list(x, fill, x_facet, y_facet))
  )
}

#' @keywords internal
ui_g_barchart_simple.picks <- function(id,
                                       x,
                                       fill,
                                       x_facet,
                                       y_facet,
                                       plot_options,
                                       plot_height,
                                       plot_width,
                                       pre_output,
                                       post_output,
                                       decorators) {
  ns <- NS(id)

  tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("myplot")),
        uiOutput(ns("table"), style = "overflow-y: scroll; max-height: 250px;")
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"), tags$br(),
        if (!is.null(x)) {
          tags$div(
            tags$strong("X variable"),
            teal.picks::picks_ui(id = ns("x"), picks = x)
          )
        },
        if (!is.null(fill)) {
          tags$div(
            tags$strong("Fill"),
            teal.picks::picks_ui(id = ns("fill"), picks = fill)
          )
        },
        if (!is.null(x_facet)) {
          tags$div(
            tags$strong("Column facetting variable"),
            teal.picks::picks_ui(id = ns("x_facet"), picks = x_facet)
          )
        },
        if (!is.null(y_facet)) {
          tags$div(
            tags$strong("Row facetting variable"),
            teal.picks::picks_ui(id = ns("y_facet"), picks = y_facet)
          )
        },
        teal::ui_transform_teal_data(
          ns("decorator"),
          transformators = select_decorators(decorators, "plot")
        ),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Additional plot settings",
            if (!is.null(fill)) {
              radioButtons(
                inputId = ns("barlayout"),
                label = "Covariate Bar Layout",
                choices = c("Side by side" = "side_by_side", "Stacked" = "stacked"),
                selected = if (plot_options$stacked) "stacked" else "side_by_side",
                inline = TRUE
              )
            },
            if (!is.null(x_facet)) {
              checkboxInput(
                ns("facet_scale_x"),
                "Fixed scales for column facets",
                value = TRUE
              )
            },
            if (!is.null(y_facet)) {
              checkboxInput(
                ns("facet_scale_y"),
                "Fixed scales for row facets",
                value = TRUE
              )
            },
            checkboxInput(
              ns("label_bars"),
              "Label bars",
              value = `if`(is.null(plot_options$label_bars), TRUE, plot_options$label_bars)
            ),
            checkboxInput(
              ns("rotate_bar_labels"),
              "Rotate bar labels",
              value = `if`(is.null(plot_options$rotate_bar_labels), FALSE, plot_options$rotate_bar_labels)
            ),
            checkboxInput(
              ns("rotate_x_label"),
              "Rotate x label",
              value = `if`(is.null(plot_options$rotate_x_label), FALSE, plot_options$rotate_x_label)
            ),
            checkboxInput(
              ns("rotate_y_label"),
              "Rotate y label",
              value = `if`(is.null(plot_options$rotate_y_label), FALSE, plot_options$rotate_y_label)
            ),
            checkboxInput(
              ns("flip_axis"),
              "Flip axes",
              value = `if`(is.null(plot_options$flip_axis), FALSE, plot_options$flip_axis)
            ),
            checkboxInput(
              ns("show_n"),
              "Show n",
              value = `if`(is.null(plot_options$show_n), TRUE, plot_options$show_n)
            ),
            sliderInput(
              inputId = ns("expand_y_range"),
              label = "Y-axis range expansion (fraction on top)",
              min = 0,
              max = 1,
              value = 0.5,
              step = 0.1
            )
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

#' @keywords internal
srv_g_barchart_simple.picks <- function(id,
                                        data,
                                        x,
                                        fill,
                                        x_facet,
                                        y_facet,
                                        plot_height,
                                        plot_width,
                                        ggplot2_args,
                                        decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    # Build picks list (omit NULLs)
    picks_list <- Filter(Negate(is.null), list(x = x, fill = fill, x_facet = x_facet, y_facet = y_facet))

    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data = data
    )

    # Helper: get currently selected variable name for a given slot
    sel_var <- function(name) {
      if (is.null(picks_list[[name]])) {
        return(NULL)
      }
      as.vector(selectors[[name]]()$variables$selected)
    }

    anl_q <- reactive({
      validate_input(
        inputId = "x-variables-selected",
        condition = is.null(x) || length(sel_var("x")) > 0,
        message = "Please select an x-variable."
      )

      obj <- req(data())
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj %>% teal.code::eval_code('library("dplyr")')
    })

    merged <- teal.picks::merge_srv("merge", data = anl_q, selectors = selectors, output_name = "ANL")

    # Validate duplicated variable selection
    validate_checks <- reactive({
      x_name <- sel_var("x")
      fill_name <- sel_var("fill")
      x_facet_name <- sel_var("x_facet")
      y_facet_name <- sel_var("y_facet")

      all_vars <- c(x_name, fill_name, x_facet_name, y_facet_name)
      dups <- duplicated(all_vars[!is.na(all_vars)])
      validate_input(
        inputId = "x-variables-selected",
        condition = !any(dups),
        message = "Each variable can only be used once across X, Fill, Column facet, and Row facet."
      )

      teal::validate_has_data(merged$data()[["ANL"]], 2)
    })

    count_q <- reactive({
      validate_checks()

      anl_q <- merged$data()
      groupby_vars <- r_groupby_vars()

      count_by_group <- function(groupby_vars, data_name) {
        n_name <- get_n_name(groupby_vars)
        count_by_group_expr(groupby_vars = groupby_vars, data_name = data_name)
      }

      count_exprs <- count_by_group(groupby_vars, data_name = "ANL")

      if (input$show_n) {
        count_exprs2 <- sapply(groupby_vars[-1], count_by_group, data_name = "counts")
        count_str_to_col_exprs <- sapply(groupby_vars[-1], count_str_to_column_expr)
        count_exprs <- c(count_exprs, count_exprs2, count_str_to_col_exprs)
      }

      anl_q <- anl_q %>%
        teal.code::eval_code(code = count_exprs)

      # Slice to one row per subgroup and select relevant columns
      anl_q <- anl_q %>%
        teal.code::eval_code(
          as.expression(
            c(
              bquote(attr(counts[[.(get_n_name(groupby_vars))]], "label") <- "Count"),
              bquote(
                counts <- counts %>%
                  dplyr::group_by_at(.(as.vector(groupby_vars))) %>%
                  dplyr::slice(1) %>%
                  dplyr::ungroup() %>%
                  dplyr::select(.(as.vector(groupby_vars)), dplyr::starts_with("n_"))
              )
            )
          )
        )

      anl_q
    })

    all_q <- reactive({
      groupby_vars <- as.list(r_groupby_vars())

      y_lab <- substitute(
        teal.modules.clinical::column_annotation_label(counts, y_name),
        list(y_name = get_n_name(groupby_vars))
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args,
        module_plot = teal.widgets::ggplot2_args(
          labs = list(
            title = quote(plot_title),
            y = y_lab
          ),
          theme = list(plot.title = quote(ggplot2::element_text(hjust = 0.5)))
        )
      )

      plot_call <- make_barchart_simple_call(
        y_name = get_n_name(groupby_vars),
        x_name = groupby_vars$x_name,
        fill_name = groupby_vars$fill_name,
        x_facet_name = groupby_vars$x_facet_name,
        y_facet_name = groupby_vars$y_facet_name,
        label_bars = input$label_bars,
        barlayout = input$barlayout,
        flip_axis = input$flip_axis,
        rotate_bar_labels = input$rotate_bar_labels,
        rotate_x_label = input$rotate_x_label,
        rotate_y_label = input$rotate_y_label,
        expand_y_range = input$expand_y_range,
        facet_scales = get_facet_scale(input$facet_scale_x, input$facet_scale_y),
        ggplot2_args = all_ggplot2_args
      )

      ANL <- count_q()[["ANL"]]
      obj <- count_q() %>%
        teal.code::eval_code(substitute(
          env = list(groupby_vars = paste(groupby_vars, collapse = ", ")),
          plot_title <- sprintf(
            "Number of patients (total N = %s) for each combination of (%s)",
            nrow(ANL),
            groupby_vars
          )
        ))
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, code = plot_call)
    })

    decorated_all_q_code <- teal::srv_transform_teal_data(
      "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "plot"),
      expr = quote(plot)
    )

    plot_r <- reactive(decorated_all_q_code()[["plot"]])

    output$table <- renderTable({
      count_q()[["counts"]]
    })

    # Get grouping variables from selectors (NULL if slot not used)
    r_groupby_vars <- function() {
      x_name <- sel_var("x")
      fill_name <- sel_var("fill")
      x_facet_name <- sel_var("x_facet")
      y_facet_name <- sel_var("y_facet")

      # Treat empty character selections as NULL
      if (identical(x_name, character(0))) x_name <- NULL
      if (identical(fill_name, character(0))) fill_name <- NULL
      if (identical(x_facet_name, character(0))) x_facet_name <- NULL
      if (identical(y_facet_name, character(0))) y_facet_name <- NULL

      c(
        x_name = x_name, fill_name = fill_name,
        x_facet_name = x_facet_name, y_facet_name = y_facet_name
      )
    }

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_all_q_code)
  })
}
