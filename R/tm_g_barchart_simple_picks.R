#' @describeIn tm_g_barchart_simple [teal.picks]-based encodings (`picks`).
#' @export
tm_g_barchart_simple.picks <- function(
    x = NULL,
    fill = NULL,
    x_facet = NULL,
    y_facet = NULL,
    label = "Count Barchart",
    plot_options = NULL,
    plot_height = c(600L, 200L, 2000L),
    plot_width = NULL,
    pre_output = NULL,
    post_output = NULL,
    ggplot2_args = teal.widgets::ggplot2_args(),
    transformators = list(),
    decorators = list()) {
  checkmate::assert_list(plot_options, null.ok = TRUE)

  encoding_slots <- list(x = x, fill = fill, x_facet = x_facet, y_facet = y_facet)
  for (arg_name in names(encoding_slots)) {
    p <- encoding_slots[[arg_name]]
    if (is.null(p)) {
      next
    }
    checkmate::assert_class(p, "picks", .var.name = arg_name)
    checkmate::assert_false(
      teal.picks::is_pick_multiple(p$variables),
      .var.name = sprintf("`%s` must use variables(..., multiple = FALSE)", arg_name)
    )
  }

  picks_slot_datanames <- function(p) {
    if (is.null(p)) {
      return(character(0L))
    }
    ch <- p$datasets$choices
    if (checkmate::test_character(ch, min.len = 1L)) {
      return(unique(as.character(ch)))
    }
    sel <- p$datasets$selected
    unique(as.character(sel))
  }

  all_datanames <- unique(unlist(
    list(
      picks_slot_datanames(x),
      picks_slot_datanames(fill),
      picks_slot_datanames(x_facet),
      picks_slot_datanames(y_facet)
    ),
    use.names = FALSE
  ))
  if (length(all_datanames) == 0L) {
    stop("Could not infer dataset names from `x`, `fill`, `x_facet`, and `y_facet`. ", call. = FALSE)
  }

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(
    plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height"
  )
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
    list(stacked = FALSE),
    `if`(is.null(plot_options), list(), plot_options)
  )

  args <- list(
    x = x,
    fill = fill,
    x_facet = x_facet,
    y_facet = y_facet,
    label = label,
    plot_options = plot_options,
    plot_height = plot_height,
    plot_width = plot_width,
    pre_output = pre_output,
    post_output = post_output,
    ggplot2_args = ggplot2_args,
    transformators = transformators,
    decorators = decorators
  )

  module(
    label = label,
    server = srv_g_barchart_simple_picks,
    ui = ui_g_barchart_simple_picks,
    ui_args = args[names(args) %in% names(formals(ui_g_barchart_simple_picks))],
    server_args = args[names(args) %in% names(formals(srv_g_barchart_simple_picks))],
    transformators = transformators,
    datanames = all_datanames
  )
}

#' @keywords internal
ui_g_barchart_simple_picks <- function(id,
                                       x,
                                       fill,
                                       x_facet,
                                       y_facet,
                                       plot_options,
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
          tags$div(tags$label("X variable"), teal.picks::picks_ui(ns("x"), x))
        },
        if (!is.null(fill)) {
          tags$div(tags$label("Fill"), teal.picks::picks_ui(ns("fill"), fill))
        },
        if (!is.null(x_facet)) {
          tags$div(tags$label("Column facetting variable"), teal.picks::picks_ui(ns("x_facet"), x_facet))
        },
        if (!is.null(y_facet)) {
          tags$div(tags$label("Row facetting variable"), teal.picks::picks_ui(ns("y_facet"), y_facet))
        },
        teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "plot")),
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
srv_g_barchart_simple_picks <- function(id,
                                        data,
                                        x,
                                        fill,
                                        x_facet,
                                        y_facet,
                                        label,
                                        plot_height,
                                        plot_width,
                                        ggplot2_args,
                                        decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    picks_inputs <- Filter(
      Negate(is.null),
      list(x = x, fill = fill, x_facet = x_facet, y_facet = y_facet)
    )

    anl_selectors <- teal.picks::picks_srv(
      id = "",
      picks = picks_inputs,
      data = data
    )

    data_with_card <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj
    })

    merged_anl <- teal.picks::merge_srv(
      "merge_anl",
      data = data_with_card,
      selectors = anl_selectors,
      output_name = "ANL",
      join_fun = "dplyr::inner_join"
    )

    anl_q <- merged_anl$data

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$enable()
      iv
    })

    merge_vars <- merged_anl$variables

    r_groupby_vars <- function(mv) {
      slot_col <- function(nm) {
        if (is.null(anl_selectors[[nm]])) {
          return(NULL)
        }
        cols <- mv[[nm]]
        if (length(cols) == 0L) {
          return(NULL)
        }
        cols[[1L]]
      }

      x_name <- slot_col("x")
      fill_name <- slot_col("fill")
      x_facet_name <- slot_col("x_facet")
      y_facet_name <- slot_col("y_facet")

      c(
        x_name = x_name, fill_name = fill_name,
        x_facet_name = x_facet_name, y_facet_name = y_facet_name
      )
    }

    count_q <- reactive({
      merged_vars <- merge_vars()
      if (!is.null(x)) {
        validate(
          need(
            length(merged_vars[["x"]]) > 0L,
            "Please select an x-variable"
          )
        )
      }
      cols_src <- {
        slots <- names(merged_vars)
        out <- list()
        for (nm in slots) {
          if (is.null(anl_selectors[[nm]])) {
            next
          }
          st <- anl_selectors[[nm]]()
          if (!is.list(st) || is.null(st$variables)) {
            next
          }
          src <- st$variables$selected
          anl_cols <- merged_vars[[nm]]
          if (length(src) == 0L || length(anl_cols) == 0L) {
            next
          }
          if (length(anl_cols) != length(src)) {
            next
          }
          dn <- st$datasets$selected
          dn <- unlist(dn, recursive = FALSE, use.names = FALSE)
          if (length(dn) != 1L) {
            next
          }
          vec <- stats::setNames(anl_cols, src)
          attr(vec, "dataname") <- dn[[1L]]
          out[[nm]] <- vec
        }
        out
      }

      anl_q_local <- anl_q()
      teal::validate_has_data(anl_q_local[["ANL"]], 2)

      gb <- r_groupby_vars(merged_vars)
      gb_vals <- stats::na.omit(unlist(gb, use.names = FALSE))
      validate(
        need(
          length(unique(gb_vals)) == length(gb_vals),
          "The same variable cannot be used in more than one encoding."
        )
      )

      groupby_vars <- gb

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

      data_list <- sapply(
        names(data()),
        function(nm) reactive(data()[[nm]]),
        simplify = FALSE
      )

      anl_q_out <- anl_q_local %>%
        teal.code::eval_code(code = count_exprs)

      anl_q_out <- anl_q_out %>%
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

      relabel_call <- if (length(cols_src) > 0L && sum(lengths(cols_src)) > 0L) {
        teal.transform::get_anl_relabel_call(
          columns_source = cols_src,
          datasets = data_list,
          anl_name = "counts"
        )
      } else {
        NULL
      }
      if (!is.null(relabel_call)) {
        teal.code::eval_code(anl_q_out, relabel_call)
      } else {
        anl_q_out
      }
    })

    all_q <- reactive({
      teal::validate_inputs(iv_r())
      merged_vars <- merge_vars()
      groupby_vars_chr <- r_groupby_vars(merged_vars)
      groupby_vars <- as.list(groupby_vars_chr)

      y_lab <- substitute(
        teal.modules.clinical::column_annotation_label(counts, y_name),
        list(y_name = get_n_name(groupby_vars_chr))
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
        y_name = get_n_name(groupby_vars_chr),
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
        facet_scales = get_facet_scale(
          if (!is.null(x_facet)) isTRUE(input$facet_scale_x) else TRUE,
          if (!is.null(y_facet)) isTRUE(input$facet_scale_y) else TRUE
        ),
        ggplot2_args = all_ggplot2_args
      )

      ANL <- count_q()[["ANL"]]
      grp_txt <- paste(groupby_vars_chr, collapse = ", ")
      plot_title_call <- bquote(
        plot_title <- sprintf(
          "Number of patients (total N = %s) for each combination of (%s)",
          nrow(ANL),
          .(grp_txt)
        )
      )
      obj <- count_q() %>%
        teal.code::eval_code(plot_title_call)
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
      req(iv_r()$is_valid())
      all_q()[["counts"]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_all_q_code)
    ###
  })
}
