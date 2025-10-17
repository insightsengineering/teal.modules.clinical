#' teal Module: Simple Bar Chart and Table of Counts per Category
#'
#' This module produces a [ggplot2::ggplot()] type bar chart and summary table of counts per category.
#'
#' Categories can be defined up to four levels deep and are defined through the `x`, `fill`,
#' `x_facet`, and `y_facet` parameters. Any parameters set to `NULL` (default) are ignored.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_arguments
#' @param x (`data_extract_spec`)\cr variable on the x-axis.
#' @param fill (`data_extract_spec`)\cr grouping variable to determine bar colors.
#' @param x_facet (`data_extract_spec`)\cr row-wise faceting groups.
#' @param y_facet (`data_extract_spec`)\cr column-wise faceting groups.
#' @param plot_options (`list`)\cr list of plot options.
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
#' tm_g_barchart_simple(
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
#'   library(formatters)
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl %>%
#'     mutate(ITTFL = factor("Y") %>%
#'       with_label("Intent-To-Treat Population Flag"))
#'   ADAE <- tmc_ex_adae %>%
#'     filter(!((AETOXGR == 1) & (AESEV == "MILD") & (ARM == "A: Drug X")))
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADAE <- data[["ADAE"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_barchart_simple(
#'       label = "ADAE Analysis",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(
#'             ADSL,
#'             c(
#'               "ARM", "ACTARM", "SEX",
#'               "RACE", "ITTFL", "SAFFL", "STRATA2"
#'             )
#'           ),
#'           selected = "ACTARM",
#'           multiple = FALSE
#'         )
#'       ),
#'       fill = list(
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             choices = variable_choices(
#'               ADSL,
#'               c(
#'                 "ARM", "ACTARM", "SEX",
#'                 "RACE", "ITTFL", "SAFFL", "STRATA2"
#'               )
#'             ),
#'             selected = "SEX",
#'             multiple = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADAE",
#'           select = select_spec(
#'             choices = variable_choices(ADAE, c("AETOXGR", "AESEV", "AESER")),
#'             selected = NULL,
#'             multiple = FALSE
#'           )
#'         )
#'       ),
#'       x_facet = list(
#'         data_extract_spec(
#'           dataname = "ADAE",
#'           select = select_spec(
#'             choices = variable_choices(ADAE, c("AETOXGR", "AESEV", "AESER")),
#'             selected = "AETOXGR",
#'             multiple = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             choices = variable_choices(
#'               ADSL,
#'               c(
#'                 "ARM", "ACTARM", "SEX",
#'                 "RACE", "ITTFL", "SAFFL", "STRATA2"
#'               )
#'             ),
#'             selected = NULL,
#'             multiple = FALSE
#'           )
#'         )
#'       ),
#'       y_facet = list(
#'         data_extract_spec(
#'           dataname = "ADAE",
#'           select = select_spec(
#'             choices = variable_choices(ADAE, c("AETOXGR", "AESEV", "AESER")),
#'             selected = "AESEV",
#'             multiple = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             choices = variable_choices(
#'               ADSL,
#'               c(
#'                 "ARM", "ACTARM", "SEX",
#'                 "RACE", "ITTFL", "SAFFL", "STRATA2"
#'               )
#'             ),
#'             selected = NULL,
#'             multiple = FALSE
#'           )
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
tm_g_barchart_simple <- function(x = NULL,
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
  message("Initializing tm_g_barchart_simple")
  checkmate::assert_string(label)
  checkmate::assert_list(plot_options, null.ok = TRUE)
  if (length(c(x, fill, x_facet, y_facet)) == 0) {
    stop("at least one must be specified. 'x', 'fill', 'x_facet', 'y_facet' is NULL")
  }
  x <- teal.transform::list_extract_spec(x, allow_null = TRUE)
  fill <- teal.transform::list_extract_spec(fill, allow_null = TRUE)
  x_facet <- teal.transform::list_extract_spec(x_facet, allow_null = TRUE)
  y_facet <- teal.transform::list_extract_spec(y_facet, allow_null = TRUE)
  teal.transform::check_no_multiple_selection(x)
  teal.transform::check_no_multiple_selection(fill)
  teal.transform::check_no_multiple_selection(x_facet)
  teal.transform::check_no_multiple_selection(y_facet)
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
  assert_decorators(decorators, names = "plot")

  plot_options <- utils::modifyList(
    list(stacked = FALSE), # default
    `if`(is.null(plot_options), list(), plot_options)
  )

  ui_args <- as.list(environment())

  data_extract_list <- list(
    x = x, fill = fill,
    x_facet = x_facet, y_facet = y_facet
  )

  module(
    label = label,
    server = srv_g_barchart_simple,
    ui = ui_g_barchart_simple,
    ui_args = ui_args,
    server_args = list(
      x = x,
      fill = fill,
      x_facet = x_facet,
      y_facet = y_facet,
      label = label,
      plot_height = plot_height,
      plot_width = plot_width,
      ggplot2_args = ggplot2_args,
      decorators = decorators
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_g_barchart_simple <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(args$x, args$fill, args$x_facet, args$y_facet)

  tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("myplot")),
        uiOutput(ns("table"), style = "overflow-y: scroll; max-height: 250px;")
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"), tags$br(),
        teal.transform::datanames_input(args[c("x", "fill", "x_facet", "y_facet")]),
        if (!is.null(args$x)) {
          teal.transform::data_extract_ui(
            id = ns("x"),
            label = "X variable",
            data_extract_spec = args$x,
            is_single_dataset = is_single_dataset_value
          )
        },
        if (!is.null(args$fill)) {
          teal.transform::data_extract_ui(
            id = ns("fill"),
            label = "Fill",
            data_extract_spec = args$fill,
            is_single_dataset = is_single_dataset_value
          )
        },
        if (!is.null(args$x_facet)) {
          teal.transform::data_extract_ui(
            id = ns("x_facet"),
            label = "Column facetting variable",
            data_extract_spec = args$x_facet,
            is_single_dataset = is_single_dataset_value
          )
        },
        if (!is.null(args$y_facet)) {
          teal.transform::data_extract_ui(
            id = ns("y_facet"),
            label = "Row facetting variable",
            data_extract_spec = args$y_facet,
            is_single_dataset = is_single_dataset_value
          )
        },
        ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "plot")),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Additional plot settings",
            if (!is.null(args$fill)) {
              radioButtons(
                inputId = ns("barlayout"),
                label = "Covariate Bar Layout",
                choices = c("Side by side" = "side_by_side", "Stacked" = "stacked"),
                selected = if (args$plot_options$stacked) "stacked" else "side_by_side",
                inline = TRUE
              )
            },
            if (!(is.null(args$x_facet))) {
              checkboxInput(
                ns("facet_scale_x"),
                "Fixed scales for column facets",
                value = TRUE
              )
            },
            if (!(is.null(args$y_facet))) {
              checkboxInput(
                ns("facet_scale_y"),
                "Fixed scales for row facets",
                value = TRUE
              )
            },
            checkboxInput(
              ns("label_bars"),
              "Label bars",
              value = `if`(is.null(args$plot_options$label_bars), TRUE, args$plot_options$label_bars)
            ),
            checkboxInput(
              ns("rotate_bar_labels"),
              "Rotate bar labels",
              value = `if`(is.null(args$plot_options$rotate_bar_labels), FALSE, args$plot_options$rotate_bar_labels)
            ),
            checkboxInput(
              ns("rotate_x_label"),
              "Rotate x label",
              value = `if`(is.null(args$plot_options$rotate_x_label), FALSE, args$plot_options$rotate_x_label)
            ),
            checkboxInput(
              ns("rotate_y_label"),
              "Rotate y label",
              value = `if`(is.null(args$plot_options$rotate_y_label), FALSE, args$plot_options$rotate_y_label)
            ),
            checkboxInput(
              ns("flip_axis"),
              "Flip axes",
              value = `if`(is.null(args$plot_options$flip_axis), FALSE, args$plot_options$flip_axis)
            ),
            checkboxInput(
              ns("show_n"),
              "Show n",
              value = `if`(is.null(args$plot_options$show_n), TRUE, args$plot_options$show_n)
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
      pre_output = args$pre_output,
      post_output = args$post_output
    )
  )
}

#' @keywords internal
srv_g_barchart_simple <- function(id,
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
    rule_dupl <- function(others) {
      function(value) {
        othervals <- lapply(
          Filter(Negate(is.null), selector_list()[others]), # some selectors could be ommited in tm_g_barchart_simple
          function(x) x()$select
        )
        vars <- c(value, unlist(othervals))
        dups <- unique(vars[duplicated(vars)])
        if (value %in% dups) {
          paste("Duplicated value:", value, collapse = ", ")
        }
      }
    }

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(x = x, fill = fill, x_facet = x_facet, y_facet = y_facet),
      datasets = data,
      select_validation_rule = list(
        x = shinyvalidate::compose_rules(
          shinyvalidate::sv_required("Please select an x-variable"),
          rule_dupl(others = c("fill", "x_facet", "y_facet"))
        ),
        fill = shinyvalidate::compose_rules(
          shinyvalidate::sv_optional(),
          rule_dupl(others = c("x", "x_facet", "y_facet"))
        ),
        x_facet = shinyvalidate::compose_rules(
          shinyvalidate::sv_optional(),
          rule_dupl(others = c("fill", "x", "y_facet"))
        ),
        y_facet = shinyvalidate::compose_rules(
          shinyvalidate::sv_optional(),
          rule_dupl(others = c("fill", "x_facet", "x"))
        )
      ),
      dataset_validation_rule = list(
        fill = NULL,
        x_facet = NULL,
        y_facet = NULL
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj %>% teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    count_q <- reactive({
      anl_q <- anl_q()
      teal::validate_has_data(anl_q[["ANL"]], 2)
      groupby_vars <- r_groupby_vars()

      # count
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
        function(x) reactive(data()[[x]]),
        simplify = FALSE
      )

      anl_q <- anl_q %>%
        teal.code::eval_code(code = count_exprs)

      # add label and slice(1) as all patients in the same subgroup have same n_'s
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

      # dplyr::select loses labels
      relabel_call <- teal.transform::get_anl_relabel_call(
        columns_source = anl_inputs()$columns_source,
        datasets = data_list,
        anl_name = "counts"
      )
      if (!is.null(relabel_call)) {
        teal.code::eval_code(anl_q, relabel_call)
      } else {
        anl_q
      }
    })

    all_q <- reactive({
      teal::validate_inputs(iv_r())
      groupby_vars <- as.list(r_groupby_vars()) # so $ access works below

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

    decorated_all_q_code <- srv_decorate_teal_data(
      "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr = plot
    )

    plot_r <- reactive(decorated_all_q_code()[["plot"]])

    output$table <- renderTable({
      req(iv_r()$is_valid())
      all_q()[["counts"]]
    })

    # get grouping variables
    # NULL: not present in UI, vs character(0): no selection
    ## returns named vector of non-NULL variables to group by
    r_groupby_vars <- function() {
      x_name <- if (is.null(x)) NULL else as.vector(anl_inputs()$columns_source$x)
      fill_name <- if (is.null(fill)) NULL else as.vector(anl_inputs()$columns_source$fill)
      x_facet_name <- if (is.null(x_facet)) NULL else as.vector(anl_inputs()$columns_source$x_facet)
      y_facet_name <- if (is.null(y_facet)) NULL else as.vector(anl_inputs()$columns_source$y_facet)

      # set to NULL when empty character
      if (identical(x_name, character(0))) x_name <- NULL
      if (identical(fill_name, character(0))) fill_name <- NULL
      if (identical(x_facet_name, character(0))) x_facet_name <- NULL
      if (identical(y_facet_name, character(0))) y_facet_name <- NULL

      c(
        x_name = x_name, fill_name = fill_name,
        x_facet_name = x_facet_name, y_facet_name = y_facet_name
      ) # c() -> NULL entries are omitted
    }

    # Insert the plot into a plot with settings module from teal.widgets
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

# Helper functions for qenv ----

#' `ggplot2` call to generate simple bar chart
#'
#' @inheritParams tm_g_barchart_simple
#' @param y_name (`character` or `NULL`)\cr name of the y-axis variable.
#' @param x_name (`character` or `NULL`)\cr name of the x-axis variable. Defaults to `NULL` because it is dependent
#'   on extract input which can be empty.
#' @param fill_name (`character` or `NULL`)\cr name of the variable to determine the bar fill color.
#' @param x_facet_name (`character` or `NULL`)\cr name of the variable to use for horizontal plot faceting.
#' @param y_facet_name (`character` or `NULL`)\cr name of the variable to use for vertical plot faceting.
#' @param label_bars (`logical` or `NULL`)\cr whether bars should be labeled. If `TRUE`, label bar numbers would
#'   also be drawn as text.
#' @param barlayout (`character` or `NULL`)\cr type of the bar layout. Options are `"stacked"` (default) or
#'   `"side_by_side"`.
#' @param flip_axis (`character` or `NULL`)\cr whether to flip the plot axis.
#' @param rotate_bar_labels (`logical` or `NULL`)\cr whether bar labels should be rotated by 45 degrees.
#' @param rotate_x_label (`logical` or `NULL`)\cr whether x-axis labels should be rotated by 45 degrees.
#' @param rotate_y_label (`logical` or `NULL`)\cr whether y-axis labels should be rotated by 45 degrees.
#' @param expand_y_range (`numeric` or `NULL`)\cr fraction of y-axis range to further expand by.
#' @param facet_scales (`character`)\cr value passed to `scales` argument of [ggplot2::facet_grid()]. Options are
#'   `fixed`, `free_x`, `free_y`, and `free`.
#'
#' @return `call` to produce a `ggplot` object.
#'
#' @keywords internal
make_barchart_simple_call <- function(y_name,
                                      x_name = NULL,
                                      fill_name = NULL,
                                      x_facet_name = NULL,
                                      y_facet_name = NULL,
                                      label_bars = TRUE,
                                      barlayout = c("side_by_side", "stacked"),
                                      flip_axis = FALSE,
                                      rotate_bar_labels = FALSE,
                                      rotate_x_label = FALSE,
                                      rotate_y_label = FALSE,
                                      expand_y_range = 0,
                                      facet_scales = "free_x",
                                      ggplot2_args = teal.widgets::ggplot2_args()) {
  checkmate::assert_string(y_name)
  checkmate::assert_string(x_name, null.ok = TRUE)
  checkmate::assert_string(fill_name, null.ok = TRUE)
  checkmate::assert_string(x_facet_name, null.ok = TRUE)
  checkmate::assert_string(y_facet_name, null.ok = TRUE)
  checkmate::assert_character(c(x_name, fill_name, x_facet_name, y_facet_name))
  checkmate::assert_flag(label_bars)
  checkmate::assert_scalar(expand_y_range)
  barlayout <- match.arg(barlayout)
  checkmate::assert_flag(flip_axis, null.ok = TRUE)
  checkmate::assert_flag(rotate_x_label, null.ok = TRUE)
  checkmate::assert_flag(rotate_y_label, null.ok = TRUE)

  plot_args <- list(quote(ggplot2::ggplot(counts)))

  # aesthetic variables
  x_val_var <- if (is.null(x_name)) 0 else x_name
  plot_args <- c(
    plot_args,
    if (is.null(fill_name)) {
      bquote(ggplot2::aes(x = .data[[.(x_val_var)]]))
    } else {
      bquote(ggplot2::aes(x = .data[[.(x_val_var)]], fill = .data[[.(fill_name)]]))
    }
  )

  if (!(is.null(x_facet_name) && is.null(y_facet_name))) {
    # free_x is needed, otherwise when we facet on x and x-ticks are different for each facet value,
    # it will fit all possible x-ticks across all facet values into each facet panel
    plot_args <- c(plot_args, bquote(
      ggplot2::facet_grid(.(facet_grid_formula(x_facet_name, y_facet_name)), scales = .(facet_scales))
    ))
  }

  # position stacking or dodging for bars and text
  position <- if (is.null(fill_name) || (barlayout == "side_by_side")) {
    # vjust = on top, i.e. don't place below when only one filling (i.e. nothing really stacked)
    quote(ggplot2::position_dodge(0.9))
  } else {
    quote(ggplot2::position_stack(vjust = 0.5))
  }

  # draw bars
  if (is.null(fill_name)) {
    # nothing to put side-by-side, so put fill to one color only
    # setting color via aesthetics does not work for some reason (but x = 0 above works)
    plot_args <- c(plot_args, bquote(
      ggplot2::geom_col(
        ggplot2::aes(
          y = .data[[.(y_name)]]
        ),
        position = .(position),
        fill = .(ifelse(
          !is.null(getOption("ggplot2.discrete.colour")),
          getOption("ggplot2.discrete.colour")[1],
          "#b6cae9"
        ))
      )
    ))
  } else {
    plot_args <- c(plot_args, bquote(
      ggplot2::geom_col(ggplot2::aes(y = .data[[.(y_name)]]), position = .(position))
    ))
  }

  # draw numbers above bars
  if (label_bars) {
    # center text and move slightly to the top or to the right (depending on flip axes)
    # see https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
    if (isTRUE(flip_axis)) {
      hjust <- if (barlayout == "stacked") 0.5 else -1 # put above bars if not stacked
      vjust <- 0.5
    } else {
      hjust <- 0.5
      vjust <- if (barlayout == "stacked") 0.5 else -1 # put above bars if not stacked
    }

    plot_args <- c(plot_args, bquote(
      ggplot2::geom_text(ggplot2::aes(y = .data[[.(y_name)]], label = .data[[.(y_name)]]),
        stat = "identity",
        angle = .(if (rotate_bar_labels) 45 else 0),
        position = .(position),
        # hjust, vjust are respective to position, i.e. top, center etc. alignment
        hjust = .(hjust), vjust = .(vjust)
      )
    ))
  }

  # add legend for fill
  if (!is.null(fill_name)) {
    plot_args <- c(plot_args, bquote(
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          title = teal.modules.clinical::column_annotation_label(counts, .(fill_name))
        )
      )
    ))
  }

  if (isTRUE(flip_axis)) plot_args <- c(plot_args, quote(ggplot2::coord_flip()))

  if (expand_y_range > 0) {
    plot_args <- c(plot_args, bquote(ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(c(0, .(expand_y_range)))
    )))
  }

  if (isTRUE(rotate_x_label)) ggplot2_args$theme[["axis.text.x"]] <- quote(ggplot2::element_text(angle = 45, hjust = 1))
  if (isTRUE(rotate_y_label)) ggplot2_args$theme[["axis.text.y"]] <- quote(ggplot2::element_text(angle = 45, hjust = 1))
  if (!is.null(x_name)) {
    ggplot2_args$labs[["x"]] <- substitute(
      expr = column_annotation_label(counts, x_name),
      env = list(x_name = x_name)
    )
  } else {
    ggplot2_args$theme[["axis.text.x"]] <- quote(ggplot2::element_blank())
    ggplot2_args$theme[["axis.ticks.x"]] <- quote(ggplot2::element_blank())
  }

  parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(ggplot2_args)
  plot_args <- c(plot_args, parsed_ggplot2_args)

  bquote(plot <- .(call_concatenate(plot_args)))
}

# get name of column in "counts" data.frame
get_n_name <- function(groupby_vars) {
  paste0("n_", paste(groupby_vars, collapse = "_"))
}

# expression that counts by specified group
# n_name: name of column to add counts to, by default determined from groupby_vars
count_by_group_expr <- function(groupby_vars, data_name = "counts") {
  checkmate::assert_character(groupby_vars)
  n_name <- get_n_name(groupby_vars)

  parse(
    text = sprintf(
      "counts <- %s %%>%% dplyr::group_by(%s) %%>%% dplyr::mutate(%s = dplyr::n()) %%>%% dplyr::ungroup()",
      data_name,
      paste(groupby_vars, collapse = ","),
      n_name
    ),
    keep.source = FALSE
  )
}

get_facet_scale <- function(x, y) {
  facet_scale_x <- if (isTRUE(x)) {
    "fixed"
  } else {
    "free"
  }
  facet_scale_y <- if (isTRUE(y)) {
    "fixed"
  } else {
    "free"
  }

  if (facet_scale_x == "fixed" && facet_scale_y == "free") {
    "free_y"
  } else if (facet_scale_x == "free" && facet_scale_y == "fixed") {
    "free_x"
  } else {
    facet_scale_x # fixed or free, as x and y match
  }
}
