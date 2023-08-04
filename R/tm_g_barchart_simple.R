#' Simple bar chart plot that shows counts per category.
#'
#' Categories can be defined up to four levels deep and are defined through
#' `x`, `fill`, `x_facet` and `y_facet`. If any of them is `NULL`,
#' it is ignored.
#'
#' @inheritParams module_arguments
#' @param x (`data_extract_spec`)\cr variable on the x-axis.
#' @param fill (`data_extract_spec`)\cr grouping variable assigned to the bar colors.
#' @param x_facet (`data_extract_spec`)\cr faceting groups on the row dimension.
#' @param y_facet (`data_extract_spec`)\cr faceting groups on the col dimension.
#' @param plot_options (`list`)\cr list of plot options.
#'
#' @export
#'
#' @examples
#' library(nestcolor)
#'
#' adsl <- tmc_ex_adsl %>% dplyr::mutate(ITTFL = factor("Y") %>%
#'   formatters::with_label("Intent-To-Treat Population Flag"))
#' adae <- tmc_ex_adae %>% dplyr::filter(!((AETOXGR == 1) & (AESEV == "MILD") & (ARM == "A: Drug X")))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAE", adae)
#'   ),
#'   modules = modules(
#'     tm_g_barchart_simple(
#'       label = "ADAE Analysis",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(
#'             adsl,
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
#'               adsl,
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
#'             choices = variable_choices(adae, c("AETOXGR", "AESEV", "AESER")),
#'             selected = NULL,
#'             multiple = FALSE
#'           )
#'         )
#'       ),
#'       x_facet = list(
#'         data_extract_spec(
#'           dataname = "ADAE",
#'           select = select_spec(
#'             choices = variable_choices(adae, c("AETOXGR", "AESEV", "AESER")),
#'             selected = "AETOXGR",
#'             multiple = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             choices = variable_choices(
#'               adsl,
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
#'             choices = variable_choices(adae, c("AETOXGR", "AESEV", "AESER")),
#'             selected = "AESEV",
#'             multiple = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             choices = variable_choices(
#'               adsl,
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
                                 ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_barchart_simple")
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

  plot_options <- utils::modifyList(
    list(stacked = FALSE), # default
    `if`(is.null(plot_options), list(), plot_options)
  )

  ui_args <- as.list(environment())
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
      plot_height = plot_height,
      plot_width = plot_width,
      ggplot2_args = ggplot2_args
    ),
    datanames = "all"
  )
}

ui_g_barchart_simple <- function(id, ...) {
  ns <- shiny::NS(id)
  args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(args$x, args$fill, args$x_facet, args$y_facet)

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.modules.clinical")))
    ),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("myplot")),
        shiny::uiOutput(ns("table"), class = "overflow-y-scroll max-h-250")
      ),
      encoding = shiny::div(
        ### Reporter
        teal.reporter::simple_reporter_ui(ns("simple_reporter")),
        ###
        shiny::tags$label("Encodings", class = "text-primary"),
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
        teal.widgets::panel_group(
          teal.widgets::panel_item(
            "Additional plot settings",
            if (!is.null(args$fill)) {
              shiny::radioButtons(
                inputId = ns("barlayout"),
                label = "Covariate Bar Layout",
                choices = c("Side by side" = "side_by_side", "Stacked" = "stacked"),
                selected = if (args$plot_options$stacked) "stacked" else "side_by_side",
                inline = TRUE
              )
            },
            if (!(is.null(args$x_facet))) {
              shiny::checkboxInput(
                ns("facet_scale_x"),
                "Fixed scales for column facets",
                value = TRUE
              )
            },
            if (!(is.null(args$y_facet))) {
              shiny::checkboxInput(
                ns("facet_scale_y"),
                "Fixed scales for row facets",
                value = TRUE
              )
            },
            shiny::checkboxInput(
              ns("label_bars"),
              "Label bars",
              value = `if`(is.null(args$plot_options$label_bars), TRUE, args$plot_options$label_bars)
            ),
            shiny::checkboxInput(
              ns("rotate_bar_labels"),
              "Rotate bar labels",
              value = `if`(is.null(args$plot_options$rotate_bar_labels), FALSE, args$plot_options$rotate_bar_labels)
            ),
            shiny::checkboxInput(
              ns("rotate_x_label"),
              "Rotate x label",
              value = `if`(is.null(args$plot_options$rotate_x_label), FALSE, args$plot_options$rotate_x_label)
            ),
            shiny::checkboxInput(
              ns("rotate_y_label"),
              "Rotate y label",
              value = `if`(is.null(args$plot_options$rotate_y_label), FALSE, args$plot_options$rotate_y_label)
            ),
            shiny::checkboxInput(
              ns("flip_axis"),
              "Flip axes",
              value = `if`(is.null(args$plot_options$flip_axis), FALSE, args$plot_options$flip_axis)
            ),
            shiny::checkboxInput(
              ns("show_n"),
              "Show n",
              value = `if`(is.null(args$plot_options$show_n), TRUE, args$plot_options$show_n)
            ),
            shiny::sliderInput(
              inputId = ns("expand_y_range"),
              label = "Y-axis range expansion (fraction on top)",
              min = 0,
              max = 1,
              value = 0.5,
              step = 0.1
            )
          )
        )
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_g_barchart_simple <- function(id,
                                  data,
                                  reporter,
                                  filter_panel_api,
                                  x,
                                  fill,
                                  x_facet,
                                  y_facet,
                                  plot_height,
                                  plot_width,
                                  ggplot2_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
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

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      join_keys = get_join_keys(data),
      selector_list = selector_list
    )

    anl_q <- shiny::reactive({
      qenv <- teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data))
      qenv <- teal.code::eval_code(qenv, as.expression(anl_inputs()$expr))
      qenv
    })

    count_q <- shiny::reactive({
      qenv <- anl_q()
      teal::validate_has_data(qenv[["ANL"]], 2)
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
      qenv2 <- teal.code::eval_code(qenv, code = count_exprs)

      # add label and slice(1) as all patients in the same subgroup have same n_'s
      qenv3 <- teal.code::eval_code(
        qenv2,
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
      teal.code::eval_code(
        qenv3,
        teal.transform::get_anl_relabel_call(
          columns_source = anl_inputs()$columns_source,
          datasets = data,
          anl_name = "counts"
        )
      )
    })

    all_q <- shiny::reactive({
      teal::validate_inputs(iv_r())
      groupby_vars <- as.list(r_groupby_vars()) # so $ access works below

      ANL <- count_q()[["ANL"]] # nolint

      qenv2 <- teal.code::eval_code(count_q(), substitute(
        env = list(groupby_vars = paste(groupby_vars, collapse = ", ")),
        plot_title <- sprintf(
          "Number of patients (total N = %s) for each combination of (%s)",
          nrow(ANL),
          groupby_vars
        )
      ))

      y_lab <- substitute(
        column_annotation_label(counts, y_name),
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

      qenv3 <- teal.code::eval_code(qenv2, code = plot_call)

      # explicitly calling print on the plot inside the qenv evaluates
      # the ggplot call and therefore catches errors
      teal.code::eval_code(qenv3, code = quote(print(plot)))
    })

    plot_r <- shiny::reactive(all_q()[["plot"]])

    output$table <- shiny::renderTable({
      shiny::req(iv_r()$is_valid())
      teal.code::dev_suppress(all_q()[["counts"]])
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

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(all_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = "Bar Chart"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Barchart Plot")
        card$append_text("Barchart Plot", "header2")
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



# Helper functions for qenv ----

#' `ggplot2` call to generate simple bar chart
#'
#' `ggplot2` call to generate simple bar chart
#' @param yname (`NULL`, `character(1)`)\cr
#'  name of the `yaxis` variable.
#' @param xname (`NULL`, `character(1)`)\cr
#'  name of the `xaxis` variable.
#' @param fill_name (`NULL`, `character(1)`)\cr
#'  name of the variable distinguishing the color of the fill.
#' @param x_facet_name (`NULL`, `character(1)`)\cr
#'  name of the variable to facet the plot horizontally.
#' @param y_facet_name (`NULL`, `character(1)`)\cr
#'  name of the variable to facet the plot vertically.
#' @param label_bars (`NULL`, `logical(1)`)\cr
#'  whether to label bars.
#' @param barlayout (`NULL`, `character(1)`)\cr
#'  `type of the layout - "side_by_side"` or `"stacked"` (default).
#' @param flip_axis (`NULL`, `character(1)`)\cr
#'  whether to flip axis.
#' @param rotate_bar_labels (`NULL`, `logical(1)`)\cr
#'  whether to rotate bar label by 45 degrees.
#' @param rotate_x_label (`NULL`, `logical(1)`)\cr
#'  whether to rotate x-axis label by 45 degrees.
#' @param rotate_y_label (`NULL`, `logical(1)`)\cr
#'  whether to rotate y-axis label by 45 degrees.
#' @param expand_y_range (`NULL`, `numeric(1)`)\cr
#'  fraction of y-axis range to expand further up.
#' @param facet_scales (`fixed`, `free_x`, `free_y` or `free`) value
#'  passed to `scales` argument of `ggplot2::facet_grid`
#' @inheritParams tm_g_barchart_simple
#' @examples
#' teal.modules.clinical:::make_barchart_simple_call(y_name = "y", x_name = "x")
#' @return `call`
#' @keywords internal
make_barchart_simple_call <- function(y_name,
                                      x_name = NULL, # NULL because it depends on extract input which might be empty
                                      fill_name = NULL,
                                      x_facet_name = NULL,
                                      y_facet_name = NULL,
                                      label_bars = TRUE, # whether to also draw numbers as text, i.e. label bars
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
      bquote(ggplot2::aes_string(x = .(x_val_var)))
    } else {
      bquote(ggplot2::aes_string(x = .(x_val_var), fill = .(fill_name)))
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
        ggplot2::aes_string(
          y = .(y_name)
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
      ggplot2::geom_col(ggplot2::aes_string(y = .(y_name)), position = .(position))
    ))
  }

  # draw numbers above bars
  if (label_bars) {
    # center text and move slightly to the top or to the right (depending on flip axes)
    # see https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
    if (isTRUE(flip_axis)) {
      hjust <- if (barlayout == "stacked") 0.5 else -1 # put above bars if not stacked # nolint
      vjust <- 0.5
    } else {
      hjust <- 0.5
      vjust <- if (barlayout == "stacked") 0.5 else -1 # put above bars if not stacked # nolint
    }

    plot_args <- c(plot_args, bquote(
      ggplot2::geom_text(ggplot2::aes_string(y = .(y_name), label = .(y_name)),
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
      ggplot2::guides(fill = ggplot2::guide_legend(title = column_annotation_label(counts, .(fill_name))))
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
