#' Simple barchart plot that shows counts per category.
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
#' library(dplyr)
#' library(scda)
#' library(nestcolor)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
#'
#' adae_labels <- formatters::var_labels(ADAE, fill = FALSE)
#' ADAE <- ADAE %>% dplyr::filter(!((AETOXGR == 1) & (AESEV == "MILD") & (ARM == "A: Drug X")))
#'
#' # reinstate labels
#' formatters::var_labels(ADAE) <- adae_labels
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data('latest')$adsl"),
#'     cdisc_dataset("ADAE", ADAE,
#'       code = "ADAE <- synthetic_cdisc_data('latest')$adae
#'               adae_labels <- formatters::var_labels(ADAE, fill = FALSE)
#'               ADAE <- ADAE %>%
#'                 dplyr::filter(!((AETOXGR == 1) & (AESEV == 'MILD') & (ARM == 'A: Drug X')))
#'               formatters::var_labels(ADAE) <- adae_labels
#'               ADAE"
#'     ),
#'     check = TRUE
#'   ),
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
    filters = "all"
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
    forms = teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code"),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_g_barchart_simple <- function(id,
                                  data,
                                  reporter,
                                  x,
                                  fill,
                                  x_facet,
                                  y_facet,
                                  plot_height,
                                  plot_width,
                                  ggplot2_args,
                                  filter_panel_api) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  shiny::moduleServer(id, function(input, output, session) {
    data_extract <- list(x = x, fill = fill, x_facet = x_facet, y_facet = y_facet)
    data_extract <- data_extract[!vapply(data_extract, is.null, logical(1))]

    selector_list <- teal.transform::data_extract_multiple_srv(data_extract, data, join_keys = attr(data, "join_keys"))

    reactive_select_input <- shiny::reactive({
      selectors <- selector_list()
      extract_names <- names(selectors)
      for (extract in extract_names) {
        if (is.null(selectors[[extract]]) || length(selectors[[extract]]()$select) == 0) {
          selectors <- selectors[-which(names(selectors) == extract)]
        }
      }
      selectors
    })

    merged_data <- teal.transform::merge_expression_srv(
      selector_list = reactive_select_input,
      datasets = data,
      join_keys = attr(data, "join_keys")
    )

    data_q <- shiny::reactive({
      shiny::validate({
        shiny::need("x" %in% names(reactive_select_input()), "Please select an x-variable")
      })
      quo <- new_quosure(env = data, code = attr(data, "code"))
      quo <- eval_code(quo, as.expression(merged_data()$expr))
      teal::validate_has_data(quo[["ANL"]], 2)
      quo
    })

    count_chunk <- shiny::reactive({
      quo <- data_q()
      groupby_vars <- r_groupby_vars()

      # count
      count_by_group <- function(groupby_vars, data_name) {
        # chunk and n_names are modified
        n_name <- get_n_name(groupby_vars)
        count_by_group_expr(groupby_vars = groupby_vars, data_name = data_name)
      }

      count_exprs <- count_by_group(groupby_vars, data_name = "ANL")

      if (input$show_n) {
        count_exprs2 <- sapply(groupby_vars[-1], count_by_group, data_name = "counts")
        count_str_to_col_exprs <- sapply(groupby_vars[-1], count_str_to_column_expr)
        count_exprs <- c(count_exprs, count_exprs2, count_str_to_col_exprs)
      }
      quo2 <- eval_code(quo, code = count_exprs, name = "groupvar counts")

      # add label and slice(1) as all patients in the same subgroup have same n_'s
      quo3 <- eval_code(
        quo2,
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
        ),
        name = "add_label_n_group_by_call"
      )

      # dplyr::select loses labels
      eval_code(
        quo3,
        teal.transform::get_anl_relabel_call(
          columns_source = merged_data()$columns_source,
          datasets = data,
          anl_name = "counts"
        ),
        name = "get_anl_relabel_call"
      )
    })

    plot_chunk <- shiny::reactive({
      quo1 <- count_chunk()
      groupby_vars <- as.list(r_groupby_vars()) # so $ access works below

      quo2 <- eval_code(quo1, substitute(
        env = list(groupby_vars = paste(groupby_vars, collapse = ", ")),
        plot_title <- sprintf(
          "Number of patients (total N = %s) for each combination of (%s)",
          nrow(ANL),
          groupby_vars
        )),
        name = "plot_title_call"
      )

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
        ggplot2_args = all_ggplot2_args
      )

      quo3 <- eval_code(quo2, code = plot_call, name = "plot_call")

      # explicitly calling print on the plot inside the chunk evaluates
      # the ggplot call and therefore catches errors
      eval_code(quo3, code = quote(print(plot)), name = "print_plot_call")
    })

    plot_r <- shiny::reactive(plot_chunk()[["plot"]])

    output$table <- shiny::renderTable(plot_chunk()[["counts"]])

    # reactive vars
    # NULL: not present in UI, vs character(0): no selection

    # returns named vector of non-NULL variables to group by
    r_groupby_vars <- function() {
      x_name <- if (is.null(x)) NULL else as.vector(merged_data()$columns_source$x)
      fill_name <- if (is.null(fill)) NULL else as.vector(merged_data()$columns_source$fill)
      x_facet_name <- if (is.null(x_facet)) NULL else as.vector(merged_data()$columns_source$x_facet)
      y_facet_name <- if (is.null(y_facet)) NULL else as.vector(merged_data()$columns_source$y_facet)

      # set to NULL when empty character
      if (identical(x_name, character(0))) x_name <- NULL
      if (identical(fill_name, character(0))) fill_name <- NULL
      if (identical(x_facet_name, character(0))) x_facet_name <- NULL
      if (identical(y_facet_name, character(0))) y_facet_name <- NULL

      res <- c(
        x_name = x_name, fill_name = fill_name,
        x_facet_name = x_facet_name, y_facet_name = y_facet_name
      ) # c() -> NULL entries are omitted

      # at least one category must be specified
      shiny::validate(shiny::need(
        length(res) > 0, # c() removes NULL entries
        "Must specify at least one of x, fill, x_facet and y_facet."
      ))

      res
    }

    # Insert the plot into a plot with settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(plot_chunk())),
      title = "Bar Chart"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Barchart Plot")
        card$append_text("Barchart Plot", "header2")
        card$append_fs(filter_panel_api$get_filter_state())
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(plot_chunk()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}



# Helper functions for chunks ----

#' `ggplot2` call to generate simple barchart
#'
#' `ggplot2` call to generate simple barchart
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
#' @inheritParams tm_g_barchart_simple
#' @examples
#' teal.modules.clinical:::make_barchart_simple_call(y_name = "y", x_name = "x")
#' @return `call`
#' @keywords internal
make_barchart_simple_call <- function(y_name,
                                      x_name = NULL, # NULL because it depepends on extract input which might be empty
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
                                      ggplot2_args = teal.widgets::ggplot2_args()) {
  # c() filters out NULL
  plot_vars <- c(x_name, fill_name, x_facet_name, y_facet_name)
  shiny::validate(
    shiny::need(
      !any(duplicated(plot_vars)),
      paste("Duplicated variable(s):", paste(plot_vars[duplicated(plot_vars)], collapse = ", "))
    )
  )
  checkmate::assert_string(y_name)
  checkmate::assert_string(x_name, null.ok = TRUE)
  checkmate::assert_string(fill_name, null.ok = TRUE)
  checkmate::assert_string(x_facet_name, null.ok = TRUE)
  checkmate::assert_string(y_facet_name, null.ok = TRUE)
  checkmate::assert_flag(label_bars)
  checkmate::assert_character(plot_vars, min.len = 1)
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
      ggplot2::facet_grid(.(facet_grid_formula(x_facet_name, y_facet_name)), scales = "free_x")
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
      ggplot2::geom_col(ggplot2::aes_string(
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
      hjust <- if (barlayout == "stacked") 0.5 else -1 # put above bars if not stacked #nolintr
      vjust <- 0.5
    } else {
      hjust <- 0.5
      vjust <- if (barlayout == "stacked") 0.5 else -1 # put above bars if not stacked #nolintr
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

# chunk that counts by specified group
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
