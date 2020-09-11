# Refactoring of tm_g_barplot.R

#' Simple barchart plot that shows counts per category.
#'
#' Categories can be defined up to four levels deep and are defined through
#' `x`, `fill`, `x_facet` and `y_facet`. If any of them is `NULL`,
#' it is ignored.
#'
#' @md
#' @param x `data_extract_spec` variable on x-axis
#' @param fill `data_extract_spec` variable on x-axis
#' @param x_facet `data_extract_spec` variable on x-axis
#' @param y_facet `data_extract_spec` variable on x-axis
#' @param label `character.single` label of module
#' @param plot_options `list` list of plot options
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' library(teal.modules.clinical)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#'
#' adae_labels <- var_labels(ADAE)
#' ADAE <- ADAE %>% dplyr::filter(!((AETOXGR == 1) & (AESEV == "MILD") & (ARM == "A: Drug X")))
#'
#' # reinstate labels
#' ADAE <- do.call(rtables::var_relabel, append(list(x = ADAE), as.list(adae_labels)))
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL), cdisc_dataset("ADAE", ADAE), cdisc_dataset("ADLB", ADLB),
#'     code = "
#'     ADSL <- radsl(cached = TRUE)
#'     ADLB <- radlb(cached = TRUE)
#'     ADAE <- radae(cached = TRUE)
#'     adae_labels <- var_labels(ADAE)
#'     ADAE <- ADAE %>% dplyr::filter(!((AETOXGR == 1) & (AESEV == 'MILD') & (ARM == 'A: Drug X')))
#'     ADAE <- do.call(rtables::var_relabel, append(list(x = ADAE), as.list(adae_labels)))
#'     ",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_barchart_simple(
#'       label = "ADAE Analysis",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(ADSL,
#'                                      c("ARM", "ACTARM", "SEX",
#'                                        "RACE", "ITTFL", "SAFFL", "STRATA2")
#'           ),
#'           selected = "ACTARM",
#'           multiple = FALSE
#'         )
#'       ),
#'       fill = list(
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             choices = variable_choices(ADSL,
#'                                        c("ARM", "ACTARM", "SEX",
#'                                          "RACE", "ITTFL", "SAFFL", "STRATA2")
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
#'             choices = variable_choices(ADSL,
#'                                        c("ARM", "ACTARM", "SEX",
#'                                          "RACE", "ITTFL", "SAFFL", "STRATA2")
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
#'             choices = variable_choices(ADSL,
#'                                        c("ARM", "ACTARM", "SEX",
#'                                          "RACE", "ITTFL", "SAFFL", "STRATA2")
#'             ),
#'             selected = NULL,
#'             multiple = FALSE
#'           )
#'         )
#'       )
#'       #stacked = FALSE,
#'       #stat_type = "Count"
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' @importFrom utils modifyList
tm_g_barchart_simple <- function(x = NULL,
                                 fill = NULL,
                                 x_facet = NULL,
                                 y_facet = NULL,
                                 label = "Count Barchart",
                                 plot_options = NULL) {
  stopifnot(
    is_character_single(label),
    is.null(plot_options) || is.list(plot_options)
  )
  stopifnot(!all(vapply(list(x, fill, x_facet, y_facet), is.null, logical(1)))) # at least one must be specified
  x <- list_extract_spec(x, allow_null = TRUE)
  fill <- list_extract_spec(fill, allow_null = TRUE)
  x_facet <- list_extract_spec(x_facet, allow_null = TRUE)
  y_facet <- list_extract_spec(y_facet, allow_null = TRUE)
  check_no_multiple_selection(x)
  check_no_multiple_selection(fill)
  check_no_multiple_selection(x_facet)
  check_no_multiple_selection(y_facet)

  plot_options <- modifyList(
    list(stacked = FALSE), # default
    if_null(plot_options, list())
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
      y_facet = y_facet
    ),
    filters = "all"
  )
}

ui_g_barchart_simple <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = white_small_well(
      plot_height_output(id = ns("myplot")),
      uiOutput(ns("table"), style = "overflow-y:scroll; max-height: 250px")
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      if (!is.null(args$x)) {
        data_extract_input(
          id = ns("x"),
          label = "x:",
          data_extract_spec = args$x
        )
      },
      if (!is.null(args$fill)) {
        data_extract_input(
          id = ns("fill"),
          label = "fill:",
          data_extract_spec = args$fill
        )
      },
      if (!is.null(args$x_facet)) {
        data_extract_input(
          id = ns("x_facet"),
          label = "x facetting:",
          data_extract_spec = args$x_facet
        )
      },
      if (!is.null(args$y_facet)) {
        data_extract_input(
          id = ns("y_facet"),
          label = "y facetting:",
          data_extract_spec = args$y_facet
        )
      },
      panel_group(
        panel_item(
          title = "Plot settings",
          if (!is.null(args$fill)) {
            radioButtons(
              inputId = ns("barlayout"),
              label = "Covariate Bar Layout",
              choices = c("Side by side" = "side_by_side", "Stacked" = "stacked"),
              selected = if (args$plot_options$stacked) "stacked" else "side_by_side",
              inline = TRUE
            )
          },
          checkboxInput(
            ns("label_bars"),
            "Label bars",
            value = if_null(args$plot_options$label_bars, TRUE)
          ),
          checkboxInput(
            ns("rotate_bar_labels"),
            "Rotate bar labels",
            value = if_null(args$plot_options$rotate_bar_labels, FALSE)
          ),
          checkboxInput(
            ns("rotate_x_label"),
            "Rotate x label",
            value = if_null(args$plot_options$rotate_x_label, FALSE)
          ),
          checkboxInput(
            ns("rotate_y_label"),
            "Rotate y label",
            value = if_null(args$plot_options$rotate_y_label, FALSE)
          ),
          checkboxInput(
            ns("flip_axis"),
            "Flip axes",
            value = if_null(args$plot_options$flip_axis, FALSE)
          ),
          checkboxInput(
            ns("show_n"),
            "Show n",
            value = if_null(args$plot_options$show_n, TRUE)
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
      ),
      plot_height_input(id = ns("myplot"), value = c(600, 200, 2000))
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_g_barchart_simple <- function(input, output, session, datasets, x, fill, x_facet, y_facet) {
  data_extract <- list(
    x = x, fill = fill, x_facet = x_facet, y_facet = y_facet
  )
  data_extract <- data_extract[!vapply(data_extract, is.null, logical(1))]
  merged_data <- reactive({
    data_merge_module(
      datasets = datasets,
      data_extract = data_extract,
      input_id = names(data_extract)
    )
  })

  data_chunk <- reactive({
    ANL <- merged_data()()$data() # nolint
    validate_has_data(ANL, 3)
    chunk <- chunks$new()
    chunk$reset(envir = list2env(list(ANL = ANL)))
    chunk
  })

  count_chunk <- reactive({
    chunk <- data_chunk()$clone(deep = TRUE)
    groupby_vars <- r_groupby_vars()
    groupby_vars_l <- as.list(groupby_vars) # atomic -> list #nolintr

    # count
    n_names <- c() #nolintr
    count_by_group <- function(groupby_vars, ...) {
      # chunk and n_names are modified
      n_name <- get_n_name(groupby_vars)
      n_names <- c(n_names, n_name)
      count_by_group_chunk(chunk, groupby_vars = groupby_vars, n_name = n_name, ...)
    }

    count_by_group(groupby_vars, data_name = "ANL") # may be repeated by statements below

    if (input$show_n) {
      # count for each group
      #x_name: more complicated, done below
      if (!is.null(groupby_vars_l$fill_name)) count_by_group(groupby_vars_l$fill_name)
      if (!is.null(groupby_vars_l$x_facet_name)) count_by_group(groupby_vars_l$x_facet_name)
      if (!is.null(groupby_vars_l$y_facet_name)) count_by_group(groupby_vars_l$y_facet_name)

      if (!is.null(groupby_vars_l$fill_name)) add_count_str_to_column(chunk, column = groupby_vars_l$fill_name)
      if (!is.null(groupby_vars_l$x_facet_name)) add_count_str_to_column(chunk, column = groupby_vars_l$x_facet_name)
      if (!is.null(groupby_vars_l$y_facet_name)) add_count_str_to_column(chunk, column = groupby_vars_l$y_facet_name)

      #nolint start
      # add "(n = ...)" to right column
      # x is more complicated when x facetting, count should be per x-facet if it is selected
      # if (!is.null(groupby_vars_l$x_name) && !is.null(groupby_vars_l$fill_name)) {
      #   count_by_group(c(groupby_vars_l$x_name, groupby_vars_l$fill_name))
      #   add_count_str_to_column(chunk, column = groupby_vars_l$x_name, n_column = get_n_name(c(groupby_vars_l$x_name, groupby_vars_l$fill_name)))
      # }
      #nolint end
    }

    # add label and slice(1) as all patients in the same subgroup have same n_'s
    chunk$push(bquote({
      attr(counts[[.(get_n_name(groupby_vars))]], "label") <- "Count" # for plot
      counts <- counts %>%
        group_by_at(.(as.vector(groupby_vars))) %>%
        slice(1) %>%
        ungroup() %>%
        select(.(as.vector(groupby_vars)), starts_with("n_"))
    }))

    # dplyr::select loses labels
    chunk$push(teal.devel::get_anl_relabel_call(
      columns_source = merged_data()()$columns_source,
      datasets = datasets,
      anl_name = "counts"
    ))

    chunks_safe_eval(chunk)
    chunk
  })

  plot_chunk <- reactive({
    chunk <- count_chunk()$clone(deep = TRUE)

    groupby_vars <- as.list(r_groupby_vars()) # so $ access works below
    add_plot_call(
      chunk,
      y_name = get_n_name(groupby_vars),
      x_name = groupby_vars$x_name,
      fill_name = groupby_vars$fill_name,
      x_facet_name = groupby_vars$x_facet_name,
      y_facet_name = groupby_vars$y_facet_name,
      label_bars = input$label_bars,
      barlayout = input$barlayout,
      plot_options = list(axis_flipped = input$flip_axis, rotate_bar_labels = input$rotate_bar_labels)
    )

    apply_simple_ggplot_options(
      chunk,
      flip_axis = input$flip_axis,
      rotate_x_label = input$rotate_x_label,
      rotate_y_label = input$rotate_y_label,
      expand_y_range = input$expand_y_range
    )

    # also adds total n
    add_plot_title(chunk, groupby_vars)

    #explicitly calling print on the plot inside the chunk evaluates
    #the ggplot call and therefore catches errors
    chunk$push(quote(print(plot)))

    chunks_safe_eval(chunk)
    chunk
  })

  generate_code <- reactive({
    chunk <- plot_chunk()

    init_chunks(chunk) # set session chunks for ShowRCode

    chunk
  })

  output$plot <- renderPlot({
    generate_code()$get("plot")
  })

  output$table <- renderTable({
    generate_code()$get("counts")
  })

  # reactive vars
  # NULL: not present in UI, vs character(0): no selection

  # returns named vector of non-NULL variables to group by
  r_groupby_vars <- function() {
    x_name <- if (is.null(x)) NULL else as.vector(merged_data()()$columns_source$x)
    fill_name <- if (is.null(fill)) NULL else as.vector(merged_data()()$columns_source$fill)
    x_facet_name <- if (is.null(x_facet)) NULL else as.vector(merged_data()()$columns_source$x_facet)
    y_facet_name <- if (is.null(y_facet)) NULL else as.vector(merged_data()()$columns_source$y_facet)

    # set to NULL when empty character
    if (is_character_empty(x_name)) x_name <- NULL
    if (is_character_empty(fill_name)) fill_name <- NULL
    if (is_character_empty(x_facet_name)) x_facet_name <- NULL
    if (is_character_empty(y_facet_name)) y_facet_name <- NULL

    res <- c(
      x_name = x_name, fill_name = fill_name,
      x_facet_name = x_facet_name, y_facet_name = y_facet_name
    ) # c() -> NULL entries are omitted

    # at least one category must be specified
    validate(need(
      length(res) > 0, # c() removes NULL entries
      "Must specify at least one of x, fill, x_facet and y_facet."
    ))

    res
  }

  # generic code ----
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  merge_ex111 <- reactive(merged_data()()$expr)
  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    dataname = get_extract_datanames(list(x, fill, x_facet, y_facet)),
    merge_expression = merge_ex111(),
    #merge_expression = merged_data()()$expr, # not working
    modal_title = "Bar Chart"
  )
}



# Helper functions for chunks ----

# when x_name etc. are NULL, will not plot it
# plot_options$axis_flipped will only adjust hjust and vjust without flipping plot
add_plot_call <- function(chunk,
                          y_name, # y variable in plot
                          x_name = NULL, fill_name = NULL, x_facet_name = NULL, y_facet_name = NULL,
                          label_bars = TRUE, # whether to also draw numbers as text, i.e. label bars
                          barlayout = "stacked",
                          plot_options = NULL) {
  # c() filters out NULL
  plot_vars <- c(x_name, fill_name, x_facet_name, y_facet_name)
  stopifnot(
    is_character_single(y_name),
    is.null(x_name) || is_character_single(x_name),
    is.null(fill_name) || is_character_single(fill_name),
    is.null(x_facet_name) || is_character_single(x_facet_name),
    is.null(y_facet_name) || is_character_single(y_facet_name),
    !any(duplicated(plot_vars)),
    length(plot_vars) > 0,
    is_logical_single(label_bars),
    barlayout %in% c("side_by_side", "stacked"),
    is.list(plot_options)
  )


  plot_options <- modifyList(
    # default options
    list(
      axis_flipped = FALSE, rotate_bar_labels = FALSE
    ),
    if_null(plot_options, list())
  )

  plot_args <- c(
    quote(ggplot(counts))
  )

  # aesthetic variables
  x_val_var <- if (is.null(x_name)) 0 else x_name #nolintr
  plot_args <- c(plot_args,
                 if (is.null(fill_name)) {
                   bquote(aes_string(x = .(x_val_var)))
                 } else {
                   bquote(aes_string(x = .(x_val_var), fill = .(fill_name)))
                 }
  )

  if (!(is.null(x_facet_name) && is.null(y_facet_name))) {
    # free_x is needed, otherwise when we facet on x and x-ticks are different for each facet value,
    # it will fit all possible x-ticks across all facet values into each facet panel
    plot_args <- c(plot_args, bquote(
      facet_grid(.(facet_grid_formula(x_facet_name, y_facet_name)), scales = "free_x")
    ))
  }

  # position stacking or dodging for bars and text
  position <- if (is.null(fill_name) || (barlayout == "side_by_side")) {
    # vjust = on top, i.e. don't place below when only one filling (i.e. nothing really stacked)
    quote(position_dodge(0.9))
  } else {
    quote(position_stack(vjust = 0.5))
  }

  # draw bars
  if (is.null(fill_name)) {
    # nothing to put side-by-side, so put fill to one color only
    # setting color via aesthetics does not work for some reason (but x = 0 above works)
    plot_args <- c(plot_args, bquote(
      geom_col(aes_string(y = .(y_name)), position = .(position), fill = "#b6cae9")
    ))
  } else {
    plot_args <- c(plot_args, bquote(
      geom_col(aes_string(y = .(y_name)), position = .(position))
    ))
  }

  # draw numbers above bars
  if (label_bars) {
    # center text and move slightly to the top or to the right (depending on flip axes)
    # see https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
    if (isTRUE(plot_options$axis_flipped)) {
      hjust <- if (barlayout == "stacked") 0.5 else -1 # put above bars if not stacked #nolintr
      vjust <- 0.5 #nolintr
    } else {
      hjust <- 0.5 #nolintr
      vjust <- if (barlayout == "stacked") 0.5 else -1 # put above bars if not stacked #nolintr
    }
    plot_args <- c(plot_args, bquote(
      geom_text(aes_string(y = .(y_name), label = .(y_name)),
                stat = "identity",
                angle = .(if (plot_options$rotate_bar_labels) 45 else 0),
                position = .(position),
                # hjust, vjust are respective to position, i.e. top, center etc. alignment
                hjust = .(hjust), vjust = .(vjust)
      )
    ))
  }

  # add legend for fill
  if (!is.null(fill_name)) {
    plot_args <- c(plot_args, bquote(
      guides(fill = guide_legend(title = utils.nest::column_annotation_label(counts, .(fill_name))))
    ))
  }

  # add xlabels and ylabels
  plot_args <- c(
    plot_args,
    quote(theme(plot.title = element_text(hjust = 0.5))),
    if (!is.null(x_name)) bquote(xlab(utils.nest::column_annotation_label(counts, .(x_name)))),
    bquote(ylab(utils.nest::column_annotation_label(counts, .(y_name))))
  )

  # when x_name is NULL, modify theme to not show x-ticks
  if (is.null(x_name)) {
    plot_args <- c(plot_args, quote(
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    ))
  }

  chunk$push(bquote({
    plot <- .(call_concatenate(plot_args))
  }))
}

# NULL means option is not set
#' @importFrom scales comma
apply_simple_ggplot_options <- function(chunk,
                                        flip_axis = NULL,
                                        rotate_x_label = NULL,
                                        rotate_y_label = NULL,
                                        expand_y_range = NULL) {
  stopifnot(
    is.null(flip_axis) || is_logical_single(flip_axis),
    is.null(rotate_x_label) || is_logical_single(rotate_x_label),
    is.null(rotate_y_label) || is_logical_single(rotate_y_label),
    is.null(expand_y_range) || is_numeric_single(expand_y_range)
  )
  plot_args <- c(bquote(plot))

  if (isTRUE(flip_axis)) {
    plot_args <- c(plot_args, quote(
      coord_flip()
    ))
  }
  if (isTRUE(rotate_x_label)) {
    plot_args <- c(plot_args, quote(
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ))
  }

  if (isTRUE(rotate_y_label)) {
    plot_args <- c(plot_args, quote(
      theme(axis.text.y = element_text(angle = 45, hjust = 1))
    ))
  }

  if (!is.null(expand_y_range)) {
    plot_args <- c(plot_args, bquote(scale_y_continuous(
      labels = scales::comma,
      expand = expand_scale(c(0, .(expand_y_range)))
    )))
  }

  chunk$push(bquote({
    plot <- .(call_concatenate(plot_args))
  }))
}

# get name of column in "counts" data.frame
get_n_name <- function(groupby_vars) {
  paste0("n_", paste(groupby_vars, collapse = "_"))
}

# chunk that counts by specified group
# n_name: name of column to add counts to, by default determined from groupby_vars
count_by_group_chunk <- function(chunk, groupby_vars, n_name = NULL, data_name = "counts") {
  groupby_vars <- as.vector(groupby_vars) # as.vector unnames
  stopifnot(is_character_vector(groupby_vars, min_length = 0)) # also works for zero length

  n_name <- if_null(n_name, get_n_name(groupby_vars))
  chunk$push(bquote({
    counts <- .(as.symbol(data_name)) %>%
      group_by_at(.(groupby_vars)) %>%
      mutate(.(as.symbol(n_name)) := n()) %>%
      ungroup()
  }))
}


# Util functions ----
#' Concatenate expressions via a binary operator
#'
#' e.g. combine with \code{+} for ggplot without introducing parentheses due to associativity
#'
#' @param args arguments to concatenate with operator
#' @param bin_op binary operator to concatenate it with
#'
#' @examples
#' \dontrun{
#' # What we want to achieve
#' call("+", quote(f), quote(g))
#' call("+", quote(f), call("+", quote(g), quote(h))) # parentheses not wanted
#' call("+", call("+", quote(f), quote(g)), quote(h)) # as expected without unnecessary parentheses
#' Reduce(function(existing, new) call("+", existing, new), list(quote(f), quote(g), quote(h)))
#'
#' # how we do it
#' call_concatenate(list(quote(f), quote(g), quote(h)))
#' call_concatenate(list(quote(f)))
#' call_concatenate(list())
#' call_concatenate(list(quote(ggplot(mtcars)), quote(geom_point(aes(wt, mpg)))))
#'
#' eval(call_concatenate(list(quote(ggplot(mtcars)), quote(geom_point(aes(wt, mpg))))))
#' }
call_concatenate <- function(args, bin_op = "+") {
  stopifnot(
    is_character_single(bin_op),
    all(vapply(args, is.language, logical(1)))
  )
  # can be used for dplyr and ggplot2 to concatenate calls with +
  Reduce(function(existing, new) call(bin_op, existing, new), args)
}

# needs columns like n_, n_ARM etc. to get count from
add_count_str_to_column <- function(chunk, column, n_column = NULL) {
  n_column <- if_null(n_column, get_n_name(groupby_vars = column))
  stopifnot(
    is_character_single(column)
  )

  chunk$push(bquote({
    counts <- counts %>% mutate(
      .(as.symbol(column)) := paste0(.(as.symbol(column)), " (n = ", .(as.symbol(n_column)), ")")
    )
  }))
}

add_plot_title <- function(chunk, groupby_vars) {
  chunk$push(bquote({
    total_n <- nrow(ANL) # get it from original dataset
    plot_title <- paste0("Number of patients (total N = ", total_n,
                         .(paste0(") for each combination of (", paste(groupby_vars, collapse = ", "), ")"))
    )
    plot <- plot + ggtitle(plot_title)
  }))
}
