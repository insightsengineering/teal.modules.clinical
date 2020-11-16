#' Teal Module: Summarize Variables by Row Groups Module
#'
#' @name summary_by
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @param parallel_vars (\code{logical}) used to display \code{summarize_vars} as parallel columns
#'  (\code{FALSE} on default). Can be used only if all chosen analysis variables are numeric.
#' @param row_groups (\code{logical}) used to display \code{summarize_vars} as row groups
#'  (\code{FALSE} on default).
#' @md
#'
#' @return a \code{\link[teal]{module}} object
#'
NULL

#' @describeIn summary_by create the expressions corresponding to the analysis
#'   that should be summarized.
#'

template_summary_by <- function(parentname,
                                dataname,
                                arm_var,
                                sum_vars,
                                by_vars,
                                paramcd,
                                var_labels = character(),
                                parallel_vars = FALSE,
                                row_groups = FALSE,
                                add_total = FALSE,
                                na.rm = FALSE, # nolintr
                                denominator = c("N", "n", "omit")) {
  assert_that(
    is.string(parentname),
    is.string(dataname),
    is.string(arm_var),
    is.character(sum_vars),
    is.character(by_vars),
    is.character(paramcd),
    is.character(var_labels),
    is.flag(parallel_vars),
    is.flag(row_groups),
    is.flag(add_total),
    is.flag(na.rm)
  )
  denominator <- match.arg(denominator)


  y <- list()

  # Data processing
  y$data <- if (dataname != parentname) {
    substitute(
      expr =  anl <- df %>% filter(PARAMCD %in% paramcd),
      env = list(
        df = as.name(dataname),
        paramcd = paramcd)
    )
  } else {
    substitute(
      expr =  anl <- df,
      env = list(df = as.name(dataname))
    )
  }

  # Build layout
  layout_list <- list()

  layout_list <- add_expr(
    layout_list,
    quote(basic_table())
  )

  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = split_cols_by(
          arm_var,
          split_fun = add_overall_level("All Patients", first = FALSE)
        ),
        env = list(arm_var = arm_var)
      )
    }
    else {
      substitute(
        expr = split_cols_by(arm_var),
        env = list(arm_var = arm_var)
      )
    }
  )

  layout_list <- add_expr(
    layout_list,
    quote(add_colcounts())
  )

  for (by_var in by_vars) {

    split_label <- substitute(
      expr = var_labels(dataname)[[by_var]],
      env = list(
        dataname = as.name(dataname),
        by_var = by_var
      )
    )

    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by(
          by_var,
          split_label = split_label,
          visible_label = TRUE
        ),
        env = list(
          by_var = by_var,
          split_label = split_label
        )
      )
    )
  }

  if (parallel_vars) {
    layout_list <- add_expr(
      layout_list,
      if (length(var_labels) > 0) {
        substitute(
          expr =  split_cols_by_multivar(vars = sum_vars, varlabels = sum_var_labels),
          env = list(sum_vars = sum_vars, sum_var_labels = var_labels[sum_vars])
        )
      }
      else {
        substitute(
          expr =  split_cols_by_multivar(vars = sum_vars),
          env = list(sum_vars = sum_vars)
        )
      }
    )
  }

  env_vars <- list(
    sum_vars = sum_vars,
    sum_var_labels = var_labels[sum_vars],
    na.rm = na.rm,
    denom = ifelse(denominator == "n", "n", "N_col"),
    stats = c(
      c("n", "mean_sd", "median", "range"),
      ifelse(denominator == "omit", "count", "count_fraction")
    )
  )

  layout_list <- add_expr(
    layout_list,
    if (row_groups) {
        substitute(
          expr = summarize_row_groups(
          ),
          env = env_vars
        )
    } else if (parallel_vars) {
      if (length(var_labels) > 0) {
        substitute(
          expr = summarize_colvars(
            na.rm = na.rm,
            denom = denom,
            .stats = stats
          ),
          env = env_vars
        )
      } else {
        substitute(
          expr = summarize_colvars(
            vars = sum_vars,
            na.rm = na.rm,
            denom = denom,
            .stats = stats
          ),
          env = env_vars)
      }
    } else {
      if (length(var_labels > 0)) {
        substitute(
          expr = summarize_vars(
            vars = sum_vars,
            var_labels = sum_var_labels,
            na.rm = na.rm,
            denom = denom,
            .stats = stats
          ),
          env = env_vars
        )
      }
      else {
        substitute(
          expr = summarize_vars(
            vars = sum_vars,
            na.rm = na.rm,
            denom = denom,
            .stats = stats
          ),
          env = env_vars
        )
      }
    }
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Adjust column counts when `parallel_vars` is selected
  if (add_total) {
    if (parallel_vars){
      col_counts <- substitute(
        expr = c(
          rep(table(parentname$arm_var), each = length(sum_vars)),
          rep(sum(table(parentname$arm_var)), each = length(sum_vars))
        ),
        env = list(parentname = as.name(parentname), arm_var = arm_var, sum_vars = sum_vars)
      )
    } else {
      col_counts <- substitute(
        expr = c(
          table(parentname$arm_var),
          sum(table(parentname$arm_var))
        ),
        env = list(parentname = as.name(parentname), arm_var = arm_var, sum_vars = sum_vars)
      )
    }
  } else {
    if (parallel_vars){
      col_counts <- substitute(
        expr = rep(table(parentname$arm_var), each = length(sum_vars)),
        env = list(parentname = as.name(parentname), arm_var = arm_var, sum_vars = sum_vars)
      )
    } else {
      col_counts <- substitute(
        expr = table(parentname$arm_var),
        env = list(parentname = as.name(parentname), arm_var = arm_var)
      )
    }
  }

  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl, col_counts = col_counts),
    env = list(col_counts = col_counts)
  )
  y
}

#' @describeIn summary_by teal module function to generate the required summary by row
#'   groups module.
#' @param summarize_vars (`choices_selected`)\cr names of the variables
#'   that should be summarized.
#'
#' @export
#' @examples
#' library(random.cdisc.data)
#' adsl <- radsl(cached = TRUE)
#' adlb <- radlb(cached = TRUE)
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'ADSL <- radsl(cached = TRUE)'),
#'     cdisc_dataset("ADLB", adlb, code = 'ADLB <- radlb(cached = TRUE)'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_summary_by(
#'       label = "Summary by Row Groups Table",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       by_vars = choices_selected(
#'         choices = variable_choices(adlb, c("PARAM", "AVISIT")),
#'         selected = c("AVISIT")
#'       ),
#'       summarize_vars = choices_selected(
#'         choices = variable_choices(adlb, c("AVAL", "CHG")),
#'         selected = c("AVAL")
#'       ),
#'       denominator = "N",
#'       useNA = "ifany",
#'       paramcd = choices_selected(
#'         choices = value_choices(adlb, "PARAMCD", "PARAM"),
#'         selected = "ALT"
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_summary_by <- function(label,
                            dataname,
                            arm_var,
                            by_vars,
                            summarize_vars,
                            paramcd = choices_selected("", fixed = TRUE),
                            parallel_vars = FALSE,
                            row_groups = FALSE,
                            useNA = c("ifany", "no"), # nolintr
                            denominator = c("n", "N", "omit"),
                            pre_output = NULL,
                            post_output = NULL) {
  args <- c(as.list(environment()))

  module(
    label = label,
    ui = ui_summary_by,
    ui_args = args,
    server = srv_summary_by,
    server_args = list(dataname = dataname),
    filters = dataname
  )
}

#' @noRd
ui_summary_by <- function(id, ...) {

  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(args$dataname)),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          args$arm_var$choices,
                          args$arm_var$selected,
                          multiple = FALSE,
                          fixed = args$arm_var$fixed
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = TRUE),
      optionalSelectInput(ns("summarize_vars"),
                          "Summarize Variables",
                          args$summarize_vars$choices,
                          args$summarize_vars$selected,
                          multiple = TRUE,
                          fixed = args$summarize_vars$fixed
      ),
      optionalSelectInput(ns("paramcd"), "Select Parameter",
                          choices = args$paramcd$choices,
                          selected = args$paramcd$selected,
                          multiple = TRUE,
                          fixed = args$paramcd$fixed
      ),
      optionalSelectInput(ns("by_vars"),
                          "Row By Variable",
                          args$by_vars$choices,
                          args$by_vars$selected,
                          multiple = TRUE,
                          fixed = args$by_vars$fixed
      ),
      checkboxInput(ns("parallel_vars"), "Show summarize variables in parallel", value = args$parallel_vars),
      checkboxInput(ns("row_groups"), "Show summarize variables in row groups", value = args$row_groups),
      panel_group(
        panel_item(
          "Additional table settings",
          radioButtons(
            ns("useNA"),
            label = "Display NA counts",
            choices = c("ifany", "no"),
            selected = args$useNA
          ),
          radioButtons(
            ns("denominator"),
            label = "Denominator choice",
            choices = c("N", "n", "omit"),
            selected = args$denominator
          )
        )
      )
    ),
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}


#' @noRd
srv_summary_by <- function(input,
                           output,
                           session,
                           datasets,
                           dataname) {
  init_chunks()

  # Prepare the analysis environment (filter data, check data, populate envir).
  prepared_env <- reactive({
    adsl_f <- datasets$get_data("ADSL", filtered = TRUE)
    anl_f <- datasets$get_data(dataname, filtered = TRUE)

    arm_var <- input$arm_var
    add_total <- input$add_total
    summarize_vars <- input$summarize_vars
    useNA <- input$useNA # nolint
    denominator <- input$denominator
    by_vars <- input$by_vars
    paramcd <- input$paramcd
    parallel_vars <- input$parallel_vars
    row_groups <- input$row_groups # nolint

    validate(need(is.logical(add_total), "add total is not logical"))
    validate(need(useNA %in% c("ifany", "no"), "useNA must be ifany or no")) # nolint
    validate(need(denominator %in% c("N", "n", "omit"), "denominator must be N, n, or omit"))
    validate(need(!is.null(summarize_vars), "please select 'summarize variables'"))
    validate(need(all(summarize_vars %in% names(anl_f)), "not all variables available"))
    validate(need(!is.null(arm_var), "please select 'arm variable'"))
    validate(need(!is.null(paramcd), "please select 'parameter'"))
    validate(need(!is.null(by_vars), "please select 'By row variable'"))
    validate(need(arm_var %in% names(anl_f), "arm variable does not exist"))
    validate_has_data(anl_f, min_nrow = 1)
    validate(need(all(by_vars %in% names(anl_f)), "not all by-variables available"))
    if (parallel_vars) {
      validate(need(
        all(vapply(anl_f[summarize_vars], FUN =  is.numeric, FUN.VALUE = TRUE)),
        "Summarize variables must all be numeric to display in parallel columns."
      ))
    }

    # Send data where the analysis lives.
    e <- new.env()
    e$ADSL_FILTERED <- adsl_f #nolint
    anl_name <- paste0(dataname, "_FILTERED")
    e[[anl_name]] <- anl_f # nolint
    e
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_summary_by(
      parentname = "ADSL_FILTERED",
      dataname = paste0(dataname, "_FILTERED"),
      arm_var = input$arm_var,
      sum_vars = input$summarize_vars,
      by_vars = input$by_vars,
      var_labels = datasets$get_variable_labels(dataname, input$summarize_vars),
      paramcd = input$paramcd,
      add_total = input$add_total,
      na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE),  #nolint
      denominator = input$denominator,
      parallel_vars = input$parallel_vars,
      row_groups <- input$row_groups
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  output$table <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  # Render R code.
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Summary",
      rcode = get_rcode(
        datasets = datasets,
        datanames = dataname,
        title = "Summary by Row Groups Table"
      )
    )
  })
}
