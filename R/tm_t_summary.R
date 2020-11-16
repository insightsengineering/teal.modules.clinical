#' Teal Module: Summary of Variables
#'
#' @name summary
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#'
NULL

#' @describeIn summary create the expressions corresponding to the analysis.
#'
template_summary <- function(dataname,
                             parentname,
                             arm_var,
                             sum_vars,
                             var_labels = character(),
                             add_total = FALSE,
                             na.rm = FALSE,  #nolint
                             denominator = c("N", "n", "omit")) {
  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.character(sum_vars),
    is.character(var_labels),
    is.flag(add_total),
    is.flag(na.rm)
  )
  denominator <- match.arg(denominator)

  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = col_counts <- table(parentname$arm_var),
      env = list(parentname = as.name(parentname), arm_var = arm_var)
    )
  )
  if (add_total) {
    data_list <- add_expr(
      data_list,
      quote(col_counts <- c(col_counts, sum(col_counts)))
    )
  }
  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df,
      env = list(
        df = as.name(dataname)
      )
    )
  )
  y$data <- bracket_expr(data_list)

  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    quote(basic_table())
  )
  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = split_cols_by(arm_var, split_fun = add_overall_level(label = "All Patients", first = FALSE)),
        env = list(arm_var = arm_var)
      )
    } else {
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
  env_sum_vars <- list(
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
    if (length(var_labels) > 0) {
      substitute(
        expr = summarize_vars(
          vars = sum_vars,
          var_labels = sum_var_labels,
          na.rm = na.rm,
          denom = denom,
          .stats = stats
        ),
        env = env_sum_vars
      )
    } else {
      substitute(
        expr = summarize_vars(
          vars = sum_vars,
          na.rm = na.rm,
          denom = denom,
          .stats = stats
        ),
        env = env_sum_vars
      )
    }
  )
  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- quote(
    expr = result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
  )

  y
}

#' @describeIn summary teal module for summary of variables.
#' @param summarize_vars (`choices_selected`)\cr names of the variables
#'   that should be summarized.
#'
#' @export
#' @examples
#' # Preparation of the test case.
#' library(dplyr)
#' library(random.cdisc.data)
#' library(tern)
#'
#' adsl <- radsl(cached = TRUE)
#'
#' # Include `EOSDY` and `DCSREAS` variables below because they contain missing data.
#' stopifnot(
#'   any(is.na(adsl$EOSDY)),
#'   any(is.na(adsl$DCSREAS))
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     code = 'ADSL <- radsl(cached = TRUE)',
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_summary(
#'       label = "Demographic Table",
#'       dataname = "ADSL",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       summarize_vars = choices_selected(
#'         c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS"),
#'         c("SEX", "RACE")
#'       ),
#'       useNA = "ifany"
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_summary <- function(label,
                         dataname,
                         arm_var,
                         summarize_vars,
                         useNA = c("ifany", "no"), # nolintr
                         denominator = c("N", "n", "omit"),
                         pre_output = NULL,
                         post_output = NULL) {

  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(summarize_vars))
  useNA <- match.arg(useNA) # nolintr
  denominator <- match.arg(denominator)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_summary,
    ui = ui_summary,
    ui_args = args,
    server_args = list(dataname = dataname, label = label),
    filters = dataname
  )

}

#' @noRd
ui_summary <- function(id, ...) {

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
srv_summary <- function(input,
                        output,
                        session,
                        datasets,
                        dataname,
                        label) {
  init_chunks()

  # Prepare the analysis environment (filter data, check data, populate envir).
  prepared_env <- reactive({
    anl_f <- datasets$get_data(dataname, filtered = TRUE)
    adsl_f <- datasets$get_data("ADSL", filtered = TRUE)

    arm_var <- input$arm_var
    add_total <- input$add_total
    summarize_vars <- input$summarize_vars
    useNA <- input$useNA # nolint
    denominator <- input$denominator

    validate(need(is.logical(add_total), "add total is not logical"))
    validate(need(useNA %in% c("ifany", "no"), "useNA must be ifany or no")) # nolint
    validate(need(denominator %in% c("N", "n", "omit"), "denominator must be N, n, or omit"))
    if (useNA == "ifany") {
      validate(need(denominator != "n", "when useNA is ifany, denominator cannot be n"))
    }
    validate(need(!is.null(summarize_vars), "please select 'summarize variables'"))
    validate(need(all(summarize_vars %in% names(anl_f)), "not all variables available"))
    validate(need(!is.null(arm_var), "please select 'arm variable'"))
    validate(need(arm_var %in% names(anl_f), "arm variable does not exist"))
    validate_has_data(anl_f, min_nrow = 1)

    # Send data where the analysis lives.
    e <- new.env()
    e$ANL_FILTERED <- anl_f # nolint
    e$ADSL_FILTERED <- adsl_f #nolint
    e
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_summary(
      dataname = paste0(dataname, "_FILTERED"),
      parentname = "ADSL_FILTERED",
      arm_var = input$arm_var,
      sum_vars = input$summarize_vars,
      var_labels = datasets$get_variable_labels(dataname, input$summarize_vars),
      add_total = input$add_total,
      na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE),  #nolint
      denominator = input$denominator
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
      title = "R Code for the current Summary Table",
      rcode = get_rcode(datasets = datasets, title = label)
    )
  })
}
