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
    expr = {
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    }
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
                         parentname = ifelse(
                           is(arm_var, "data_extract_spec"),
                           datanames_input(arm_var),
                           "ADSL"
                           ),
                         arm_var,
                         summarize_vars,
                         useNA = c("ifany", "no"), # nolint
                         denominator = c("N", "n", "omit"),
                         pre_output = NULL,
                         post_output = NULL) {
  stopifnot(
    is_character_single(dataname),
    is_character_single(parentname)
  )
  useNA <- match.arg(useNA) # nolint
  denominator <- match.arg(denominator)

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    summarize_vars = cs_to_des_select(summarize_vars, dataname = dataname, multiple = TRUE)
  )

  module(
    label = label,
    server = srv_summary,
    ui = ui_summary,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label
        )
      ),
    filters = dataname
  )

}

#' @noRd
ui_summary <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(a$arm_var, a$summarize_vars)

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "summarize_vars")]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = TRUE),
      data_extract_input(
        id = ns("summarize_vars"),
        label = "Summarize Variables",
        data_extract_spec = a$summarize_vars,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        panel_item(
          "Additional table settings",
          radioButtons(
            ns("useNA"),
            label = "Display NA counts",
            choices = c("ifany", "no"),
            selected = a$useNA
          ),
          radioButtons(
            ns("denominator"),
            label = "Denominator choice",
            choices = c("N", "n", "omit"),
            selected = a$denominator
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

#' @noRd
srv_summary <- function(input,
                        output,
                        session,
                        datasets,
                        dataname,
                        parentname,
                        arm_var,
                        summarize_vars,
                        label) {
  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, summarize_vars),
    input_id = c("arm_var", "summarize_vars"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  # validate inputs
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_summarize_vars <- as.vector(anl_m$columns_source$summarize_vars)

    validate(
      need(input_arm_var, "Please select an arm variable"),
      need(input_summarize_vars, "Please select a summarize variable"),
      need(
        !(input$useNA == "ifany" && input$denominator == "n"),
        "If NA values are removed, denominator cannot be n"
      )
    )

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_summarize_vars),
      arm_var = input_arm_var
    )
  })

  # generate r code for the analysis
  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    sum_vars <- as.vector(anl_m$columns_source$summarize_vars)

    my_calls <- template_summary(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      sum_vars = sum_vars,
      var_labels = datasets$get_variable_labels(dataname, sum_vars),
      add_total = input$add_total,
      na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE), # nolint
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
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = "R Code for the current Summary Table",
    code_header = label
  )
}
