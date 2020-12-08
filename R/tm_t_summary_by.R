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
                                var_labels = character(),
                                parallel_vars = FALSE,
                                row_groups = FALSE,
                                add_total = FALSE,
                                na.rm = FALSE, # nolint
                                denominator = c("N", "n", "omit")) {
  assert_that(
    is.string(parentname),
    is.string(dataname),
    is.string(arm_var),
    is.character(sum_vars),
    is.character(by_vars),
    is.character(var_labels),
    is.flag(parallel_vars),
    is.flag(row_groups),
    is.flag(add_total),
    is.flag(na.rm)
  )
  denominator <- match.arg(denominator)


  y <- list()

  # Data processing
  y$data <- substitute(
    expr =  anl <- df,
    env = list(
      df = as.name(dataname)
    )
  )

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

  if (denominator == "omit") {
    env_vars <- list(
    sum_vars = sum_vars,
    sum_var_labels = var_labels[sum_vars],
    na.rm = na.rm,
    denom = ifelse(denominator == "n", "n", "N_col"),
    stats = c("n", "mean_sd", "median", "range", "count"),
    formats = c(
      n = "xx",
      mean_sd = "xx.xx (xx.xx)",
      median = "xx.xx",
      range = "xx.xx - xx.xx",
      count = "xx"
      )
    )
  }
  else{
    env_vars <- list(
      sum_vars = sum_vars,
      sum_var_labels = var_labels[sum_vars],
      na.rm = na.rm,
      denom = ifelse(denominator == "n", "n", "N_col"),
      stats = c("n", "mean_sd", "median", "range", "count_fraction"),
      formats = c(
        n = "xx",
        mean_sd = "xx.xx (xx.xx)",
        median = "xx.xx",
        range = "xx.xx - xx.xx",
        count_fraction = "xx (xx.%)"
        )
      )
  }



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
          split_fun = drop_split_levels,
          visible_label = TRUE
        ),
        env = list(
          by_var = by_var,
          split_label = split_label
        )
      )
    )

    if (row_groups) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          expr = summarize_row_groups(
          ),
          env = env_vars
        )
      )
    }
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


  if (row_groups) {
    layout_list <- layout_list
    }
  else {
    layout_list <- add_expr(
      layout_list,
      if (parallel_vars) {
        if (length(var_labels) > 0) {
          substitute(
            expr = summarize_colvars(
              na.rm = na.rm,
              denom = denom,
              .stats = stats
            ),
            env = env_vars
          )
        }
        else {
          substitute(
            expr = summarize_colvars(
              vars = sum_vars,
              na.rm = na.rm,
              denom = denom,
              .stats = stats
            ),
            env = env_vars)
        }
      }
      else {
        if (length(var_labels > 0)) {
          substitute(
            expr = summarize_vars(
              vars = sum_vars,
              var_labels = sum_var_labels,
              na.rm = na.rm,
              denom = denom,
              .stats = stats,
              .formats = formats
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
              .stats = stats,
              .formats = formats
            ),
            env = env_vars
          )
        }
      }
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Adjust column counts when `parallel_vars` is selected
  if (add_total) {
    if (parallel_vars) {
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
    if (parallel_vars) {
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
    expr = {
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
      },
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
                            parentname = ifelse(
                              is(arm_var, "data_extract_spec"),
                              datanames_input(arm_var),
                              "ADSL"
                              ),
                            arm_var,
                            by_vars,
                            summarize_vars,
                            paramcd = NULL,
                            parallel_vars = FALSE,
                            row_groups = FALSE,
                            useNA = c("ifany", "no"), # nolint
                            denominator = choices_selected(c("n", "N", "omit"), "omit", fixed = TRUE),
                            pre_output = NULL,
                            post_output = NULL) {

  useNA <- match.arg(useNA) # nolint
  stopifnot(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is_logical_single(parallel_vars),
    is_logical_single(row_groups),
    useNA %in% c("ifany", "no"), # nolint
    is.choices_selected(denominator),
    denominator$choices %in% c("n", "N", "omit")
  )


  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = if_not_null(
      paramcd,
      cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE)
      ),
    by_vars = cs_to_des_select(by_vars, dataname = dataname, multiple = TRUE),
    summarize_vars = cs_to_des_select(summarize_vars, dataname = dataname, multiple = TRUE)
  )

  module(
    label = label,
    ui = ui_summary_by,
    ui_args = c(data_extract_list, args),
    server = srv_summary_by,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label
        )
      ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_summary_by <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$by_vars,
    a$summarize_vars
    )

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "paramcd", "by_vars", "summarize_vars")]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      if_not_null(
        a$paramcd,
        data_extract_input(
          id = ns("paramcd"),
          label = "Select Endpoint",
          data_extract_spec = a$paramcd,
          is_single_dataset = is_single_dataset_value
        )
      ),
      data_extract_input(
        id = ns("by_vars"),
        label = "Row By Variable",
        data_extract_spec = a$by_vars,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = TRUE),
      data_extract_input(
        id = ns("summarize_vars"),
        label = "Summarize Variables",
        data_extract_spec = a$summarize_vars,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("parallel_vars"), "Show summarize variables in parallel", value = a$parallel_vars),
      checkboxInput(ns("row_groups"), "Show summarize variables in row groups", value = a$row_groups),
      panel_group(
        panel_item(
          "Additional table settings",
          radioButtons(
            ns("useNA"),
            label = "Display NA counts",
            choices = c("ifany", "no"),
            selected = a$useNA
          ),
          optionalSelectInput(
            inputId = ns("denominator"),
            label = "Denominator choice",
            choices = a$denominator$choices,
            selected = a$denominator$selected,
            fixed = a$denominator$fixed
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
srv_summary_by <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           parentname,
                           arm_var,
                           paramcd,
                           by_vars,
                           summarize_vars,
                           label) {
  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = if (is.null(paramcd)) {
      list(arm_var, by_vars, summarize_vars)
    } else {
      list(arm_var, paramcd, by_vars, summarize_vars)
    },
    input_id = if (is.null(paramcd)) {
      list("arm_var", "by_vars", "summarize_vars")
    } else {
      list("arm_var", "paramcd", "by_vars", "summarize_vars")
    },
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  # Prepare the analysis environment (filter data, check data, populate envir).
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_by_vars <- as.vector(anl_m$columns_source$by_vars)
    input_summarize_vars <- as.vector(anl_m$columns_source$summarize_vars)
    input_paramcd <- if_not_null(paramcd, unlist(paramcd$filter)["vars"])

    validate(
      need(input_arm_var, "Please select an arm variable"),
      need(input_summarize_vars, "Please select a summarize variable")
    )

    # validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_by_vars, input_summarize_vars),
      arm_var = input_arm_var,
      min_nrow = 1
    )

    if (input$parallel_vars) {
      validate(need(
        all(vapply(anl_filtered[input_summarize_vars], is.numeric, logical(1))),
        "Summarize variables must all be numeric to display in parallel columns."
      ))
    }
  })

  # The R-code corresponding to the analysis.
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

    my_calls <- template_summary_by(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      sum_vars = sum_vars,
      by_vars = as.vector(anl_m$columns_source$by_vars),
      var_labels = datasets$get_variable_labels(dataname, sum_vars),
      add_total = input$add_total,
      na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE), #nolint
      denominator = input$denominator,
      parallel_vars = input$parallel_vars,
      row_groups = input$row_groups
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
    datanames = get_extract_datanames(
      list(arm_var, paramcd, by_vars, summarize_vars)
      ),
    modal_title = "Summary by Row Groups Table",
    code_header = label
  )
}
