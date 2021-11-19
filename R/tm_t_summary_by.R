#' Template: Summarize Variables by Row Groups Module
#'
#' @inheritParams template_arguments
#' @param parallel_vars (`logical`) used to display `summarize_vars` as parallel columns
#'  (`FALSE` on default). Can be used only if all chosen analysis variables are numeric.
#' @param row_groups (`logical`) used to display `summarize_vars` as row groups
#'  (`FALSE` on default).
#' @param numeric_stats (`character`)\cr
#'  selected statistics for numeric summarize variables to be displayed. Possible values are `n`, `mean_sd`, `mean_ci`,
#'  `median`, `median_ci`, `quantiles`, `range`. All are selected by default.
#' @param drop_zero_levels (`logical`) used to remove rows with zero counts from the result table.
#'
#' @seealso [tm_t_summary_by()]
#'
template_summary_by <- function(parentname,
                                dataname,
                                arm_var,
                                id_var,
                                sum_vars,
                                by_vars,
                                var_labels = character(),
                                add_total = TRUE,
                                parallel_vars = FALSE,
                                row_groups = FALSE,
                                na.rm = FALSE, # nolint
                                na_level = "<Missing>",
                                numeric_stats = c(
                                  "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range"
                                ),
                                denominator = c("N", "n", "omit"),
                                drop_arm_levels = TRUE,
                                drop_zero_levels = TRUE) {
  assert_that(
    is.string(parentname),
    is.string(dataname),
    is.string(arm_var),
    is.string(id_var),
    is.character(sum_vars),
    is.character(by_vars),
    is.character(var_labels),
    is.flag(add_total),
    is.flag(parallel_vars),
    is.flag(row_groups),
    is.flag(na.rm),
    is.string(na_level),
    is.flag(drop_arm_levels),
    is.character(numeric_stats),
    is.flag(drop_zero_levels)
  )
  denominator <- match.arg(denominator)


  y <- list()

  # Data processing
  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr =  anl <- df %>%
        df_explicit_na(omit_columns = setdiff(names(df), c(by_vars, sum_vars)), na_level = na_level),
      env = list(
        df = as.name(dataname),
        by_vars = by_vars,
        sum_vars = sum_vars,
        na_level = na_level
      )
    )
  )

  data_list <- add_expr(
    data_list,
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = arm_var,
      drop_arm_levels = drop_arm_levels
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- df_explicit_na(parentname, na_level = na_level),
      env = list(parentname = as.name(parentname), na_level = na_level))
  )

  y$data <- bracket_expr(data_list)

  # Build layout
  y$layout_prep <- quote(split_fun <- drop_split_levels)
  if (row_groups) {
  # nolint start
    y$layout_cfun <- quote(
      cfun_unique <- function(x, labelstr = "", .N_col) {
        y <- length(unique(x))
        rcell(
          c(y , y / .N_col),
          label = labelstr
        )
      }
    )
  # nolint end
}

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
      na_level = na_level,
      denom = ifelse(denominator == "n", "n", "N_col"),
      stats = c(numeric_stats, "count")
    )
  }
  else{
    env_vars <- list(
      sum_vars = sum_vars,
      sum_var_labels = var_labels[sum_vars],
      na.rm = na.rm,
      na_level = na_level,
      denom = ifelse(denominator == "n", "n", "N_col"),
      stats = c(numeric_stats, "count_fraction")
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
          split_fun = split_fun,
          label_pos = "topleft"
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
          expr = summarize_row_groups(var = id_var, cfun = cfun_unique),
          env = list(
            id_var = id_var
          )
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
  } else {
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
              na_level = na_level,
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
              na_level = na_level,
              denom = denom,
              .stats = stats
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

  if (drop_zero_levels) {
    y$table <- substitute(
      expr = {
        all_zero <- function(tr) {
          if (!is(tr, "TableRow") || is(tr, "LabelRow"))
            return(FALSE)
          rvs <- unlist(unname(row_values(tr)))
          all(rvs == 0)
        }
        result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent) %>% trim_rows(criteria = all_zero)
        result
      },
      env = list(parent = as.name(parentname))
    )
  } else {
    y$table <- substitute(
      expr = {
        result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent)
        result
      },
      env = list(parent = as.name(parentname))
    )
  }

  y
}

#' Teal Module: Summarize Variables by Row Groups Module
#'
#' @param drop_arm_levels (`logical`)\cr drop the unused `arm_var` levels.
#'   When `TRUE`, `arm_var` levels are set to those used in the `dataname` dataset. When `FALSE`,
#'   `arm_var` levels are set to those used in the `parentname` dataset.
#'   If `dataname` dataset and `parentname` dataset are the same (i.e. ADSL), then `drop_arm_levels` will always be
#'   TRUE regardless of the user choice when `tm_t_summary_by` is called.
#' @param numeric_stats (`character`)\cr
#'   selected statistics for numeric summarize variables to be displayed. Possible values are `n`, `mean_sd`, `mean_ci`,
#'   `median`, `median_ci`, `range`. By default,  `n`, `mean_sd`, `median`, `range` are selected.
#' @param drop_zero_levels (`logical`) used to remove rows with zero counts from the result table.
#' @inheritParams module_arguments
#' @inheritParams template_summary_by
#'
#' @export
#' @examples
#' # Preparation of the test case.
#' library(scda)
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adlb <- synthetic_cdisc_data("latest")$adlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADLB", adlb, code = 'ADLB <- synthetic_cdisc_data("latest")$adlb'),
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
#'       add_total = TRUE,
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
                            id_var = choices_selected(
                              variable_choices(dataname, subset = "USUBJID"), selected = "USUBJID", fixed = TRUE
                            ),
                            paramcd = NULL,
                            add_total = TRUE,
                            parallel_vars = FALSE,
                            row_groups = FALSE,
                            useNA = c("ifany", "no"), # nolint
                            na_level = "<Missing>",
                            numeric_stats = c("n", "mean_sd", "median", "range"),
                            denominator = choices_selected(c("n", "N", "omit"), "omit", fixed = TRUE),
                            drop_arm_levels = TRUE,
                            drop_zero_levels = TRUE,
                            pre_output = NULL,
                            post_output = NULL) {
  logger::log_info("Initializing tm_t_summary_by")
  useNA <- match.arg(useNA) # nolint
  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is.choices_selected(id_var),
    is.flag(add_total),
    is.flag(drop_zero_levels),
    is_logical_single(parallel_vars),
    is_logical_single(row_groups),
    useNA %in% c("ifany", "no"), # nolint
    is_character_single(na_level),
    is.choices_selected(denominator),
    denominator$choices %in% c("n", "N", "omit"),
    is_logical_single(drop_arm_levels),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
      ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
      )
    )

  allowed_numeric_stats <- c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range")
  if (!all(numeric_stats %in% allowed_numeric_stats)) {
    stop("numeric_stats needs to be one of ", paste(allowed_numeric_stats, collapse = ", "))
  }

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
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
        label = label,
        na_level = na_level
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
    a$id_var,
    a$paramcd,
    a$by_vars,
    a$summarize_vars
    )

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "id_var",  "paramcd", "by_vars", "summarize_vars")]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
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
          checkboxInput(ns("drop_zero_levels"), "Drop rows with 0 count", value = a$drop_zero_levels),
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
          ),
          checkboxGroupInput(
            ns("numeric_stats"),
            label = "Choose the statistics to display for numeric variables",
            choices = c(
              "n" = "n",
              "Mean (SD)" = "mean_sd",
              "Mean 95% CI" = "mean_ci",
              "Median" = "median",
              "Median 95% CI" = "median_ci",
              "25% and 75%-ile" = "quantiles",
              "Min - Max" = "range"
            ),
            selected = a$numeric_stats
          ),
          if (a$dataname == a$parentname) {
            shinyjs::hidden(
              checkboxInput(
                ns("drop_arm_levels"),
                label = "it's a BUG if you see this",
                value = TRUE
              )
            )
          } else {
            checkboxInput(
              ns("drop_arm_levels"),
              label = sprintf("Drop columns not in filtered %s", a$dataname),
              value = a$drop_arm_levels
            )
          }
        )
      ),
      panel_group(
        panel_item(
          "Additional Variables Info",
          data_extract_input(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
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
                           id_var,
                           paramcd,
                           by_vars,
                           summarize_vars,
                           add_total,
                           na_level,
                           drop_arm_levels,
                           drop_zero_levels,
                           label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  vars <- list(arm_var = arm_var, id_var = id_var, by_vars = by_vars, summarize_vars = summarize_vars)
  if (!is.null(paramcd)) {
    vars[["paramcd"]] <- paramcd
  }

  anl_selectors <- data_extract_multiple_srv(
    vars,
    datasets = datasets
  )

  anl_merged <- data_merge_srv(
    selector_list = anl_selectors,
    datasets = datasets,
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var = arm_var),
    anl_name = "ANL_ADSL"
  )

  # Prepare the analysis environment (filter data, check data, populate envir).
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_by_vars <- anl_selectors$by_vars()$select_ordered
    input_summarize_vars <- anl_selectors$summarize_vars()$select_ordered
    input_paramcd <- if_not_null(paramcd, unlist(paramcd$filter)["vars_selected"])

    # validate inputs
    validate(
      need(input_arm_var, "Please select a treatment variable."),
      need(input_id_var, "Please select a subject identifier."),
      need(input_summarize_vars, "Please select a summarize variable."),
      if (!all(input_summarize_vars %in% names(adsl_filtered))) {
        need(input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]],
        "`Select Endpoint` is not selected.")
        },
      need(!is.null(input$numeric_stats), "Please select at least one statistic to display.")
    )
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_by_vars, input_summarize_vars, input_id_var),
      arm_var = input_arm_var
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

    my_calls <- template_summary_by(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      sum_vars = anl_selectors$summarize_vars()$select_ordered,
      by_vars = anl_selectors$by_vars()$select_ordered,
      var_labels = get_var_labels(datasets, dataname, anl_selectors$summarize_vars()$select_ordered),
      id_var = as.vector(anl_m$columns_source$id_var),
      na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE), #nolint
      na_level = na_level,
      numeric_stats = input$numeric_stats,
      denominator = input$denominator,
      add_total = input$add_total,
      parallel_vars = input$parallel_vars,
      row_groups = input$row_groups,
      drop_arm_levels = input$drop_arm_levels,
      drop_zero_levels = input$drop_zero_levels
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("result")
  })

  callModule(
    table_with_settings_srv,
    id = "table",
    table_r = table
  )

  # Render R code.
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm_var, id_var, paramcd, by_vars, summarize_vars)
      ),
    modal_title = "Summary by Row Groups Table",
    code_header = label
  )
}
