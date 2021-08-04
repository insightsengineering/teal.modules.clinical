#' Template: Adverse Events grouped by Grade with threshold
#'
#' @inheritParams template_arguments
#' @param event_type (`character`)\cr type of event that is summarized (e.g. adverse event, treatment).
#'   Default is "event".
#' @param sort_criteria (`character`)\cr how to sort the final table. Default option `freq_desc` sorts
#'   by decreasing total number of patients with event. Alternative option `alpha` sorts events
#'   alphabetically.
#' @param prune_freq (`number`)\cr threshold to use for trimming table using event incidence rate in any column.
#' @param prune_diff (`number`)\cr threshold to use for trimming table using as criteria difference in
#'   rates between any two columns.
#'
#' @seealso [tm_t_events_col_by_grade()]
#'
template_events_col_by_grade <- function(dataname,
                                         parentname,
                                         arm_var,
                                         grading_groups = list(
                                           "Any Grade (%)" = c("1", "2", "3", "4", "5"),
                                           "Grade 3-4 (%)" = c("3", "4"),
                                           "Grade 5 (%)" = "5"
                                         ),
                                         add_total = FALSE,
                                         ae_soc,
                                         ae_term,
                                         ae_grade = "AETOXGR",
                                         event_type = "event",
                                         sort_criteria = c("freq_desc"),
                                         prune_freq = 0.10,
                                         prune_diff = 0,
                                         drop_arm_levels = FALSE
                                         ) {
  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.list(grading_groups),
    is.flag(add_total),
    is.string(ae_soc) || is.null(ae_soc),
    is.string(ae_term),
    is.string(ae_grade),
    is.string(event_type),
    is_numeric_single(prune_freq),
    is_numeric_single(prune_diff),
    is.flag(drop_arm_levels)
  )

  sort_criteria <- match.arg(sort_criteria)

  y <- list()

  # Start data steps.
  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df,
      env = list(df = as.name(dataname))
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
      col_counts <- rep(table(parentname[[arm_var]]), each = length(grading_groups)),
      env = list(parentname = as.name(parentname), grading_groups = grading_groups, arm_var = arm_var)
    )
  )

  data_pipe <- list()
  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = anl <- anl %>% group_by("USUBJID", arm_var, ae_soc, ae_term),
      env = list(arm_var = arm_var, ae_soc = ae_soc, ae_term = ae_term)
    )
  )
  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = summarize(MAXAETOXGR = factor(max(as.numeric(ae_grade))), .group = "drop"),
      env = list(ae_grade = ae_grade)
    )
  )
  data_pipe <- add_expr(
    data_pipe,
    quote(ungroup())
  )
  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = mutate(AEDECOD = droplevels(as.factor(ae_term))),
      env = list(ae_term = ae_term)
    )
  )
  data_pipe <- add_expr(
    data_pipe,
    quote(df_explicit_na())
  )
  data_pipe <- pipe_expr(data_pipe)
  data_list <- add_expr(
    data_list,
    data_pipe
  )
  y$data <- bracket_expr(data_list)

  # Start layout steps.
  layout_list <- list()
  layout_list <- add_expr(layout_list, quote(basic_table()))
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(var = arm_var),
      env = list(arm_var = arm_var)
    )
  )
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by_groups("MAXAETOXGR", groups = grading_groups),
      env = list(grading_groups = grading_groups)
    )
  )

  # if (add_total) {
  #   layout_list <- add_expr(
  #     layout_list,
  #     quote(
  #       add_overall_col(label = "All Patients")
  #     )
  #   )
  # }

  # for variant 8 in STREAM manual
  if(!is.null(ae_soc)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = split_rows_by(ae_soc, child_labels = "visible", nested = FALSE),
        env = list(ae_soc = ae_soc)
      )
    )

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = append_varlabels(df = anl, vars = ae_soc),
        env = list(ae_soc = ae_soc)
      )
    )

    unique_label <- paste0("Total number of patients with at least one ", event_type)
    layout_list <- add_expr(
      layout_list,
      substitute(
        summarize_num_patients(
          var = "USUBJID",
          .stats = "unique",
          .labels = unique_label,
        ),
        env = list(unique_label = unique_label)
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_vars(
        ae_term,
        na.rm = FALSE,
        denom = "N_col",
        .stats = "count_fraction",
        .formats = c(count_fraction = format_fraction_threshold(0.01))
      ),
      env = list(ae_term = ae_term)
    )
  )

  if (is.null(ae_soc)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = append_varlabels(df = anl, vars = ae_term),
        env = list(ae_term = ae_term)
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = append_varlabels(df = anl, vars = ae_term, indent = 1L),
        env = list(ae_term = ae_term)
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- quote(result <- build_table(lyt = lyt, df = anl, alt_counts_df = col_counts))

  # Start pruning table.
  prune_list <- list()
  prune_list <- add_expr(
    prune_list,
    quote(
      criteria_fun <- function(tr) {
        is(tr, "ContentRow")
      }
    )
  )
  prune_list <- add_expr(
    prune_list,
    substitute(
      expr = lengths <- lapply(grading_groups, length),
      env = list(grading_groups = grading_groups)
    )
  )
  prune_list <- add_expr(
    prune_list,
    quote(start_index <- unname(which.max(lengths)))
  )
  prune_list <- add_expr(
    prune_list,
    quote(col_indices <- seq(start_index, ncol(result), by = length(grading_groups)))
  )

  prune_list <- add_expr(
    prune_list,
    substitute(
      expr = at_least_percent_any <- has_fraction_in_any_col(atleast = prune_freq, col_indices = col_indices),
      env = list(prune_freq = prune_freq)
    )
  )

  if (prune_diff > 0) {
    prune_list <- add_expr(
      prune_list,
      substitute(
        expr = at_least_percent_diff <- has_fractions_difference(atleast = prune_diff, col_indices = col_indices),
        env = list(prune_diff = prune_diff)
      )
    )
  }

  prune_pipe <- list()
  prune_pipe <- add_expr(
    prune_pipe,
    quote(
      pruned_result <- result %>% trim_rows(criteria = criteria_fun)
    )
  )

  if (prune_freq > 0 & prune_diff > 0) {
    prune_pipe <- add_expr(
      prune_pipe,
      quote(prune_table(keep_rows(at_least_percent_any & at_least_percent_diff)))
    )
  } else if (prune_freq > 0 & prune_diff == 0) {
    prune_pipe <- add_expr(
      prune_pipe,
      quote(prune_table(keep_rows(at_least_percent_any)))
    )
  } else {
    prune_pipe <- add_expr(
      prune_pipe,
      quote(prune_table())
    )
  }
  prune_pipe <- pipe_expr(prune_pipe)
  prune_list <- add_expr(
    prune_list,
    prune_pipe
  )

  y$prune <- bracket_expr(prune_list)

  # Start sorting pruned table.
  sort_list <- list()

  if (!is.null(ae_soc)) {
    sort_list <- add_expr(
      sort_list,
      quote(scorefun_soc <- score_occurrences_cont_cols(col_indices = col_indices))
    )
  }

  sort_list <- add_expr(
    sort_list,
    quote(scorefun_term <- score_occurrences_cols(col_indices = col_indices))
  )

  if (is.null(ae_soc)) {
      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = {
            pruned_and_sorted_result <- pruned_result %>%
              sort_at_path(path = c(ae_term), scorefun = scorefun_term, decreasing = TRUE)
            pruned_and_sorted_result
          },
          env = list(ae_term = ae_term)
        )
      )
  } else {
      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = {
            pruned_and_sorted_result <- pruned_result %>%
              sort_at_path(path = c(ae_soc), scorefun = scorefun_soc, decreasing = TRUE) %>%
              sort_at_path(path = c(ae_soc, "*", ae_term), scorefun = scorefun_term, decreasing = TRUE)
            pruned_and_sorted_result
          },
          env = list(
            ae_soc = ae_soc,
            ae_term = ae_term
          )
        )
      )
    }

  y$sort <- bracket_expr(sort_list)

  y
}

#' Teal Module: Events by Term
#'
#' @inheritParams module_arguments
#' @inheritParams template_events
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(scda)
#'
#' adsl <- haven::read_sas("./R/adsl.sas7bdat") %>% df_explicit_na()
#' adae <- haven::read_sas("./R/adae.sas7bdat", encoding = "latin1") %>% df_explicit_na()
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'ADSL <- haven::read_sas("./R/adsl.sas7bdat") %>% df_explicit_na()'),
#'     cdisc_dataset("ADAE", adae, code = 'ADAE <- haven::read_sas("./R/adae.sas7bdat", encoding = "latin1") %>% df_explicit_na()')
#'   ),
#'   modules = root_modules(
#'     tm_t_events_col_by_grade(
#'       label = "Adverse Event by Grade Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       ae_term = choices_selected(
#'         choices = variable_choices(adae, c("AETERM", "AEDECOD")),
#'         selected = c("AEDECOD")
#'        ),
#'       ae_soc = choices_selected(
#'         choices = variable_choices(adae, c("AEBODSYS", "AESOC")),
#'         selected = "AEBODSYS"
#'        ),
#'       event_type = "adverse event"
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_events_col_by_grade <- function(label,
                                     dataname,
                                     parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                                     arm_var,
                                     ae_soc,
                                     ae_term,
                                     # ae_grade,
                                     grading_groups = list(
                                       "Any Grade (%)" = c("1", "2", "3", "4", "5"),
                                       "Grade 3-4 (%)" = c("3", "4"),
                                       "Grade 5 (%)" = "5"
                                     ),
                                     add_total = FALSE,
                                     event_type = "event",
                                     sort_criteria = c("freq_desc"),
                                     prune_freq = 0.10,
                                     prune_diff = 0,
                                     drop_arm_levels = FALSE,
                                     pre_output = NULL,
                                     post_output = NULL) {

  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_logical_single(add_total),
    is_character_single(event_type),
    is_numeric_single(prune_freq),
    is_numeric_single(prune_diff),
    is.flag(drop_arm_levels),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  sort_criteria <- match.arg(sort_criteria)

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    ae_soc = cs_to_des_select(ae_soc, dataname = dataname),
    ae_term = cs_to_des_select(ae_term, dataname = dataname)
    # ae_grade = cs_to_des_select(ae_grade, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_t_events_col_by_grade,
    server = srv_t_events_col_by_grade,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        event_type = event_type,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_events_col_by_grade <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(a$arm_var, a$ae_soc, a$ae_term)

  standard_layout(
    output = white_small_well(
      table_with_settings_ui(ns("table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "ae_soc", "ae_term")]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("ae_soc"),
        label = "Body System or Organ Class",
        data_extract_spec = a$ae_soc,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("ae_term"),
        label = "Adverse Event MedDRA term",
        data_extract_spec = a$ae_term,
        is_single_dataset = is_single_dataset_value
      ),
      # data_extract_input(
      #   id = ns("ae_grade"),
      #   label = "AE Toxicity Grade",
      #   data_extract_spec = a$ae_grade,
      #   is_single_dataset = is_single_dataset_value
      # ),
      # checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total),
      panel_item(
        "Additional table settings",
        # checkboxInput(
        #   ns("drop_arm_levels"),
        #   label = "Drop columns not in filtered analysis dataset",
        #   value = a$drop_arm_levels
        # ),
        # selectInput(
        #   inputId = ns("sort_criteria"),
        #   label = "Sort Criteria",
        #   choices = c("Decreasing frequency" = "freq_desc",
        #               "Alphabetically" = "alpha"
        #   ),
        #   selected = a$sort_criteria,
        #   multiple = FALSE
        # ),
        helpText("Pruning Options"),
        numericInput(
          inputId = ns("prune_freq"),
          label = "Minimum Incidence Rate(%) in any of the treatment groups",
          value = a$prune_freq,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        ),
        numericInput(
          inputId = ns("prune_diff"),
          label = "Minimum Difference Rate(%) between any of the treatment groups",
          value = a$prune_diff,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_events_col_by_grade <- function(input,
                                output,
                                session,
                                datasets,
                                dataname,
                                parentname,
                                event_type,
                                arm_var,
                                ae_soc,
                                ae_term,
                                # ae_grade,
                                # drop_arm_levels,
                                label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, ae_soc, ae_term),
    input_id = c("arm_var", "ae_soc", "ae_term"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  arm_var_user_input <- get_input_order("arm_var", arm_var$dataname)

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- arm_var_user_input()
    input_level_term <- c(
      as.vector(anl_m$columns_source$ae_soc),
      as.vector(anl_m$columns_source$ae_term)
      # as.vector(anl_m$columns_source$ae_grade)
    )

    validate(
      need(input_arm_var, "Please select a treatment variable"),
      need(is.factor(adsl_filtered[[input_arm_var]]), "Treatment variable is not a factor.")
    )
    teal.devel::validate_has_elements(
      input_level_term,
      "Please select at least one of \"Body System or Organ Class\" or \"Adverse Event Code\" variables."
    )
    validate(
      need(
        input$prune_freq >= 0 && input$prune_freq <= 100,
        "Please provide an Incidence Rate between 0 and 100 (%)."
      ),
      need(
        input$prune_diff >= 0 && input$prune_diff <= 100,
        "Please provide a Difference Rate between 0 and 100 (%)."
      )
    )

    # validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_level_term),
      arm_var = input_arm_var
    )
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

    input_ae_soc <- as.vector(anl_m$columns_source$ae_soc)
    input_ae_term <- as.vector(anl_m$columns_source$ae_term)
    # input_ae_grade <- as.vector(anl_m$columns_source$ae_grade)

    my_calls <- template_events_col_by_grade(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = arm_var_user_input(),
      ae_soc = if (length(input_ae_soc) != 0) input_ae_soc else NULL,
      ae_term = if (length(input_ae_term) != 0) input_ae_term else NULL,
      # ae_grade = if (length(input_ae_grade) != 0) input_ae_grade else NULL,
      # add_total = input$add_total,
      event_type = event_type,
      sort_criteria = input$sort_criteria,
      prune_freq = input$prune_freq / 100,
      prune_diff = input$prune_diff / 100
      # drop_arm_levels = input$drop_arm_levels
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("pruned_and_sorted_result")
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
    datanames = get_extract_datanames(list(arm_var, ae_soc, ae_term)),
    modal_title = "Event Table",
    code_header = label
  )
}

