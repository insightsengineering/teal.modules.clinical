#' Template: Events by Term
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
#' @seealso [tm_t_events()]
#'
template_events <- function(dataname,
                            parentname,
                            arm_var,
                            hlt,
                            llt,
                            add_total = TRUE,
                            event_type = "event",
                            sort_criteria = c("freq_desc", "alpha"),
                            prune_freq = 0,
                            prune_diff = 0) {
  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(hlt) || is.null(hlt),
    is.string(llt) || is.null(llt),
    is.character(c(llt, hlt)),
    is.flag(add_total),
    is.string(event_type),
    is_numeric_single(prune_freq),
    is_numeric_single(prune_diff)
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

  if (sort_criteria == "alpha") {

    if (!is.null(hlt)) {
      data_list <- add_expr(
        data_list,
        substitute_names(
          expr = anl <- anl %>% dplyr::mutate(a = as.character(a)),
          names = list(a = as.name(hlt))
        )
      )
    }

    if (!is.null(llt)) {
      data_list <- add_expr(
        data_list,
        substitute_names(
          expr = anl <- anl %>% dplyr::mutate(a = as.character(a)),
          names = list(a = as.name(llt))
        )
      )
    }

  }

  term_vars <- c(hlt, llt)

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- anl %>%
        df_explicit_na(omit_columns = setdiff(names(anl), term_vars)),
      env = list(
        term_vars = term_vars
      )
    )
  )
  y$data <- bracket_expr(data_list)

  # Start layout steps.
  layout_list <- list()
  layout_list <- add_expr(layout_list, quote(basic_table()))
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(var = arm_var) %>%
        add_colcounts(),
      env = list(arm_var = arm_var)
    )
  )

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      quote(
        add_overall_col(label = "All Patients")
      )
    )
  }

  unique_label <- paste0("Total number of patients with at least one ", event_type)
  nonunique_label <- paste0("Overall total number of ", event_type, "s")

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_num_patients(
        var = "USUBJID",
        .stats = c("unique", "nonunique"),
        .labels = c(
          unique = unique_label,
          nonunique = nonunique_label
        )),
      env = list(unique_label = unique_label, nonunique_label = nonunique_label)
    )
  )

  one_term <- is.null(hlt) || is.null(llt)

  if (one_term) {
    term_var <- ifelse(is.null(hlt), llt, hlt)

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = count_occurrences(vars = term_var, .indent_mods = -1L) %>%
          append_varlabels(dataname, term_var),
        env = list(
          term_var = term_var,
          dataname = as.name(dataname)
        )
      )
    )
  } else {
    # Case when both hlt and llt are used.

    y$layout_prep <- quote(split_fun <- drop_split_levels)

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr =
          split_rows_by(
            hlt,
            child_labels = "visible",
            nested = FALSE,
            indent_mod = -1L,
            split_fun = split_fun
          ) %>%
          append_varlabels(dataname, hlt) %>%
          summarize_num_patients(
            var = "USUBJID",
            .stats = c("unique", "nonunique"),
            .labels = c(
              unique = unique_label,
              nonunique = nonunique_label
            )) %>%
          count_occurrences(vars = llt, .indent_mods = -1L) %>%
          append_varlabels(dataname, llt, indent = TRUE),
        env = list(
          dataname = as.name(dataname),
          hlt = hlt,
          llt = llt,
          unique_label = unique_label,
          nonunique_label = nonunique_label
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent),
    env = list(parent = as.name(parentname))
  )

  # Start pruning table.
  prune_list <- list()
  prune_list <- add_expr(
    prune_list,
    quote(
      pruned_result <- result %>% prune_table()
    )
  )

  if (prune_freq > 0 || prune_diff > 0) {

    # Do not use "All Patients" column for pruning conditions.
    prune_list <- add_expr(
      prune_list,
      substitute(
        expr = col_indices <- seq_along(table(parent$arm_var)),
        env = list(
          parent = as.name(parentname),
          arm_var = as.name(arm_var)
        )
      )
    )

    if (prune_freq > 0 && prune_diff == 0) {

      prune_list <- add_expr(
        prune_list,
        substitute(
          expr = row_condition <- has_fraction_in_any_col(atleast = prune_freq, col_indices = col_indices),
          env = list(prune_freq = prune_freq)
        )
      )

    } else if (prune_freq == 0 && prune_diff > 0) {

      prune_list <- add_expr(
        prune_list,
        substitute(
          expr = row_condition <- has_fractions_difference(atleast = prune_diff, col_indices = col_indices),
          env = list(prune_diff = prune_diff)
        )
      )

    } else if (prune_freq > 0 && prune_diff > 0) {

      prune_list <- add_expr(
        prune_list,
        substitute(
          expr = row_condition <- has_fraction_in_any_col(atleast = prune_freq, col_indices = col_indices) &
            has_fractions_difference(atleast = prune_diff, col_indices = col_indices),
          env = list(prune_freq = prune_freq, prune_diff = prune_diff)
        )
      )
    }

    # Apply pruning conditions.
    prune_list <- add_expr(
      prune_list,
      substitute(
        expr = pruned_result <- pruned_result %>% prune_table(keep_rows(row_condition))
      )
    )
  }

  y$prune <- bracket_expr(prune_list)

  # Start sorting pruned table.
  sort_list <- list()

  if (sort_criteria == "alpha") {

    if (prune_freq == 0 && prune_diff == 0) {

      # This is just a dummy step to get the right variable result.
      # No additional sorting is needed because during the data pre-processing step,
      # llt and/or hlt are converted to factors with alphabetically sorted levels.
      # So the order in y$table table is already alphabetically sorted.
      sort_list <- add_expr(
        sort_list,
        quote({
          pruned_and_sorted_result <- pruned_result
          pruned_and_sorted_result
        })
      )

    } else {

      sort_list <- add_expr(
        sort_list,
        quote(
          criteria_fun <- function(tr) {
            is(tr, "ContentRow")
          }
        )
      )

      sort_list <- add_expr(
        sort_list,
        quote(
          pruned_and_sorted_result <- trim_rows(pruned_result, criteria = criteria_fun)
        )
      )
    }

  } else {
    # Sort by decreasing frequency.

    # When the "All Patients" column is present we only use that for scoring.
    scorefun_hlt <- if (add_total) {
      substitute(
        expr = cont_n_onecol(length(levels(parent$arm_var)) + 1),
        env = list(
          parent = as.name(parentname),
          arm_var = as.name(arm_var)
        )
      )
    } else {
      quote(cont_n_allcols)
    }
    scorefun_llt <- if (add_total) {
      substitute(
        expr = score_occurrences_cols(col_indices = length(levels(parent$arm_var)) + 1),
        env = list(
          parent = as.name(parentname),
          arm_var = as.name(arm_var)
        )
      )
    } else {
      quote(score_occurrences)
    }

    if (one_term) {
      term_var <- ifelse(is.null(hlt), llt, hlt)

      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = {
            pruned_and_sorted_result <- pruned_result %>%
              sort_at_path(path =  c(term_var), scorefun = scorefun_llt)
            pruned_and_sorted_result
          },
          env = list(
            term_var = term_var,
            scorefun_llt = scorefun_llt
          )
        )
      )

    } else {
      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = {
            pruned_and_sorted_result <- pruned_result %>%
              sort_at_path(path = c(hlt), scorefun = scorefun_hlt) %>%
              sort_at_path(path = c(hlt, "*", llt), scorefun = scorefun_llt)
          },
          env = list(
            llt = llt,
            hlt = hlt,
            scorefun_hlt = scorefun_hlt,
            scorefun_llt = scorefun_llt
          )
        )
      )

      if (prune_freq > 0 || prune_diff > 0) {

        sort_list <- add_expr(
          sort_list,
          quote(
            criteria_fun <- function(tr) {
              is(tr, "ContentRow")
            }
          )
        )

        sort_list <- add_expr(
          sort_list,
          quote(
            pruned_and_sorted_result <- trim_rows(pruned_and_sorted_result, criteria = criteria_fun)
          )
        )
      }

      sort_list <- add_expr(
        sort_list,
        quote(pruned_and_sorted_result)
      )
    }

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
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE)
#' adae <- radae(cached = TRUE)
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADAE", adae, code = "ADAE <- radae(cached = TRUE)")
#'   ),
#'   modules = root_modules(
#'     tm_t_events(
#'       label = "Adverse Event Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       llt = choices_selected(
#'         choices = variable_choices(adae, c("AETERM", "AEDECOD")),
#'         selected = c("AEDECOD")
#'        ),
#'       hlt = choices_selected(
#'         choices = variable_choices(adae, c("AEBODSYS", "AESOC")),
#'         selected = "AEBODSYS"
#'        ),
#'       add_total = TRUE,
#'       event_type = "adverse event"
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_events <- function(label,
                        dataname,
                        parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                        arm_var,
                        hlt,
                        llt,
                        add_total = TRUE,
                        event_type = "event",
                        sort_criteria = c("freq_desc", "alpha"),
                        prune_freq = 0,
                        prune_diff = 0,
                        pre_output = NULL,
                        post_output = NULL) {

  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_logical_single(add_total),
    is_character_single(event_type),
    is_numeric_single(prune_freq),
    is_numeric_single(prune_diff),
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
    hlt = cs_to_des_select(hlt, dataname = dataname),
    llt = cs_to_des_select(llt, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_t_events_byterm,
    server = srv_t_events_byterm,
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
ui_t_events_byterm <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(a$arm_var, a$hlt, a$llt)

  standard_layout(
    output = white_small_well(
      uiOutput(ns("table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "hlt", "llt")]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("hlt"),
        label = "Event High Level Term",
        data_extract_spec = a$hlt,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("llt"),
        label = "Event Low Level Term",
        data_extract_spec = a$llt,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total),
      panel_item(
        "Additional table settings",
        selectInput(
          inputId = ns("sort_criteria"),
          label = "Sort Criteria",
          choices = c("Decreasing frequency" = "freq_desc",
                      "Alphabetically" = "alpha"
          ),
          selected = a$sort_criteria,
          multiple = FALSE
        ),
        helpText("Pruning Options"),
        numericInput(
          inputId = ns("prune_freq"),
          label = "Incidence Rate (%)",
          value = a$prune_freq,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        ),
        numericInput(
          inputId = ns("prune_diff"),
          label = "Difference Rate (%)",
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
srv_t_events_byterm <- function(input,
                                output,
                                session,
                                datasets,
                                dataname,
                                parentname,
                                event_type,
                                arm_var,
                                hlt,
                                llt,
                                label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, hlt, llt),
    input_id = c("arm_var", "hlt", "llt"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_level_term <- c(
      as.vector(anl_m$columns_source$hlt),
      as.vector(anl_m$columns_source$llt)
    )

    validate(need(input_arm_var, "Please select a treatment variable"))
    teal.devel::validate_has_elements(
      input_level_term,
      "Please select at least one of \"LOW LEVEL TERM\" or \"HIGH LEVEL TERM\" variables."
    )
    validate(
      need(is.factor(adsl_filtered[[input_arm_var]]), "Treatment variable is not a factor.")
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

    input_hlt <- as.vector(anl_m$columns_source$hlt)
    input_llt <- as.vector(anl_m$columns_source$llt)

    my_calls <- template_events(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      hlt = if (length(input_hlt) != 0) input_hlt else NULL,
      llt = if (length(input_llt) != 0) input_llt else NULL,
      add_total = input$add_total,
      event_type = event_type,
      sort_criteria = input$sort_criteria,
      prune_freq = input$prune_freq / 100,
      prune_diff = input$prune_diff / 100
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  output$table <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("pruned_and_sorted_result"))
  })

  # Render R code.
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(arm_var, hlt, llt)),
    modal_title = "Event Table",
    code_header = label
  )
}
