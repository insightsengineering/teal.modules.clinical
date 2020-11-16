#' Teal Module: Events by Term
#'
#' @name events_by_term
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#'
NULL

#' @describeIn events_by_term create the expression corresponding to the analysis.
#'
#' @param event_type (`string`)\cr type of event that is summarized (e.g. adverse event, treatment).
#'   Default is "event".
#' @param sort_criteria (`string`)\cr how to sort the final table. Default option `freq_desc` sorts
#'   by decreasing total number of patients with event. Alternative option `alpha` sorts events
#'   alphabetically.
#' @param prune_freq (`number`)\cr threshold to use for trimming table using event incidence rate in any column.
#' @param prune_diff (`number`)\cr threshold to use for trimming table using as criteria difference in
#'   rates between any two columns.
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
      expr = col_n <- table(parentname$arm_var),
      env = list(parentname = as.name(parentname), arm_var = arm_var)
    )
  )
  if (add_total) {
    data_list <- add_expr(
      data_list,
      quote(col_n <- c(col_n, "All Patients" = sum(col_n)))
    )
  }

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
        expr = count_occurrences(vars = term_var, .indent_mods = -1L),
        env = list(term_var = term_var)
      )
    )
  } else {
    # Case when both hlt and llt are used.

    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by(hlt, child_labels = "visible", nested = FALSE, indent_mod = -1L) %>%

          summarize_num_patients(
            var = "USUBJID",
            .stats = c("unique", "nonunique"),
            .labels = c(
              unique = unique_label,
              nonunique = nonunique_label
            )) %>%
          count_occurrences(vars = llt, .indent_mods = -1L),
        env = list(hlt = hlt, llt = llt, unique_label = unique_label, nonunique_label = nonunique_label)
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- quote(
    result <- build_table(lyt = lyt, df = anl, col_counts = col_n)
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
    if (add_total) {
      prune_list <- add_expr(
        prune_list,
        quote(
          expr = col_indices <- seq_along(1:(length(col_n) - 1))
        )
      )
    } else {
      prune_list <- add_expr(
        prune_list,
        quote(
          expr = col_indices <- seq_along(1:length(col_n))
        )
      )
    }

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
        quote(
          pruned_and_sorted_result <- pruned_result
        )
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
      quote(cont_n_onecol(length(col_n)))
    } else {
      quote(cont_n_allcols)
    }
    scorefun_llt <- if (add_total) {
      quote(score_occurrences_cols(col_indices = length(col_n)))
    } else {
      quote(score_occurrences)
    }

    if (one_term) {
      term_var <- ifelse(is.null(hlt), llt, hlt)

      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = pruned_and_sorted_result <- pruned_result %>%
            sort_at_path(path =  c(term_var), scorefun = scorefun_llt),
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
          expr = pruned_and_sorted_result <- pruned_result %>%
            sort_at_path(path =  c(hlt), scorefun = scorefun_hlt) %>%
            sort_at_path(path =  c(hlt, "*", llt), scorefun = scorefun_llt),
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

    }

  }
  y$sort <- bracket_expr(sort_list)

  y

}

#' @noRd
ui_t_events_byterm <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        a$arm_var$choices,
        a$arm_var$selected,
        multiple = FALSE,
        fixed = a$arm_var$fixed),
      optionalSelectInput(
        ns("hlt"),
        "Event High Level Term",
        a$hlt$choices,
        a$hlt$selected,
        multiple = FALSE,
        fixed = a$hlt$fixed),
      optionalSelectInput(
        ns("llt"),
        "Event Low Level Term",
        a$llt$choices,
        a$llt$selected,
        multiple = FALSE,
        fixed = a$llt$fixed),
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
          value = 0,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        ),
        numericInput(
          inputId = ns("prune_diff"),
          label = "Difference Rate (%)",
          value = 0,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        )
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
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
                                event_type) {
  init_chunks()

  # Prepare the analysis environment (filter data, check data, populate envir).
  prepared_env <- reactive({
    adsl_f <- datasets$get_data("ADSL", filtered = TRUE)
    anl_f <- datasets$get_data(dataname, filtered = TRUE)

    arm_var <- input$arm_var
    hlt <- input$hlt
    llt <- input$llt

    validate_standard_inputs(
      adsl = adsl_f,
      adslvars = c("STUDYID", "USUBJID", arm_var),
      anl = anl_f,
      anlvars = c("STUDYID", "USUBJID", llt, hlt),
      arm_var = arm_var,
      max_n_levels_armvar = NULL,
      min_nrow = 1
    )
    teal.devel::validate_has_elements(
      c(llt, hlt),
      "Please select at least one of \"LOW LEVEL TERM\" or \"HIGH LEVEL TERM\" variables."
    )
    validate(need(is.factor(adsl_f[[arm_var]]), "Arm variable is not a factor."))
    validate(need(
      input$prune_freq >= 0 && input$prune_freq <= 100,
      "Pruning threshold for incidence rate must be between 0 and 100.")
    )
    validate(need(
      input$prune_diff >= 0 && input$prune_diff <= 100,
      "Pruning threshold for difference rate must be between 0 and 100.")
    )

    # Send data where the analysis lives.
    e <- new.env()
    anl_name <- paste0(dataname, "_FILTERED")
    e[[anl_name]] <- anl_f
    e$ADSL_FILTERED <- adsl_f # nolint
    e
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_events(
      dataname = paste0(dataname, "_FILTERED"),
      parentname = "ADSL_FILTERED",
      arm_var = input$arm_var,
      hlt = input$hlt,
      llt = input$llt,
      add_total = input$add_total,
      event_type = event_type,
      sort_criteria = input$sort_criteria,
      prune_freq <- input$prune_freq / 100,
      prune_diff <- input$prune_diff / 100
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
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Summary",
      rcode = get_rcode(
        datasets = datasets,
        datanames = dataname,
        title = "Event Table"
      )
    )
  })
}


#' @describeIn events_by_term teal module for events by term.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE)
#' adae <- radae(cached = TRUE)
#'
#' # Process `adcm` to keep only one path per treatment.
#' adcm <- radcm(cached = TRUE) %>%
#'  group_by(STUDYID, USUBJID, CMDECOD) %>%
#'  slice(1) %>%
#'  ungroup
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADAE", adae, code = "ADAE <- radae(cached = TRUE)"),
#'     cdisc_dataset("ADCM", adcm, code = "ADCM <- radcm(cached = TRUE) %>%
#'         group_by(STUDYID, USUBJID, CMDECOD) %>%
#'         slice(1) %>%
#'         ungroup"
#'     )
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
#'     ),
#'     tm_t_events(
#'       label = "Concomitant Medication Table",
#'       dataname = "ADCM",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       llt = choices_selected(
#'         choices = variable_choices(adcm, "CMDECOD"),
#'         selected = "CMDECOD"
#'        ),
#'       hlt = choices_selected(
#'         choices = variable_choices(adcm, "CMCLAS"),
#'         selected = NULL
#'        ),
#'       add_total = TRUE,
#'       event_type = "treatment"
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_events <- function(label,
                        dataname,
                        arm_var,
                        hlt,
                        llt,
                        add_total = TRUE,
                        event_type = "event",
                        sort_criteria = c("freq_desc", "alpha"),
                        prune_freq = 0,
                        prune_diff = 0) {

  stop_if_not(
    list(is_character_single(label), "Label should be single (i.e. not vector) character type of object")
  )
  stop_if_not(
    list(is_character_single(dataname), "Dataname should be single (i.e. not vector) character type of object")
  )
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(hlt))
  stopifnot(is.choices_selected(llt))
  stopifnot(is_logical_single(add_total))
  stop_if_not(is_character_single(event_type))
  sort_criteria <- match.arg(sort_criteria)
  stop_if_not(is_numeric_single(prune_freq))
  stop_if_not(is_numeric_single(prune_diff))

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_events_byterm,
    server = srv_t_events_byterm,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      event_type = event_type
    ),
    filters = dataname
  )
}
