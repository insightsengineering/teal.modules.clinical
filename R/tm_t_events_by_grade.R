#' Template: Events by Grade
#'
#' @inheritParams template_arguments
#' @param label_hlt (`string`)\cr label of the `hlt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param label_llt (`string`)\cr label of the `llt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param id (`character`) \cr unique identifier of patients in datasets, default to `"USUBJID"`.
#' @param grade (`character`) \cr name of the severity level variable.
#' @param label_grade (`string`)\cr label of the `grade` variable from `dataname`. The label will be extracted from the
#' module.
#'
#' @seealso [tm_t_events_by_grade()]
#' @keywords internal
#'
template_events_by_grade <- function(dataname,
                                     parentname,
                                     arm_var,
                                     id = "",
                                     hlt,
                                     llt,
                                     label_hlt = NULL,
                                     label_llt = NULL,
                                     grade,
                                     label_grade = NULL,
                                     prune_freq = 0,
                                     prune_diff = 0,
                                     add_total = TRUE,
                                     total_label = "All Patients",
                                     drop_arm_levels = TRUE,
                                     basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(hlt) || is.null(hlt),
    assertthat::is.string(llt) || is.null(llt),
    !is.null(hlt) || !is.null(llt),
    assertthat::is.string(label_hlt) || is.null(label_hlt),
    assertthat::is.string(label_llt) || is.null(label_llt),
    assertthat::is.string(grade),
    assertthat::is.string(label_grade) || is.null(label_grade),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label),
    assertthat::is.flag(drop_arm_levels)
  )
  checkmate::assert_scalar(prune_freq)
  checkmate::assert_scalar(prune_diff)

  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- dataname,
      env = list(
        dataname = as.name(dataname)
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
      dataname <- df_explicit_na(dataname),
      env = list(dataname = as.name(dataname))
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      dataname <- df_explicit_na(dataname),
      env = list(dataname = as.name("anl"))
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- df_explicit_na(parentname),
      env = list(parentname = as.name(parentname))
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = grade_groups <- list("- Any Intensity -" = levels(dataname$grade)),
      env = list(
        dataname = as.name(dataname),
        grade = grade
      )
    )
  )

  y$data <- bracket_expr(data_list)

  y$layout_prep <- quote(split_fun <- trim_levels_in_group)

  layout_list <- list()

  basic_title <- if (is.null(hlt) && !is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_llt)
  } else if (!is.null(hlt) && is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_hlt)
  } else if (!is.null(hlt) && !is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_hlt, " and ", label_llt)
  } else {
    paste0("Adverse Event summary by ", label_grade)
  }

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(title = basic_title)
    )
  )

  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_cols_by(arm_var),
      env = list(arm_var = arm_var)
    )
  )

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_overall_col(label = total_label),
        env = list(total_label = total_label)
      )
    )
  }

  one_term <- is.null(hlt) || is.null(llt)

  if (one_term) {
    term_var <- ifelse(is.null(hlt), llt, hlt)
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_colcounts() %>%
          summarize_occurrences_by_grade(
            var = grade,
            grade_groups = grade_groups
          ) %>%
          rtables::split_rows_by(
            term_var,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L,
            split_fun = split_fun(grade),
            label_pos = "topleft",
            split_label = formatters::var_labels(dataname[term_var])
          ) %>%
          summarize_num_patients(
            var = id,
            .stats = "unique",
            .labels = c("- Any Intensity -")
          ) %>%
          count_occurrences_by_grade(var = grade, .indent_mods = -1L) %>%
          append_varlabels(dataname, grade, indent = 1L),
        env = list(
          id = id,
          arm_var = arm_var,
          term_var = term_var,
          grade = grade,
          dataname = as.name(dataname)
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_colcounts() %>%
          summarize_occurrences_by_grade(
            var = grade,
            grade_groups = grade_groups
          ) %>%
          rtables::split_rows_by(
            hlt,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L,
            split_fun = split_fun(grade),
            label_pos = "topleft",
            split_label = formatters::var_labels(dataname[hlt])
          ) %>%
          summarize_occurrences_by_grade(
            var = grade,
            grade_groups = grade_groups
          ) %>%
          rtables::split_rows_by(
            llt,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L,
            split_fun = split_fun(grade),
            label_pos = "topleft",
            split_label = formatters::var_labels(dataname[llt])
          ) %>%
          summarize_num_patients(
            var = id,
            .stats = "unique",
            .labels = c("- Any Intensity -")
          ) %>%
          count_occurrences_by_grade(var = grade, .indent_mods = -1L) %>%
          append_varlabels(dataname, grade, indent = 2L),
        env = list(
          id = id,
          arm_var = arm_var,
          hlt = hlt,
          llt = llt,
          grade = grade,
          dataname = as.name(dataname)
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
    expr = result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent),
    env = list(parent = as.name(parentname))
  )

  # Start pruning table.
  prune_list <- list()
  prune_list <- add_expr(
    prune_list,
    quote(
      pruned_result <- result
    )
  )

  if (prune_freq > 0 || prune_diff > 0) {
    # Do not use "All Patients" column for pruning conditions.
    prune_list <- add_expr(
      prune_list,
      substitute(
        expr = col_indices <- 1:(ncol(result) - add_total),
        env = list(add_total = add_total)
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
        expr = pruned_result <- pruned_result %>% rtables::prune_table(keep_content_rows(row_condition))
      )
    )
  }

  y$prune <- bracket_expr(prune_list)

  # Start sort the pruned table.
  sort_list <- list()
  scorefun <- if (add_total) {
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
  if (one_term) {
    term_var <- ifelse(is.null(hlt), llt, hlt)

    sort_list <- add_expr(
      sort_list,
      substitute(
        expr = {
          pruned_and_sorted_result <- pruned_result %>%
            sort_at_path(path = term_var, scorefun = scorefun, decreasing = TRUE)
          pruned_and_sorted_result
        },
        env = list(
          term_var = term_var,
          scorefun = scorefun
        )
      )
    )
  } else {
    sort_list <- add_expr(
      sort_list,
      substitute(
        expr = {
          pruned_and_sorted_result <- pruned_result %>%
            sort_at_path(path = hlt, scorefun = scorefun, decreasing = TRUE) %>%
            sort_at_path(path = c(hlt, "*", llt), scorefun = scorefun, decreasing = TRUE)
        },
        env = list(
          llt = llt,
          hlt = hlt,
          scorefun = scorefun
        )
      )
    )

    sort_list <- add_expr(
      sort_list,
      quote(pruned_and_sorted_result)
    )
  }
  y$sort <- bracket_expr(sort_list)

  y
}

#' Template: Adverse Events grouped by Grade with threshold
#'
#' @inheritParams template_arguments
#' @param id (`character`) \cr unique identifier of patients in datasets, default to `"USUBJID"`.
#' @param label_hlt (`string`)\cr label of the `hlt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param label_llt (`string`)\cr label of the `llt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param grade (`character`) \cr grade term which grading_groups is based on, default to `"AETOXGR"`.
#' @param label_grade (`string`)\cr label of the `grade` variable from `dataname`. The label will be extracted from the
#' module.
#' @param grading_groups (`character`) \cr list of grading groups.
#'
#' @seealso [tm_t_events_by_grade()]
#' @keywords internal
#'
template_events_col_by_grade <- function(dataname,
                                         parentname,
                                         arm_var,
                                         grading_groups = list(
                                           "Any Grade (%)" = c("1", "2", "3", "4", "5"),
                                           "Grade 1-2 (%)" = c("1", "2"),
                                           "Grade 3-4 (%)" = c("3", "4"),
                                           "Grade 5 (%)" = "5"
                                         ),
                                         add_total = TRUE,
                                         total_label = "All Patients",
                                         id = "USUBJID",
                                         hlt,
                                         llt,
                                         label_hlt = NULL,
                                         label_llt = NULL,
                                         grade = "AETOXGR",
                                         label_grade = NULL,
                                         prune_freq = 0.1,
                                         prune_diff = 0,
                                         drop_arm_levels = TRUE,
                                         basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    is.list(grading_groups),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label),
    assertthat::is.string(id),
    assertthat::is.string(hlt) || is.null(hlt),
    assertthat::is.string(llt),
    assertthat::is.string(grade),
    assertthat::is.string(label_hlt) || is.null(label_hlt),
    assertthat::is.string(label_llt) || is.null(label_llt),
    assertthat::is.string(label_grade) || is.null(label_grade),
    assertthat::is.flag(drop_arm_levels)
  )
  checkmate::assert_scalar(prune_freq)
  checkmate::assert_scalar(prune_diff)


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

  ## add_total for patients grouping across all arms
  if (add_total) {
    data_list <- add_expr(
      data_list,
      substitute(
        col_counts <- rep(c(table(parentname[[arm_var]]), nrow(parentname)), each = length(grading_groups)),
        env = list(parentname = as.name(parentname), grading_groups = grading_groups, arm_var = arm_var)
      )
    )
  } else {
    data_list <- add_expr(
      data_list,
      substitute(
        col_counts <- rep(table(parentname[[arm_var]]), each = length(grading_groups)),
        env = list(parentname = as.name(parentname), grading_groups = grading_groups, arm_var = arm_var)
      )
    )
  }

  data_pipe <- list()
  if (!is.null(hlt)) {
    data_pipe <- add_expr(
      data_pipe,
      substitute(
        expr = anl <- anl %>% dplyr::group_by(id, arm_var, hlt, llt),
        env = list(id = as.name(id), arm_var = as.name(arm_var), hlt = as.name(hlt), llt = as.name(llt))
      )
    )
  } else {
    data_pipe <- add_expr(
      data_pipe,
      substitute(
        expr = anl <- anl %>% dplyr::group_by(id, arm_var, llt),
        env = list(id = as.name(id), arm_var = as.name(arm_var), llt = as.name(llt))
      )
    )
  }

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = dplyr::summarize(MAXAETOXGR = factor(max(as.numeric(grade)))),
      env = list(grade = as.name(grade))
    )
  )
  data_pipe <- add_expr(
    data_pipe,
    quote(dplyr::ungroup())
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

  layout_list <- list()
  basic_title <- if (is.null(hlt) && !is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_llt)
  } else if (!is.null(hlt) && is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_hlt)
  } else if (!is.null(hlt) && !is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_hlt, " and ", label_llt)
  } else {
    paste0("Adverse Event summary by ", label_grade)
  }


  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(title = basic_title)
    )
  )

  # Start layout steps.
  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::split_cols_by(var = arm_var, split_fun = add_overall_level(total_label, first = FALSE)),
        env = list(
          arm_var = arm_var,
          total_label = total_label
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::split_cols_by(var = arm_var),
        env = list(arm_var = arm_var)
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by_groups("MAXAETOXGR", groups = grading_groups),
      env = list(grading_groups = grading_groups)
    )
  )

  if (!is.null(hlt)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::split_rows_by(
          hlt,
          child_labels = "visible",
          nested = FALSE,
          split_fun = trim_levels_in_group(llt)
        ),
        env = list(hlt = hlt, llt = llt)
      )
    )

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = append_varlabels(df = anl, vars = hlt),
        env = list(hlt = hlt)
      )
    )

    unique_label <- paste0("Total number of patients with at least one adverse event")
    layout_list <- add_expr(
      layout_list,
      substitute(
        summarize_num_patients(
          var = id,
          .stats = "unique",
          .labels = unique_label,
        ),
        env = list(id = id, unique_label = unique_label)
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_vars(
        llt,
        na.rm = FALSE,
        denom = "N_col",
        .stats = "count_fraction",
        .formats = c(count_fraction = format_fraction_threshold(0.01))
      ),
      env = list(llt = llt)
    )
  )

  if (is.null(hlt)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = append_varlabels(df = anl, vars = llt),
        env = list(llt = llt)
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = append_varlabels(df = anl, vars = llt, indent = 1L),
        env = list(llt = llt)
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- quote(result <- rtables::build_table(lyt = lyt, df = anl, col_counts = col_counts))

  # Start sorting table.
  sort_list <- list()

  sort_list <- add_expr(
    sort_list,
    substitute(
      expr = lengths <- lapply(grading_groups, length),
      env = list(grading_groups = grading_groups)
    )
  )
  sort_list <- add_expr(
    sort_list,
    quote(start_index <- unname(which.max(lengths)))
  )
  sort_list <- add_expr(
    sort_list,
    substitute(
      expr = col_indices <- seq(start_index, ncol(result), by = length(grading_groups)),
      env = list(grading_groups = grading_groups)
    )
  )

  if (!is.null(hlt)) {
    sort_list <- add_expr(
      sort_list,
      quote(scorefun_soc <- score_occurrences_cont_cols(col_indices = col_indices))
    )
  }

  sort_list <- add_expr(
    sort_list,
    quote(scorefun_term <- score_occurrences_cols(col_indices = col_indices))
  )

  if (is.null(hlt)) {
    sort_list <- add_expr(
      sort_list,
      substitute(
        expr = {
          sorted_result <- result %>%
            sort_at_path(path = c(llt), scorefun = scorefun_term, decreasing = TRUE)
        },
        env = list(llt = llt)
      )
    )
  } else {
    sort_list <- add_expr(
      sort_list,
      substitute(
        expr = {
          sorted_result <- result %>%
            sort_at_path(path = c(hlt), scorefun = scorefun_soc, decreasing = TRUE) %>%
            sort_at_path(path = c(hlt, "*", llt), scorefun = scorefun_term, decreasing = TRUE)
        },
        env = list(
          hlt = hlt,
          llt = llt
        )
      )
    )
  }

  y$sort <- bracket_expr(sort_list)

  # Start pruning table.
  prune_list <- list()
  prune_list <- add_expr(
    prune_list,
    quote(
      criteria_fun <- function(tr) {
        inherits(tr, "ContentRow")
      }
    )
  )

  if (prune_freq > 0) {
    prune_list <- add_expr(
      prune_list,
      substitute(
        expr = at_least_percent_any <- has_fraction_in_any_col(atleast = prune_freq, col_indices = col_indices),
        env = list(prune_freq = prune_freq)
      )
    )
  }

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
      pruned_and_sorted_result <- sorted_result %>% rtables::trim_rows(criteria = criteria_fun)
    )
  )

  if (prune_freq > 0 && prune_diff > 0) {
    prune_pipe <- add_expr(
      prune_pipe,
      quote(rtables::prune_table(keep_rows(at_least_percent_any & at_least_percent_diff)))
    )
  } else if (prune_freq > 0 && prune_diff == 0) {
    prune_pipe <- add_expr(
      prune_pipe,
      quote(rtables::prune_table(keep_rows(at_least_percent_any)))
    )
  } else if (prune_freq == 0 && prune_diff > 0) {
    prune_pipe <- add_expr(
      prune_pipe,
      quote(rtables::prune_table(keep_rows(at_least_percent_diff)))
    )
  } else {
    prune_pipe <- add_expr(
      prune_pipe,
      quote(rtables::prune_table())
    )
  }
  prune_pipe <- pipe_expr(prune_pipe)
  prune_list <- add_expr(
    prune_list,
    prune_pipe
  )
  prune_list <- add_expr(
    prune_list,
    quote(pruned_and_sorted_result)
  )

  y$prune <- bracket_expr(prune_list)

  y
}

#' Teal Module: Events by Grade
#'
#' @inheritParams module_arguments
#' @inheritParams template_events_by_grade
#' @inheritParams template_events_col_by_grade
#' @param col_by_grade (`flag`) \cr whether to display the grading groups in nested columns.
#' @param grading_groups (`character`) \cr list of grading groups used when col_by_grade = TRUE.
#'
#' @export
#' @examples
#' adsl <- tmc_ex_adsl
#' lbls_adae <- formatters::var_labels(tmc_ex_adae)
#' adae <- tmc_ex_adae %>%
#'   dplyr::mutate_if(is.character, as.factor) # be certain of having factors
#' formatters::var_labels(adae) <- lbls_adae
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAE", adae)
#'   ),
#'   modules = modules(
#'     tm_t_events_by_grade(
#'       label = "Adverse Events by Grade Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       llt = choices_selected(
#'         choices = variable_choices(adae, c("AETERM", "AEDECOD")),
#'         selected = c("AEDECOD")
#'       ),
#'       hlt = choices_selected(
#'         choices = variable_choices(adae, c("AEBODSYS", "AESOC")),
#'         selected = "AEBODSYS"
#'       ),
#'       grade = choices_selected(
#'         choices = variable_choices(adae, c("AETOXGR", "AESEV")),
#'         selected = "AETOXGR"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_events_by_grade <- function(label,
                                 dataname,
                                 parentname = ifelse(
                                   inherits(arm_var, "data_extract_spec"),
                                   teal.transform::datanames_input(arm_var),
                                   "ADSL"
                                 ),
                                 arm_var,
                                 hlt,
                                 llt,
                                 grade,
                                 grading_groups = list(
                                   "Any Grade (%)" = c("1", "2", "3", "4", "5"),
                                   "Grade 1-2 (%)" = c("1", "2"),
                                   "Grade 3-4 (%)" = c("3", "4"),
                                   "Grade 5 (%)" = "5"
                                 ),
                                 col_by_grade = FALSE,
                                 prune_freq = 0,
                                 prune_diff = 0,
                                 add_total = TRUE,
                                 total_label = "All Patients",
                                 drop_arm_levels = TRUE,
                                 pre_output = NULL,
                                 post_output = NULL,
                                 basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_events_by_grade")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(col_by_grade)
  checkmate::assert_scalar(prune_freq)
  checkmate::assert_scalar(prune_diff)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    hlt = cs_to_des_select(hlt, dataname = dataname),
    llt = cs_to_des_select(llt, dataname = dataname),
    grade = cs_to_des_select(grade, dataname = dataname)
  )

  module(
    label = label,
    server = srv_t_events_by_grade,
    ui = ui_t_events_by_grade,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        total_label = total_label,
        grading_groups = grading_groups,
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_events_by_grade <- function(id, ...) {
  ns <- shiny::NS(id)
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(a$arm_var, a$hlt, a$llt, a$grade)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "hlt", "llt", "grade")]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("hlt"),
        label = "Event High Level Term",
        data_extract_spec = a$hlt,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("llt"),
        label = "Event Low Level Term",
        data_extract_spec = a$llt,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("grade"),
        label = "Event Grade",
        data_extract_spec = a$grade,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::checkboxInput(
        ns("add_total"),
        "Add All Patients column",
        value = a$add_total
      ),
      shiny::checkboxInput(
        ns("col_by_grade"),
        "Display grade groupings in nested columns",
        value = a$col_by_grade
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional table settings",
          shiny::checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          ),
          shiny::helpText("Pruning Options"),
          shiny::numericInput(
            inputId = ns("prune_freq"),
            label = "Minimum Incidence Rate(%) in any of the treatment groups",
            value = a$prune_freq,
            min = 0,
            max = 100,
            step = 1,
            width = "100%"
          ),
          shiny::numericInput(
            inputId = ns("prune_diff"),
            label = "Minimum Difference Rate(%) between any of the treatment groups",
            value = a$prune_diff,
            min = 0,
            max = 100,
            step = 1,
            width = "100%"
          )
        )
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_events_by_grade <- function(id,
                                  data,
                                  reporter,
                                  filter_panel_api,
                                  dataname,
                                  parentname,
                                  label,
                                  arm_var,
                                  hlt,
                                  llt,
                                  grade,
                                  col_by_grade,
                                  grading_groups,
                                  drop_arm_levels,
                                  total_label,
                                  basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(arm_var = arm_var, hlt = hlt, llt = llt, grade = grade),
      datasets = data,
      select_validation_rule = list(
        arm_var = shinyvalidate::sv_required("A treatment variable is required"),
        grade = shinyvalidate::sv_required("An event grade is required"),
        hlt = ~ if (length(selector_list()$llt()$select) + length(.) == 0) {
          "Please select at least one of \"LOW LEVEL TERM\" or \"HIGH LEVEL TERM\" variables."
        },
        llt = shinyvalidate::compose_rules(
          ~ if (length(selector_list()$hlt()$select) + length(.) == 0) {
            "Please select at least one of \"LOW LEVEL TERM\" or \"HIGH LEVEL TERM\" variables."
          },
          ~ if (col_by_grade() && length(.) == 0) {
            "Low Level Term must be present when grade groupings are displayed in nested columns."
          }
        )
      )
    )

    col_by_grade <- shiny::reactive({
      input$col_by_grade
    })

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule(
        "prune_freq", shinyvalidate::sv_required("Please provide an Incidence Rate between 0 and 100 (%).")
      )
      iv$add_rule(
        "prune_freq",
        shinyvalidate::sv_between(0, 100, message_fmt = "Please provide an Incidence Rate between 0 and 100 (%).")
      )
      iv$add_rule(
        "prune_diff", shinyvalidate::sv_required("Please provide a Difference Rate between 0 and 100 (%).")
      )
      iv$add_rule(
        "prune_diff",
        shinyvalidate::sv_between(0, 100, message_fmt = "Please provide a Difference Rate between 0 and 100 (%).")
      )
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      join_keys = get_join_keys(data),
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      join_keys = get_join_keys(data),
      anl_name = "ANL_ADSL"
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]
      adsl_keys <- merged$adsl_input_r()$keys

      assertthat::assert_that("USUBJID" %in% adsl_keys)

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_level_term <- c(
        as.vector(merged$anl_input_r()$columns_source$hlt),
        as.vector(merged$anl_input_r()$columns_source$llt)
      )
      input_grade <- as.vector(merged$anl_input_r()$columns_source$grade)

      shiny::validate(
        shiny::need(is.factor(adsl_filtered[[input_arm_var]]), "Treatment variable is not a factor.")
      )
      if (input$col_by_grade) {
        shiny::validate(
          shiny::need(
            is.factor(anl_filtered[[input_grade]]) &&
              all(as.character(unique(anl_filtered[[input_grade]])) %in% as.character(c(1:5))),
            "Data includes records with grade levels outside of 1-5. Please use filter panel to exclude from analysis in order to display grade grouping in nested columns." # nolint
          )
        )
      } else {
        shiny::validate(
          shiny::need(
            is.factor(anl_filtered[[input_grade]]),
            "Event grade variable must be a factor."
          )
        )
      }

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c(adsl_keys, input_arm_var),
        anl = anl_filtered,
        anlvars = c(adsl_keys, input_level_term, input_grade),
        arm_var = input_arm_var
      )
    })

    # The R-code corresponding to the analysis.
    table_q <- shiny::reactive({
      validate_checks()
      ANL <- merged$anl_q()[["ANL"]] # nolint

      input_hlt <- as.vector(merged$anl_input_r()$columns_source$hlt)
      input_llt <- as.vector(merged$anl_input_r()$columns_source$llt)
      input_grade <- as.vector(merged$anl_input_r()$columns_source$grade)
      label_hlt <- if (length(input_hlt) != 0) attributes(ANL[[input_hlt]])$label else NULL
      label_llt <- if (length(input_llt) != 0) attributes(ANL[[input_llt]])$label else NULL
      label_grade <- if (length(input_grade) != 0) attributes(ANL[[input_grade]])$label else NULL
      label_grade <- if (is.null(label_grade)) input_grade else NULL

      my_calls <- if (input$col_by_grade) {
        template_events_col_by_grade(
          dataname = "ANL",
          parentname = "ANL_ADSL",
          add_total = input$add_total,
          total_label = total_label,
          grading_groups = grading_groups,
          arm_var = as.vector(merged$anl_input_r()$columns_source$arm_var),
          id = "USUBJID",
          hlt = if (length(input_hlt) != 0) input_hlt else NULL,
          llt = if (length(input_llt) != 0) input_llt else NULL,
          label_hlt = label_hlt,
          label_llt = label_llt,
          grade = if (length(input_grade) != 0) input_grade else NULL,
          label_grade = label_grade,
          prune_freq = input$prune_freq / 100,
          prune_diff = input$prune_diff / 100,
          drop_arm_levels = input$drop_arm_levels,
          basic_table_args = basic_table_args
        )
      } else {
        template_events_by_grade(
          dataname = "ANL",
          parentname = "ANL_ADSL",
          arm_var = as.vector(merged$anl_input_r()$columns_source$arm_var),
          id = "USUBJID",
          hlt = if (length(input_hlt) != 0) input_hlt else NULL,
          llt = if (length(input_llt) != 0) input_llt else NULL,
          label_hlt = label_hlt,
          label_llt = label_llt,
          grade = input_grade,
          label_grade = label_grade,
          prune_freq = input$prune_freq / 100,
          prune_diff = input$prune_diff / 100,
          add_total = input$add_total,
          total_label = total_label,
          drop_arm_levels = input$drop_arm_levels,
          basic_table_args = basic_table_args
        )
      }
      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
    })

    # Outputs to render.
    table_r <- shiny::reactive({
      table_q()[["pruned_and_sorted_result"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(table_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(table_q())))
    )

    # Render R code.
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(table_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Events by Grade Table")
        card$append_text("Events by Grade Table", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(table_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
