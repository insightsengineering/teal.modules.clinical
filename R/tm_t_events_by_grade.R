#' Template: Events by Grade
#'
#' @inheritParams template_arguments
#' @param label_hlt (`string`)\cr label of the `hlt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param label_llt (`string`)\cr label of the `llt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param id (`character`) \cr unique identifier of patients in datasets, default to "USUBJID".
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
                                     id = "USUBJID",
                                     hlt,
                                     llt,
                                     label_hlt = NULL,
                                     label_llt = NULL,
                                     grade,
                                     label_grade = NULL,
                                     prune_freq = 0,
                                     prune_diff = 0,
                                     add_total = TRUE,
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

  basic_title <- if (is.null(hlt) & !is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_llt)
  } else if (!is.null(hlt) & is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_hlt)
  } else if (!is.null(hlt) & !is.null(llt)) {
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
      quote(
        rtables::add_overall_col(label = "All Patients")
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
            split_label = teal::variable_labels(dataname[term_var])
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
            split_label = teal::variable_labels(dataname[hlt])
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
            split_label = teal::variable_labels(dataname[llt])
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
#' @param id (`character`) \cr unique identifier of patients in datasets, default to "USUBJID".
#' @param label_hlt (`string`)\cr label of the `hlt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param label_llt (`string`)\cr label of the `llt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param grade (`character`) \cr grade term which grading_groups is based on, default to "AETOXGR".
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
  basic_title <- if (is.null(hlt) & !is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_llt)
  } else if (!is.null(hlt) & is.null(llt)) {
    paste0("Adverse Event summary by ", label_grade, ": ", label_hlt)
  } else if (!is.null(hlt) & !is.null(llt)) {
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
        expr = rtables::split_cols_by(var = arm_var, split_fun = add_overall_level("All Patients", first = FALSE)),
        env = list(arm_var = arm_var)
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

  # for variant 8 in STREAM manual
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

  if (prune_freq > 0 & prune_diff > 0) {
    prune_pipe <- add_expr(
      prune_pipe,
      quote(rtables::prune_table(keep_rows(at_least_percent_any & at_least_percent_diff)))
    )
  } else if (prune_freq > 0 & prune_diff == 0) {
    prune_pipe <- add_expr(
      prune_pipe,
      quote(rtables::prune_table(keep_rows(at_least_percent_any)))
    )
  } else if (prune_freq == 0 & prune_diff > 0) {
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
#' library(dplyr)
#' library(scda)
#' library(tern)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adae <- synthetic_cdisc_data("latest")$adae
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'adsl <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADAE", adae, code = 'adae <- synthetic_cdisc_data("latest")$adae')
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
                                 drop_arm_levels = TRUE,
                                 pre_output = NULL,
                                 post_output = NULL,
                                 basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_events_by_grade")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_flag(add_total)
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
        grading_groups = grading_groups,
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_events_by_grade <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(a$arm_var, a$hlt, a$llt, a$grade)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
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
      checkboxInput(
        ns("add_total"),
        "Add All Patients column",
        value = a$add_total
      ),
      checkboxInput(
        ns("col_by_grade"),
        "Display grade groupings in nested columns",
        value = a$col_by_grade
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          ),
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
      )
    ),
    forms = teal::get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_events_by_grade <- function(id,
                                  datasets,
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
                                  basic_table_args) {
  stopifnot(is_cdisc_data(datasets))
  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()

    anl_merged <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(arm_var = arm_var, hlt = hlt, llt = llt, grade = grade),
      merge_function = "dplyr::inner_join"
    )

    adsl_merged <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    validate_checks <- reactive({
      adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
      anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
      adsl_keys <- datasets$get_keys(parentname)

      anl_m <- anl_merged()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_level_term <- c(
        as.vector(anl_m$columns_source$hlt),
        as.vector(anl_m$columns_source$llt)
      )
      input_grade <- as.vector(anl_m$columns_source$grade)

      validate(
        need(input_arm_var, "Please select a treatment variable"),
        need(input_grade, "Please select a grade variable")
      )
      teal::validate_has_elements(
        input_level_term,
        "Please select at least one of \"LOW LEVEL TERM\" or \"HIGH LEVEL TERM\" variables.\n If the module is for displaying adverse events with grading groups in nested columns, \"LOW LEVEL TERM\" cannot be empty" # nolint
      )
      validate(
        need(is.factor(adsl_filtered[[input_arm_var]]), "Treatment variable is not a factor.")
      )
      if (input$col_by_grade) {
        validate(
          need(
            is.factor(anl_filtered[[input_grade]]) &&
              all(as.character(unique(anl_filtered[[input_grade]])) %in% as.character(c(1:5))),
            "Data includes records with grade levels outside of 1-5. Please use filter panel to exclude from analysis in order to display grade grouping in nested columns." # nolint
          )
        )
      } else {
        validate(
          need(
            is.factor(anl_filtered[[input_grade]]),
            "Event grade variable must be a factor."
          )
        )
      }
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
      if (input$col_by_grade) {
        validate(
          need(
            as.vector(anl_m$columns_source$llt),
            "Low Level Term must be present for nested grade grouping display."
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
    call_preparation <- reactive({
      validate_checks()

      teal.code::chunks_reset()
      anl_m <- anl_merged()
      teal.code::chunks_push_data_merge(anl_m)
      teal.code::chunks_push_new_line()

      anl_adsl <- adsl_merged()
      teal.code::chunks_push_data_merge(anl_adsl)
      teal.code::chunks_push_new_line()
      input_hlt <- as.vector(anl_m$columns_source$hlt)
      input_llt <- as.vector(anl_m$columns_source$llt)
      input_grade <- as.vector(anl_m$columns_source$grade)
      label_hlt <- if (length(input_hlt) != 0) attributes(anl_m$data()[[input_hlt]])$label else NULL
      label_llt <- if (length(input_llt) != 0) attributes(anl_m$data()[[input_llt]])$label else NULL
      label_grade <- if (length(input_grade) != 0) attributes(anl_m$data()[[input_grade]])$label else NULL
      label_grade <- if (is.null(label_grade)) input_grade else NULL

      my_calls <- if (input$col_by_grade) {
        template_events_col_by_grade(
          dataname = "ANL",
          parentname = "ANL_ADSL",
          add_total = input$add_total,
          grading_groups = grading_groups,
          arm_var = as.vector(anl_m$columns_source$arm_var),
          id = datasets$get_keys(parentname)[2],
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
          arm_var = as.vector(anl_m$columns_source$arm_var),
          id = datasets$get_keys(parentname)[2],
          hlt = if (length(input_hlt) != 0) input_hlt else NULL,
          llt = if (length(input_llt) != 0) input_llt else NULL,
          label_hlt = label_hlt,
          label_llt = label_llt,
          grade = input_grade,
          label_grade = label_grade,
          prune_freq = input$prune_freq / 100,
          prune_diff = input$prune_diff / 100,
          add_total = input$add_total,
          drop_arm_levels = input$drop_arm_levels,
          basic_table_args = basic_table_args
        )
      }
      mapply(expression = my_calls, teal.code::chunks_push)
    })

    # Outputs to render.
    table <- reactive({
      call_preparation()
      teal.code::chunks_safe_eval()
      teal.code::chunks_get_var("pruned_and_sorted_result")
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table
    )

    # Render R code.
    teal::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.transform::get_extract_datanames(
        list(arm_var, hlt, llt, grade)
      ),
      modal_title = "AE by Grade Table",
      code_header = label
    )
  })
}
