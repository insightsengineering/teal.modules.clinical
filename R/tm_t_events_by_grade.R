#' Template: Events by Grade
#'
#' Creates a valid expression to generate a table to summarize events by grade.
#'
#' @inheritParams template_arguments
#' @param id (`character`)\cr unique identifier of patients in datasets, default to `"USUBJID"`.
#' @param grade (`character`)\cr name of the severity level variable.
#' @param label_grade (`string`)\cr label of the `grade` variable from `dataname`. The label will be extracted from the
#' module.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_events_by_grade()]
#'
#' @keywords internal
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
                                     total_label = default_total_label(),
                                     na_level = tern::default_na_str(),
                                     drop_arm_levels = TRUE,
                                     basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(hlt, null.ok = TRUE)
  checkmate::assert_string(llt, null.ok = TRUE)
  if (is.null(hlt) && is.null(llt)) stop("At least one of 'hlt' or 'llt' can not be empty.")
  checkmate::assert_string(label_hlt, null.ok = TRUE)
  checkmate::assert_string(label_llt, null.ok = TRUE)
  checkmate::assert_string(grade)
  checkmate::assert_string(label_grade, null.ok = TRUE)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(drop_arm_levels)
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
      expr = dataname <- tern::df_explicit_na(dataname, na_level = na_str),
      env = list(dataname = as.name(dataname), na_str = na_level)
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      expr = dataname <- tern::df_explicit_na(dataname, na_level = na_str),
      env = list(dataname = as.name("anl"), na_str = na_level)
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- tern::df_explicit_na(parentname, na_level = na_str),
      env = list(parentname = as.name(parentname), na_str = na_level)
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

  y$layout_prep <- quote(split_fun <- rtables::trim_levels_in_group)

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
      module_table = teal.widgets::basic_table_args(show_colcounts = TRUE, title = basic_title)
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
        expr = tern::summarize_occurrences_by_grade(
          var = grade,
          grade_groups = grade_groups,
          na_str = na_str
        ) %>%
          rtables::split_rows_by(
            term_var,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L,
            split_fun = split_fun(grade),
            label_pos = "topleft",
            split_label = teal.data::col_labels(dataname[term_var])
          ) %>%
          tern::summarize_num_patients(
            var = id,
            .stats = "unique",
            .labels = c("- Any Intensity -"),
            na_str = na_str
          ) %>%
          tern::count_occurrences_by_grade(var = grade, .indent_mods = -1L, na_str = na_str) %>%
          tern::append_varlabels(dataname, grade, indent = 1L),
        env = list(
          id = id,
          arm_var = arm_var,
          term_var = term_var,
          grade = grade,
          dataname = as.name(dataname),
          na_str = na_level
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::summarize_occurrences_by_grade(
          var = grade,
          grade_groups = grade_groups,
          na_str = na_str
        ) %>%
          rtables::split_rows_by(
            hlt,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L,
            split_fun = split_fun(grade),
            label_pos = "topleft",
            split_label = teal.data::col_labels(dataname[hlt])
          ) %>%
          tern::summarize_occurrences_by_grade(
            var = grade,
            grade_groups = grade_groups,
            na_str = na_str
          ) %>%
          rtables::split_rows_by(
            llt,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L,
            split_fun = split_fun(grade),
            label_pos = "topleft",
            split_label = teal.data::col_labels(dataname[llt])
          ) %>%
          tern::summarize_num_patients(
            var = id,
            .stats = "unique",
            .labels = c("- Any Intensity -"),
            na_str = na_str
          ) %>%
          tern::count_occurrences_by_grade(var = grade, .indent_mods = -1L, na_str = na_str) %>%
          tern::append_varlabels(dataname, grade, indent = 2L),
        env = list(
          id = id,
          arm_var = arm_var,
          hlt = hlt,
          llt = llt,
          grade = grade,
          dataname = as.name(dataname),
          na_str = na_level
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
      expr = rtables::cont_n_onecol(length(levels(parent$arm_var)) + 1),
      env = list(
        parent = as.name(parentname),
        arm_var = as.name(arm_var)
      )
    )
  } else {
    quote(rtables::cont_n_allcols)
  }
  if (one_term) {
    term_var <- ifelse(is.null(hlt), llt, hlt)

    sort_list <- add_expr(
      sort_list,
      substitute(
        expr = {
          pruned_and_sorted_result <- pruned_result %>%
            rtables::sort_at_path(path = term_var, scorefun = scorefun, decreasing = TRUE)
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
            rtables::sort_at_path(path = hlt, scorefun = scorefun, decreasing = TRUE) %>%
            rtables::sort_at_path(path = c(hlt, "*", llt), scorefun = scorefun, decreasing = TRUE)
        },
        env = list(
          llt = llt,
          hlt = hlt,
          scorefun = scorefun
        )
      )
    )
  }
  y$sort <- bracket_expr(sort_list)

  y
}

#' Template: Adverse Events Grouped by Grade with Threshold
#'
#' Creates a valid expression to generate a table to summarize adverse events grouped by grade.
#'
#' @inheritParams template_arguments
#' @param id (`character`)\cr name of variable to uniquely identify patients in datasets.
#' @param grade (`character`)\cr name of grade variable to base `grading_groups` on.
#' @param label_grade (`character`)\cr label of the `grade` variable from `dataname`.
#' @param grading_groups (`list`)\cr named list of grading groups.
#'
#' @inherit template_arguments return
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
                                         total_label = default_total_label(),
                                         id = "USUBJID",
                                         hlt,
                                         llt,
                                         label_hlt = NULL,
                                         label_llt = NULL,
                                         grade = "AETOXGR",
                                         label_grade = NULL,
                                         prune_freq = 0.1,
                                         prune_diff = 0,
                                         na_level = tern::default_na_str(),
                                         drop_arm_levels = TRUE,
                                         basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_list(grading_groups)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(id)
  checkmate::assert_string(hlt, null.ok = TRUE)
  checkmate::assert_string(llt)
  checkmate::assert_string(grade)
  checkmate::assert_string(label_hlt, null.ok = TRUE)
  checkmate::assert_string(label_llt, null.ok = TRUE)
  checkmate::assert_string(label_grade, null.ok = TRUE)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(drop_arm_levels)
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
    substitute(
      expr = tern::df_explicit_na(na_level = na_str),
      env = list(na_str = na_level)
    )
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
        expr = rtables::split_cols_by(
          var = arm_var,
          split_fun = rtables::add_overall_level(total_label, first = FALSE)
        ),
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
          split_fun = rtables::trim_levels_in_group(llt)
        ),
        env = list(hlt = hlt, llt = llt)
      )
    )

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::append_varlabels(df = anl, vars = hlt),
        env = list(hlt = hlt)
      )
    )

    unique_label <- paste0("Total number of patients with at least one adverse event")
    layout_list <- add_expr(
      layout_list,
      substitute(
        tern::summarize_num_patients(
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
      tern::analyze_vars(
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
        expr = tern::append_varlabels(df = anl, vars = llt),
        env = list(llt = llt)
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::append_varlabels(df = anl, vars = llt, indent = 1L),
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
      quote(scorefun_soc <- tern::score_occurrences_cont_cols(col_indices = col_indices))
    )
  }

  sort_list <- add_expr(
    sort_list,
    quote(scorefun_term <- tern::score_occurrences_cols(col_indices = col_indices))
  )

  if (is.null(hlt)) {
    sort_list <- add_expr(
      sort_list,
      substitute(
        expr = {
          sorted_result <- result %>%
            rtables::sort_at_path(path = c(llt), scorefun = scorefun_term, decreasing = TRUE)
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
            rtables::sort_at_path(path = c(hlt), scorefun = scorefun_soc, decreasing = TRUE) %>%
            rtables::sort_at_path(path = c(hlt, "*", llt), scorefun = scorefun_term, decreasing = TRUE)
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
  y$prune <- bracket_expr(prune_list)

  y
}

#' teal Module: Events by Grade
#'
#' This module produces a table to summarize events by grade.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_events_by_grade
#' @inheritParams template_events_col_by_grade
#' @param col_by_grade (`logical`)\cr whether to display the grading groups in nested columns.
#' @param grading_groups (`list`)\cr named list of grading groups used when `col_by_grade = TRUE`.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`TableTree` as created from `rtables::build_table`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_events_by_grade(
#'    ..., # arguments for module
#'    decorators = list(
#'      table = teal_transform_module(...) # applied only to `table` output
#'    )
#' )
#' ```
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.clinical")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @export
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' data <- teal_data()
#' data <- within(data, {
#'   library(teal.modules.clinical)
#'   library(teal.data)
#'   library(rtables)
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl
#'   .lbls_adae <- col_labels(tmc_ex_adae)
#'   ADAE <- tmc_ex_adae %>%
#'     mutate_if(is.character, as.factor) #' be certain of having factors
#'   col_labels(ADAE) <- .lbls_adae
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADAE <- data[["ADAE"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_events_by_grade(
#'       label = "Adverse Events by Grade Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       llt = choices_selected(
#'         choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
#'         selected = c("AEDECOD")
#'       ),
#'       hlt = choices_selected(
#'         choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
#'         selected = "AEBODSYS"
#'       ),
#'       grade = choices_selected(
#'         choices = variable_choices(ADAE, c("AETOXGR", "AESEV")),
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
                                 total_label = default_total_label(),
                                 na_level = tern::default_na_str(),
                                 drop_arm_levels = TRUE,
                                 pre_output = NULL,
                                 post_output = NULL,
                                 basic_table_args = teal.widgets::basic_table_args(),
                                 transformators = list(),
                                 decorators = list()) {
  message("Initializing tm_t_events_by_grade")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(hlt, "choices_selected")
  checkmate::assert_class(llt, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(col_by_grade)
  checkmate::assert_scalar(prune_freq)
  checkmate::assert_scalar(prune_diff)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

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
        na_level = na_level,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_t_events_by_grade <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(a$arm_var, a$hlt, a$llt, a$grade)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
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
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
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
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_events_by_grade <- function(id,
                                  data,
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
                                  na_level,
                                  basic_table_args,
                                  decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
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

    col_by_grade <- reactive({
      input$col_by_grade
    })

    iv_r <- reactive({
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
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Events by Grade"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    validate_checks <- reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]
      adsl_keys <- merged$adsl_input_r()$keys

      checkmate::assert(
        .var.name = "adsl_keys",
        if ("USUBJID" %in% adsl_keys) TRUE else "Must contain \"USUBJID\""
      )

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_level_term <- c(
        as.vector(merged$anl_input_r()$columns_source$hlt),
        as.vector(merged$anl_input_r()$columns_source$llt)
      )
      input_grade <- as.vector(merged$anl_input_r()$columns_source$grade)

      validate(
        need(is.factor(adsl_filtered[[input_arm_var]]), "Treatment variable is not a factor.")
      )
      if (input$col_by_grade) {
        validate(
          need(
            is.factor(anl_filtered[[input_grade]]) &&
              all(as.character(unique(anl_filtered[[input_grade]])) %in% as.character(c(1:5))),
            paste(
              "Data includes records with grade levels outside of 1-5.",
              "Please use filter panel to exclude from analysis in order to display grade grouping in nested columns."
            )
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
    table_q <- reactive({
      validate_checks()
      ANL <- merged$anl_q()[["ANL"]]

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
          na_level = na_level,
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
          na_level = na_level,
          drop_arm_levels = input$drop_arm_levels,
          basic_table_args = basic_table_args
        )
      }
      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })


    table_renamed_q <- reactive({
      req(table_q())
      teal.code::eval_code(table_q(), "table <- pruned_and_sorted_result")
    })

    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = table_renamed_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )
    # Outputs to render.
    table_r <- reactive({
      decorated_table_q()[["table"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_table_q())))
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = label
    )
    decorated_table_q
  })
}
