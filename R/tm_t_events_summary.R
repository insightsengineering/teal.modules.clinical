#' Template: Adverse Events Summary
#'
#' Creates a valid expression to generate an adverse events summary table.
#'
#' @inheritParams template_arguments
#' @param dthfl_var (`character`)\cr name of variable for subject death flag from `parentname`.
#'   Records with `"Y"` are summarized in the table row for "Total number of deaths".
#' @param dcsreas_var (`character`)\cr name of variable for study discontinuation reason from `parentname`.
#'   Records with `"ADVERSE EVENTS"` are summarized in the table row for
#'   "Total number of patients withdrawn from study due to an AE".
#' @param flag_var_anl (`character`)\cr name of flag variable from `dataset` used to count adverse event sub-groups
#'   (e.g. Serious events, Related events, etc.). Variable labels are used as table row names if they exist.
#' @param flag_var_aesi (`character`)\cr name of flag variable from `dataset` used to count adverse event special
#'   interest groups. All flag variables must be of type `logical`. Variable labels are used as table row names if
#'   they exist.
#' @param aeseq_var (`character`)\cr name of variable for adverse events sequence number from `dataset`. Used for
#'   counting total number of events.
#' @param count_dth (`logical`)\cr whether to show count of total deaths (based on `dthfl_var`). Defaults to `TRUE`.
#' @param count_wd (`logical`)\cr whether to show count of patients withdrawn from study due to an adverse event
#'   (based on `dcsreas_var`). Defaults to `TRUE`.
#' @param count_subj (`logical`)\cr whether to show count of unique subjects (based on `USUBJID`). Only applies if
#'   event flag variables are provided.
#' @param count_pt (`logical`)\cr whether to show count of unique preferred terms (based on `llt`). Only applies if
#'   event flag variables are provided.
#' @param count_events (`logical`)\cr whether to show count of events (based on `aeseq_var`). Only applies if event
#'   flag variables are provided.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_events_summary()]
#'
#' @keywords internal
template_events_summary <- function(anl_name,
                                    parentname,
                                    arm_var,
                                    dthfl_var = "DTHFL",
                                    dcsreas_var = "DCSREAS",
                                    flag_var_anl = NULL,
                                    flag_var_aesi = NULL,
                                    aeseq_var = "AESEQ",
                                    llt = "AEDECOD",
                                    add_total = TRUE,
                                    total_label = default_total_label(),
                                    na_level = tern::default_na_str(),
                                    count_dth = TRUE,
                                    count_wd = TRUE,
                                    count_subj = TRUE,
                                    count_pt = TRUE,
                                    count_events = TRUE) {
  checkmate::assert_string(anl_name)
  checkmate::assert_string(parentname)
  checkmate::assert_character(arm_var, min.len = 1, max.len = 2)
  checkmate::assert_string(dthfl_var)
  checkmate::assert_string(dcsreas_var)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_character(flag_var_anl, null.ok = TRUE)
  checkmate::assert_character(flag_var_aesi, null.ok = TRUE)
  checkmate::assert_string(aeseq_var)
  checkmate::assert_string(llt)
  checkmate::assert_flag(count_dth)
  checkmate::assert_flag(count_wd)
  checkmate::assert_flag(count_subj)
  checkmate::assert_flag(count_pt)
  checkmate::assert_flag(count_events)

  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- anl_name,
      env = list(anl_name = as.name(anl_name))
    )
  )

  # Since this is a compound table with one layout based on `parentname`
  # and one layout on `dataname`, columns will be filtered to match levels
  # present in `parentname` only so `drop_arm_levels` = FALSE.
  data_list <- add_expr(
    data_list,
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = arm_var[[1]],
      drop_arm_levels = FALSE
    )
  )
  if (length(arm_var) == 2) {
    data_list <- add_expr(
      data_list,
      prepare_arm_levels(
        dataname = "anl",
        parentname = parentname,
        arm_var = arm_var[[2]],
        drop_arm_levels = FALSE
      )
    )
  }

  data_list <- add_expr(
    data_list,
    quote(study_id <- unique(anl[["STUDYID"]]))
  )

  # Create dummy variable for counting patients with an AE
  data_list <- add_expr(
    data_list,
    quote(anl$tmp_aefl <- "Y")
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = {
        anl[[a]] <- as.character(anl[[a]])
        anl <- anl %>%
          dplyr::mutate(
            USUBJID_AESEQ = paste(usubjid, aeseq_var, sep = "@@")
          )
      },
      env = list(
        a = llt,
        usubjid = as.name("USUBJID"),
        aeseq_var = as.name(aeseq_var)
      )
    )
  )
  if (length(flag_var_anl) > 0) {
    data_list <- add_expr(
      data_list,
      substitute(
        flag_var_anl_label <- teal.data::col_labels(anl[, flag_var_anl], fill = FALSE),
        env = list(flag_var_anl = flag_var_anl)
      )
    )
  }

  if (length(flag_var_aesi) > 0) {
    data_list <- add_expr(
      data_list,
      substitute(
        flag_var_aesi_label <- teal.data::col_labels(anl[, flag_var_aesi], fill = FALSE),
        env = list(flag_var_aesi = flag_var_aesi)
      )
    )
  }

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

  y$data <- bracket_expr(data_list)

  # Layout to be used with `parentname` dataset
  # because not all subjects may exist in `anl_name` dataset.
  layout_parent_list <- list()
  layout_parent_list <- add_expr(
    layout_parent_list,
    quote(rtables::basic_table(show_colcounts = TRUE))
  )

  layout_parent_list <- add_expr(
    layout_parent_list,
    substitute(
      expr = rtables::split_cols_by(var = arm_var),
      env = list(arm_var = arm_var[[1]])
    )
  )
  if (length(arm_var) == 2) {
    layout_parent_list <- add_expr(
      layout_parent_list,
      substitute(
        expr = rtables::split_cols_by(nested_col, split_fun = rtables::drop_split_levels),
        env = list(nested_col = arm_var[[2]])
      )
    )
  }

  if (add_total) {
    layout_parent_list <- add_expr(
      layout_parent_list,
      substitute(
        expr = rtables::add_overall_col(label = total_label),
        env = list(total_label = total_label)
      )
    )
  }

  if (count_dth) {
    layout_parent_list <- add_expr(
      layout_parent_list,
      substitute(
        expr = tern::count_values(
          dthfl_var,
          values = "Y",
          .labels = c(count_fraction = "Total number of deaths"),
          .formats = c(count_fraction = tern::format_count_fraction),
          denom = "N_col"
        ),
        env = list(dthfl_var = dthfl_var)
      )
    )
  }

  if (count_wd) {
    layout_parent_list <- add_expr(
      layout_parent_list,
      substitute(
        expr = tern::count_values(
          dcsreas_var,
          values = "ADVERSE EVENT",
          .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
          .formats = c(count_fraction = tern::format_count_fraction),
          denom = "N_col"
        ),
        env = list(dcsreas_var = dcsreas_var)
      )
    )
  }

  y$layout_parent <- substitute(
    expr = lyt_parent <- layout_parent_pipe,
    env = list(
      layout_parent_pipe = pipe_expr(layout_parent_list)
    )
  )

  table_parent_list <- list()
  table_parent_list <- add_expr(
    table_parent_list,
    substitute(
      expr = table_parent <- rtables::build_table(lyt = lyt_parent, df = df_parent, alt_counts_df = df_parent),
      env = list(df_parent = as.name(parentname))
    )
  )
  y$table_parent <- pipe_expr(table_parent_list)

  layout_anl_list <- list()
  layout_anl_list <- add_expr(
    layout_anl_list,
    quote(rtables::basic_table(show_colcounts = TRUE))
  )

  layout_anl_list <- add_expr(
    layout_anl_list,
    substitute(
      expr = rtables::split_cols_by(var = arm_var),
      env = list(arm_var = arm_var[[1]])
    )
  )
  if (length(arm_var) == 2) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = rtables::split_cols_by(nested_col, split_fun = rtables::drop_split_levels),
        env = list(nested_col = arm_var[[2]])
      )
    )
  }

  if (add_total) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = rtables::add_overall_col(label = tot_label),
        env = list(tot_label = total_label)
      )
    )
  }

  layout_anl_list <- add_expr(
    layout_anl_list,
    quote(
      expr = tern::count_patients_with_event(
        vars = "USUBJID",
        filters = c("tmp_aefl" = "Y"),
        denom = "N_col",
        .stats = "count_fraction",
        .labels = c(
          count_fraction = "Total number of patients with at least one adverse event"
        ),
        .indent_mods = c(count_fraction = 0L),
        table_names = "total_pts_at_least_one"
      ) %>% tern::count_values(
        "STUDYID",
        values = study_id,
        .stats = "count",
        .labels = c(count = "Total AEs"),
        table_names = "total_aes"
      )
    )
  )

  table_anl_list <- list()
  table_anl_list <- add_expr(
    table_anl_list,
    substitute(
      expr = table_anl <- rtables::build_table(lyt = lyt_anl, df = anl, alt_counts_df = df_parent),
      env = list(df_parent = as.name(parentname))
    )
  )

  condition1 <- count_subj && is.character(flag_var_anl)
  if (condition1) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = tern::count_patients_with_flags(
          var = "USUBJID",
          flag_variables = flag_var_anl_label,
          table_names = "count_subj_anl",
          denom = "N_col",
          var_labels = "Total number of patients with at least one",
          show_labels = "visible"
        ),
        env = list(flag_var_anl = flag_var_anl)
      )
    )
  }

  condition2 <- count_pt && is.character(flag_var_anl)
  if (condition2) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = tern::count_patients_with_flags(
          var = llt,
          flag_variables = flag_var_anl_label,
          table_names = "count_pt_anl",
          .stats = "count",
          .formats = c(count = "xx"),
          denom = "N_col",
          var_labels = "Total number of unique preferred terms which are",
          show_labels = "visible"
        ),
        env = list(flag_var_anl = flag_var_anl, llt = llt)
      )
    )
  }

  condition3 <- count_events && is.character(flag_var_anl)
  if (condition3) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = tern::count_patients_with_flags(
          var = "USUBJID_AESEQ",
          flag_variables = flag_var_anl_label,
          table_names = "count_events_anl",
          .stats = "count",
          .formats = c(count = "xx"),
          denom = "N_col",
          var_labels = "Total number of adverse events which are",
          show_labels = "visible"
        ),
        env = list(flag_var_anl = flag_var_anl)
      )
    )
  }

  condition4 <- count_subj && is.character(flag_var_aesi)
  if (condition4) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = tern::count_patients_with_flags(
          var = "USUBJID",
          flag_variables = flag_var_aesi_label,
          table_names = "count_subj_aesi",
          denom = "N_col",
          var_labels = "Medical concepts: number of patients with",
          show_labels = "visible"
        ),
        env = list(flag_var_aesi = flag_var_aesi)
      )
    )
  }

  condition5 <- count_pt && is.character(flag_var_aesi)
  if (condition5) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = tern::count_patients_with_flags(
          var = llt,
          flag_variables = flag_var_aesi_label,
          table_names = "count_pt_aesi",
          .stats = "count",
          .formats = c(count = "xx"),
          denom = "N_col",
          var_labels = "Medical concepts: number of unique preferred terms which are part of",
          show_labels = "visible"
        ),
        env = list(flag_var_aesi = flag_var_aesi, llt = llt)
      )
    )
  }

  condition6 <- count_events && is.character(flag_var_aesi)
  if (condition6) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = tern::count_patients_with_flags(
          var = "USUBJID_AESEQ",
          flag_variables = flag_var_aesi_label,
          table_names = "count_events_aesi",
          .stats = "count",
          .formats = c(count = "xx"),
          denom = "N_col",
          var_labels = "Medical concepts: number of adverse events which are part of",
          show_labels = "visible"
        ),
        env = list(flag_var_aesi = flag_var_aesi)
      )
    )
  }

  y$layout_anl <- substitute(
    expr = lyt_anl <- layout_anl_pipe,
    env = list(
      layout_anl_pipe = pipe_expr(layout_anl_list)
    )
  )

  y$table_anl <- pipe_expr(table_anl_list)

  table_list <- list()
  table_list <- add_expr(
    table_list,
    quote(
      rtables::col_info(table_parent) <- rtables::col_info(table_anl)
    )
  )

  all_conditions <- c(
    condition1,
    condition2,
    condition3,
    condition4,
    condition5,
    condition6
  )

  if (any(all_conditions) && (count_dth || count_wd)) {
    table_list <- add_expr(
      table_list,
      quote(
        expr = table <- rtables::rbind(
          table_anl[1:2, ],
          table_parent,
          table_anl[3:nrow(table_anl), ]
        )
      )
    )
  } else if (any(all_conditions)) {
    table_list <- add_expr(
      table_list,
      quote(
        expr = table <- rtables::rbind(
          table_anl[1:2, ],
          table_anl[3:nrow(table_anl), ]
        )
      )
    )
  } else {
    table_list <- add_expr(
      table_list,
      quote(
        table <- rtables::rbind(table_anl, table_parent)
      )
    )
  }

  y$table <- bracket_expr(table_list)

  y
}

#' teal Module: Adverse Events Summary
#'
#' This module produces an adverse events summary table.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_arguments
#' @inheritParams template_events_summary
#' @param arm_var ([teal.picks::variables()])\cr object with all
#'   available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table.
#'   If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param dthfl_var ([teal.picks::variables()])\cr object
#'   with all available choices and preselected option for variable names that can be used as death flag variable.
#'   Records with `"Y"`` are summarized in the table row for "Total number of deaths".
#' @param dcsreas_var ([teal.picks::variables()])\cr object
#'   with all available choices and preselected option for variable names that can be used as study discontinuation
#'   reason variable. Records with `"ADVERSE EVENTS"` are summarized in the table row for
#'   "Total number of patients withdrawn from study due to an AE".
#' @param flag_var_anl ([teal.picks::variables()] or `NULL`)\cr
#'   vector with names of flag variables from `dataset` used to count adverse event sub-groups (e.g. Serious events,
#'   Related events, etc.). Variable labels are used as table row names if they exist.
#' @param flag_var_aesi ([teal.picks::variables()] or `NULL`)\cr
#'   vector with names of flag variables from `dataset` used to count adverse event special interest groups. All flag
#'   variables must be of type `logical`. Variable labels are used as table row names if they exist.
#' @param aeseq_var ([teal.picks::variables()])\cr variable for
#'   adverse events sequence number from `dataset`. Used for counting total number of events.
#' @param llt ([teal.picks::variables()])\cr adverse event term / low-level term column (e.g. `AEDECOD`).
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
#' tm_t_events_summary(
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
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' data <- teal_data()
#' data <- within(data, {
#'   library(teal.modules.clinical)
#'   library(dplyr)
#'   library(tern)
#'   library(formatters)
#'   ADSL <- tmc_ex_adsl %>%
#'     mutate(
#'       DTHFL = case_when(
#'         !is.na(DTHDT) ~ "Y",
#'         TRUE ~ ""
#'       ) %>% with_label("Subject Death Flag")
#'     )
#'   ADAE <- tmc_ex_adae
#'
#'   .add_event_flags <- function(dat) {
#'     dat <- dat %>%
#'       mutate(
#'         TMPFL_SER = AESER == "Y",
#'         TMPFL_REL = AEREL == "Y",
#'         TMPFL_GR5 = AETOXGR == "5",
#'         TMP_SMQ01 = !is.na(SMQ01NAM),
#'         TMP_SMQ02 = !is.na(SMQ02NAM),
#'         TMP_CQ01 = !is.na(CQ01NAM)
#'       )
#'     column_labels <- list(
#'       TMPFL_SER = "Serious AE",
#'       TMPFL_REL = "Related AE",
#'       TMPFL_GR5 = "Grade 5 AE",
#'       TMP_SMQ01 = aesi_label(dat[["SMQ01NAM"]], dat[["SMQ01SC"]]),
#'       TMP_SMQ02 = aesi_label("Y.9.9.9.9/Z.9.9.9.9 AESI"),
#'       TMP_CQ01 = aesi_label(dat[["CQ01NAM"]])
#'     )
#'     col_labels(dat)[names(column_labels)] <- as.character(column_labels)
#'     dat
#'   }
#'
#'   #' Generating user-defined event flags.
#'   ADAE <- ADAE %>% .add_event_flags()
#'
#'   .ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
#'   .aesi_vars <- names(ADAE)[startsWith(names(ADAE), "TMP_")]
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_events_summary(
#'       label = "Adverse Events Summary",
#'       dataname = "ADAE",
#'       arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
#'       flag_var_anl = variables(
#'         choices = any_of(data[[".ae_anl_vars"]]),
#'         selected = data[[".ae_anl_vars"]][1],
#'         multiple = TRUE,
#'         ordered = TRUE,
#'         fixed = FALSE
#'       ),
#'       flag_var_aesi = variables(
#'         choices = any_of(data[[".aesi_vars"]]),
#'         selected = data[[".aesi_vars"]][1],
#'         multiple = TRUE,
#'         ordered = TRUE,
#'         fixed = FALSE
#'       ),
#'       add_total = TRUE
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_events_summary <- function(label,
                                dataname,
                                parentname = "ADSL",
                                arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
                                flag_var_anl = NULL,
                                flag_var_aesi = NULL,
                                dthfl_var = variables(choices = any_of(c("DTHFL")), selected = "DTHFL", fixed = TRUE),
                                dcsreas_var = variables(choices = any_of(c("DCSREAS")), selected = "DCSREAS", fixed = TRUE),
                                llt = variables(choices = any_of(c("AEDECOD")), selected = "AEDECOD", fixed = TRUE),
                                aeseq_var = variables(choices = any_of(c("AESEQ")), selected = "AESEQ", fixed = TRUE),
                                add_total = TRUE,
                                total_label = default_total_label(),
                                na_level = tern::default_na_str(),
                                count_dth = TRUE,
                                count_wd = TRUE,
                                count_subj = TRUE,
                                count_pt = TRUE,
                                count_events = TRUE,
                                pre_output = NULL,
                                post_output = NULL,
                                basic_table_args = teal.widgets::basic_table_args(),
                                transformators = list(),
                                decorators = list()) {
  message("Initializing tm_t_events_summary")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(flag_var_anl, "variables", null.ok = TRUE)
  checkmate::assert_class(flag_var_aesi, "variables", null.ok = TRUE)
  checkmate::assert_class(dthfl_var, "variables")
  checkmate::assert_class(dcsreas_var, "variables")
  checkmate::assert_class(llt, "variables")
  checkmate::assert_class(aeseq_var, "variables")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(count_dth)
  checkmate::assert_flag(count_wd)
  checkmate::assert_flag(count_subj)
  checkmate::assert_flag(count_pt)
  checkmate::assert_flag(count_events)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  arm_var <- picks(datasets(parentname, parentname), arm_var)
  dthfl_var <- picks(datasets(parentname, parentname), dthfl_var)
  dcsreas_var <- picks(datasets(parentname, parentname), dcsreas_var)
  llt <- picks(datasets(dataname, dataname), llt)
  aeseq_var <- picks(datasets(dataname, dataname), aeseq_var)
  if (!is.null(flag_var_anl)) {
    flag_var_anl <- picks(datasets(dataname, dataname), flag_var_anl)
  }
  if (!is.null(flag_var_aesi)) {
    flag_var_aesi <- picks(datasets(dataname, dataname), flag_var_aesi)
  }

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_events_summary,
    server = srv_t_events_summary,
    ui_args = args[names(args) %in% names(formals(ui_t_events_summary))],
    server_args = args[names(args) %in% names(formals(srv_t_events_summary))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}

#' @keywords internal
ui_t_events_summary <- function(id,
                                arm_var,
                                dthfl_var,
                                dcsreas_var,
                                flag_var_anl,
                                flag_var_aesi,
                                aeseq_var,
                                llt,
                                add_total,
                                count_dth,
                                count_wd,
                                count_subj,
                                count_pt,
                                count_events,
                                pre_output,
                                post_output,
                                decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Treatment Variable"),
        picks_ui(ns("arm_var"), arm_var)
      ),
      `if`(
        is.null(flag_var_anl),
        NULL,
        tags$div(
          tags$label("Event Flag Variables"),
          picks_ui(ns("flag_var_anl"), flag_var_anl)
        )
      ),
      `if`(
        is.null(flag_var_aesi),
        NULL,
        tags$div(
          tags$label("AE Basket Flag Variables"),
          picks_ui(ns("flag_var_aesi"), flag_var_aesi)
        )
      ),
      checkboxInput(
        ns("add_total"),
        "Add All Patients column",
        value = add_total
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "table")),
      bslib::accordion_panel(
        "Table Settings",
        open = TRUE,
        checkboxInput(
          ns("count_dth"),
          "Count deaths",
          value = count_dth
        ),
        checkboxInput(
          ns("count_wd"),
          "Count withdrawals due to AE",
          value = count_wd
        ),
        checkboxInput(
          ns("count_subj"),
          "Count patients",
          value = count_subj
        ),
        checkboxInput(
          ns("count_pt"),
          "Count preferred terms",
          value = count_pt
        ),
        checkboxInput(
          ns("count_events"),
          "Count events",
          value = count_events
        )
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          tags$div(
            tags$label("Death Flag Variable"),
            picks_ui(ns("dthfl_var"), dthfl_var)
          ),
          tags$div(
            tags$label("Study Discontinuation Reason Variable"),
            picks_ui(ns("dcsreas_var"), dcsreas_var)
          ),
          tags$div(
            tags$label("AE Sequence Variable"),
            picks_ui(ns("aeseq_var"), aeseq_var)
          ),
          tags$div(
            tags$label("AE Term Variable"),
            picks_ui(ns("llt"), llt)
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_t_events_summary <- function(id,
                                 data,
                                 dataname,
                                 parentname,
                                 arm_var,
                                 dthfl_var,
                                 dcsreas_var,
                                 flag_var_anl,
                                 flag_var_aesi,
                                 aeseq_var,
                                 llt,
                                 label,
                                 total_label,
                                 na_level,
                                 basic_table_args,
                                 decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    picks_list <- list(
      arm_var = arm_var,
      dthfl_var = dthfl_var,
      dcsreas_var = dcsreas_var,
      aeseq_var = aeseq_var,
      llt = llt
    )
    if (!is.null(flag_var_anl)) {
      picks_list$flag_var_anl <- flag_var_anl
    }
    if (!is.null(flag_var_aesi)) {
      picks_list$flag_var_aesi <- flag_var_aesi
    }

    selectors <- picks_srv(id = "", picks = picks_list, data = data)

    anl_selectors <- selectors
    adsl_selectors <- selectors[c("arm_var", "dthfl_var", "dcsreas_var")]

    data_with_card <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj
    })
    merged_anl <- merge_srv("merge_anl", data = data_with_card, selectors = anl_selectors, output_name = "ANL")
    merged_adsl_anl <- merge_srv("merge_adsl_anl", data = merged_anl$data, selectors = adsl_selectors, output_name = "ANL_ADSL")
    anl_q <- merged_adsl_anl$data

    validate_checks <- reactive({
      input_arm <- anl_selectors$arm_var()$variables$selected
      validate(
        need(length(input_arm) %in% 1:2, "Please select exactly 1 or 2 treatment variables")
      )
      validate(
        need(
          length(anl_selectors$dthfl_var()$variables$selected) >= 1L,
          "Death Flag Variable is required"
        ),
        need(
          length(anl_selectors$dcsreas_var()$variables$selected) >= 1L,
          "Study Discontinuation Reason Variable is required"
        ),
        need(
          length(anl_selectors$aeseq_var()$variables$selected) >= 1L,
          "AE Sequence Variable is required"
        ),
        need(
          length(anl_selectors$llt()$variables$selected) >= 1L,
          "AE Term Variable is required"
        )
      )

      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- as.vector(anl_selectors$arm_var()$variables$selected)
      input_dthfl_var <- as.vector(anl_selectors$dthfl_var()$variables$selected)
      input_dcsreas_var <- as.vector(anl_selectors$dcsreas_var()$variables$selected)
      input_flag_var_anl <- if (!is.null(flag_var_anl)) {
        as.vector(anl_selectors$flag_var_anl()$variables$selected)
      } else {
        NULL
      }
      input_flag_var_aesi <- if (!is.null(flag_var_aesi)) {
        as.vector(anl_selectors$flag_var_aesi()$variables$selected)
      } else {
        NULL
      }
      input_aeseq_var <- as.vector(anl_selectors$aeseq_var()$variables$selected)
      input_llt <- as.vector(anl_selectors$llt()$variables$selected)

      validate(
        need(
          is.factor(adsl_filtered[[input_arm_var[[1]]]]) && is.factor(anl_filtered[[input_arm_var[[1]]]]),
          "The treatment variable selected must be a factor variable in all datasets used."
        ),
        if (length(input_arm_var) == 2) {
          need(
            is.factor(adsl_filtered[[input_arm_var[[2]]]]) && all(!adsl_filtered[[input_arm_var[[2]]]] %in% c(
              "", NA
            )),
            "Please check nested treatment variable which needs to be a factor without NA or empty strings."
          )
        },
        need(
          identical(levels(adsl_filtered[[input_arm_var[[1]]]]), levels(anl_filtered[[input_arm_var[[1]]]])),
          "The treatment variable selected must have the same levels across all datasets used."
        )
      )

      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_dthfl_var, input_dcsreas_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_flag_var_anl, input_flag_var_aesi, input_aeseq_var, input_llt),
        arm_var = input_arm_var[[1]]
      )
    })

    # The R-code corresponding to the analysis.
    table_q <- reactive({
      validate_checks()

      input_flag_var_anl <- if (!is.null(flag_var_anl)) {
        as.vector(anl_selectors$flag_var_anl()$variables$selected)
      } else {
        NULL
      }
      input_flag_var_aesi <- if (!is.null(flag_var_aesi)) {
        as.vector(anl_selectors$flag_var_aesi()$variables$selected)
      } else {
        NULL
      }

      my_calls <- template_events_summary(
        anl_name = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_selectors$arm_var()$variables$selected),
        dthfl_var = as.vector(anl_selectors$dthfl_var()$variables$selected),
        dcsreas_var = as.vector(anl_selectors$dcsreas_var()$variables$selected),
        flag_var_anl = if (length(input_flag_var_anl)) input_flag_var_anl else NULL,
        flag_var_aesi = if (length(input_flag_var_aesi)) input_flag_var_aesi else NULL,
        aeseq_var = as.vector(anl_selectors$aeseq_var()$variables$selected),
        llt = as.vector(anl_selectors$llt()$variables$selected),
        add_total = input$add_total,
        total_label = total_label,
        na_level = na_level,
        count_dth = input$count_dth,
        count_wd = input$count_wd,
        count_subj = input$count_subj,
        count_pt = input$count_pt,
        count_events = input$count_events
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      all_basic_table_args <- teal.widgets::resolve_basic_table_args(user_table = basic_table_args)
      teal.code::eval_code(
        obj,
        as.expression(unlist(my_calls))
      ) %>%
        teal.code::eval_code(
          substitute(
            expr = {
              rtables::main_title(table) <- title
              rtables::main_footer(table) <- footer
              rtables::prov_footer(table) <- p_footer
              rtables::subtitles(table) <- subtitle
            }, env = list(
              title = `if`(is.null(all_basic_table_args$title), label, all_basic_table_args$title),
              footer = `if`(is.null(all_basic_table_args$main_footer), "", all_basic_table_args$main_footer),
              p_footer = `if`(is.null(all_basic_table_args$prov_footer), "", all_basic_table_args$prov_footer),
              subtitle = `if`(is.null(all_basic_table_args$subtitles), "", all_basic_table_args$subtitles)
            )
          )
        )
    })

    # Outputs to render.

    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = table_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
