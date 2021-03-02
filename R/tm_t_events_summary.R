#' Template: Adverse Events Summary
#'
#' @inheritParams template_arguments
#' @param dthfl_var (`character`)\cr
#'  variable for subject death flag from `parentname`. Records with `"Y"`` are summarized in
#'  the table row for "Total number of deaths".
#' @param dcsreas_var (`character`)\cr
#'   variable for study discontinuation reason from `parentname`. Records with `"ADVERSE EVENTS"`
#'   are summarized in the table row for "Total number of patients withdrawn from study due to an AE".
#' @param flag_var_anl (`character`)\cr
#'   flag variable from `dataset` used to count adverse event sub-groups (e.g. Serious
#'   events, Related events, etc.). Variable labels are used as table row names if they exist.
#' @param flag_var_aesi (`character`)\cr
#'   flag variable from `dataset` used to count adverse event special interest groups.
#'   All flag variables must be of type `logical`. Variable labels are used as table
#'   row names if they exist.
#' @param aeseq_var (`character`)\cr
#'   variable for adverse events sequence number from `dataset`. Used for counting total
#'   number of events.
#' @param count_subj (`logical`)\cr w
#'   whether to show count of unique subjects based on `USUBJID`. Only applies if event flag
#'   variables are provided.
#' @param count_pt (`logical`)\cr
#'   whether to show count of unique preferred terms based on `llt`. Only applies if event
#'   flag variables are provided.
#' @param count_events (`logical`)\cr
#'   whether to show count of events based on `aeseq_var`. Only applies if event flag variables
#'   are provided.
#'
#' @seealso [tm_t_events_summary()]
#'
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
                                    count_subj = TRUE,
                                    count_pt = TRUE,
                                    count_events = TRUE) {

  assert_that(
    is.string(anl_name),
    is.string(parentname),
    is.string(arm_var),
    is.string(dthfl_var),
    is.string(dcsreas_var),
    is.flag(add_total),
    is.character(flag_var_anl) || is.null(NULL),
    is.character(flag_var_aesi) || is.null(NULL),
    is.string(aeseq_var),
    is.string(llt),
    is.flag(count_subj),
    is.flag(count_pt),
    is.flag(count_events)
  )

  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- anl_name,
      env = list(anl_name = as.name(anl_name))
    )
  )

  data_list <- add_expr(
    data_list,
    quote(study_id <- unique(anl[["STUDYID"]]))
  )

  data_list <- add_expr(
    data_list,
    substitute_names(
      expr = anl <- anl %>% dplyr::mutate(
        a = as.character(a),
        USUBJID_AESEQ = paste(usubjid, aeseq_var, sep = "@@")
      ),
      names = list(
        a = as.name(llt),
        usubjid = as.name("USUBJID"),
        aeseq_var = as.name(aeseq_var)
      )
    )
  )

  if (length(flag_var_anl) > 0){
    data_list <- add_expr(
      data_list,
      substitute(flag_var_anl_label <- var_labels(anl[, flag_var_anl]),
                 env = list(flag_var_anl = flag_var_anl)
      )
    )
  }

  if (length(flag_var_aesi) > 0){
    data_list <- add_expr(
      data_list,
      substitute(flag_var_aesi_label <- var_labels(anl[, flag_var_aesi]),
                 env = list(flag_var_aesi = flag_var_aesi)
      )
    )
  }

  y$data <- bracket_expr(data_list)

  # Layout to be used with `parentname` dataset
  # because not all subjects may exist in `anl_name` dataset.
  layout_parent_list <- list()
  layout_parent_list <- add_expr(
    layout_parent_list,
    quote(basic_table())
  )
  layout_parent_list <- add_expr(
    layout_parent_list,
    substitute(
      expr = split_cols_by(arm_var) %>% add_colcounts(),
      env = list(arm_var = arm_var)
    )
  )
  if (add_total) {
    layout_parent_list <- add_expr(
      layout_parent_list,
      quote(add_overall_col(label = "All Patients"))
    )
  }
  layout_parent_list <- add_expr(
    layout_parent_list,
    substitute(
      expr = count_values(
        dthfl_var,
        values = "Y",
        .labels = c(count_fraction = "Total number of deaths"),
        denom = "N_col"
      ) %>%
        count_values(
          dcsreas_var,
          values = "ADVERSE EVENT",
          .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
          denom = "N_col"
        ),
      env = list(dthfl_var = dthfl_var, dcsreas_var = dcsreas_var)
    )
  )

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
      expr = result_parent <- build_table(lyt = lyt_parent, df = df_parent, alt_counts_df = df_parent),
      env = list(df_parent = as.name(parentname))
    )
  )
  y$table_parent <- pipe_expr(table_parent_list)

  layout_anl_list <- list()
  layout_anl_list <- add_expr(
    layout_anl_list,
    quote(basic_table())
  )

  layout_anl_list <- add_expr(
    layout_anl_list,
    substitute(
      expr = split_cols_by(arm_var) %>% add_colcounts(),
      env = list(arm_var = arm_var)
    )
  )

  if (add_total) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      quote(add_overall_col(label = "All Patients"))
    )
  }

  layout_anl_list <- add_expr(
    layout_anl_list,
    quote(
      expr = count_patients_with_event(
        vars = "USUBJID",
        filters = c("STUDYID" = study_id),
        denom = "N_col",
        .stats = "count_fraction",
        .labels = c(
          count_fraction = "Total number of patients with at least one adverse event" #nolint
        ),
        .indent_mods = c(count_fraction = 0L),
        table_names = "total_pts_at_least_one"
      ) %>% count_values(
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
      expr = result_anl <- build_table(lyt = lyt_anl, df = anl, alt_counts_df = df_parent),
      env = list(df_parent = as.name(parentname))
    )
  )

  count_flags <- c(count_subj, count_pt, count_events)

  condition1 <- count_subj && is.character(flag_var_anl)
  if (condition1) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = count_patients_with_flags(
          var = "USUBJID",
          flag_variables = flag_var_anl_label,
          table_names = paste0("count_subj_", flag_var_anl),
          .indent_mods = 1L
        ),
        env = list(flag_var_anl = flag_var_anl)
      )
    )

    table_anl_list <- add_expr(
      table_anl_list,
      substitute(
        expr = insert_rrow(
          rrow("Total number of patients with at least one", ""),
          at = position
        ),
        env = list(
          position = sum(2, h_count_rows(count_flags[1], vars_anl = flag_var_anl))
        )
      )
    )
  }

  condition2 <- count_pt && is.character(flag_var_anl)
  if (condition2) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = count_patients_with_flags(
          var = llt,
          flag_variables = flag_var_anl_label,
          table_names = paste0("count_pt_", flag_var_anl),
          .stats = "count",
          .formats = c(count = "xx"),
          .indent_mods = 1L
        ),
        env = list(flag_var_anl = flag_var_anl, llt = llt)
      )
    )

    table_anl_list <- add_expr(
      table_anl_list,
      substitute(
        expr = insert_rrow(
          rrow("Total number of unique preferred terms which are", ""),
          at = position
        ),
        env = list(
          position = sum(2, h_count_rows(count_flags[1:2], vars_anl = flag_var_anl))
        )
      )
    )
  }

  condition3 <- count_events && is.character(flag_var_anl)
  if (condition3) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = count_patients_with_flags(
          var = "USUBJID_AESEQ",
          flag_variables = flag_var_anl_label,
          table_names = paste0("count_events_", flag_var_anl),
          .stats = "count",
          .formats = c(count = "xx"),
          .indent_mods = 1L
        ),
        env = list(flag_var_anl = flag_var_anl)
      )
    )

    table_anl_list <- add_expr(
      table_anl_list,
      substitute(
        expr = insert_rrow(
          rrow("Total number of adverse events which are", ""),
          at = position
        ),
        env = list(
          position = sum(2, h_count_rows(count_flags, vars_anl = flag_var_anl))
        )
      )
    )
  }

  condition4 <- count_subj && is.character(flag_var_aesi)
  if (condition4) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = count_patients_with_flags(
          var = "USUBJID",
          flag_variables = flag_var_aesi_label,
          table_names = paste0("count_subj_", flag_var_aesi),
          .indent_mods = 1L
        ),
        env = list(flag_var_aesi = flag_var_aesi)
      )
    )
    table_anl_list <- add_expr(
      table_anl_list,
      substitute(
        expr = insert_rrow(
          rrow("Medical concepts: number of patients with", ""),
          at = position
        ),
        env = list(
          position = sum(
            2,
            h_count_rows(
              x_anl = count_flags,
              vars_anl = flag_var_anl,
              x_aesi = count_flags[1],
              vars_aesi = flag_var_aesi
            )
          )
        )
      )
    )
  }

  condition5 <- count_pt && is.character(flag_var_aesi)
  if (condition5) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = count_patients_with_flags(
          var = llt,
          flag_variables = flag_var_aesi_label,
          table_names = paste0("count_pt_", flag_var_aesi),
          .stats = "count",
          .formats = c(count = "xx"),
          .indent_mods = 1L
        ),
        env = list(flag_var_aesi = flag_var_aesi, llt = llt)
      )
    )
    table_anl_list <- add_expr(
      table_anl_list,
      substitute(
        expr = insert_rrow(
          rrow("Medical concepts: number of unique preferred terms which are part of", #nolint
               ""
          ),
          at = position
        ),
        env = list(
          position = sum(
            2,
            h_count_rows(
              x_anl = count_flags,
              vars_anl = flag_var_anl,
              x_aesi = count_flags[1:2],
              vars_aesi = flag_var_aesi
            )
          )
        )
      )
    )
  }

  condition6 <- count_events && is.character(flag_var_aesi)
  if (condition6) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = count_patients_with_flags(
          var = "USUBJID_AESEQ",
          flag_variables = flag_var_aesi_label,
          table_names = paste0("count_events_", flag_var_aesi),
          .stats = "count",
          .formats = c(count = "xx"),
          .indent_mods = 1L
        ),
        env = list(flag_var_aesi = flag_var_aesi)
      )
    )

    table_anl_list <- add_expr(
      table_anl_list,
      substitute(
        expr = insert_rrow(
          rrow(
            "Medical concepts: number of adverse events which are part of",
            ""
          ),
          at = position
        ),
        env = list(
          position = sum(
            2,
            h_count_rows(
              x_anl = count_flags,
              vars_anl = flag_var_anl,
              x_aesi = count_flags,
              vars_aesi = flag_var_aesi
            )
          )
        )
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
      col_info(result_parent) <- col_info(result_anl)
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

  if (any(all_conditions)) {

    table_list <- add_expr(
      table_list,
      quote(
        expr = result <- rbind(
          result_anl[1:2, ],
          result_parent,
          result_anl[3:nrow(result_anl), ]
        )
      )
    )

  } else {

    table_list <- add_expr(
      table_list,
      quote(
        result <- rbind(result_anl, result_parent)
      )
    )
  }

  print_expr <- list(quote(result))

  y$table <- bracket_expr(c(table_list, print_expr))

  y
}

#' Teal Module: Adverse Events Summary
#'
#' @inheritParams module_arguments
#' @inheritParams template_events_summary
#' @param dthfl_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'  variable for subject death flag from `parentname`. Records with `"Y"`` are summarized in
#'  the table row for "Total number of deaths".
#' @param dcsreas_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr variable
#'   for study discontinuation reason from `parentname`. Records with `"ADVERSE EVENTS"` are
#'   summarized in the table row for "Total number of patients withdrawn from study due to an AE".
#' @param flag_var_anl ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   vector with names of flag variables from `dataset` used to count adverse event
#'   sub-groups (e.g. Serious events, Related events, etc.). Variable labels are used
#'   as table row names if they exist.
#' @param flag_var_aesi ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   vector with names of flag variables from `dataset` used to count adverse event
#'   special interest groups. All flag variables must be of type `logical`. Variable
#'   labels are used as table row names if they exist.
#' @param aeseq_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr variable
#' for adverse events sequence number from `dataset`. Used for counting total number of events.
#'
#' @export
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE) %>%
#'   mutate(
#'     DTHFL = case_when(  # nolint
#'       !is.na(DTHDT) ~ "Y",
#'       TRUE ~ ""
#'     )
#'   ) %>%
#'   rtables::var_relabel(
#'     DTHFL = "Subject Death Flag"
#'   )
#'
#' ADAE <- radae(cached = TRUE)
#'
#' add_event_flags <- function(dat) {
#'   dat %>%
#'     dplyr::mutate(
#'       TMPFL_SER = AESER == "Y",
#'       TMPFL_REL = AEREL == "Y",
#'       TMPFL_GR5 = AETOXGR == "5",
#'       TMP_SMQ01 = !is.na(SMQ01NAM),
#'       TMP_SMQ02 = !is.na(SMQ02NAM),
#'       TMP_CQ01 = !is.na(CQ01NAM)
#'     ) %>%
#'     rtables::var_relabel(
#'       TMPFL_SER = "Serious AE",
#'       TMPFL_REL = "Related AE",
#'       TMPFL_GR5 = "Grade 5 AE",
#'       TMP_SMQ01 = aesi_label(ADAE$SMQ01NAM, ADAE$SMQ01SC),
#'       TMP_SMQ02 = aesi_label("Y.9.9.9.9/Z.9.9.9.9 AESI"),
#'       TMP_CQ01 = aesi_label(ADAE$CQ01NAM)
#'     )
#' }
#'
#' # Generating user-defined event flags.
#' ADAE <- ADAE %>% add_event_flags()
#'
#' ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
#' aesi_vars <- names(ADAE)[startsWith(names(ADAE), "TMP_")]
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code =
#'           'ADSL <- radsl(cached = TRUE) %>%
#'             mutate(
#'               DTHFL = case_when(  # nolint
#'                 !is.na(DTHDT) ~ "Y",
#'                 TRUE ~ ""
#'               )
#'             ) %>%
#'             rtables::var_relabel(
#'               DTHFL = "Subject Death Flag"
#'             )'),
#'     cdisc_dataset("ADAE", ADAE, code =
#'           'ADAE <- radae(cached = TRUE)
#'           add_event_flags <- function(dat) {
#'             dat %>%
#'               dplyr::mutate(
#'                 TMPFL_SER = AESER == "Y",
#'                 TMPFL_REL = AEREL == "Y",
#'                 TMPFL_GR5 = AETOXGR == "5",
#'                 TMP_SMQ01 = !is.na(SMQ01NAM),
#'                 TMP_SMQ02 = !is.na(SMQ02NAM),
#'                 TMP_CQ01 = !is.na(CQ01NAM)
#'               ) %>%
#'               rtables::var_relabel(
#'                 TMPFL_SER = "Serious AE",
#'                 TMPFL_REL = "Related AE",
#'                 TMPFL_GR5 = "Grade 5 AE",
#'                 TMP_SMQ01 = aesi_label(ADAE$SMQ01NAM, ADAE$SMQ01SC),
#'                 TMP_SMQ02 = aesi_label("Y.9.9.9.9/Z.9.9.9.9 AESI"),
#'                 TMP_CQ01 = aesi_label(ADAE$CQ01NAM)
#'               )
#'           }
#'           # Generating user-defined event flags.
#'           ADAE <- ADAE %>% add_event_flags()')
#'     ),
#'   modules = root_modules(
#'     tm_t_events_summary(
#'       label = "Adverse Events Summary",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(
#'         choices = variable_choices("ADSL", c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       flag_var_anl = choices_selected(
#'         choices = variable_choices("ADAE", ae_anl_vars),
#'         selected = ae_anl_vars[1],
#'         keep_order = TRUE,
#'         fixed = FALSE
#'       ),
#'       flag_var_aesi = choices_selected(
#'         choices = variable_choices("ADAE", aesi_vars),
#'         selected = aesi_vars[1],
#'         keep_order = TRUE,
#'         fixed = FALSE
#'       ),
#'       add_total = TRUE
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_events_summary <- function(label,
                                dataname,
                                parentname = ifelse(
                                  is(arm_var, "data_extract_spec"),
                                  datanames_input(arm_var),
                                  "ADSL"
                                ),
                                arm_var,
                                flag_var_anl = NULL,
                                flag_var_aesi = NULL,
                                dthfl_var = choices_selected(
                                  variable_choices(parentname, "DTHFL"), "DTHFL", fixed = TRUE
                                ),
                                dcsreas_var = choices_selected(
                                  variable_choices(parentname, "DCSREAS"), "DCSREAS", fixed = TRUE
                                ),
                                llt = choices_selected(
                                  variable_choices(dataname, "AEDECOD"), "AEDECOD", fixed = TRUE
                                ),
                                aeseq_var = choices_selected(
                                  variable_choices(dataname, "AESEQ"), "AESEQ", fixed = TRUE
                                ),
                                add_total = TRUE,
                                count_subj = TRUE,
                                count_pt = TRUE,
                                count_events = TRUE,
                                pre_output = NULL,
                                post_output = NULL) {

  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is_logical_single(add_total),
    is_logical_single(count_subj),
    is_logical_single(count_pt),
    is_logical_single(count_events),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    dthfl_var = cs_to_des_select(dthfl_var, dataname = parentname),
    dcsreas_var = cs_to_des_select(dcsreas_var, dataname = parentname),
    flag_var_anl = if_not_null(
      flag_var_anl,
      cs_to_des_select(flag_var_anl, dataname = dataname, multiple = TRUE)
    ),
    flag_var_aesi = if_not_null(
      flag_var_aesi,
      cs_to_des_select(flag_var_aesi, dataname = dataname, multiple = TRUE)
    ),
    aeseq_var = cs_to_des_select(aeseq_var, dataname = dataname),
    llt = cs_to_des_select(llt, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_t_events_summary,
    ui_args = c(data_extract_list, args),
    server = srv_t_events_summary,
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
ui_t_events_summary <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$dthfl_var,
    a$dcsreas_var,
    a$flag_var_anl,
    a$flag_var_aesi,
    a$aeseq_var,
    a$llt
  )

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "dthfl_var", "dcsreas_var", "flag_var_anl", "flag_var_aesi", "aeseq_var", "llt")]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      if_not_null(
        a$flag_var_anl,
        data_extract_input(
          id = ns("flag_var_anl"),
          label = "Event Flag Variables",
          data_extract_spec = a$flag_var_anl,
          is_single_dataset = is_single_dataset_value
        )
      ),
      if_not_null(
        a$flag_var_aesi,
        data_extract_input(
          id = ns("flag_var_aesi"),
          label = "AE Basket Flag Variables",
          data_extract_spec = a$flag_var_aesi,
          is_single_dataset = is_single_dataset_value
        )
      ),
      checkboxInput(
        ns("add_total"),
        "Add All Patients column",
        value = a$add_total),
      tags$label("Table Settings"),
      checkboxInput(
        ns("count_subj"),
        "Count patients",
        value = a$count_subj),
      checkboxInput(
        ns("count_pt"),
        "Count preferred terms",
        value = a$count_pt),
      checkboxInput(
        ns("count_events"),
        "Count events",
        value = a$count_events),
      panel_group(
        panel_item(
          "Additional Variables Info",
          data_extract_input(
            id = ns("dthfl_var"),
            label = "Death Flag Variable",
            data_extract_spec = a$dthfl_var,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("dcsreas_var"),
            label = "Study Discontinuation Reason Variable",
            data_extract_spec = a$dcsreas_var,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("aeseq_var"),
            label = "AE Sequence Variable",
            data_extract_spec = a$aeseq_var,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("llt"),
            label = "AE Term Variable",
            data_extract_spec = a$llt,
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
srv_t_events_summary <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 parentname,
                                 arm_var,
                                 dthfl_var,
                                 dcsreas_var,
                                 flag_var_anl,
                                 flag_var_aesi,
                                 aeseq_var,
                                 llt,
                                 label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  data_extract_vars <- list(arm_var, dthfl_var, dcsreas_var, aeseq_var, llt)
  data_extract_inputs <- c("arm_var", "dthfl_var", "dcsreas_var", "aeseq_var", "llt")

  if (!is.null(flag_var_anl)) {
    data_extract_vars <- c(data_extract_vars, list(flag_var_anl))
    data_extract_inputs <- c(data_extract_inputs, "flag_var_anl")
  }

  if (!is.null(flag_var_aesi)) {
    data_extract_vars <- c(data_extract_vars, list(flag_var_aesi))
    data_extract_inputs <- c(data_extract_inputs, "flag_var_aesi")
  }

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = data_extract_vars,
    input_id = data_extract_inputs,
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, dthfl_var, dcsreas_var),
    input_id = c("arm_var", "dthfl_var", "dcsreas_var"),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_dthfl_var <- as.vector(anl_m$columns_source$dthfl_var)
    input_dcsreas_var <- as.vector(anl_m$columns_source$dcsreas_var)
    input_flag_var_anl <- if_not_null(flag_var_anl, as.vector(anl_m$columns_source$flag_var_anl))
    input_flag_var_aesi <- if_not_null(flag_var_aesi, as.vector(anl_m$columns_source$flag_var_aesi))
    input_aeseq_var <- as.vector(anl_m$columns_source$aeseq_var)
    input_llt <- as.vector(anl_m$columns_source$llt)

    validate(need(input_arm_var, "Please select a treatment variable"))
    validate(
      need(is.factor(adsl_filtered[[input_arm_var]]), "Treatment variable is not a factor.")
    )

    # validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_dthfl_var, input_dcsreas_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_flag_var_anl, input_flag_var_aesi, input_aeseq_var, input_llt),
      arm_var = input_arm_var,
      max_n_levels_armvar = NULL,
      min_nrow = 1
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

    input_flag_var_anl <- as.vector(anl_m$columns_source$flag_var_anl)
    input_flag_var_aesi <- as.vector(anl_m$columns_source$flag_var_aesi)

    my_calls <- template_events_summary(
      anl_name = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      dthfl_var = as.vector(anl_m$columns_source$dthfl_var),
      dcsreas_var = as.vector(anl_m$columns_source$dcsreas_var),
      flag_var_anl = if (length(input_flag_var_anl) != 0) input_flag_var_anl else NULL,
      flag_var_aesi = if (length(input_flag_var_aesi) != 0) input_flag_var_aesi else NULL,
      aeseq_var = as.vector(anl_m$columns_source$aeseq_var),
      llt = as.vector(anl_m$columns_source$llt),
      add_total = input$add_total,
      count_subj = input$count_subj,
      count_pt = input$count_pt,
      count_events = input$count_events
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
    datanames = get_extract_datanames(data_extract_vars),
    modal_title = "Adverse Event Summary Table",
    code_header = label
  )

}


#' @noRd
h_count_rows <- function(x_anl = NULL, x_aesi = NULL, vars_anl = NULL, vars_aesi = NULL) {

  assert_that(
    is_logical_vector(x_anl) || is.null(x_anl),
    is_character_vector(vars_anl) || is.null(vars_anl),
    is_logical_vector(x_aesi) || is.null(x_aesi),
    is_character_vector(vars_aesi) || is.null(vars_aesi)
  )

  n_anl <- sum(as.numeric(x_anl))
  n_aesi <- sum(as.numeric(x_aesi))

  n_anl_vars <- length(vars_anl)
  n_aesi_vars <- length(vars_aesi)

  result <- 0

  if (n_anl_vars > 0) {
    result <- result + ((n_anl - 1) * n_anl_vars) + n_anl
  }

  if (n_aesi_vars > 0) {
    result <- result + ((n_aesi - 1) * n_aesi_vars) + n_aesi + n_anl_vars
  }

  result

}
