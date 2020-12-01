#' Teal Module: Adverse Events Summary
#'
#' @name adverse_events_summary
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#'
NULL

#' @describeIn adverse_events_summary creates the expression corresponding
#'   to the analysis. Requires that the variables DTHDT and DCSREAS
#'   exist in `parent_name` dataset. Similarly, requires that variables
#'   STUDYID, USUBJID, AESEQ and AEDECOD exist in `anl_name` dataset.
#' @param flag_var_anl (`character`)\cr vector with names of flag variables from
#'   `dataset` used to count adverse event sub-groups (e.g. Serious events, Related events, etc.).
#'   All flag variables must be of type `logical`. If vector is named,
#'   then names will be used as a table row names.
#' @param flag_var_aesi (`character`)\cr vector with names of flag variables from
#'   `dataset` used to count adverse event special interest groups. All flag variables
#'   must be of type `logical`. If vector is named, then names will be used as a table row names.
#' @param count_subj (`flag`)\cr whether to show count of unique subjects
#'   based on `USUBJID`. Only applies if event flag variables are provided.
#' @param count_pt (`flag`)\cr whether to show count of unique preferred terms based on
#'   `AEDECOD`. Only applies if event flag variables are provided.
#' @param count_events (`flag`)\cr whether to show count of events.
#'   Only applies if event flag variables are provided.
#'
template_events_summary <- function(anl_name,
                                    parent_name,
                                    arm_var,
                                    add_total = TRUE,
                                    flag_var_anl = NULL,
                                    flag_var_aesi = NULL,
                                    count_subj = TRUE,
                                    count_pt = TRUE,
                                    count_events = TRUE) {

  assert_that(
    is.string(anl_name),
    is.string(parent_name),
    is.string(arm_var),
    is.flag(add_total),
    is.character(flag_var_anl) || is.null(NULL),
    is.character(flag_var_aesi) || is.null(NULL),
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
    substitute(
      expr = col_counts <- table(parent_name$arm_var),
      env = list(
        parent_name = as.name(parent_name),
        arm_var = as.name(arm_var)
      )
    )
  )

  if (add_total) {
    data_list <- add_expr(
      data_list,
      quote(
        col_counts <- c(col_counts, "All Patients" = sum(col_counts))
      )
    )
  }

  data_list <- add_expr(
    data_list,
    quote(study_id <- unique(anl[["STUDYID"]]))
  )

  data_list <- add_expr(
    data_list,
    quote(anl <- anl %>% dplyr::mutate(
      AEDECOD = as.character(AEDECOD),
      USUBJID_AESEQ = paste(USUBJID, AESEQ, sep = "@@")
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Layout to be used with `parent_name` dataset
  # because not all subjects may exist in `anl_name` dataset.
  layout_parent_list <- list()
  layout_parent_list <- add_expr(
    layout_parent_list,
    quote(basic_table())
  )
  layout_parent_list <- add_expr(
    layout_parent_list,
    substitute(
      expr = split_cols_by(arm_var),
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
    quote(
      count_values(
        "DTHFL",
        values = "Y",
        .labels = c(count_fraction = "Total number of deaths")
      ) %>%
      count_values(
        "DCSREAS",
        values = "ADVERSE EVENT",
        .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE")
      )
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
      expr = result_parent <- build_table(lyt = lyt_parent, df = df_parent, col_counts = col_counts),
      env = list(df_parent = as.name(parent_name))
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
      expr = split_cols_by(arm_var),
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
        .stats = "count_fraction",
        .labels = c(count_fraction = "Total AEs"),
        table_names = "total_aes"
      )
    )
  )

  table_anl_list <- list()
  table_anl_list <- add_expr(
    table_anl_list,
    quote(result_anl <- build_table(lyt = lyt_anl, df = anl, col_counts = col_counts))
  )

  count_flags <- c(count_subj, count_pt, count_events)

  condition1 <- count_subj && is.character(flag_var_anl)
  if (condition1) {
    layout_anl_list <- add_expr(
      layout_anl_list,
      substitute(
        expr = count_patients_with_flags(
          var = "USUBJID",
          flag_variables = flag_var_anl,
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
          var = "AEDECOD",
          flag_variables = flag_var_anl,
          table_names = paste0("count_pt_", flag_var_anl),
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
          flag_variables = flag_var_anl,
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
          flag_variables = flag_var_aesi,
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
          var = "AEDECOD",
          flag_variables = flag_var_aesi,
          table_names = paste0("count_pt_", flag_var_aesi),
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
          flag_variables = flag_var_aesi,
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

  table_list <- add_expr(
    table_list,
    quote(print(result))
  )

  y$table <- bracket_expr(table_list)

  y
}

#' @describeIn adverse_events_summary teal module for adverse events summary.
#'
#' @export
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE) %>%
#'   mutate(
#'     DTHFL = case_when(  # nolint
#'       !is.na(DTHDT) ~ "Y",
#'       TRUE ~ ""
#'     ),
#'     DCSREAS = as.character(DCSREAS),
#'     DCSREAS = case_when(  # nolint
#'       is.na(DCSREAS) ~ "",
#'       TRUE ~ DCSREAS
#'     )
#'   )
#'
#' adae <- radae(cached = TRUE)
#'
#' # Helper function for generating user-defined event flags.
#' extract_aesi_label <- function(flag, scope = NULL) {
#'   init_lbl <- obj_label(flag)
#'   flag <- unique(flag)[!is.na(unique(flag))]
#'   lbl <- if (length(flag) == 1 & !is.null(scope)) {
#'     scope <- unique(scope)[!is.na(unique(scope))]
#'     paste0(flag, " (", scope, ")")
#'   } else if (length(flag) == 1 & is.null(scope)) {
#'     flag
#'   } else {
#'     init_lbl
#'   }
#'   lbl
#' }
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
#'       TMP_SMQ01 = extract_aesi_label(adae$SMQ01NAM, adae$SMQ01SC),
#'       TMP_SMQ02 = extract_aesi_label("Y.9.9.9.9/Z.9.9.9.9 AESI"),
#'       TMP_CQ01 = extract_aesi_label(adae$CQ01NAM)
#'     )
#' }
#'
#' # Generating user-defined event flags.
#' adae <- adae %>% add_event_flags()
#'
#' ae_anl_vars <- var_labels(adae, fill = TRUE)[startsWith(names(adae), "TMPFL_")] #nolint
#' aesi_vars <- var_labels(adae, fill = TRUE)[startsWith(names(adae), "TMP_")] #nolint
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADAE", adae, code = "ADAE <- radae(cached = TRUE)")
#'     ),
#'   modules = root_modules(
#'     tm_t_events_summary(
#'       label = "Adverse Events Summary",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       add_total = TRUE,
#'       flag_var_anl = ae_anl_vars,
#'       flag_var_aesi = aesi_vars
#'       )
#'     )
#'   )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#'
#'
tm_t_events_summary <- function(label,
                                dataname,
                                arm_var,
                                add_total = TRUE,
                                flag_var_anl = NULL,
                                flag_var_aesi = NULL,
                                count_subj = TRUE,
                                count_pt = TRUE,
                                count_events = TRUE) {

  args <- as.list(environment())
  module(
    label = label,
    server = srv_t_events_summary,
    ui = ui_t_events_summary,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      flag_var_anl = flag_var_anl,
      flag_var_aesi = flag_var_aesi
    ),
    filters = dataname
  )
}



#' @noRd
ui_t_events_summary <- function(id, ...){

  ns <- NS(id)
  args <- list(...)
  standard_layout(
    output = white_small_well(uiOutput(ns("as_html"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(args$dataname)),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        args$arm_var$choices,
        args$arm_var$selected,
        multiple = FALSE,
        fixed = args$arm_var$fixed),
      checkboxInput(
        ns("add_total"),
        "Add All Patients column",
        value = args$add_total),
      tags$label("Table Settings"),
      checkboxInput(
        ns("count_subj"),
        "Count patients",
        value = args$count_subj),
      checkboxInput(
        ns("count_pt"),
        "Count preferred terms",
        value = args$count_pt),
      checkboxInput(
        ns("count_events"),
        "Count events",
        value = args$count_events)
    ),
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    )
 )
}

#' @noRd
srv_t_events_summary <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 label,
                                 flag_var_anl = NULL,
                                 flag_var_aesi = NULL) {
  init_chunks()
  prepared_env <- reactive({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    # validate/validate functions
    if (!is.null(flag_var_anl)){
      assert_that(
        all(names(flag_var_anl) %in% colnames(anl_filtered)),
        msg = "flag variable (flag_var_anl) names must be in ADAE dataset colnames" #nolint
      )

      assert_that(
        all(nchar((gsub(" ", "", flag_var_anl))) > 0),
        msg = paste(
          "Empyt labels in ae_anl_vars: \n",
          paste(
            "Missing label in:",
            names(
              flag_var_anl[which(nchar((gsub(" ", "", flag_var_anl))) == 0)]
              ),
            "\n"
            ),
          paste("`", names(flag_var_anl), "`", sep = "", collapse = "\t"),
          "\n",
          paste("`", flag_var_anl, "`", sep = "", collapse = "\t")
          )
      )

    }
    if (!is.null(flag_var_aesi)){
      assert_that(
        all(names(flag_var_aesi) %in% colnames(anl_filtered)),
        msg = "flag variable (flag_var_aesi) names must be in ADAE dataset colnames" #nolint
      )
      assert_that(
        all(nchar((gsub(" ", "", flag_var_aesi))) > 0),
        msg = paste("Empyt labels in ae_anl_vars: \n",
          paste(
            "Missing label in:",
            names(
              flag_var_aesi[which(nchar((gsub(" ", "", flag_var_aesi))) == 0)]
              ),
            "\n"),

          paste("`", names(flag_var_aesi), "`", sep = "", collapse = "\t"),
          "\n",
          paste("`", flag_var_aesi, "`", sep = "", collapse = "\t"))
      )


    }
    if (is.null(flag_var_anl) && is.null(flag_var_aesi)){
      showNotification("No user specified reporting criteria detected.
                       Only default summary is produced.
                       Table setting options won't customize the output.
                       ", type = "message", duration = NULL)
    }

    validate(
      need(
        is.factor(adsl_filtered[[input$arm_var]]),
        "Arm variable is not a factor."
        )
    )


    # Send data where the analysis lives.
    e <- new.env()
    e[[paste0(dataname, "_FILTERED")]] <- anl_filtered
    e$ADSL_FILTERED <- adsl_filtered #nolint
    e
  })




  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_events_summary(
      anl_name = paste0(dataname, "_FILTERED"),
      parent_name = "ADSL_FILTERED",
      arm_var = input$arm_var,
      add_total = input$add_total,
      count_subj = input$count_subj,
      count_pt = input$count_pt,
      count_events = input$count_events,
      flag_var_anl = flag_var_anl,
      flag_var_aesi = flag_var_aesi
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  output$as_html <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  # Render R code.
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "AE Summary",
      rcode = get_rcode(datasets = datasets, title = input$label)
    )
  })

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
