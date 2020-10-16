#' Adverse Events Summary Teal Module
#'
#' @description This module produces an Adverse Event summary table that matches the
#'   STREAM template \code{AET01}.
#'
#' @inheritParams tm_t_events
#' @param flag_var_anl vector with names of flag variables from \code{dataset} to count adverse
#' event sub-groups (e.g. Serious events, Related events, etc.). All flag variables must be of type
#' \code{logical}. If a variable has a label attribute then it will be used as a table row name.
#' @param flag_var_aesi vector with names of flag variables from \code{dataset} to count adverse
#' event special interest groups. All flag variables must be of type \code{logical}. If a variable
#' has a label attribute then it will be used as a table row name.
#' @param count_subj \code{(logical)} whether to show count of unique subjects based on \code{USUBJID}.
#' Only applies if event flag variables are provided.
#' @param count_pt whether to show count of unique preferred terms based on \code{AEDECOD}.
#' Only applies if event flag variables are provided.
#' @param count_events whether to show count of events. Only applies if event flag variables are provided.
#'
#' @details
#' This module should be used to produce summaries based only on events-level dataset (e.g. \code{ADAE}).
#' For tabulation of variables from subject-level datasets like \code{ADSL}, use \code{\link{tm_t_summary}}.
#'
#' @return an \code{\link[teal]{module}} object
#'
#' @export
#'
#' @examples
#'
#' # save the code below in a file app.R in order to use get_code() functionality
#' # by uncommenting code = get_code("app.R")).
#'
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#'
#' # define analysis subgroups from ADAE
#' add_event_flags <- function(dat){
#'   dat %>%
#'     dplyr::mutate(
#'       TMPFL_SER = AESER == "Y",
#'       TMPFL_REL = AEREL == "Y",
#'       TMPFL_GR5 = AETOXGR == "5",
#'       TMP_SMQ01 = AEDECOD %in% c("dcd B.2.1.2.1", "dcd C.2.1.2.1"),
#'       TMP_CQ01 = AEDECOD %in% c("dcd D.1.1.1.1", "dcd A.1.1.1.2")
#'     ) %>%
#'     rtables::var_relabel(
#'       TMPFL_SER = "Serious AE",
#'       TMPFL_REL = "Related AE",
#'       TMPFL_GR5 = "Grade 5 AE",
#'       TMP_SMQ01 = "B.2.1.2.1/C.2.1.2.1 (SMQ) (broad)",
#'       TMP_CQ01 = "AESI D1.A1"
#'     )
#'  }
#'
#'  ADAE <- ADAE %>%
#'    add_event_flags()
#'
#'
#' anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
#' aesi_vars <- names(ADAE)[startsWith(names(ADAE), "TMP_SMQ") | startsWith(names(ADAE), "TMP_CQ")]
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADAE", ADAE,
#'       code = "ADAE <- radae(cached = TRUE)
#'               add_event_flags <- function(dat){
#'               dat %>%
#'                 dplyr::mutate(
#'                   TMPFL_SER = AESER == 'Y',
#'                   TMPFL_REL = AEREL == 'Y',
#'                   TMPFL_GR5 = AETOXGR == '5',
#'                   TMP_SMQ01 = AEDECOD %in% c('dcd B.2.1.2.1', 'dcd C.2.1.2.1'),
#'                   TMP_CQ01 = AEDECOD %in% c('dcd D.1.1.1.1', 'dcd A.1.1.1.2')
#'                 ) %>%
#'                 var_relabel(
#'                   TMPFL_SER = 'Serious AE',
#'                   TMPFL_REL = 'Related AE',
#'                   TMPFL_GR5 = 'Grade 5 AE',
#'                   TMP_SMQ01 = 'B.2.1.2.1/C.2.1.2.1 (SMQ) (broad)',
#'                   TMP_CQ01 = 'AESI D1.A1'
#'                 )
#'               }
#'            ADAE <- ADAE %>%
#'              add_event_flags()"
#'       ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_events_summary(
#'       label = "Adverse Event Summary",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       flag_var_anl = anl_vars,
#'       flag_var_aesi = aesi_vars,
#'       add_total = TRUE
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_events_summary <- function(label,
                                dataname,
                                arm_var,
                                add_total = TRUE,
                                flag_var_anl = NULL,
                                flag_var_aesi = NULL,
                                count_subj = TRUE,
                                count_pt = FALSE,
                                count_events = FALSE) {
  stop_if_not(list(
    is_character_single(label),
    "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is_character_vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is_logical_single(add_total))
  stop_if_not(list(
    is_character_vector(flag_var_anl) || is.null(flag_var_anl),
    "flag_var_anl should be NULL or a character vector object."))
  stop_if_not(list(
    is_character_vector(flag_var_aesi) || is.null(flag_var_aesi),
    "flag_var_aesi should be NULL or a character vector object."))
  stopifnot(is_logical_single(count_subj))
  stopifnot(is_logical_single(count_pt))
  stopifnot(is_logical_single(count_events))

  args <- as.list(environment())
  module(
    label = label,
    ui = ui_t_events_summary,
    server = srv_t_events_summary,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      flag_var_anl = flag_var_anl,
      flag_var_aesi = flag_var_aesi
    ),
    filters = dataname
  )

}

ui_t_events_summary <- function(id, ...){
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
      checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total),
      tags$label("Table Settings"),
      checkboxInput(ns("count_subj"), "Count patients", value = a$count_subj),
      checkboxInput(ns("count_pt"), "Count preferred terms", value = a$count_pt),
      checkboxInput(ns("count_events"), "Count events", value = a$count_events)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_t_events_summary <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 flag_var_anl = NULL,
                                 flag_var_aesi = NULL) {
  init_chunks()

  output$table <- renderUI({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    arm_var <- input$arm_var
    add_total <- input$add_total
    count_subj <- input$count_subj
    count_pt <- input$count_pt
    count_events <- input$count_events

    validate(need(is.logical(add_total), "add total is not logical"))
    validate_has_elements(arm_var, "please select 'arm variables'")
    validate_has_variable(adsl_filtered, arm_var, "arm variable does not exist")
    validate_has_variable(anl_filtered, "AEDECOD", "AEDECOD variable does not exist")
    validate_has_data(adsl_filtered, min_nrow = 1)
    validate_has_data(anl_filtered, min_nrow = 1)
    if (!is.null(flag_var_anl)) {
      validate_in(
        flag_var_anl,
        names(anl_filtered),
        "Not all flag_var_anl variables exist in dataset.")
      validate(need(
        all(vapply(anl_filtered[, flag_var_anl], is.logical, FUN.VALUE = logical(1))),
        "Not all variables from flag_var_anl are of type logical."))
    }
    if (!is.null(flag_var_aesi)) {
      validate_in(
        flag_var_aesi,
        names(anl_filtered),
        "Not all flag_var_aesi variables exist in dataset.")
      validate(need(
        all(vapply(anl_filtered[, flag_var_aesi], is.logical, FUN.VALUE = logical(1))),
        "Not all variables from flag_var_aesi are of type logical."))
    }

    adsl_name <- "ADSL_FILTERED"
    assign(adsl_name, adsl_filtered)
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("STUDYID", "USUBJID", arm_var))
    anl_vars <- unique(c("STUDYID", "USUBJID", "AEDECOD", flag_var_anl, flag_var_aesi))
    chunks_push(bquote({
      ADSL_S <- ADSL_FILTERED[, .(adsl_vars)] # nolint
      ANL_S <- .(as.name(anl_name))[, .(anl_vars)] # nolint
      ANL_MERGED <- merge(ADSL_S, ANL_S, all.x = FALSE, all.y = FALSE, by = c("STUDYID", "USUBJID")) # nolint
    })
    )

    chunks_push(
      call(
        "<-",
        as.name("ANL_MERGED"),
        call(
          "%>%",
          as.name("ANL_MERGED"),
          teal.devel::get_relabel_call(
            labels = c(
              datasets$get_variable_labels("ADSL", adsl_vars),
              datasets$get_variable_labels(dataname, anl_vars)
            )
          )
        )
      )
    )

    total <- if (add_total) "All Patients" else NULL # nolint

    chunks_push(bquote({

      tbl_ae_overall <- t_el_events_per_term_id(
        id = ANL_MERGED[["USUBJID"]],
        col_by = ANL_MERGED[[.(arm_var)]],
        col_N = table(ADSL_S[[.(arm_var)]]),
        total = .(total),
        total_events = "Total number of events",
        subjects_with_events = "Total number of patients with at least one adverse event"
      )
    }))

    if (isTRUE(count_subj) && !is.null(flag_var_anl)) {

      chunks_push(bquote({

        tbl_anl_subj <- t_count_unique(
          x = ANL_MERGED[["USUBJID"]],
          col_by = ANL_MERGED[[.(arm_var)]],
          subgroups = ANL_MERGED[, .(flag_var_anl), drop  = FALSE],
          col_N = table(ADSL_S[[.(arm_var)]]),
          total = .(total),
          na_rm = FALSE,
          denominator = "N"
        )

        tbl_anl_subj <- insert_rrow(indent(tbl_anl_subj),
                                    rrow("Total number of patients with at least one"))
      }))

    } else {

      chunks_push(bquote({
        tbl_anl_subj <-  empty_rtable()
      }))

    }

    if (isTRUE(count_subj) && !is.null(flag_var_aesi)) {

      chunks_push(bquote({

        tbl_aesi_subj <- t_count_unique(
          x = ANL_MERGED[["USUBJID"]],
          col_by = ANL_MERGED[[.(arm_var)]],
          subgroups = ANL_MERGED[, .(flag_var_aesi), drop  = FALSE],
          col_N = table(ADSL_S[[.(arm_var)]]),
          total = .(total),
          na_rm = FALSE,
          denominator = "N"
        )

        tbl_aesi_subj <- insert_rrow(indent(tbl_aesi_subj),
                                     rrow("Medical concepts: number of patients with"))
      }))

    } else {

      chunks_push(bquote({
        tbl_aesi_subj <-  empty_rtable()
      }))

    }

    if (isTRUE(count_pt) && !is.null(flag_var_anl)) {

      chunks_push(bquote({

        tbl_anl_pt <- t_count_unique(
          x = ANL_MERGED[["AEDECOD"]],
          col_by = ANL_MERGED[[.(arm_var)]],
          subgroups = ANL_MERGED[, .(flag_var_anl), drop  = FALSE],
          col_N = table(ADSL_S[[.(arm_var)]]),
          total = .(total),
          na_rm = FALSE,
          denominator = "omit"
        )

        tbl_anl_pt <- insert_rrow(indent(tbl_anl_pt),
                                  rrow("Total number of unique preferred terms which are"))
      }))

    } else {

      chunks_push(bquote({
        tbl_anl_pt <-  empty_rtable()
      }))

    }


    if (isTRUE(count_pt) && !is.null(flag_var_aesi)) {

      chunks_push(bquote({

        tbl_aesi_pt <- t_count_unique(
          x = ANL_MERGED[["AEDECOD"]],
          col_by = ANL_MERGED[[.(arm_var)]],
          subgroups = ANL_MERGED[, .(flag_var_aesi), drop  = FALSE],
          col_N = table(ADSL_S[[.(arm_var)]]),
          total = .(total),
          na_rm = FALSE,
          denominator = "omit"
        )

        tbl_aesi_pt <- insert_rrow(
          indent(tbl_aesi_pt),
          rrow("Medical concepts: number of unique preferred terms which are part of")
        )
      }))

    } else {

      chunks_push(bquote({
        tbl_aesi_pt <-  empty_rtable()
      }))

    }


    if (isTRUE(count_events) && !is.null(flag_var_anl)) {

      chunks_push(bquote({

        tbl_anl_events <- t_count_true(
          x = ANL_MERGED[, .(flag_var_anl), drop  = FALSE],
          col_by = ANL_MERGED[[.(arm_var)]],
          col_N =  table(ADSL_S[[.(arm_var)]]),
          total = .(total),
          denominator = "omit"
        )

        tbl_anl_events <- insert_rrow(
          indent(tbl_anl_events),
          rrow("Total number of adverse events which are")
        )

      }))

    } else {

      chunks_push(bquote({
        tbl_anl_events <-  empty_rtable()
      }))
    }

    if (isTRUE(count_events) && !is.null(flag_var_aesi)) {

      chunks_push(bquote({

        tbl_aesi_events <- t_count_true(
          x = ANL_MERGED[, .(flag_var_aesi), drop  = FALSE],
          col_by = ANL_MERGED[[.(arm_var)]],
          col_N =  table(ADSL_S[[.(arm_var)]]),
          total = .(total),
          denominator = "omit"
        )

        tbl_aesi_events <- insert_rrow(
          indent(tbl_aesi_events),
          rrow("Medical concepts: number of adverse events which are part of")
        )

      }))

    } else {

      chunks_push(bquote({
        tbl_aesi_events <-  empty_rtable()
      }))
    }

    chunks_push(
      bquote({
        tbl <- rbindl_rtables(
          list(
            tbl_ae_overall,
            tbl_anl_subj, tbl_anl_pt, tbl_anl_events,
            tbl_aesi_subj, tbl_aesi_pt, tbl_aesi_events
          ),
          gap = 1
        )
        tbl
      })
    )

    chunks_safe_eval()
    result_tbl <- chunks_get_var("tbl")
    as_html(result_tbl)
  })

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
