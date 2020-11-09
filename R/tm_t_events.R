#' Teal Module: Events by Term
#'
#' @name events_by_term
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' library(tern)
#'
#' adsl <- radsl(cached = TRUE)
#' adae <- radae(cached = TRUE)
#'
NULL

#' @describeIn events_by_term create the expression corresponding to the analysis.
#'
#' @param event_type (`string`)\cr type of event that is summarized (e.g. adverse event, treatment).
#'   Default is "event".
#'
#' @export
#' @examples
#'
#' # Generate an expression for the analysis of events.
#' a <- template_events(
#'   dataname = "adae",
#'   parentname = "adsl",
#'   arm_var = "ACTARMCD",
#'   hlt = "AEBODSYS",
#'   llt = "AEDECOD"
#' )
#'
#' styled_expr(a$data)
#' styled_expr(a$layout)
#' styled_expr(a$table)
#' styled_expr(a$prune)
#' styled_expr(a$sort)
#'
#' b <- mapply(expr = a, FUN = eval)
#' b$data
#' b$layout
#' b$table
#' b$prune
#' b$sort
#'
template_events <- function(
  dataname,
  parentname,
  arm_var,
  hlt,
  llt,
  add_total = TRUE,
  event_type = "event") {

  # Data.
  y <- list()
  y$data <- substitute(
    expr = anl <- df,
    env = list(
      df = as.name(dataname)
    )
  )

  # Layout.
  layout_list <- list()
  layout_list <- add_expr(layout_list, substitute(basic_table()))
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
      substitute(
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
  }

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

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )


  col_counts <- substitute(
    expr = table(parentname$arm_var),
    env = list(parentname = as.name(parentname), arm_var = arm_var)
  )

  if (add_total) {
    col_counts <- substitute(
      expr = c(col_counts, "All Patients" = sum(col_counts))
    )
  }

  # Full table.
  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl, col_counts = col_counts),
    env = list(df = as.name(dataname), col_counts = col_counts)
  )

  # Pruned table.
  y$prune <- substitute(
    expr = pruned_result <- result %>% prune_table()
  )

  # Sort pruned table.
  if (one_term) {
    term_var <- ifelse(is.null(hlt), llt, hlt)

    y$sort <- substitute(
      expr = pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(path =  c(term_var), scorefun = score_occurrences),
      env = list(term_var = term_var)
    )
  } else {
    y$sort <- substitute(
      expr = pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(path =  c(hlt), scorefun = cont_n_allcols) %>%
        sort_at_path(path =  c(hlt, "*", llt), scorefun = score_occurrences),
      env = list(llt = llt, hlt = hlt)
    )
  }

  y

}

#' @describeIn events_by_term teal module for events by term.
#' @export
#'
#' @examples
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADAE", adae, code = "ADAE <- radae(cached = TRUE)"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_events(
#'       label = "Adverse Event Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       llt = choices_selected(
#'         choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
#'         selected = c("AEDECOD")
#'        ),
#'       hlt = choices_selected(
#'         choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
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
                        arm_var,
                        hlt,
                        llt,
                        add_total = TRUE,
                        event_type = "event") {
  module(
    label = label,
    ui = function(id, datasets) {
      ns <- NS(id)
      htmlOutput(ns("tbd"))
    },
    server = function(input, output, session, datasets) {
      output$tbd <- renderUI({
        p("Module is currently refactored")
      })
    },
    filters = "ADSL"
  )
}


# REFACTOR
# nolint start
# tm_t_events <- function(label,
#                         dataname,
#                         arm_var,
#                         hlt,
#                         llt,
#                         add_total = TRUE,
#                         event_type = "event") {
#
#   stop_if_not(list(is_character_single(label), "Label should be single (i.e. not vector) character type of object"))
#   stop_if_not(list(is_character_vector(dataname), "Dataname should vector of characters"))
#   stopifnot(is.choices_selected(arm_var))
#   stopifnot(is.choices_selected(hlt))
#   stopifnot(is.choices_selected(llt))
#   stopifnot(is_logical_single(add_total))
#   stop_if_not(is_character_single(event_type))
#   args <- as.list(environment())
#   module(
#     label = label,
#     ui = ui_t_events_byterm,
#     server = srv_t_events_byterm,
#     ui_args = args,
#     server_args = list(
#       dataname = dataname,
#       event_type = event_type
#     ),
#     filters = dataname
#   )
# }
#
# ui_t_events_byterm <- function(id, ...){
#   ns <- NS(id)
#   a <- list(...)
#   standard_layout(
#     output = white_small_well(uiOutput(ns("table"))),
#     encoding = div(
#       tags$label("Encodings", class = "text-primary"),
#       helpText("Analysis data:", tags$code(a$dataname)),
#       optionalSelectInput(
#         ns("arm_var"),
#         "Arm Variable",
#         a$arm_var$choices,
#         a$arm_var$selected,
#         multiple = FALSE,
#         fixed = a$arm_var$fixed),
#       optionalSelectInput(
#         ns("hlt"),
#         "Event High Level Term",
#         a$hlt$choices,
#         a$hlt$selected,
#         multiple = FALSE,
#         fixed = a$hlt$fixed),
#       optionalSelectInput(
#         ns("llt"),
#         "Event Low Level Term",
#         a$llt$choices,
#         a$llt$selected,
#         multiple = FALSE,
#         fixed = a$llt$fixed),
#       checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total)
#     ),
#     forms = get_rcode_ui(ns("rcode")),
#     pre_output = a$pre_output,
#     post_output = a$post_output
#   )
# }
#
# srv_t_events_byterm <- function(input, output, session, datasets, dataname, event_type){
#   init_chunks()
#   output$table <- renderUI({
#     adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
#     anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
#
#     arm_var <- input$arm_var
#     add_total <- input$add_total
#     hlt <- input$hlt
#     llt <- input$llt
#
#     validate(need(is.logical(add_total), "add total is not logical"))
#     validate_has_elements(llt, "Please select \"LOW LEVEL TERM\" variable")
#     validate_has_elements(arm_var, "please select 'arm variables'")
#     validate_has_variable(adsl_filtered, arm_var, "arm variable does not exist")
#     validate_has_data(adsl_filtered, min_nrow = 1)
#     validate_has_data(anl_filtered, min_nrow = 1)
#
#     adsl_name <- "ADSL_FILTERED"
#     assign(adsl_name, adsl_filtered)
#     anl_name <- paste0(dataname, "_FILTERED")
#     assign(anl_name, anl_filtered)
#
#     chunks_reset(envir = environment())
#
#     adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
#     anl_vars <- c("USUBJID", "STUDYID", llt, hlt)
#
#     chunks_push(
#       call(
#         "<-",
#         as.name("ADSL_S"),
#         call(
#           "%>%",
#           as.name(adsl_name),
#           as.call(c(
#             list(quote(dplyr::select)),
#             lapply(adsl_vars, as.name)
#           ))
#         )
#       )
#     )
#
#     chunks_push(
#       call(
#         "<-",
#         as.name("ANL_S"),
#         call(
#           "%>%",
#           as.name(anl_name),
#           as.call(c(
#             list(quote(dplyr::select)),
#             lapply(anl_vars, as.name)
#           ))
#         )
#       )
#     )
#
#     chunks_push(
#       call(
#         "<-",
#         as.name("ANL_MERGED"),
#         call(
#           "%>%",
#           call(
#             "%>%",
#             as.call(c(
#               quote(merge),
#               list(
#                 x = as.name("ADSL_S"),
#                 y = as.name("ANL_S"),
#                 all.x = FALSE,
#                 all.y = FALSE,
#                 by = c("USUBJID", "STUDYID")
#               )
#             )),
#             as.call(c(
#               quote(df_explicit_na),
#               list(
#                 omit_columns = c("USUBJID", "STUDYID", arm_var),
#                 char_as_factor =  FALSE
#               )
#             ))
#           ),
#           teal.devel::get_relabel_call(
#             labels = c(
#               datasets$get_variable_labels("ADSL", adsl_vars),
#               datasets$get_variable_labels(dataname, anl_vars)
#             )
#           )
#         )
#       )
#     )
#
#     total <- if (add_total) "All Patients" else NULL # nolint
#
#     chunks_push(bquote({
#       tbl <- t_events_per_term_id(
#         terms = ANL_MERGED[, .(c(hlt, llt)), drop = FALSE],
#         id = ANL_MERGED[["USUBJID"]],
#         col_by = as.factor(ANL_MERGED[[.(arm_var)]]),
#         col_N = table(.(as.name(adsl_name))[[.(arm_var)]]),
#         total = .(total),
#         event_type = .(event_type)
#       )
#       tbl
#     }))
#
#     chunks_safe_eval()
#     tbl <- chunks_get_var("tbl")
#     as_html(tbl)
#   })
#
#   callModule(
#     module = get_rcode_srv,
#     id = "rcode",
#     datasets = datasets,
#     datanames = dataname,
#     modal_title = "R Code for the Current Event Table",
#     code_header = "Event Table"
#   )
# }
# nolint end
