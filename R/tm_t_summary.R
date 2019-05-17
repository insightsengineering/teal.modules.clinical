#' Summarize Variables Module
#'
#' This module is for \code{\link[tern]{t_summary}}.
#'
#' @inheritParams tm_t_tte
#' @param summarize_vars \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#'   for variable names that can be used for summary
#'
#' @export
#'
#' @template author_waddella
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ASL <- radsl(seed = 1)
#' keys(ASL) <- c("STUDYID", "USUBJID")
#'
#' x <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = 'library(tern)
#'             ASL <- random.cdisc.data::radsl(seed = 1)
#'             keys(ASL) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_t_summary(
#'       label = "Demographic Table",
#'       dataname = "ASL",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       summarize_vars = choices_selected(c("SEX", "RACE", "BMRKR2"), c("SEX", "RACE")))
#'     )
#'   )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
tm_t_summary <- function(label,
                         dataname,
                         arm_var,
                         summarize_vars,
                         pre_output = NULL,
                         post_output = NULL) {

  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(summarize_vars))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_summary,
    ui = ui_t_summary,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )

}

ui_t_summary <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = teal.devel::white_small_well(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var$choices,
                          a$arm_var$selected,
                          multiple = FALSE),
      optionalSelectInput(ns("summarize_vars"),
                          "Summarize Variables",
                          a$summarize_vars$choices,
                          a$summarize_vars$selected,
                          multiple = TRUE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_t_summary <- function(input, output, session, datasets, dataname) {
  use_chunks(session)

  table_call <- reactive({
    anl_f <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    arm_var <- input$arm_var
    summarize_vars <- input$summarize_vars

    teal.devel::validate_has_data(anl_f, min_nrow = 3)
    validate(need(!is.null(summarize_vars), "please select 'summarize variables'"))
    validate(need(all(summarize_vars %in% names(anl_f)), "not all variables available"))
    validate(need(anl_f[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% anl_f[[arm_var]]), "arm values can not contain empty strings ''"))

    data_name <- paste0(dataname, "_FILTERED")
    assign(data_name, anl_f)

    renew_chunk_environment(envir = environment())
    renew_chunks()

    table_chunk_expr <- bquote(
      tbl <- t_summary(
        x = .(as.name(data_name))[, .(summarize_vars), drop = FALSE],
        col_by = as.factor(.(as.name(data_name))[[.(arm_var)]]),
        total = "All Patients",
        useNA = "ifany"
      )
    )
    set_chunk("tm_t_summary_tbl", expression = table_chunk_expr)

    return(invisible)
  })

  output$table <- renderUI({
    table_call()
    eval_remaining()
    tbl <- get_envir_chunks()$tbl
    validate(need(is(tbl, "rtable"), "Evaluation with tern t_summary failed."))

    rtables::as_html(tbl)

  })

  observeEvent(input$show_rcode, {
    teal.devel::show_rcode_modal(
      title = "Summary",
      rcode = get_rcode(
        datasets = datasets,
        dataname = dataname,
        title = "Summary table"
      )
    )
  })
}
