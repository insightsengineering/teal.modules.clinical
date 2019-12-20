#' Summarize Variables Module
#'
#' This module is for \code{\link[tern]{t_summary}}.
#'
#' @inheritParams tm_t_tte
#' @param summarize_vars \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#'   for variable names that can be used for summary
#'
#' @importFrom dplyr select
#' @importFrom rtables as_html var_relabel
#'
#' @export
#'
#' @template author_waddella
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = 'ADSL <- radsl(cached = TRUE)',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_t_summary(
#'       label = "Demographic Table",
#'       dataname = "ADSL",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       summarize_vars = choices_selected(c("SEX", "RACE", "BMRKR2"), c("SEX", "RACE"))
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
    output = white_small_well(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var$choices,
                          a$arm_var$selected,
                          multiple = FALSE),
      checkboxInput(ns("add_total"), "Add All Patients column", value = TRUE),
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

  init_chunks()

  output$table <- renderUI({
    anl_f <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    arm_var <- input$arm_var
    add_total <- input$add_total
    summarize_vars <- input$summarize_vars

    validate(need(is.logical(add_total), "add total is not logical"))
    validate(need(!is.null(summarize_vars), "please select 'summarize variables'"))
    validate(need(all(summarize_vars %in% names(anl_f)), "not all variables available"))
    validate(need(!is.null(arm_var), "please select 'arm variable'"))
    validate(need(arm_var %in% names(anl_f), "arm variable does not exist"))
    validate_has_data(anl_f, min_nrow = 3)

    data_name <- paste0(dataname, "_FILTERED")
    assign(data_name, anl_f)

    chunks_reset(envir = environment())

    chunks_push(
      call(
        "<-",
        as.name("ANL"),
        Reduce(
          function(x, y) `if`(!is.null(y), call("%>%", x, y), x),
          c(
            as.name(data_name),
            as.call(c(
              list(quote(dplyr::select)),
              lapply(c(summarize_vars, arm_var), as.name)
            )),
            get_relabel_call(datasets, dataname, summarize_vars)
          )
        )
      )
    )

    total <- if (add_total) "All Patients" else NULL

    chunks_push(bquote({
      tbl <- t_summary(
        x = .(as.name("ANL"))[, .(summarize_vars), drop = FALSE],
        col_by = as.factor(.(as.name("ANL"))[[.(arm_var)]]),
        total = .(total),
        useNA = "ifany"
      )
      tbl
    }))

    chunks_safe_eval()

    tbl <- chunks_get_var("tbl")
    as_html(tbl)
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Summary",
      rcode = get_rcode(
        datasets = datasets,
        title = "Summary table"
      )
    )
  })
}
