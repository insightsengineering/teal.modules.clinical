#' Summarize variables with by group module
#'
#' This module produces a summary table that includes row-by variables based on \code{\link[tern]{t_summary_by}}.
#'
#' @inheritParams tm_t_summary
#' @param dataname (\code{character}) analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#' @param by_vars \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#'   for variable names used to split the summary by rows.
#' @param parallel_vars (\code{logical}) used to display \code{summarize_vars} as parallel columns
#'  (\code{FALSE} on default). Can be used only if all chosen analysis variables are numeric.
#' @param denominator for calculating percentages. Only applies to categorical variables.
#'   See "denominator" in \code{\link[tern]{t_summary.factor}} for details.
#'
#' @return a \code{\link[teal]{module}} object
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE) %>%
#' mutate(
#'    CHGCAT1 = case_when(
#'      CHG > 0 ~ "INCREASE",
#'      CHG < 0 ~ "DECREASE",
#'      CHG == 0 ~ "NO CHANGE"
#'    )
#'  ) %>%
#'  var_relabel(CHGCAT1 = "Change from Baseline Category 1")
#'
#' app <- init(
#'  data = cdisc_data(
#'    cdisc_dataset("ADSL", ADSL),
#'    cdisc_dataset("ADLB", ADLB),
#'    code = 'ADSL <- radsl(cached = TRUE)
#'            ADLB <- radlb(cached = TRUE) %>%
#'             mutate(
#'                CHGCAT1 = case_when(
#'                  CHG > 0 ~ "INCREASE",
#'                  CHG < 0 ~ "DECREASE",
#'                  CHG == 0 ~ "NO CHANGE"
#'                )
#'              ) %>%
#'              var_relabel(CHGCAT1 = "Change from Baseline Category 1")',
#'    check = FALSE
#'  ),
#'  modules = root_modules(
#'    tm_t_summary_by(
#'      label = "By-Variable Table",
#'      dataname = "ADLB",
#'      arm_var = choices_selected(
#'        choices = variable_choices(ADSL, c("ARM", "ARMCD")),
#'        selected = "ARM"
#'      ),
#'      by_vars = choices_selected(
#'        choices = variable_choices(ADLB, c("PARAM", "AVISIT")),
#'        selected = c("PARAM", "AVISIT")
#'      ),
#'      summarize_vars = choices_selected(
#'        choices = variable_choices(ADLB, c("AVAL", "CHG", "CHGCAT1")),
#'        selected = c("AVAL")
#'      ),
#'      denominator = "N"
#'    )
#'  )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_summary_by <- function(label,
                         dataname,
                         arm_var,
                         by_vars,
                         summarize_vars,
                         parallel_vars = FALSE,
                         denominator = c("n", "N", "omit"),
                         pre_output = NULL,
                         post_output = NULL) {

  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(by_vars))
  stopifnot(is.choices_selected(summarize_vars))
  denominator <- match.arg(denominator)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_summary_by,
    ui = ui_t_summary_by,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      denominator = denominator
    ),
    filters = dataname
  )

}

ui_t_summary_by <- function(id, ...) {

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

      optionalSelectInput(ns("by_vars"),
                          "Row By Variable",
                          a$by_vars$choices,
                          a$by_vars$selected,
                          multiple = TRUE),

      optionalSelectInput(ns("summarize_vars"),
                          "Summarize Variables",
                          a$summarize_vars$choices,
                          a$summarize_vars$selected,
                          multiple = TRUE),

      checkboxInput(ns("parallel_vars"), "Show summarize variables in parallel", value = a$parallel_vars)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_t_summary_by <- function(input, output, session, datasets, dataname, denominator) {

  init_chunks()

  output$table <- renderUI({
    adsl_filtered <- datasets$get_data("ADSL", reactive = TRUE, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    arm_var <- input$arm_var
    add_total <- input$add_total
    by_vars <- input$by_vars
    summarize_vars <- input$summarize_vars
    parallel_vars <- input$parallel_vars

    validate(need(is.logical(add_total), "add total is not logical"))
    validate(need(is.logical(parallel_vars), "parallel variables is not logical"))
    validate(need(!is.null(summarize_vars), "please select 'summarize variables'"))
    validate(need(all(summarize_vars %in% names(anl_filtered)), "not all variables available"))
    validate(need(!is.null(arm_var), "please select 'arm variable'"))
    validate(need(arm_var %in% names(anl_filtered), "arm variable does not exist"))
    validate_has_data(anl_filtered, min_nrow = 3)
    validate(need(all(by_vars %in% names(anl_filtered)), "not all by-variables available"))

    adsl_name <- "ADSL_FILTERED"
    assign(adsl_name, adsl_filtered)
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    anl_vars <- unique(c("USUBJID", "STUDYID", by_vars, summarize_vars))

    if(parallel_vars){
      validate(need(all(vapply(anl_filtered[summarize_vars],
                               FUN =  is.numeric,
                               FUN.VALUE = TRUE)),
                    "Summarize variables must all be numeric to display in parallel columns."))
    }

    chunks_push(
      call(
        "<-",
        as.name("ADSL_S"),
        call(
          "%>%",
          as.name(adsl_name),
          as.call(c(
            list(quote(dplyr::select)),
            lapply(adsl_vars, as.name)
          ))
        )
      )
    )

    chunks_push(
      call(
        "<-",
        as.name("ANL_S"),
        call(
          "%>%",
          as.name(anl_name),
          as.call(c(
            list(quote(dplyr::select)),
            lapply(anl_vars, as.name)
          ))
        )
      )
    )

    chunks_push(
      call(
        "<-",
        as.name("ANL_MERGED"),
        call(
          "%>%",
          as.call(c(
            quote(merge),
            list(
              x = as.name("ADSL_S"),
              y = as.name("ANL_S"),
              all.x = FALSE,
              all.y = FALSE,
              by = c("USUBJID", "STUDYID")
            )
          )),
          as.call(c(
            quote(rtables::var_relabel),
            {
              labels <- c(
                datasets$get_data_labels("ADSL", adsl_vars),
                datasets$get_data_labels(dataname, anl_vars)
              )
              labels[!duplicated(labels)]
            }
          ))
        )
      )
    )

    total <- if (add_total) "All Patients" else NULL

    chunks_push(
      call(
        "<-",
        as.name("tbl"),
        call(
          "t_summary_by",
          x = if(bquote(.(parallel_vars))){
            bquote(.(as.name("ANL_MERGED"))[, .(summarize_vars), drop = FALSE] %>%
                     compare_in_header())
          } else {
            if(is.null(bquote(.(by_vars)))){
              bquote(.(as.name("ANL_MERGED"))[, .(summarize_vars), drop = FALSE])
            } else {
              bquote(.(as.name("ANL_MERGED"))[, .(summarize_vars), drop = TRUE])
            }
          },
          row_by = if(is.null(bquote(.(by_vars)))){
            bquote(factor(rep("All", nrow(.(as.name("ANL_MERGED"))))))
          } else {
            bquote(nested_by(.(as.name("ANL_MERGED"))[, .(by_vars), drop = FALSE]))
          },
          col_by = bquote(as.factor(.(as.name("ANL_MERGED"))[[.(arm_var)]])),
          col_N = bquote(table(.(as.name(adsl_name))[[.(arm_var)]])),
          total = bquote(.(total)),
          useNA = "ifany",
          denominator = bquote(.(denominator))
        )
      )
    )

    if(is.null(by_vars)){
      chunks_push(
        call(
          "<-",
          as.name("tbl"),
          call(
            "[",
            as.name("tbl"),
            -1
          )
        )
      )
    }

    chunks_safe_eval()

    tbl <- chunks_get_var("tbl")
    as_html(tbl)
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Summary",
      rcode = get_rcode(
        datasets = datasets,
        title = "Summary-by table"
      )
    )
  })
}
