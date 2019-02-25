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
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radsl(seed = 1)
#' attr(ASL, "source") <- "random.cdisc.data::radsl(seed = 1)"
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_t_summary(
#'        label = "Demographic Table",
#'        dataname = "ASL",
#'        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'        summarize_vars = choices_selected(c("SEX", "RACE", "BMRKR2"), c("SEX", "RACE"))
#'     )
#'   )
#' )
#' 
#' \dontrun{
#' 
#' shinyApp(x$ui, x$server) 
#' 
#' } 
tm_t_summary <- function(label,
                         dataname,
                         arm_var,
                         summarize_vars,
                         pre_output = NULL,
                         post_output = NULL,
                         code_data_processing = NULL) {
  
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(summarize_vars))
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_t_summary,
    ui = ui_t_summary,
    ui_args = args,
    server_args = list(dataname = dataname, code_data_processing = code_data_processing),
    filters = dataname
  )
  
}

ui_t_summary <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var$choices, a$arm_var$selected, multiple = FALSE),
      optionalSelectInput(ns("summarize_vars"), "Summarize Variables", a$summarize_vars$choices, a$summarize_vars$selected, multiple = TRUE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_t_summary <- function(input, output, session, datasets, dataname, code_data_processing) {
  
  chunks <- list(
    analysis = "# Not Calculated"
  )
  
  output$table <- renderUI({
    
    ANL_f <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    arm_var <- input$arm_var
    summarize_vars <- input$summarize_vars
    
    chunks$analysis <<- "# Not Calculated"
    
    validate_has_data(ANL_f, min_nrow = 3)
    validate(need(!is.null(summarize_vars), "please select 'summarize variables'"))
    validate(need(all(summarize_vars %in% names(ANL_f)), "not all variables available"))
    validate(need(ANL_f[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% ANL_f[[arm_var]]), "arm values can not contain empty strings ''"))
    
    data_name <- paste0(dataname, "_FILTERED")
    assign(data_name, ANL_f)
    
    chunks$analysis <<- call(
      "t_summary",
      x = bquote(.(as.name(data_name))[, .(summarize_vars), drop=FALSE]),
      col_by = bquote(as.factor(.(as.name(data_name))[[.(arm_var)]])),
      total = "All Patients",
      useNA = "ifany" 
    )
    
    tbl <- try(eval(chunks$analysis))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
    
    as_html(tbl)
  })
  
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "Summarize Variables",
      datanames = dataname,
      datasets = datasets,
      code_data_processing
    )
    
    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunks$analysis, width.cutoff = 60))
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Demographic Table",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
}
