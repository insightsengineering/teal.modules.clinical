

#' Summarize Variable Teal Module
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam('ASL', N = 1000)
#' 
#' attr(ASL, "source") <- "random.cdisc.data::radam('ASL') "
#' 
#' ASL$ARMCD <- ASL$ARM
#' levels(ASL$ARMCD) <- c("DUMMY A", "DUMMY B")
#' 
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_t_summarize_variables(
#'        label = "Demographic Table",
#'        dataname = "ASL",
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        summarize_vars =  c("SEX"),
#'        summarize_vars_choices = c("SEX", "RACE", "BAGE")
#'    )
#'   )
#' )   
#' shinyApp(x$ui, x$server) 
#' 
#'   
#' } 
#' 
#' 
tm_t_summarize_variables <- function(label,
                                     dataname,
                                     arm_var,
                                     arm_var_choices = arm_var,
                                     summarize_vars,
                                     summarize_vars_choices = summarize_vars,
                                     pre_output = NULL,
                                     post_output = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_t_summarize_variables,
    ui = ui_t_summarize_variables,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )
  
}

ui_t_summarize_variables <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var_choices, a$arm_var, multiple = FALSE),
      optionalSelectInput(ns("summarize_vars"), "Summarize Variables", a$summarize_vars_choices, a$summarize_vars, multiple = TRUE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_t_summarize_variables <- function(input, output, session, datasets, dataname) {
  
  output$table <- renderUI({

    ANL_f <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    arm_var <- input$arm_var
    summarize_vars <- input$summarize_vars
    
    chunk_analysis <<- ""
    
    validate_has_data(ANL_f, min_nrow = 15)    
    validate(need(!is.null(summarize_vars), "please select 'summarize variables'"))
    validate(need(all(summarize_vars %in% names(ANL_f)), "not all variables available"))
    validate(need(ANL_f[[arm_var]], "Arm variable does not exist"))
    
    data_name <- paste0(dataname, "_FILTERED")
    assign(data_name, ANL_f)
    
    chunk_analysis <<- call(
      "t_summarize_variables",
      data = bquote(.(as.name(data_name))[, .(summarize_vars), drop=FALSE]),
      col_by = bquote(.(as.name(data_name))[[.(arm_var)]]),
      total = "All Patients",
      useNA_factors = "ifany" 
    )

    tbl <- try(eval(chunk_analysis))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
    
    as_html(tbl)
  })
  
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "Summarize Variables",
      dataname = dataname, 
      datasets = datasets
    )
    
    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunk_analysis, width.cutoff = 60))
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Time To Event Table",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
}

