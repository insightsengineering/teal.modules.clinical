#' Cross table based on rtables
#' 
#' @inheritParams tm_t_tte
#' @param x_var variable name selected variable when shiny app starts
#' @param x_var_choices vector with variable names available as choices
#' @param y_var variable name selected variable when shiny app starts
#' @param y_var_choices vector with variable names available as choices
#' 
#' @importFrom rtables rrowl rtablel as_html
#' @importFrom stats addmargins
#' @export
#' 
#' @template author_waddella
#' 
#' @examples
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radsl(seed = 1)
#' 
#' attr(ASL, "source") <- "random.cdisc.data::radsl(seed = 1)"
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_t_percentage_cross_table("Cross Table", "ASL", x_var = "STRATA1", y_var = "STRATA2")
#'   )
#' )
#' 
#' \dontrun{
#' 
#' shinyApp(x$ui, x$server)
#' 
#' }
tm_t_percentage_cross_table <- function(
  label = "Cross Table",
  dataname,
  x_var,
  x_var_choices = x_var,
  y_var,
  y_var_choices = y_var) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_percentage_cross_table,
    ui = ui_percentage_cross_table,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )
  
}


ui_percentage_cross_table <- function(id, ...) {
  
  a <- list(...)
  
  ns <- NS(id)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      optionalSelectInput(ns("x_var"), "x var", a$x_var_choices, a$x_var),
      optionalSelectInput(ns("y_var"), "y var", a$y_var_choices, a$y_var)
    )
  )
}


srv_percentage_cross_table <- function(input, output, session, datasets, dataname) {

  output$table <- renderUI({
    
    ANL_filtered <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    x_var <- input$x_var
    y_var <- input$y_var
    
    validate(need(ANL_filtered, "data missing"))
    validate(need(nrow(ANL_filtered) > 10, "need at least 10 patients"))
    
    x <- ANL_filtered[[x_var]]
    y <- ANL_filtered[[y_var]]
    
    validate(need(x, "selected x_var does not exist"))
    validate(need(y, "selected y_var does not exist"))
    
    
    X <- addmargins(table(x,y))
    P <- X / X[nrow(X), ncol(X)]
    
    tbl <- try({
      rows <- lapply(1:nrow(X), function(i) {
        
        x_i <- X[i, ]
        p_i <- P[i, ]
        
        r <- Map(function(xii, pii) c(xii, pii), x_i, p_i)
        
        rrowl(rownames(X)[i], r)
      })
      
      
      rtablel(header = colnames(X), rows, format = "xx (xx.xx%)")
    })


    if (is(tbl, "try-error")) validate(need(FALSE, paste0("creating the table failed:\n\n", tbl)))
    
    as_html(tbl)
    
  })
  
}
