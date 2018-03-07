#' @title Response Table Teal Module
#' 
#' @description
#' This module produces a response summary table that matches the STREAM 
#' template rspt01
#' 
#' @inheritParams teal::standard_layout
#' @inheritParams tm_t_tte
#' @param label full name label of module
#' @param paramcd filter the rows in ARS given the paramcd value
#' @param paramcd_choices choices of possible poaramcd
#' @param arm.var selected variable to use as arms
#' @param arm.var_choices choices of arm variables
#' @param strata.var categorical variable name(s) for stratified model
#' @param strata.var_choices choices of stratification factors 
#' 
#' @details Additional standard UI inputs include \code{responders},
#' \code{incl_missing} (default TRUE), \code{ref_arm}, \code{comp_arm} and
#' \code{combine_arm} (default FALSE)
#' 
#' Default values of the inputs \code{var_arm}, \code{ref_arm} and
#' \code{comp_arm} are set to NULL, and updated accordingly based on seletion of
#' \code{paramcd} and \code{arm.var}
#' 
#' Package \code{forcats} used to re-format arm data into leveled factors.
#' 
#' Reference arms automatically combined if multiple arms selected as reference
#' group.
#' 
#' @return an \code{\link[teal]{module}} object
#'   
#' @export
#' 
#' @examples  
#' 
#' \dontrun{ 
#' 
#' library(random.cdisc.data)
#'
#' ASL <- radam('ASL', start_with = list(
#'   ITTFL = 'Y',
#'   SEX = c("M", "F"),
#'   MLIVER = paste("mliver", 1:3),
#'   ARM = paste("ARM", LETTERS[1:3])
#' ))
#' 
#' ASL$ARM <- as.factor(ASL$ARM)
#' 
#' ARS <- radam('ARS', ADSL = ASL)
#' 
#' attr(ASL, "source") <- "random.cdisc.data::radam('ASL', start_with = list(ITTFL = 'Y', SEX = c('M', 'F'), MLIVER = paste('mliver', 1:3),  ARM = paste('ARM', LETTERS[1:3]))); ASL$ARM <- as.factor(ASL$ARM)"
#' attr(ARS, "source") <- "random.cdisc.data::radam('ARS', ADSL = ASL)"
#' 
#' 
#' x <- teal::init( 
#'   data = list(ASL = ASL, ARS = ARS),
#'   modules = root_modules(
#'     tm_t_rsp(
#'        label = "Response Table",
#'        dataname = 'ARS',
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        paramcd = "BESRSPI",
#'        paramcd_choices = unique(ARS$PARAMCD),
#'        strata_var = "SEX",
#'        strata_var_choices = c("SEX", "MLIVER")
#'     )
#'   )
#' )
#' 
#' shinyApp(x$ui, x$server) 
#' 
#' 
#'   
#' } 
tm_t_rsp <- function(label,
                     dataname,
                     arm_var = "ARM",
                     arm_var_choices = arm_var,
                     arm_ref_comp = NULL,
                     paramcd = "OVRSPI",
                     paramcd_choices = paramcd,
                     strata_var = NULL,
                     strata_var_choices = strata_var,
                     include_missing = TRUE,
                     pre_output = NULL,
                     post_output = NULL,
                     code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_t_rsp,
    ui = ui_t_rsp,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      arm_ref_comp = arm_ref_comp,
      code_data_processing = code_data_processing,
      paramcd_choices = paramcd_choices
    ),
    filters = dataname
  )
  
  
}


#' UI part for response table teal module
#' 
#' @inheritParams tm_response_table
#' @param id namespace id
#'   
#' @details Additional standard UI inputs include \code{responders},
#' \code{incl_missing} (default TRUE), \code{ref_arm}, \code{comp_arm} and
#' \code{combin_arm} (default FALSE)
#' 
#' Default values of the inputs \code{var_arm}, \code{ref_arm} and
#' \code{comp_arm} are set to NULL, and updated accordingly based on seletion of
#' \code{paramcd} and \code{arm.var}
#' 
#' @noRd
#' 
ui_t_rsp <- function(id, ...) {

  ns <- NS(id)
  
  a <- list(...)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("response_table"))),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      #Response related parameters
      optionalSelectInput(ns("paramcd"),
                          div("PARAMCD", tags$br(), helpText("Select one type of response to analyze.")), 
                          choices = a$paramcd_choices, selected = a$paramcd, multiple = FALSE),
      selectInput(ns("responders"), "Responders", 
                  choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("incl_missing"), "Include missing as non-responders?", value = a$include_missing),
      #Arm related parameters
      optionalSelectInput(ns("arm_var"), "Arm Variable", 
                          a$arm_var_choices, a$arm_var, multiple = FALSE),
      selectInput(ns("ref_arm"), "Reference Group", 
                          choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Multiple reference groups are automatically combined into a single group."),
      selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("combine_comp_arms"), "Combine all comparison groups?", value = FALSE),
      #Stratification related parameters
      optionalSelectInput(ns("strata_var"), "Stratification Factors",
                  choices = a$strata_var_choices, selected = a$strata_var, multiple = TRUE,
                  label_help = helpText("taken from:", tags$code("ASL"))
                  )
      
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' Server part for response table teal module
#' 
#' @inheritParams tm_response_table
#' @param id namespace id
#' 
#' @details 
#' 
#' Selection for standard UI inputs \code{responders}, \code{ref_arm} and
#' \code{comp_arm} are updated upon selection of \code{paramcd} and
#' \code{arm.var}.
#' 
#' Package \code{forcats} used to re-format arm data into leveled factors. 
#' Reference arms automatically combined if multiple arms selected as reference
#' group.
#' 
#' @importFrom forcats fct_relevel fct_collapse
#' 
#' @noRd
#' 
srv_t_rsp <- function(input, output, session, datasets, dataname, arm_ref_comp, code_data_processing, paramcd_choices) {
  
  
  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",    # from UI
    ASL = datasets$get_data('ASL', filtered = FALSE, reactive = FALSE),
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_rsp"
  )
  

  

   # Update UI choices depending on selection of previous options
  
  ANL <- datasets$get_data(dataname, filtered = FALSE, reactive = FALSE)

  observe({
    
    paramcd <- input$paramcd

    responder_choices <- unique(ANL$AVALC[ANL$PARAMCD == paramcd])
    
    updateSelectInput(session, "responders", 
                      choices = responder_choices,
                      selected = intersect(c("CR", "PR"), responder_choices))
    
  })
  
  output$response_table <-  renderUI({
    
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ANL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    paramcd <- input$paramcd
    responders <- input$responders
    incl_missing <- input$incl_missing
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_comp_arms <- input$combine_comp_arms
    strata_var <- input$strata_var
    
    if (length(strata_var) == 0) strata_var <- NULL
    
    
    #as.global(ASL_FILTERED, ANL_FILTERED, paramcd, responders, incl_missing, arm_var, ref_arm, comp_arm,
    #          combine_comp_arms, strata_var)
    
    
    # Chunks
    chunk_vars <<- ""
    chunk_data <<- ""
    chunk_t_rsp <<- "# No Calculated"
      
      
    # Validate your input
    validate_standard_inputs(
      ASL = ASL_FILTERED,
      aslvars = c("USUBJID", "STUDYID", arm_var, strata_var),
      ANL = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "AVALC"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )
    
    validate_in(responders, ANL_FILTERED$AVALC, "responder values do not exist")
    validate(need(is.logical(combine_comp_arms), "need combine arm information"))
    
    
    
    # perform analysis
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED) # so that we can refer to the 'correct' data name
    
    asl_vars <- c("USUBJID", "STUDYID", arm_var, strata_var)
    anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALC", "PARAMCD")
    
    ## Now comes the analysis code
    chunk_vars <<- bquote({
      ref_arm <- .(ref_arm)
      comp_arm <- .(comp_arm)
      strata_var <- .(strata_var)
      combine_comp_arms <- .(combine_comp_arms)
    })
    
    chunk_data <<- bquote({
      ASL_p <- subset(ASL_FILTERED, 
                      .(as.name(arm_var)) %in% c(ref_arm, comp_arm))
      
      ANL_endpoint <- subset(.(as.name(anl_name)), PARAMCD == .(paramcd))
      if (any(duplicated(ANL_endpoint[,c("USUBJID", "STUDYID")]))) 
        stop("only one row per patient expected")
      
      ANL <- merge(
        x = ASL_p[, .(asl_vars), drop = FALSE],
        y = ANL_endpoint[, .(anl_vars), drop = FALSE],
        all.x = FALSE, all.y = FALSE, by=c("USUBJID", "STUDYID")
      )
      
      ARM <- relevel(as.factor(ANL[[.(arm_var)]]), ref_arm[1])
      
      ARM <- combine_levels(ARM, ref_arm)
      if (combine_comp_arms) {
        ARM <- combine_levels(ARM, comp_arm)
      }
      
      ANL[[.(arm_var)]] <- droplevels(ARM)
    })
    
    eval(chunk_data)
    validate(need(nrow(ANL) > 15, "need at least 15 data points"))
    
    
    chunk_t_rsp <<- call(
      "t_rsp",
      rsp = bquote(ANL$AVALC %in% .(responders)),
      col_by = bquote(ANL[[.(arm_var)]]),
      partition_rsp_by = bquote(as.factor(ANL$AVALC)),
      strata_data = if (length(strata_var) >0) bquote(ANL[, .(strata_var), drop=FALSE]) else NULL
    )
    
    tbl <- try(eval(chunk_t_rsp))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate response table:\n\n", tbl)))
    
    as_html(tbl)
    
  })
  

  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "Response Table",
      dataname = if (is.null(code_data_processing)) dataname else datasets$datanames(), 
      datasets = datasets,
      code_data_processing
    )
    
    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunk_vars)),
      "",
      remove_enclosing_curly_braces(deparse(chunk_data)),
      "",
      deparse(chunk_t_rsp)
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Response Table",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
}
