#' @title Response Table Teal Module
#' 
#' @description
#' This module produces a response summary table that matches the STREAM 
#' template rspt01
#' 
#' @inheritParams teal::standard_layout
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
      optionalSelectInput(ns("arm_var"), "Grouping Variable", 
                          a$arm_var_choices, a$arm_var, multiple = FALSE,
                          label_help = helpText("Select one variable to use for grouping")),
      selectInput(ns("ref_arm"), "Reference Group", 
                          choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Reference groups automatically combined into a single group if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("combine_arm"), "Combine all comparison groups?", value = FALSE),
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
    combine_arm <- input$combine_arm
    strata_var <- input$strata_var
    
    
    # Validate your input
    validate_has_data(ASL_FILTERED)
    validate_has_data(ANL_FILTERED, min_nrow = 15)    

    validate(need(!is.null(paramcd) && paramcd %in% ANL_FILTERED$PARAMCD,
                  "PARAMCD does not exist"))
    
    validate(need(!is.null(responders) && all(responders %in% ANL_FILTERED$AVALC),
                  "responders AVALC does not exist"))
    
    validate(need(!is.null(comp_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(all(c(ref_arm, comp_arm) %in% ANL_FILTERED[[var_arm]]), paste("arm variable not found in", dataname)))
    
    validate(need(all(strata_var %in% names(ANL_FILTERED)), paste("stratification factor not found in", dataname)))

    
    tbl <- as.rtable(table(iris$Species))
    as_html(tbl)
    
  })
  
    
  
#   # Deal With Reactivity/Inputs
#   ARS_filtered <- reactive({
#     ARS_f <- datasets$get_data("ARS", reactive = TRUE, filtered = TRUE)
#     ARS_f
#   })
#   
# 
#   
#   output$response_table <- renderUI({
#     

#     
#     # Assign inputs to global
#     # teal:::as.global(ARS_filtered)
#     # teal:::as.global(paramcd)
#     # teal:::as.global(responders)
#     # teal:::as.global(incl_missing)
#     # teal:::as.global(var_arm)
#     # teal:::as.global(ref_arm)
#     # teal:::as.global(comp_arm)
#     # teal:::as.global(combine_arm)
#     # teal:::as.global(var_strata)
#     
#     
#     # Get final analysis dataset
#     ANL1 <- ARS_filtered %>% filter(PARAMCD == paramcd)
# 
#     ANL <- ANL1[ANL1[[var_arm]] %in% c(ref_arm, comp_arm), ]
#   
#     validate(need(nrow(ANL) > 0, "no data left"))
# 
#     #--- Manipulation of response and arm variables ---#
#     # Recode/filter responses if want to include missing as non-responders
#     if (incl_missing == TRUE) {
#       ANL$AVALC[ANL$AVALC==""] <- "NE"
#     } else {
#       ANL <- ANL %>% filter(AVALC != "")
#     }
#     
#     # Recode grouping according to ref_arm, comp_arm and combine_arm settings
#     arm1 <- factor(ANL[[var_arm]])
#   
#     if (length(ref_arm) > 1) {
#       refname <- paste0(ref_arm, collapse = "/")
#       armtmp <- fct_collapse(arm1, refs = ref_arm)
#       arm2 <- fct_relevel(armtmp, "refs", comp_arm)
#       levels(arm2)[which(levels(arm2)=="refs")] <- refname
#     } else {
#       arm2 <- fct_relevel(arm1, ref_arm, comp_arm)
#     }
#     
#     if (length(comp_arm) > 1 && combine_arm == TRUE) {
#       compname <- paste0(comp_arm, collapse = "/")
#       ARM <- fct_collapse(arm2, comps = comp_arm)
#       levels(ARM)[which(levels(ARM)=="comps")] <- compname
#     } else {
#       ARM <- arm2
#     }
# 
#     tbl <- try(response_table(
#       response = ANL$AVALC,
#       value.resp = responders,
#       value.nresp = setdiff(ANL$AVALC, responders),
#       arm = ARM,
#       strata_data = if (!is.null(var_strata)) ANL[var_strata] else NULL
#     ))
#     
#     if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate response table:\n\n", tbl)))
#     
#     as_html(tbl)
#   })
}
