
#' Forest Survival Plot Teal Module
#' 
#' @param label a character string displayed as module label 
#' @param dataname The name of the analysis dataset
#' @param arm_var default variable name used as the arm variable
#' @param arm_var_choices a character vector for the choices of \code{arm_var} 
#' @param subgroup_var a vector of variable names used as the default subgroups
#' @param subgroup_var_choices a vector of variable names to choose the \code{subgroup_var} from
#' @param paramcd default response type from PARAMCD
#' @param paramcd_choices a vector of possible \code{paramcd}
#' @param plot_height height of the forest plot
#' @param cex multiplier applied to overall fontsize
#' @param pre_output text displayed at the top of the plot
#' @param post_output text displayed at the bottom of the plot
#' @param code_data_processing xxx?
#' 
#' @export
#' 
#' @author Yuyao Song (songy24), \email{yuyao.song@roche.com}
#' 
#' @examples  
#' 
#' \dontrun{
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL", start_with = list(RACE = c("white", "asian")))
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ASL$ARMCD <- factor(gsub("ARM", "DUMMY", as.character(ASL$ARM)))
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ARS = ARS),
#'   modules = root_modules(
#'     tm_g_forest_rsp(
#'        label = "Forest Survival",
#'        dataname = "ATE",
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),        
#'        paramcd = "OS",
#'        paramcd_choices = c("OS", "PFS"),
#'        plot_height = c(600, 200, 2000),
#'        subgroup_var = c("RACE", "SEX"),
#'        subgroup_var_choices = names(ASL)
#'    )
#'   )
#' )   
#'      
#' } 

tm_g_forest_tte <- function(label,
                            dataname,
                            arm_var = "ARM",
                            arm_var_choices = arm_var,
                            subgroup_var,
                            subgroup_var_choices = subgroup_var,
                            paramcd = "OS",
                            paramcd_choices = paramcd,
                            plot_height = c(600, 200, 2000),
                            cex = 1.3,
                            pre_output = helpText("graph needs to be of a certain width to be displayed"),
                            post_output = NULL,
                            code_data_processing = NULL) {
  

  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_g_forest_tte,
    ui = ui_g_forest_tte,
    ui_args = args,
    server_args = list(dataname = dataname, cex = cex, code_data_processing = code_data_processing),
    filters = "ATE"
  )
}

ui_g_forest_tte <- function(id, ...) {
  
  a <- list(...)
  
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("paramcd"), div("PARAMCD", tags$br(), helpText("Select an endpoint to analyze.")), 
                          a$paramcd_choices, a$paramcd, multiple = FALSE),
      optionalSelectInput(ns("arm_var"), div("Arm Variable", tags$br(), helpText("Select one variable to use for comparison")), 
                          a$arm_var_choices, a$arm_var, multiple = FALSE),
      selectInput(ns("ref_arm"), "Reference Arm", 
                  choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Arm", choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      optionalSelectInput(ns("subgroup_var"), "Subgroup Variables", a$subgroup_var_choices, a$subgroup_var, multiple = TRUE,
                          label_help = helpText("are taken from", tags$code("ASL"))),
      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
} 

srv_g_forest_tte <- function(input, output, session, datasets, dataname, cex = 1.5, code_data_processing) {
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("forest_plot"), height=plot_height)
  })
  
  
  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",    # from UI
    ASL = datasets$get_data('ASL', filtered = FALSE, reactive = FALSE),
    arm_ref_comp = NULL,
    module = "tm_g_forest_rsp"
  )
  
  
  # Update UI choices depending on selection of previous options
  observe({
    
    paramcd <- input$paramcd
    
    ANL <- datasets$get_data(dataname, filtered = FALSE, reactive = FALSE)
    
    rsp_choices <- unique(ANL$AVALC[ANL$PARAMCD == paramcd])
    
    updateSelectInput(session, "responders", 
                      choices = rsp_choices,
                      selected = intersect(rsp_choices, c("CR", "PR")))
    
  })
  
  ### need asl labels for labelling the plots
  #temp_ASL <- datasets$get_data("ASL", filtered=FALSE, reactive = FALSE)  
  #ASL_labels <- unlist(Filter(function(x)!is.null(x), sapply(temp_ASL, function(v) attr(v, "label"))))
  
  output$forest_plot <- renderPlot({
    
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)    
    ANL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    
    # validate your input values
    validate_has_data(ASL_FILTERED)
    validate_has_data(ANL_FILTERED, min_nrow = 15) 
    
    paramcd <- input$paramcd
    responders <- input$responders
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var
    
    
    validate(need(length(ref_arm) > 0 && length(comp_arm) > 0,
                  "need at least one reference and one comparison arm"))
    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(length(responders) > 0 && all(responders %in% ANL_FILTERED$AVALC),
                  "response AVALC choice does not exist"))
    
    validate(need(length(paramcd)>0 && paramcd %in% ANL_FILTERED$PARAMCD,
                  "PARAMCD choice does not exist"))
    
    
    
    # Delete chunks that are used for reproducible code
    chunk_vars <<- ""
    chunk_data <<- ""
    chunk_t_forest_tte <<- "# No Calculated" 
    
    # anl_data_name <- paste0(dataname, "_FILTERED")
    # assign(anl_data_name, ANL_FILTERED)
    
    chunk_vars <<- bquote({
      ref_arm <- .(ref_arm)
      comp_arm <- .(comp_arm)
    })
    
    chunk_data <<- bquote({
      ASL_p <- subset(ASL_FILTERED, ASL_FILTERED[[.(arm_var)]] %in% c(ref_arm, comp_arm))
      ANL_p <- subset(.(ANL_FILTERED), PARAMCD %in% .(paramcd))
      
      ANL <- merge(ASL_p, ANL_p, all.x = TRUE, all.y = FALSE, by = c("USUBJID", "STUDYID"))
      
      ARM <- droplevels(relevel(as.factor(ANL[[.(arm_var)]]), ref_arm[1]))
      ARM <- combine_levels(ARM, ref_arm)
      ARM <- combine_levels(ARM, comp_arm)
      
    })
    
    chunk_t_forest_rsp <<- call(
      "t_forest_rsp",
      rsp = bquote(ANL$AVALC %in% .(responders)),
      col_by = quote(ARM),
      group_data = if (length(subgroup_var) > 0) bquote({ANL[, .(subgroup_var), drop=FALSE]}) else NULL,
      total = "All Patients",
      na.omit.group = TRUE
    )                       
    
    
    # as.global(ARM, ANL)
    as.global(tbl, chunk_vars, chunk_data, chunk_t_forest_tte)    
    eval(chunk_vars)
    eval(chunk_data)
 
    t_forest_tte(
      time_to_event = ANL_FILTERED$AVAL,
      event = ATE_f$CNSR == 0,
      col_by = ARM, 
      group_data = ANL[, c("SEX", "RACE"), drop= FALSE],
      total = "All Patients", 
      na.omit.group = TRUE
    )
    
    tbl <- try(eval(chunk_t_forest_tte))
  })
}
