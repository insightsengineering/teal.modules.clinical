#' teal module for Kaplan-Meier Plot from grid 
#' 
#' This is teal module produces a grid style KM plot for data with ADaM structure
#' 
#' @param label unique name for tabpanel
#' @param dataname dataset name
#' @param arm_var parameter for seperating curves
#' @param arm_var_choices options for \code{arm_var}
#' @param arm_ref_comp optional, if specified it must be a named list with each
#'   element corresponding to an arm variable in \code{ASL} and the element must
#'   be another list with the elements named \code{ref} and \code{comp} that the
#'   defined the default reference and comparison arms when the arm variable is
#'   changed.
#' @param paramcd selected endpoint from ADaM variable \code{PARAMCD}
#' @param paramcd_choices options for \code{endpoint}
#' @param facet_var parameter for facet plotting
#' @param facet_var_choices options for \code{facet_var}
#' @param strata_var parameter for stratification analysis in Cox PH model
#' @param strata_var_choices options for \code{strata_var}
#' @param plot_height plot height specification
#' @param code_data_processing string with data preprocessing before the teal
#'   app is initialized
#' 
#' @importFrom survival Surv strata
#' @importFrom gridExtra arrangeGrob
#' @import forcats
#' @import grid
#' @import dplyr
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
#' ATE <- radam('ATE', ADSL = ASL)
#' 
#' attr(ASL, "source") <- "random.cdisc.data::radam('ASL', start_with = list(ITTFL = 'Y', SEX = c('M', 'F'), MLIVER = paste('mliver', 1:3),  ARM = paste('ARM', LETTERS[1:3]))); ASL$ARM <- as.factor(ASL$ARM)"
#' attr(ATE, "source") <- "random.cdisc.data::radam('ATE', ADSL = ASL)"
#' arm_ref_comp = list(
#'    ARM = list(
#'       ref = "ARM A",
#'       comp = c("ARM B", "ARM C")
#'    ),
#'    ARMCD = list(
#'       ref = "ARM B",
#'       comp = "ARM A"
#'    )
#' )
#' 
#' ## Initialize Teal
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_kmplot(
#'        label = "KM PLOT",
#'        dataname = 'ATE',
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        arm_ref_comp = arm_ref_comp,
#'        paramcd_choices = c("OS", "PFS"),
#'        facet_var = "MLIVER",
#'        facet_var_choices = c("SEX", "MLIVER"),
#'        strata_var = "SEX",
#'        strata_var_choices = c("SEX", "MLIVER")
#'     )  
#'   )
#' )
#' ## Initiate Shiny App
#' shinyApp(ui = x$ui, server = x$server)
#' 
#' }

tm_kmplot <- function(label,
                       dataname,
                       arm_var = "ARM",
                       arm_var_choices = arm_var,
                       arm_ref_comp = NULL,
                       paramcd = "OS",
                       paramcd_choices = paramcd,
                       facet_var = NULL,
                       facet_var_choices = facet_var,
                       strata_var = NULL,
                       strata_var_choices = strata_var,
                       plot_height = c(1200, 400, 5000),
                       pre_output = helpText("x-axes for different factes may not have the same scale"),
                       post_output = NULL,
                       code_data_processing = NULL
){
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = dataname,
    server = srv_kmplot,
    server_args = list(dataname = dataname,
                       arm_ref_comp = arm_ref_comp,
                       code_data_processing = code_data_processing),
    ui = ui_kmplot,
    ui_args = args
  )
}

ui_kmplot <- function(id, ...) {
  
  a <- list(...)
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"), "Treatment Variable", choices = a$arm_var_choices,
                          selected = a$arm_var, multiple = FALSE),
      optionalSelectInput(ns("paramcd"), "Time to Event (Endpoint)", choices = a$paramcd_choices, 
                          selected = a$paramcd, multiple = FALSE),
      optionalSelectInput(ns("strata_var"), "Stratify by", choices = a$strata_var_choices, 
                          selected = a$strata_var, multiple = TRUE,
                          label_help = helpText("currently taken from ASL")),
      optionalSelectInput(ns("facet_var"), "Facet Plots by:", choices = a$facet_var_choices, 
                          selected = a$facet_var, multiple = TRUE,
                          label_help = helpText("currently taken from ASL" )),
      selectInput(ns("ref_arm"), "Reference Arm", choices = NULL, 
                  selected = NULL, multiple = TRUE),
      helpText("Reference groups automatically combined into a single group if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("combine_comp_arms"), "Combine all comparison groups?", value = FALSE),
      tags$label("Plot Settings", class = "text-primary"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_kmplot <- function(input, output, session, datasets, 
                        dataname, arm_ref_comp, code_data_processing) {
  
  
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",     
    ASL = datasets$get_data('ASL', filtered = FALSE, reactive = FALSE),
    arm_ref_comp = arm_ref_comp,
    module = "tm_kmplot"
  )
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("kmplot"), height=plot_height)
  })
  
  
  output$kmplot <- renderPlot({
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    
    paramcd <- input$paramcd
    arm_var <- input$arm_var
    facet_var <<- input$facet_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    strata_var <- input$strata_var
    combine_comp_arms <- input$combine_comp_arms
    
    if (length(facet_var) == 0) facet_var <<- NULL
    if (length(strata_var) == 0) strata_var <- NULL
    
    chunk_vars <<- ""
    chunk_data <<- ""
    chunk_facet <<- ""
    chunk_t_kmplot <<- "# No Calculated"
    
    validate_standard_inputs(
      ASL = ASL_FILTERED,
      aslvars = c("USUBJID", "STUDYID", arm_var, strata_var, facet_var),
      ANL = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "CNSR"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )
    
    validate(need(is.logical(combine_comp_arms), "need combine arm information"))
    
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED)
    
    chunk_vars <<- bquote({
      ref_arm <- .(ref_arm)
      comp_arm <- .(comp_arm)
      strata_var <- .(strata_var)
      facet_var <- .(facet_var)
      combine_comp_arms <- .(combine_comp_arms)
    })
    
    chunk_data <<- bquote({
      ASL_p <- subset(ASL_FILTERED, .(as.name(arm_var)) %in% c(ref_arm, comp_arm))
      ANL_p <- subset(.(as.name(anl_name)), PARAMCD %in% .(paramcd))
      
      ANL <- merge(ASL_p, ANL_p,
                   all.x = FALSE, all.y = FALSE, by = c("USUBJID", "STUDYID"))
      
      ARM <- relevel(as.factor(ANL[[.(arm_var)]]), ref_arm[1])
      
      ARM <- combine_levels(ARM, ref_arm)
      if (combine_comp_arms) {
        ARM <- combine_levels(ARM, comp_arm)
      }
      
      ANL[[.(arm_var)]] <- droplevels(ARM)
    })
    
    eval(chunk_data)
    validate(need(nrow(ANL) > 15, "need at least 15 data points"))
    
    formula_km <<- eval(bquote({
      as.formula(paste0("Surv(AVAL, 1-CNSR) ~", .(arm_var)))
    }))
    
    formula_coxph <<- eval(bquote({
      as.formula(
        paste0("Surv(AVAL, 1-CNSR) ~", .(arm_var), 
               ifelse(is.null(strata_var), "", paste0("+ strata(", paste(.(strata_var), collapse = ","), ")")))
      )
    }))
    
    info_coxph <<- eval(bquote({
      paste0("Cox Proportional Model: ", 
             ifelse(is.null(strata_var), "Unstratified Analysis", paste0("Stratified by ", paste(.(strata_var), collapse = ","))))
      
    }))
    
    
    if (is.null(facet_var)){
      
      chunk_t_kmplot <<- bquote({
        fit_km <- survfit(formula_km, data = ANL, conf.type = "plain")
        fit_coxph <- coxph(formula_coxph, data = ANL, ties = "exact")
        tbl_km <- t_km(fit_km)
        tbl_coxph <- t_coxph(fit_coxph)
        text_coxph <- paste0(info_coxph, "\n", toString(tbl_coxph, gap = 1))
        coxph_grob <- textGrob(label = text_coxph, x= unit(1, "lines"), y = unit(1, "lines"), 
                               just = c("left", "bottom"),
                             gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"),
                             vp = vpPath("plotArea", "topCurve"))
        grid.newpage()
        p <- g_km(fit_km = fit_km, col = NULL, draw = FALSE)  
        p <-  addTable(p, tbl_km,
                   x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
                   y = unit(1, "npc") -  unit(1, "lines"),
                   just = c("left", "top"))  
        p <- addGrob(p, coxph_grob) 
        grid.draw(p)
        
      })
    }
    
    if (!is.null(facet_var)){
      
      chunk_facet <<- bquote({
        facet_df <- ANL[.(facet_var)] 
        
        lab <- Map(function(var, x) paste0(var, "= '", x, "'"), 
                   .(facet_var), facet_df)
        lab <-  Reduce(function(x, y) paste(x, y, sep = ", "), unname(lab)) 
        lab <- factor(unlist(lab))
        dfs <- split(ANL, lab)
        max_min <-  min(sapply(dfs, function(x) {
          max(x[["AVAL"]], na.rm = TRUE)
        }))
        xticks <- max(1, floor(max_min/10))
      })
      
      eval(chunk_facet)
      
      chunk_t_kmplot <<- bquote({
        grid.newpage()
       pl <-  Map(function(x, label){
              x[[.(arm_var)]] <- factor(x[[.(arm_var)]])
              fit_km <- survfit(formula_km, data = x, conf.type = "plain")
              fit_coxph <- coxph(formula_coxph, data = x, ties = "exact")
              tbl_km <- t_km(fit_km)
              tbl_coxph <- t_coxph(fit_coxph)
              text_coxph <- paste0(info_coxph, "\n", toString(tbl_coxph, gap = 1))
              coxph_grob <- textGrob(label = text_coxph, x= unit(1, "lines"), y = unit(1, "lines"), 
                                     just = c("left", "bottom"),
                                     gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"),
                                     vp = vpPath("plotArea", "topCurve"))
              if (nrow(x) < 5){
                  textGrob(paste0("Less than 5 patients in ", label, "group"))
              } else {
                  p <- g_km(fit_km = fit_km, col = NULL, title = paste0("Kaplan - Meier Plot for: ", label), 
                            xticks = xticks, draw = FALSE)  
                  p <-  addTable(p, tbl_km,
                                 x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
                                 y = unit(1, "npc") -  unit(1, "lines"),
                                 just = c("left", "top"))  
                  p <- addGrob(p, coxph_grob)
             }
            }, dfs, levels(lab))
        grid.draw(gridExtra::arrangeGrob(grobs = pl, ncol = 1) )
      })
      
    }
    
    eval(chunk_t_kmplot)
    
  })
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "Kaplan Meier Plot",
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
      paste0("formula_km <- ", deparse(formula_km)),
      "",
      paste0("formula_coxph  <- ", deparse(formula_coxph)),
      "",
      paste0("info_coxph <- ", deparse(info_coxph)),
      "",
      if (!is.null(facet_var)) remove_enclosing_curly_braces(deparse(chunk_facet)),
      "",
      remove_enclosing_curly_braces(deparse(chunk_t_kmplot))
       
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Kaplan Meier Plot",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
}

