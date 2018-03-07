#' teal module for Kaplan-Meier Plot from grid 
#' 
#' This is teal module produces a grid style KM plot for data with ADaM structure
#' 
#' @param label unique name for tabpanel
#' @param dataname dataset name
#' @param treatment_var parameter for seperating curves
#' @param treatment_var_choices options for \code{treatment_var}
#' @param endpoint selected endpoint from ADaM variable \code{PARAMCD}
#' @param endpoint_choices options for \code{endpoint}
#' @param facet_var parameter for facet plotting
#' @param facet_var_choices options for \code{facet_var}
#' @param strata_var parameter for stratification analysis in Cox PH model
#' @param strata_var_choices options for \code{strata_var}
#' @param plot_height plot height specification
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
#' 
#' ## Initialize Teal
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_kmplot(
#'        label = "KM PLOT",
#'        dataname = 'ATE',
#'        treatment_var_choices = c("ARM", "ARMCD"),
#'        endpoint_choices = c("OS", "PFS"),
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
                      treatment_var = "ARM",
                      treatment_var_choices = treatment_var,
                      endpoint = "OS",
                      endpoint_choices = endpoint,
                      facet_var = NULL,
                      facet_var_choices = facet_var,
                      strata_var = NULL,
                      strata_var_choices = strata_var,
                      plot_height = c(1200, 400, 5000),
                      pre_output = helpText("x-axes for different factes may not have the same scale"),
                      post_output = NULL
){
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = dataname,
    server = srv_kmplot,
    server_args = list(dataname = dataname),
    ui = ui_kmplot,
    ui_args = args
  )
}

ui_kmplot <- function(
  id, 
  label,
  dataname,
  treatment_var = "ARM",
  treatment_var_choices = treatment_var,
  endpoint = "OS",
  endpoint_choices = endpoint,
  strata_var = NULL,
  strata_var_choices = strata_var,
  facet_var = NULL,
  facet_var_choices = facet_var,
  plot_height = c(700, 400, 3000),
  pre_output = NULL,
  post_output = NULL
  ) {
  
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code(dataname)),
      optionalSelectInput(ns("arm_var"), "Treatment Variable", choices = treatment_var_choices,
                          selected = treatment_var, multiple = FALSE),
      optionalSelectInput(ns("tteout"), "Time to Event (Endpoint)", choices = endpoint_choices, 
                          selected = endpoint, multiple = FALSE),
      optionalSelectInput(ns("strata_var"), "Stratify by", choices = strata_var_choices, 
                          selected = strata_var, multiple = TRUE,
                          label_help = helpText("currently taken from ASL")),
      optionalSelectInput(ns("facetby"), "Facet Plots by:", choices = facet_var_choices, 
                          selected = facet_var, multiple = TRUE,
                          label_help = helpText("currently taken from ASL" )),
      selectInput(ns("ref_arm"), "Reference Arm", choices = NULL, 
                  selected = NULL, multiple = TRUE),
      helpText("Reference groups automatically combined into a single group if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("combine_arm"), "Combine all comparison groups?", value = FALSE),
      tags$label("Plot Settings", class = "text-primary"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", plot_height, ticks = FALSE)
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}


srv_kmplot <- function(input, output, session, datasets, dataname) {
  
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("kmplot"), height=plot_height)
  })
  
  ATE_FILTERED <- reactive({
    ATE_F <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    validate(need(ATE_F, paste0("Need ", dataname,  "data")))
    ATE_F
  })
  ASL_FILTERED <- reactive({
    ASL_F <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    validate(need(ASL_F, "Need ASL data"))
    ASL_F
  })
  
  observe({
    ASL <- datasets$get_data("ASL", filtered = FALSE, reactive = TRUE)
    
    chs <- sapply(ASL, unique, simplify = FALSE, USE.NAMES = TRUE)
    
    updateSelectInput(session,  "ref_arm" , choices = chs[input$arm_var],
                      selected = chs[[input$arm_var]][1])
    updateSelectInput(session, "comp_arm", choices = chs[input$arm_var],
                      selected = chs[[input$arm_var]][-1])
    
  })
  
  
  output$kmplot <- renderPlot({
    ATE_FILTERED <- ATE_FILTERED()
    ASL_FILTERED <- ASL_FILTERED()
    tteout <- input$tteout
    arm_var <- input$arm_var
    facetby <- input$facetby
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    strata_var <- input$strata_var
    combine_arm <- input$combine_arm


    validate(need(!is.null(comp_arm), "select at least one comparison arm"))
    validate(need(!is.null(ref_arm), "select at least one reference arm"))
    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    validate(need(arm_var %in% names(ASL_FILTERED), "arm_var is not in ASL"))
    validate(need(is.null(facetby)  || facetby %in% names(ASL_FILTERED), "facet by not correct"))
    validate(need(ref_arm %in% ASL_FILTERED[[arm_var]], "reference arm does not exist in left over ARM values"))

    
    ANL2 <- merge(
      ASL_FILTERED,
      ATE_FILTERED,
      all.x = TRUE, all.y = FALSE,
      by=c("USUBJID", "STUDYID")
    )
    
    ANL1 <- ANL2 %>% filter(PARAMCD == tteout) 
    ANL <- ANL1[ANL1[[arm_var]] %in% c(comp_arm, ref_arm) , ]
    
    validate(need(nrow(ANL) > 10, "Need more than 10 observations"))
   
    
    if (length(ref_arm)>1) {
      new_ref_arm <- paste(ref_arm, collapse = "/")
      ANL[[arm_var]] <- do.call(fct_collapse, setNames(list(ANL[[arm_var]], ref_arm), c("f", new_ref_arm)))
      ref_arm <- new_ref_arm
    }
    
    if (combine_arm) {
      ANL[[arm_var]] <- do.call(fct_collapse, setNames(list(ANL[[arm_var]], comp_arm), c("f", paste(comp_arm, collapse = "/"))))
    }
    
    ANL[[arm_var]] <- fct_relevel(ANL[[arm_var]], ref_arm)
    
    formula_km <- as.formula(
      paste0("Surv(AVAL, 1-CNSR) ~", arm_var)
    )
    
    if (length(strata_var) != 0){
      formula_coxph <- as.formula(
        paste0("Surv(AVAL, 1-CNSR) ~", arm_var ,  "+ strata(", paste(strata_var, collapse = ","), ")")
      )
      info_coxph <- paste0("Cox Proportional Model: Stratified by ", paste(strata_var, collapse = ","))
    } else{
      formula_coxph <- formula_km
      info_coxph <- "Cox Proportional Model: Unstratified Analysis"
    }

    tbl_km <- kmAnnoData(formula_km = formula_km, data = ANL) 
    tbl_cox <- coxphAnnoData(formula_coxph = formula_coxph, 
                             data = ANL, cox_ties = "exact", info_coxph = info_coxph)
    results <- try({
      if (length(facetby) == 0){
        kmGrob(title = "Kaplan - Meier Plot",
               formula_km = formula_km, data = ANL) %>%
        addTable(vp = vpPath("plotarea", "topcurve"),
                 x = unit(1, "npc") - stringWidth(tbl_km) - unit(1, "lines"),
                 y = unit(1, "npc") -  unit(1, "lines"),
                 just = c("left", "top"),
                 tbl = tbl_km) %>%
        addTable (vp = vpPath("plotarea", "topcurve"),
                    x= unit(1, "lines"), y = unit(1, "lines"),
                    just = c("left", "bottom"),
                    tbl = tbl_cox) %>%
         grid.draw()
      } else {
        
        facet_df <- ANL[facetby]
        
        n_unique <- sum(!duplicated(facet_df))
        
        lab <- Map(function(var, x) paste0(var, "= '", x,"'"), facetby, facet_df) %>%
          unname() %>%
          Reduce(function(x, y) paste(x, y, sep = ", "), .) %>%
          unlist() %>%
          factor()
        
        if (length(unique(lab)) != n_unique) stop("algorithm error")  
        
        dfs <- split(ANL, lab)
        max_min <- sapply(dfs, function(x){ max(x[["AVAL"]], na.rm = TRUE)}) %>% min(.) 
        xaxis_by <- max(1, floor(max_min/10))
        
       mapply(function(x, label){
          km <- kmAnnoData(formula_km = formula_km, data = x) 
          cox <- coxphAnnoData(formula_coxph = formula_coxph, 
                                   data = x, cox_ties = "exact", info_coxph = info_coxph)
          
          kmGrob(title = paste0("Kaplan - Meier Plot for: ", label),
                 formula_km = formula_km, data = x, xaxis_by = xaxis_by) %>%
            addTable(vp = vpPath("plotarea", "topcurve"),
                     x = unit(1, "npc") - stringWidth(km) - unit(1, "lines"),
                     y = unit(1, "npc") -  unit(1, "lines"),
                     just = c("left", "top"),
                     tbl = km) %>%
            addTable (vp = vpPath("plotarea", "topcurve"),
                      x= unit(1, "lines"), y = unit(1, "lines"),
                      just = c("left", "bottom"),
                      tbl = cox)
        }, dfs, levels(lab), SIMPLIFY = FALSE) %>%
        arrangeGrob(grobs = ., ncol = 1) %>% grid.draw()
      }
      TRUE
    })
    
    if (is(results, "try-error")) validate(need(FALSE, paste0("Could not calculate kmplots\n\n", results)))
    
  })
}

