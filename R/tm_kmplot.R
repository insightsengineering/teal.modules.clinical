#' teal module for Kaplan-Meier Plot from grid 
#' 
#' This is teal module produces a grid style KM plot for data with ADaM structure
#' 
#' @param label unique name for tabpanel
#' @param dataname dataset name
#' @param arm_var parameter for seperating curves
#' @param arm_var_choices options for \code{arm_var}
#' @param paramcd selected endpoint from ADaM variable \code{PARAMCD}
#' @param paramcd_choices options for \code{endpoint}
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
#'        arm_var_choices = c("ARM", "ARMCD"),
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
                      post_output = NULL
){
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = dataname,
    server = srv_kmplot,
    server_args = list(dataname = dataname,
                       arm_ref_comp = arm_ref_comp),
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
      optionalSelectInput(ns("tteout"), "Time to Event (Endpoint)", choices = a$paramcd_choices, 
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
      checkboxInput(ns("combine_arm"), "Combine all comparison groups?", value = FALSE),
      tags$label("Plot Settings", class = "text-primary"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_kmplot <- function(input, output, session, datasets, dataname, arm_ref_comp) {
  
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
  
  ANL_FILTERED <- reactive({
    ANL_F <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    validate(need(ANL_F, paste0("Need ", dataname,  "data")))
    ANL_F
  })
  ASL_FILTERED <- reactive({
    ASL_F <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    validate(need(ASL_F, "Need ASL data"))
    ASL_F
  })

  
  output$kmplot <- renderPlot({
    ANL_FILTERED <- ANL_FILTERED()
    ASL_FILTERED <- ASL_FILTERED()
    tteout <- input$tteout
    arm_var <- input$arm_var
    facet_var <- input$facet_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    strata_var <- input$strata_var
    combine_arm <- input$combine_arm


    validate_standard_inputs(
      ASL = ASL_FILTERED,
      aslvars = c("USUBJID", "STUDYID", arm_var, strata_var, facet_var),
      ANL = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "CNSR"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )
    
    ANL2 <- merge(
      ASL_FILTERED,
      ANL_FILTERED,
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
      comp_arm <- paste(comp_arm, collapse = "/")
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
    
    fit_km <- survfit(formula_km, data = ANL, conf.type = "plain")
    fit_coxph <- coxph(formula_coxph, data = ANL, ties = "exact")
    
    tbl_km <- kmAnnoData(fit_km) 
    tbl_cox <- coxphAnnoData(fit_coxph, info_coxph = info_coxph)
    results <- try({
      if (length(facet_var) == 0){
        kmGrob(title = "Kaplan - Meier Plot", fit_km = fit_km) %>%
        addTable(vp = vpPath("plotArea", "topCurve"),
                 x = unit(1, "npc") - stringWidth(tbl_km) - unit(1, "lines"),
                 y = unit(1, "npc") -  unit(1, "lines"),
                 just = c("left", "top"),
                 tbl = tbl_km) %>%
        addTable (vp = vpPath("plotArea", "topCurve"),
                    x= unit(1, "lines"), y = unit(1, "lines"),
                    just = c("left", "bottom"),
                    tbl = tbl_cox) %>%
         grid.draw()
      } else {
        
        facet_df <- ANL[facet_var]
        
        n_unique <- sum(!duplicated(facet_df))
        
        lab <- Map(function(var, x) paste0(var, "= '", x,"'"), facet_var, facet_df) %>%
          unname() %>%
          Reduce(function(x, y) paste(x, y, sep = ", "), .) %>%
          unlist() %>%
          factor()
        
        if (length(unique(lab)) != n_unique) stop("algorithm error")  
        
        dfs <- split(ANL, lab)
        max_min <- sapply(dfs, function(x){ max(x[["AVAL"]], na.rm = TRUE)}) %>% min(.) 
        xaxis_by <- max(1, floor(max_min/10))
        
       Map(function(x, label){
          km <- kmAnnoData(fit_km = fit_km ) 
          cox <- coxphAnnoData(fit_coxph = fit_coxph, info_coxph = info_coxph)
          
          kmGrob(title = paste0("Kaplan - Meier Plot for: ", label),
                 fit_km = fit_km, xaxis_by = xaxis_by) %>%
            addTable(vp = vpPath("plotArea", "topCurve"),
                     x = unit(1, "npc") - stringWidth(km) - unit(1, "lines"),
                     y = unit(1, "npc") -  unit(1, "lines"),
                     just = c("left", "top"),
                     tbl = km) %>%
            addTable (vp = vpPath("plotArea", "topCurve"),
                      x= unit(1, "lines"), y = unit(1, "lines"),
                      just = c("left", "bottom"),
                      tbl = cox)
        }, dfs, levels(lab)) %>%
        arrangeGrob(grobs = ., ncol = 1) %>% grid.draw()
      }
      TRUE
    })
    
    if (is(results, "try-error")) validate(need(FALSE, paste0("Could not calculate kmplots\n\n", results)))
    
  })
}

