#' Time To Event Table Teal Module
#' 
#' Time to event table as defined in \code{\link[tern]{t_tte}} in the
#' \code{tern} package
#' 
#' @param label menue item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param arm_var single name of variable in analysis data that is used as
#'   \code{\link[tern]{col_by}} argument.
#' @param arm_var_choices vector with variable names that can be used as
#'   \code{arm_var}
#' @param paramcd single selected endpoint filtered with \code{PARAMCD} variable
#' @param paramcd_choices vector with \code{paramcd} choices
#' @param strata_var vector with variable names that should be used for
#'   stratification
#' @param strata_var_choices vector with possible choices for \code{strata_var}
#' @param time_points vector with timepoints as used in \code{\link[tern]{t_tte}}
#' @param time_points_choices vector with possible \code{time_points}s
#' @param time_unit string with unit of \code{dataname$AVAL}
#' @param event_desrc_var variable name with the event description information,
#'   optional
#' @inheritParams teal::standard_layout
#' 
#' 
#' @details 
#' This modules expects that the analysis data has the following variables
#' 
#' \tabular{ll}{
#'  \code{AVAL} \tab time to event\cr
#'  \code{CNSR} \tab boolean or 0,1 is element in \code{AVAL} censored\cr
#'  \code{PARAMCD} \tab variable used to filter for endpoint (e.g. OS), after filtering for \code{paramcd} one observation per patient is expected
#' }
#' 
#' The arm variables, stratification variables and taken from the \code{ASL}
#' data.
#' 
#' 
#' @export
#' 
#' 
#' @importFrom forcats fct_collapse fct_relevel
#' 
#' @examples  
#' 
#' \dontrun{
#' library(random.cdisc.data)
#'
#' ASL <- radam('ASL', start_with = list(SEX = c("M", "F"), MLIVER = paste("mliver", 1:3)))
#' ATE <- radam('ATE', ADSL = ASL)
#' 
#' ASL$ARMCD <- gsub(" ", "_", ASL$ARM, fixed = TRUE)
#' 
#' arm_var <- reactiveVal("ARM", "arm variable")
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_t_tte(
#'        label = "Time To Event Table",
#'        dataname = 'ATE',
#'        arm_var = arm_var,
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        arm_ref_comp = list(
#'           ARM = list(
#'              ref = "ARM A",
#'              comp = c("ARM B", "ARM C")
#'           ),
#'           ARMCD = list(
#'              ref = "ARM_B",
#'              comp = "ARM_A"
#'           )
#'        ),
#'        paramcd = "OS",
#'        paramcd_choices = unique(ATE$PARAMCD),
#'        strata_var = "SEX",
#'        strata_var_choices = c("SEX", "MLIVER"),
#'        time_points = 6,
#'        time_points_choices = c(6, 8),
#'        time_unit = "months"
#'    )
#'   )
#' )   
#' shinyApp(x$ui, x$server) 
#'   
#' } 
tm_t_tte <- function(label,
                     dataname,
                     arm_var = "ARM",
                     arm_var_choices = arm_var,
                     arm_ref_comp = NULL,
                     paramcd = "OS",
                     paramcd_choices = paramcd,
                     strata_var = NULL,
                     strata_var_choices = strata_var,
                     time_points,
                     time_points_choices = time_points,
                     time_unit = "months",
                     event_desrc_var = NULL, 
                     pre_output = NULL,
                     post_output = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_t_tte,
    ui = ui_t_tte,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      arm_var = arm_var,
      arm_ref_comp = arm_ref_comp,
      time_unit = time_unit,
      event_desrc_var = event_desrc_var
    ),
    filters = dataname
  )
}

ui_t_tte <- function(id,
                     label,
                     dataname,
                     arm_var,
                     arm_var_choices,
                     arm_reference,
                     arm_comparison,
                     paramcd,
                     paramcd_choices = paramcd,
                     strata_var,
                     strata_var_choices,
                     time_points,
                     time_points_choices,
                     time_unit,
                     event_desrc_var,
                     pre_output,
                     post_output) {
  
  ns <- NS(id)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("tte_table"))),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(ns("paramcd"), "Select Endpoint", paramcd_choices, paramcd, multiple = FALSE),
      optionalSelectInput(ns("arm_var"), "ARM", arm_var_choices, arm_var, multiple = FALSE),
      selectInput(ns("ref_arm"), "Reference Group", choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Multiple reference groups are automatically combined into a single group."),
      selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("combine_comp_arms"), "Combine all comparison groups?", value = FALSE),
      optionalSelectInput(ns("strata_var"), "Stratify by",
                          strata_var_choices, strata_var, multiple = TRUE,
                          label_help = helpText("from ", tags$code("ASL"))),
      optionalSelectInput(ns("time_points"), "Time Points", time_points_choices, time_points, multiple = TRUE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_t_tte <- function(input, output, session, datasets, dataname,
                      arm_var, arm_ref_comp,
                      time_unit, event_desrc_var) {
  
  ASL <- datasets$get_data('ASL', filtered = FALSE, reactive = FALSE)
  

  
  output$tte_table <- renderUI({

#    # resolve all reactive expressions
#    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
#    ATE_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
#
#    paramcd <- input$paramcd
#    strata_var <- input$strata_var
#    arm_var <- input$arm_var
#    ref_arm <- input$ref_arm
#    comp_arm <- input$comp_arm
#    combine_comp_arms <- input$combine_comp_arms
#    time_points <- input$time_points
#    
#    
#    # validate your input values
#    validate_has_data(ASL_filtered)
#    validate_has_data(ATE_filtered, min_nrow = 15)    
#    
#    validate(need(ASL_filtered[[arm_var]], "no valid arm selected"))
#    
#    validate(need(!is.null(ref_arm) && !is.null(comp_arm),
#                  "need at least one reference and one comparison arm"))
#    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
#                  "reference and treatment group cannot overlap"))
#    
#    validate(need(paramcd %in% ATE_filtered$PARAMCD, "selected PARAMCD not in ATE"))
#    validate(need(is.logical(combine_comp_arms), "need combine arm information"))
#    
#    validate(need(all(strata_var %in% names(ATE_filtered)),
#                  "some baseline risk variables are not valid"))
#    
#    
#    ## Now comes the static analysis code
#   
#    ASL_p <- ASL_filtered %>%
#      filter(ITTFL == 'Y', ARM %in% c(ref_arm, comp_arm))
#    
#    ATE_endpoint <- ATE_filtered %>%
#      filter(PARAMCD == 'OS')
#    
#    ATE_p <- reorder_to_match_id(ATE_endpoint, ref=ASL_p, key=c("USUBJID", "STUDYID"))
#    
#    ARM <- combine_levels(ASL$ARM, ref_arm)
#    
#    if (combine_comp_arms) {
#      ARM <- combine_levels(ARM, comp_arm)
#    }
#
#    strata_data <- if (length(strata_var) == 0) {
#      NULL
#    } else {
#      ASL[strata_var]
#    } 
#    
#    validate(need(nrow(ATE_p) > 15, "need at least 15 data points"))
#    
#    tbl <- try(
#      t_tte(
#        tte = ATE_p$AVAL,
#        is_event = ATE_p$CNSR == 0,
#        event_descr = ATE_p[[event_desrc_var]],
#        col_by = ARM,
#        strata_data = strata_data,
#        time_points = time_points
#      )
#    )
#    
#   if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate time to event table:\n\n", tbl)))
    
    tbl <- as.rtable(table(iris$Species))
    as_html(tbl)
  })
}
