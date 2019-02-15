#' Forest Survival Plot teal Module
#' 
#' This is teal module produces a grid style Forest plot for time-to-event data
#' with ADaM structure
#' 
#' @inheritParams tm_g_forest_rsp
#' 
#' @export
#' 
#' @template author_song24
#' 
#' @examples
#' 
#' library(random.cdisc.data)
#' library(dplyr)
#' 
#' ASL <- radsl(seed = 1) %>%
#'   mutate(RACE = droplevels(RACE))
#' ATE <- radtte(ASL, seed = 1)
#' 
#' attr(ASL, "source") <- "random.cdisc.data::radsl(seed = 1) %>% 
#'   mutate(RACE = droplevels(RACE))"
#' attr(ATE, "source") <- "random.cdisc.data::radtte(ASL, seed = 1)"
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_g_forest_tte(
#'        label = "Forest Survival",
#'        dataname = "ATE",
#'        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'        subgroup_var = choices_selected(names(ASL), c("RACE", "SEX")),
#'        paramcd = choices_selected(c("OS", "PFS"), "OS"),
#'        plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#' 
#' \dontrun{
#' 
#' shinyApp(x$ui, x$server) 
#' 
#' } 
tm_g_forest_tte <- function(label,
                            dataname,
                            arm_var,
                            subgroup_var,
                            paramcd,
                            plot_height = c(600, 200, 2000),
                            cex = 1.3,
                            pre_output = helpText("graph needs to be of a certain width to be displayed"),
                            post_output = NULL,
                            code_data_processing = NULL) {
  
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(subgroup_var))
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_g_forest_tte,
    ui = ui_g_forest_tte,
    ui_args = args,
    server_args = list(dataname = dataname, cex = cex, code_data_processing = code_data_processing),
    filters = dataname
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
                          a$paramcd$choices, a$paramcd$selected, multiple = FALSE),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var$choices, a$arm_var$selected, multiple = FALSE),
      selectInput(ns("ref_arm"), "Reference Arm", 
                  choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Arm", choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      optionalSelectInput(ns("subgroup_var"), "Subgroup Variables", a$subgroup_var$choices, a$subgroup_var$selected, multiple = TRUE,
                          label_help = helpText("are taken from", tags$code("ASL"))),
      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
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
    module = "tm_g_forest_tte"
  )
  
  
  chunks <- list(
    vars = "# Not Calculated",
    data = "# Not Calculated",
    t_forest_tte = "# Not Calculated",
    row_name_wrap = "# Not Calculated",
    p_forest_tte = "# Not Calculated"
  )
  
  output$forest_plot <- renderPlot({
    
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ANL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    paramcd <- input$paramcd
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var
    
    # as.global(ASL_FILTERED, ANL_FILTERED, paramcd, arm_var, ref_arm, comp_arm, subgroup_var)
    
    # Delete chunks that are used for reproducible code
    for (i in seq_along(chunks)) chunks[[i]] <<- "# Not calculated"
    
    # validate your input values
    validate_standard_inputs(
      ASL = ASL_FILTERED,
      aslvars = c("USUBJID", "STUDYID", arm_var, subgroup_var),
      ANL = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "AVALU", "CNSR"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )
    
    validate_in(paramcd, ANL_FILTERED$PARAMCD, "Time-to-Event Endpoint cannot be found in PARAMCD")
    
    anl_data_name <- paste0(dataname, "_FILTERED")
    assign(anl_data_name, ANL_FILTERED)
    
    chunks$vars <<- bquote({
      ref_arm <- .(ref_arm)
      comp_arm <- .(comp_arm)
    })
    
    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, subgroup_var))
    anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALU", "CNSR")
    
    chunks$data <<- bquote({
      ASL_p <- subset(ASL_FILTERED, ASL_FILTERED[[.(arm_var)]] %in% c(ref_arm, comp_arm))
      ANL_p <- subset(.(as.name(anl_data_name)), PARAMCD %in% .(paramcd))
      
      ANL <- merge(ASL_p[, .(asl_vars)], ANL_p[, .(anl_vars)],
                   all.x = FALSE, all.y = FALSE, by = c("USUBJID", "STUDYID"))
      
      ARM <- relevel(as.factor(ANL[[.(arm_var)]]), ref_arm[1])
      
      ARM <- combine_levels(ARM, ref_arm)
      ARM <- combine_levels(ARM, comp_arm)
      
      ANL[[.(arm_var)]] <- droplevels(ARM)
      
      levels(ANL[[.(arm_var)]]) <- sapply(levels(ANL[[.(arm_var)]]), function(x) paste(strwrap(x, width = 15), collapse = "\n"))
      
    })
    
    eval(chunks$data)
    validate(need(nrow(ANL) > 15, "need at least 15 data points"))
    
    chunks$t_forest_tte <<- call(
      "t_forest_tte",
      tte = bquote(ANL$AVAL),
      is_event = bquote(ANL$CNSR == 0),
      col_by = bquote(ANL[[.(arm_var)]]),
      time_unit = bquote(tolower(ANL$AVALU[1])),
      group_data = if (length(subgroup_var) > 0) bquote({ANL[, .(subgroup_var), drop=FALSE]}) else NULL,
      total = "All Patients",
      na.omit.group = TRUE,
      dense_header = TRUE
    )
    
    tbl <- try(eval(chunks$t_forest_tte))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate forest table:\n\n", tbl)))
    
    
    chunks$row_name_wrap <<- quote({
      row.names(tbl) <- sapply(row.names(tbl), function(x) paste(strwrap(x, width = 20), collapse = "\n"))
    })
    
    chunks$p_forest_tte <<- call(
      "g_forest",
      tbl = quote(tbl),
      col_x = 8,
      col_ci = 9,
      vline = 1,
      forest_header = bquote(paste0(rev(levels(ANL[[.(arm_var)]])), "\nbetter")),
      xlim = c(.1, 10),
      logx = TRUE,
      x_at = c(.1, 1, 10)
    )
    
    eval(chunks$row_name_wrap)
    eval(chunks$p_forest_tte)
    #    if (is(p, "try-error")) validate(need(FALSE, paste0("could not calculate forest plot:\n\n", p)))
  })
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "Time-to-Event Forest Plot",
      datanames = if (is.null(code_data_processing)) dataname else datasets$datanames(), 
      datasets = datasets,
      code_data_processing
    )
    
    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunks$vars)),
      "",
      remove_enclosing_curly_braces(deparse(chunks$data)),
      "",
      paste("tbl <-", paste(deparse(chunks$t_forest_tte), collapse = "\n")),
      "",
      remove_enclosing_curly_braces(deparse(chunks$row_name_wrap)),
      "",
      remove_enclosing_curly_braces(deparse(chunks$p_forest_tte))
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Time-to-Event Forest Plot",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
    
  })
}
