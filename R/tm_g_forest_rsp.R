#' Forest Response Plot teal module
#'
#' This is teal module produces a grid style Forest plot for response data with ADaM structure
#'
#' @inheritParams tm_t_tte
#' @param subgroup_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as the default subgroups
#' @param plot_height vector with three elements defining selected, min and max plot height
#' @param cex multiplier applied to overall fontsize
#'
#' @export
#'
#' @template author_song24
#'
#' @examples
#'
#' library(random.cdisc.data)
#'
#' asl <- radsl(seed = 1)
#' ars <- subset(radrs(asl, seed = 1), AVISIT == "Follow Up")
#'
#' attr(asl, "source") <- "random.cdisc.data::radsl(seed = 1)"
#' attr(ars, "source") <- 'subset(random.cdisc.data::radrs(asl, seed = 1), AVISIT == "Follow Up")'
#'
#' x <- teal::init(
#'   data = list(ASL = asl, ARS = ars),
#'   modules = root_modules(
#'     tm_g_forest_rsp(
#'        label = "Forest Response",
#'        dataname = "ARS",
#'        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'        paramcd = choices_selected(c("BESRSPI", "INVET", "OVRINV" ), "OVRINV"),
#'        subgroup_var = choices_selected(names(asl), c("RACE", "SEX")),
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
tm_g_forest_rsp <- function(label,
                            dataname,
                            arm_var,
                            paramcd,
                            subgroup_var,
                            plot_height = c(700, 200, 2000),
                            cex = 1.3,
                            pre_output = helpText("graph needs to be of a certain width to be displayed"),
                            post_output = NULL,
                            code_data_processing = NULL){

  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(subgroup_var))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_forest_rsp,
    ui = ui_g_forest_rsp,
    ui_args = args,
    server_args = list(dataname = dataname, cex = cex, code_data_processing = code_data_processing),
    filters = dataname
  )
}


ui_g_forest_rsp <- function(id, ...) {

  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("paramcd"),
                          div("PARAMCD", tags$br(), helpText("Select one type of response to analyze.")),
                          a$paramcd$choices,
                          a$paramcd$selected,
                          multiple = FALSE),
      selectInput(ns("responders"),
                  "Responders",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var$choices,
                          a$arm_var$selected,
                          multiple = FALSE),
      selectInput(ns("ref_arm"),
                  "Reference Arm",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      selectInput(ns("comp_arm"),
                  "Comparison Arm",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      optionalSelectInput(ns("subgroup_var"),
                          "Subgroup Variables",
                          a$subgroup_var$choices,
                          a$subgroup_var$selected,
                          multiple = TRUE,
                          label_help = helpText("are taken from", tags$code("ASL"))),
      tags$label("Plot Settings",
                 class = "text-primary",
                 style = "margin-top: 15px;"),
      optionalSliderInputValMinMax(ns("plot_height"),
                                   "plot height",
                                   a$plot_height,
                                   ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"),
                         "Show R Code",
                         width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_g_forest_rsp <- function(input, output, session, datasets, dataname, cex = 1.5, code_data_processing) {

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("forest_plot"), height = plot_height)
  })

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  teal.devel::arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",    # from UI
    asl = datasets$get_data("ASL", filtered = FALSE, reactive = FALSE),
    arm_ref_comp = NULL,
    module = "tm_g_forest_rsp"
  )


  # Update UI choices depending on selection of previous options
  observe({

    paramcd <- input$paramcd

    anl <- datasets$get_data(dataname, filtered = FALSE, reactive = FALSE)

    rsp_choices <- unique(anl$AVALC[anl$PARAMCD == paramcd])

    updateSelectInput(session, "responders",
                      choices = rsp_choices,
                      selected = intersect(rsp_choices, c("CR", "PR")))

  })


  chunks <- list(
    vars = "# Not Calculated",
    data = "# Not Calculated",
    t_forest_rsp = "# Not Calculated",
    row_name_wrap = "# Not Calculated",
    p_forest_rsp = "# Not Calculated"
  )

  output$forest_plot <- renderPlot({

    asl_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    paramcd <- input$paramcd
    responders <- input$responders
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var

    teal.devel::as.global(asl_filtered, anl_filtered, paramcd, responders, arm_var, ref_arm, comp_arm, subgroup_var)

    # Delete chunks that are used for reproducible code
    for (i in seq_along(chunks)) chunks[[i]] <<- "# Not calculated"

    # validate your input values
    teal.devel::validate_standard_inputs(
      asl = asl_filtered,
      aslvars = c("USUBJID", "STUDYID", arm_var, subgroup_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "AVALC"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    teal.devel::validate_in(responders, anl_filtered$AVALC, "responder values cannot be found in AVALC")
    teal.devel::validate_in(paramcd, anl_filtered$PARAMCD, "Response parameter cannot be found in PARAMCD")

    # perform analysis
    anl_data_name <- paste0(dataname, "_filtered")
    assign(anl_data_name, anl_filtered)

    chunks$vars <<- bquote({
      ref_arm <- .(ref_arm)
      comp_arm <- .(comp_arm)
    })

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, subgroup_var)) # nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVALC") # nolint

    chunks$data <<- bquote({
      asl_p <- subset(asl_filtered, .(as.name(arm_var)) %in% c(ref_arm, comp_arm))
      anl_p <- subset(.(as.name(anl_data_name)), PARAMCD %in% .(paramcd))

      anl <- merge(asl_p[, .(asl_vars)], anl_p[, .(anl_vars)],
                   all.x = FALSE, all.y = FALSE, by = c("USUBJID", "STUDYID"))

      arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])

      arm <- combine_levels(arm, ref_arm)
      arm <- combine_levels(arm, comp_arm)

      anl[[.(arm_var)]] <- droplevels(arm)

      levels(anl[[.(arm_var)]]) <- sapply(
        levels(anl[[.(arm_var)]]),
        function(x) paste(strwrap(x, width = 15), collapse = "\n")
      )
    })

    eval(chunks$data)
    validate(need(nrow(anl) > 15, "need at least 15 data points"))
    validate(need(!any(duplicated(anl$USUBJID)), "patients have multiple records in the analysis data."))

    chunks$t_forest_rsp <<- call(
      "t_forest_rsp",
      rsp = bquote(anl$AVALC %in% .(responders)),
      col_by = bquote(anl[[.(arm_var)]]),
      group_data = if (length(subgroup_var) > 0){
          bquote({
            anl[, .(subgroup_var), drop = FALSE]
          })
        } else {
          NULL
        },
      total = "All Patients",
      na_omit_group = TRUE,
      dense_header = TRUE
    )

    tbl <- try(eval(chunks$t_forest_rsp))

    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate forest table:\n\n", tbl)))


    chunks$row_name_wrap <<- quote({
      row.names(tbl) <- sapply(row.names(tbl), function(x) paste(strwrap(x, width = 20), collapse = "\n"))
    })

    chunks$p_forest_rsp <<- call(
      "g_forest",
      tbl = quote(tbl),
      col_x = 8,
      col_ci = 9,
      vline = 1,
      forest_header = bquote(paste0(levels(anl[[.(arm_var)]]), "\nbetter")),
      xlim = c(.1, 10),
      logx = TRUE,
      x_at = c(.1, 1, 10)
    )

    eval(chunks$row_name_wrap)
    eval(chunks$p_forest_rsp)
  })


  observeEvent(input$show_rcode, {

    header <- teal.devel::get_rcode_header(
      title = "Response Forest Plot",
      datanames = if (is.null(code_data_processing)) dataname else datasets$datanames(),
      datasets = datasets,
      code_data_processing
    )

    str_rcode <- paste(c(
      "",
      header,
      "",
      teal.devel::remove_enclosing_curly_braces(deparse(chunks$vars)),
      "",
      teal.devel::remove_enclosing_curly_braces(deparse(chunks$data)),
      "",
      paste("tbl <-", paste(deparse(chunks$t_forest_rsp), collapse = "\n")),
      "",
      teal.devel::remove_enclosing_curly_braces(deparse(chunks$row_name_wrap)),
      "",
      teal.devel::remove_enclosing_curly_braces(deparse(chunks$p_forest_rsp))
    ), collapse = "\n")

    showModal(
      modalDialog(
        title = "R Code for the Current Reponse Forest Plot",
        tags$pre(tags$code(class = "R", str_rcode)),
        easyClose = TRUE,
        size = "l"
      )
    )
  })

}
