#' Forest Survival Plot teal Module
#'
#' This is teal module produces a grid style Forest plot for time-to-event data
#' with ADaM structure
#'
#' @inheritParams tm_g_forest_rsp
#'
#'
#'
#' @export
#'
#' @template author_song24
#'
#' @examples
#' # reproducible teal app
#' library(random.cdisc.data)
#' library(teal)
#'
#' asl <- radsl(seed = 1)
#' ate <- radtte(asl, seed = 1)
#'
#' asl$RACE <- droplevels(asl$RACE)
#' keys(asl) <- keys(ate) <- c("USUBJID", "STUDYID")
#'
#' x <- init(
#'   data = cdisc_data(
#'     ASL = asl,
#'     ATE = ate,
#'     code = 'library(tern)
#'             asl <- random.cdisc.data::radsl(seed = 1)
#'             ate <- random.cdisc.data::radtte(asl, seed = 1)
#'             asl$RACE <- droplevels(asl$RACE)
#'             keys(asl) <- keys(ate) <- c("USUBJID", "STUDYID")',
#'     check = FALSE
#'     ),
#'   modules = root_modules(
#'     tm_g_forest_tte(
#'        label = "Forest Survival",
#'        dataname = "ATE",
#'        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'        subgroup_var = choices_selected(names(asl), c("RACE", "SEX")),
#'        paramcd = choices_selected(c("OS", "PFS"), "OS"),
#'        plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' \dontrun{
#'   shinyApp(x$ui, x$server)
#' }
tm_g_forest_tte <- function(label,
                            dataname,
                            arm_var,
                            subgroup_var,
                            paramcd,
                            plot_height = c(600, 200, 2000),
                            cex = 1.3,
                            pre_output = helpText("graph needs to be of a certain width to be displayed"),
                            post_output = NULL) {

  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(subgroup_var))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_forest_tte,
    ui = ui_g_forest_tte,
    ui_args = args,
    server_args = list(dataname = dataname, cex = cex),
    filters = dataname
  )
}


ui_g_forest_tte <- function(id, ...) {

  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("paramcd"),
                          div("PARAMCD", tags$br(), helpText("Select an endpoint to analyze.")),
                          a$paramcd$choices,
                          a$paramcd$selected,
                          multiple = FALSE),
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
      tags$label("Plot Settings", class = "text-primary", style = "margin-top: 15px;"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_g_forest_tte <- function(input, output, session, datasets, dataname, cex = 1.5) {
  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  teal.devel::arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",    # from UI
    asl = datasets$get_data("ASL", filtered = FALSE, reactive = FALSE),
    arm_ref_comp = NULL,
    module = "tm_g_forest_tte"
  )
  use_chunks(session)

  table_reactive <- reactive({
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE) #nolint
    ANL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE) #nolint

    paramcd <- input$paramcd
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var

    # validate your input values
    teal.devel::validate_standard_inputs(
      asl = ASL_FILTERED,
      aslvars = c("USUBJID", "STUDYID", arm_var, subgroup_var),
      anl = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "AVALU", "CNSR"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    teal.devel::validate_in(paramcd, ANL_FILTERED$PARAMCD, "Time-to-Event Endpoint cannot be found in PARAMCD")

    anl_data_name <- paste0(dataname, "_FILTERED")
    assign(anl_data_name, ANL_FILTERED)

    # Delete chunks that are used for reproducible code
    renew_chunk_environment(envir = environment())
    renew_chunks()

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, subgroup_var)) #nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALU", "CNSR") #nolint

    set_chunk(
      id = "tm_g_forest_tte_anl",
      expression = bquote({
        ref_arm <- .(ref_arm)
        comp_arm <- .(comp_arm)
        asl_p <- subset(ASL_FILTERED, ASL_FILTERED[[.(arm_var)]] %in% c(ref_arm, comp_arm))
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

        anl
    }))

    set_chunk(
      id = "tm_g_forest_tte_tbl",
      expression = bquote({
        tbl <- t_forest_tte(
          tte = anl$AVAL,
          is_event = anl$CNSR == 0,
          col_by = anl[[.(arm_var)]],
          time_unit = tolower(anl$AVALU[1]),
          group_data = if (length(.(subgroup_var)) > 0) {
              anl[, .(subgroup_var), drop = FALSE]
            } else {
              NULL
            },
          total = "All Patients",
          na_omit_group = TRUE,
          dense_header = TRUE
        )

        row.names(tbl) <- sapply(row.names(tbl), function(x) paste(strwrap(x, width = 20), collapse = "\n"))
        tbl
    }))

    set_chunk(
      id = "tm_g_forest_tte_plot",
      expression = call(
          "g_forest",
          tbl = quote(tbl),
          col_x = 8,
          col_ci = 9,
          vline = 1,
          forest_header = bquote(paste0(rev(levels(anl[[.(arm_var)]])), "\nbetter")),
          xlim = c(.1, 10),
          logx = TRUE,
          x_at = c(.1, 1, 10)
      )
    )

    invisible(NULL)
  })

  output$forest_plot <- renderPlot({
    table_reactive()

    anl <- eval_chunk("tm_g_forest_tte_anl")
    validate(need(nrow(anl) > 15, "need at least 15 data points"))

    tbl <- eval_chunk("tm_g_forest_tte_tbl")
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate forest table:\n\n", tbl)))

    eval_remaining()
  })

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("forest_plot"), height = plot_height)
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for the Current Time-to-Event Forest Plot",
      rcode = get_rcode(
        datasets = datasets,
        dataname = dataname,
        title = "Time-to-Event Forest Plot"
      )
    )
  })
}
