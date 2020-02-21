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
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(ADSL, cached = TRUE)
#'
#' ADSL$RACE <- droplevels(ADSL$RACE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADTTE", ADTTE),
#'     code = 'ADSL <- ADSL <- radsl(cached = TRUE)
#'             ADTTE <- radtte(ADSL, cached = TRUE)
#'             ADSL$RACE <- droplevels(ADSL$RACE)',
#'     check = FALSE
#'     ),
#'   modules = root_modules(
#'     tm_g_forest_tte(
#'        label = "Forest Survival",
#'        dataname = "ADTTE",
#'        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'        subgroup_var = choices_selected(names(ADSL), c("RACE", "SEX")),
#'        paramcd = choices_selected(c("OS", "PFS"), "OS"),
#'        plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' \dontrun{
#'   shinyApp(app$ui, app$server)
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
                          "PARAMCD",
                          a$paramcd$choices,
                          a$paramcd$selected,
                          multiple = FALSE,
                          fixed = a$paramcd$fixed,
                          label_help = helpText("Select an endpoint to analyze.")
      ),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var$choices,
                          a$arm_var$selected,
                          multiple = FALSE,
                          fixed = a$arm_var$fixed
      ),
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
                          label_help = helpText("are taken from", tags$code("ADSL")),
                          fixed = a$subgroup_var$fixed
      ),
      tags$label("Plot Settings", class = "text-primary", style = "margin-top: 15px;"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_forest_tte <- function(input, output, session, datasets, dataname, cex = 1.5) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var", # from UI
    adsl = datasets$get_data("ADSL", filtered = FALSE, reactive = FALSE),
    arm_ref_comp = NULL,
    module = "tm_g_forest_tte"
  )

  output$forest_plot <- renderPlot({
    ADSL_FILTERED <- datasets$get_data("ADSL", reactive = TRUE, filtered = TRUE) #nolint
    ANL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE) #nolint

    paramcd <- input$paramcd
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var

    # validate your input values
    validate_standard_inputs(
      adsl = ADSL_FILTERED,
      adslvars = c("USUBJID", "STUDYID", arm_var, subgroup_var),
      anl = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "AVALU", "CNSR"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate_in(paramcd, ANL_FILTERED$PARAMCD, "Time-to-Event Endpoint cannot be found in PARAMCD")

    anl_data_name <- paste0(dataname, "_FILTERED")
    assign(anl_data_name, ANL_FILTERED)

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, subgroup_var)) #nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALU", "CNSR") #nolint

    chunks_push(bquote({
      ref_arm <- .(ref_arm)
      comp_arm <- .(comp_arm)
      adsl_p <- subset(ADSL_FILTERED, ADSL_FILTERED[[.(arm_var)]] %in% c(ref_arm, comp_arm))
      anl_p <- subset(.(as.name(anl_data_name)), PARAMCD %in% .(paramcd))

      anl <- merge(adsl_p[, .(adsl_vars)], anl_p[, .(anl_vars)],
                   all.x = FALSE, all.y = FALSE, by = c("USUBJID", "STUDYID"))
      arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])
      arm <- combine_levels(arm, ref_arm)
      arm <- combine_levels(arm, comp_arm)

      anl[[.(arm_var)]] <- droplevels(arm)

      levels(anl[[.(arm_var)]]) <- sapply(
        levels(anl[[.(arm_var)]]),
        function(x) paste(strwrap(x, width = 15), collapse = "\n")
      )
    }))

    chunks_safe_eval()
    anl <- chunks_get_var("anl")
    validate(need(nrow(anl) > 15, "need at least 15 data points"))

    chunks_push(bquote({
      tbl <- t_forest_tte(
        tte = anl$AVAL,
        is_event = anl$CNSR == 0,
        col_by = anl[[.(arm_var)]],
        time_unit = tolower(anl$AVALU[1]),
        row_by_list = if (length(.(subgroup_var)) > 0) {
            anl[, .(subgroup_var), drop = FALSE]
          } else {
            NULL
          },
        total = "All Patients",
        dense_header = TRUE
      )

      row.names(tbl) <- sapply(row.names(tbl), function(x) paste(strwrap(x, width = 20), collapse = "\n"))
    }))

    chunks_safe_eval()

    chunks_push(call(
      "g_forest",
      tbl = quote(tbl),
      col_x = 8,
      col_ci = 9,
      vline = 1,
      forest_header = bquote(paste0(rev(levels(anl[[.(arm_var)]])), "\nbetter")),
      xlim = c(.1, 10),
      logx = TRUE,
      x_at = c(.1, 1, 10)
    ))

    p <- chunks_safe_eval()

    p
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
        title = "Time-to-Event Forest Plot"
      )
    )
  })
}
