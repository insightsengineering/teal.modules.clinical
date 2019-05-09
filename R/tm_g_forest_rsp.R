#' Forest Response Plot teal module
#'
#' This is teal module produces a grid style Forest plot for response data with ADaM structure
#'
#' @param subgroup_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as the default subgroups
#' @param plot_height vector with three elements defining selected, min and max plot height
#' @param cex multiplier applied to overall fontsize
#'
#' @inheritParams tm_t_tte
#' @export
#'
#' @template author_song24
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' asl <- radsl(seed = 1)
#' ars <- subset(radrs(asl, seed = 1), AVISIT == "Follow Up")
#'
#' keys(asl) <- c("STUDYID", "USUBJID")
#' keys(ars) <- c("STUDYID", "USUBJID")
#'
#' x <- teal::init(
#'   data = cdisc_data(
#'    ASL = asl,
#'    ARS = ars,
#'    code = 'library(tern)
#'            asl <- random.cdisc.data::radsl(seed = 1)
#'            ars <- subset(random.cdisc.data::radrs(asl, seed = 1), AVISIT == "Follow Up")
#'            keys(asl) <- c("STUDYID", "USUBJID")
#'            keys(ars) <- c("STUDYID", "USUBJID")',
#'    check = FALSE),
#'   modules = root_modules(
#'     tm_g_forest_rsp(
#'       label = "Forest Response",
#'       dataname = "ARS",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       paramcd = choices_selected(c("BESRSPI", "INVET", "OVRINV" ), "OVRINV"),
#'       subgroup_var = choices_selected(names(asl), c("RACE", "SEX")),
#'       plot_height = c(600L, 200L, 2000L)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
tm_g_forest_rsp <- function(label,
                            dataname,
                            arm_var,
                            paramcd,
                            subgroup_var,
                            plot_height = c(700L, 200L, 2000L),
                            cex = 1.3,
                            pre_output = helpText("graph needs to be of a certain width to be displayed"),
                            post_output = NULL) {

  stop_if_not(list(is.character.single(label), "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is.character.vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(subgroup_var))
  stop_if_not(list(
    is.integer.vector(plot_height) && length(plot_height) == 3,
    "plot_height should be vector of three integers specyfing selected height, min and max height"
  ))
  stop_if_not(list(
    plot_height[2] < plot_height[3] && plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3],
    "selected plot_height should be between min and max, min should be lower than max"
  ))
  stop_if_not(list(is_numeric_single(cex), "cex should be single numeric object))"))
  stop_if_not(list(
    is.null(pre_output) || is(pre_output, "shiny.tag"),
    "pre_output should be either null or shiny.tag type of object"
  ))
  stop_if_not(list(
    is.null(pre_output) || is(pre_output, "shiny.tag"),
    "pre_output should be either null or shiny.tag type of object"
  ))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_forest_rsp,
    ui = ui_g_forest_rsp,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      cex = cex
    ),
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
      optionalSelectInput(
        ns("paramcd"),
        div("PARAMCD", tags$br(), helpText("Select one type of response to analyze.")),
        a$paramcd$choices,
        a$paramcd$selected,
        multiple = FALSE
      ),
      selectInput(
        ns("responders"),
        "Responders",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        a$arm_var$choices,
        a$arm_var$selected,
        multiple = FALSE
      ),
      selectInput(
        ns("ref_arm"),
        "Reference Arm",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      selectInput(
        ns("comp_arm"),
        "Comparison Arm",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      optionalSelectInput(
        ns("subgroup_var"),
        "Subgroup Variables",
        a$subgroup_var$choices,
        a$subgroup_var$selected,
        multiple = TRUE,
        label_help = helpText("are taken from", tags$code("ASL"))
      ),
      tags$label(
        "Plot Settings",
        class = "text-primary",
        style = "margin-top: 15px;"
      ),
      optionalSliderInputValMinMax(
        ns("plot_height"),
        "plot height",
        a$plot_height,
        ticks = FALSE
      )
    ),
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_g_forest_rsp <- function(input,
                             output,
                             session,
                             datasets,
                             dataname,
                             cex) {

  use_chunks(session)

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    asl = datasets$get_data("ASL", filtered = FALSE, reactive = FALSE),
    arm_ref_comp = NULL,
    module = "tm_g_forest_rsp"
  )


  # Update UI choices depending on selection of previous options
  observe({
    paramcd <- input$paramcd
    anl <- datasets$get_data(dataname, filtered = FALSE, reactive = FALSE)
    rsp_choices <- unique(anl$AVALC[anl$PARAMCD == paramcd])

    updateSelectInput(
      session, "responders",
      choices = rsp_choices,
      selected = intersect(rsp_choices, c("CR", "PR"))
    )
  })

  tm_g_forest_rsp_call <- reactive({
    asl_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    paramcd <- input$paramcd
    responders <- input$responders
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var

    # validate your input values
    validate_standard_inputs(
      asl = asl_filtered,
      aslvars = c("USUBJID", "STUDYID", arm_var, subgroup_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "AVALC"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate_in(responders, anl_filtered$AVALC, "responder values cannot be found in AVALC")
    validate_in(paramcd, anl_filtered$PARAMCD, "Response parameter cannot be found in PARAMCD")

    # perform analysis
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)
    asl_name <- "ASL_FILTERED"
    assign(asl_name, asl_filtered)

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, subgroup_var)) # nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVALC") # nolint

    renew_chunk_environment(envir = environment())
    renew_chunks()

    chunk_data_expr <- bquote({
      asl_p <- subset(.(as.name(asl_name)), .(as.name(arm_var)) %in% c(.(ref_arm), .(comp_arm)))
      anl_p <- subset(.(as.name(anl_name)), PARAMCD %in% .(paramcd))

      anl <- merge(asl_p[, .(asl_vars)], anl_p[, .(anl_vars)],
                   all.x = FALSE, all.y = FALSE, by = c("USUBJID", "STUDYID"))

      arm <- relevel(as.factor(anl[[.(arm_var)]]), .(ref_arm)[1])

      arm <- combine_levels(arm, .(ref_arm))
      arm <- combine_levels(arm, .(comp_arm))

      anl[[.(arm_var)]] <- droplevels(arm)

      levels(anl[[.(arm_var)]]) <- sapply(
        levels(anl[[.(arm_var)]]),
        function(x) {
          paste(strwrap(x, width = 15), collapse = "\n")
        }
      )

      anl
    })
    set_chunk("tm_g_forest_rsp_data", chunk_data_expr)

    chunk_table_expr <- bquote(
      tbl <- t_forest_rsp(
        rsp = anl$AVALC %in% .(responders),
        col_by = anl[[.(arm_var)]],
        group_data = .(if (length(subgroup_var) > 0) {
          bquote(anl[, .(subgroup_var), drop = FALSE])
        } else {
          bquote(NULL)
        }),
        total = "All Patients",
        na_omit_group = TRUE,
        dense_header = TRUE
      )
    )
    set_chunk("tm_g_forest_rsp_table", chunk_table_expr)

    chunk_row_expr <- quote(
      row.names(tbl) <- sapply(
        row.names(tbl),
        function(x) {
          paste(strwrap(x, width = 20), collapse = "\n")
        }
      )
    )
    set_chunk("tm_g_forest_rsp_row", chunk_row_expr)

    chunk_g_expr <- call(
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
    set_chunk("tm_g_forest_rsp", chunk_g_expr)

    invisible(NULL)
  })

  output$forest_plot <- renderPlot({
    tm_g_forest_rsp_call()

    anl <- eval_chunk("tm_g_forest_rsp_data")
    validate(need(nrow(anl) > 15, "need at least 15 data points"))
    validate(need(!any(duplicated(anl$USUBJID)), "patients have multiple records in the analysis data."))


    tbl <- try(eval_chunk("tm_g_forest_rsp_table"))
    if (is(tbl, "try-error")) {
      validate(need(FALSE, paste0("could not calculate forest table:\n\n", tbl)))
    }

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
      title = "R Code for the Current Reponse Forest Plot",
      rcode = get_rcode(
        datasets = datasets,
        dataname = dataname,
        title = "Response Forest Plot"
      )
    )
  })

}
