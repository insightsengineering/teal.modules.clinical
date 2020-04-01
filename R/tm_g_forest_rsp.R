#' Forest Response Plot teal module
#'
#' This is teal module produces a grid style Forest plot for response data with ADaM structure
#'
#' @param subgroup_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as the default subgroups
#' @param fixed_symbol_size (\code{logical}) When (\code{TRUE}), the same symbol size is used for plotting each
#' estimate. Otherwise, the symbol size will be proportional to the sample size in each each subgroup.
#' @param plot_height vector with three elements defining selected, min and max plot height
#' @param cex multiplier applied to overall font size
#'
#' @inheritParams tm_t_tte
#' @export
#'
#' @template author_song24
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADRS <- radrs(cached = TRUE) %>%
#'      dplyr::filter(PARAMCD %in% c("BESRSPI", "INVET"))
#'
#' app <- init(
#'   data = cdisc_data(
#'    cdisc_dataset("ADSL", ADSL),
#'    cdisc_dataset("ADRS", ADRS),
#'    code = 'ADSL <- radsl(cached = TRUE)
#'            ADRS <- radrs(cached = TRUE) %>%
#'            dplyr::filter(PARAMCD %in% c("BESRSPI", "INVET"))',
#'    check = FALSE),
#'   modules = root_modules(
#'     tm_g_forest_rsp(
#'       label = "Forest Response",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       paramcd = choices_selected(c("BESRSPI", "INVET"), "BESRSPI"),
#'       subgroup_var = choices_selected(names(ADSL), c("BMRKR2", "SEX")),
#'       strata_var = choices_selected(c("STRATA1", "STRATA2"), "STRATA2"),
#'       plot_height = c(600L, 200L, 2000L)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_forest_rsp <- function(label,
                            dataname,
                            arm_var,
                            paramcd,
                            subgroup_var,
                            strata_var,
                            conf_int = choices_selected(c(0.8, 0.85, 0.90, 0.95, 0.99, 0.995), 0.95, keep_order = TRUE),
                            fixed_symbol_size = TRUE,
                            plot_height = c(700L, 200L, 2000L),
                            cex = 1.3,
                            pre_output = helpText("graph needs to be of a certain width to be displayed"),
                            post_output = NULL) {

  stop_if_not(list(is_character_single(label), "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is_character_vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(subgroup_var))
  stopifnot(is.choices_selected(strata_var))
  stopifnot(is.choices_selected(conf_int))
  stopifnot(is_logical_single(fixed_symbol_size))
  stop_if_not(list(
    is_integer_vector(plot_height) && length(plot_height) == 3,
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
        "PARAMCD",
        a$paramcd$choices,
        a$paramcd$selected,
        multiple = FALSE,
        fixed = a$paramcd$fixed,
        label_help = helpText("Select one type of response to analyze.")
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
        multiple = FALSE,
        fixed = a$arm_var$fixed
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
        label_help = helpText("are taken from", tags$code("ADSL")),
        fixed = a$subgroup_var$fixed
      ),
      optionalSelectInput(ns("strata_var"),
                          "Stratify by",
                          a$strata_var$choices,
                          a$strata_var$selected,
                          multiple = TRUE,
                          label_help = helpText("from ", tags$code("ADSL")),
                          fixed = a$strata_var$fixed
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
      ),
      optionalSliderInputValMinMax(
        ns("plot_width"),
        "plot width",
        c(980L, 500L, 2000L),
        ticks = FALSE
      ),
      panel_group(
        panel_item(
        "Additional plot settings",
        optionalSelectInput(
          ns("conf_int"),
          "Level of Confidence",
          a$conf_int$choices,
          a$conf_int$selected,
          multiple = FALSE,
          fixed = a$conf_int$fixed
        ),
        checkboxInput(ns("fixed_symbol_size"), "Fixed symbol size", value = TRUE)
      )
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

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    adsl = datasets$get_data("ADSL", filtered = FALSE, reactive = FALSE),
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


  output$forest_plot <- renderPlot({
    adsl_filtered <- datasets$get_data("ADSL", reactive = TRUE, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    paramcd <- input$paramcd
    responders <- input$responders
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var
    strata_var <- input$strata_var
    conf_int <- as.numeric(input$conf_int)
    col_symbol_size <- if (input$fixed_symbol_size) {
      NULL
    } else {
      1
    }

    # validate your input values
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", arm_var, subgroup_var, strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "AVALC"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate_in(responders, anl_filtered$AVALC, "Responder values cannot be found in AVALC.")
    validate_in(paramcd, anl_filtered$PARAMCD, "Response parameter cannot be found in PARAMCD.")
    validate(
      need(length(conf_int) == 1, "Please select level of confidence."),
      need(all(vapply(adsl_filtered[, subgroup_var], is.factor, logical(1))),
           "Not all subgroup variables are factors.")
    )

    # perform analysis
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)
    adsl_name <- "ADSL_FILTERED"
    assign(adsl_name, adsl_filtered)

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, subgroup_var, strata_var)) # nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVALC") # nolint

    chunks_reset(envir = environment())

    chunks_push(bquote({

      adsl_p <- subset(.(as.name(adsl_name)), .(as.name(arm_var)) %in% c(.(ref_arm), .(comp_arm)))
      anl_p <- subset(.(as.name(anl_name)), PARAMCD %in% .(paramcd))

      adsl_p[, .(subgroup_var)] <- droplevels(adsl_p[, .(subgroup_var)])
      anl <- merge(adsl_p[, .(adsl_vars)], anl_p[, .(anl_vars)],
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
    }))

    chunks_safe_eval()
    anl <- chunks_get_var("anl")

    validate(need(nrow(anl) > 15, "need at least 15 data points"))
    validate(need(!any(duplicated(anl$USUBJID)), "patients have multiple records in the analysis data."))

    chunks_push(bquote({
      tbl <- t_forest_rsp(
        rsp = anl$AVALC %in% .(responders),
        col_by = anl[[.(arm_var)]],
        row_by_list = .(if (length(subgroup_var) > 0) {
          bquote(anl[, .(subgroup_var), drop = FALSE])
        } else {
          bquote(NULL)
        }),
        strata_data = if (!is.null(.(strata_var))) {
          anl[, .(strata_var), drop = FALSE]
        } else {
          NULL
        },
        conf_int = .(conf_int),
        total = "All Patients",
        dense_header = TRUE
      )

      row.names(tbl) <- sapply(
        row.names(tbl),
        function(x) {
          paste(strwrap(x, width = 20), collapse = "\n")
        }
      )
    }))

    chunks_safe_eval()

    chunks_push(bquote({
      p <- g_forest(
        tbl = tbl,
        col_x = 8,
        col_ci = 9,
        vline = 1,
        forest_header = paste0(levels(anl[[.(arm_var)]]), "\nbetter"),
        xlim = c(.1, 10),
        logx = TRUE,
        x_at = c(.1, 1, 10),
        draw = FALSE,
        col_symbol_size = .(col_symbol_size)
      )
      if (!is.null(footnotes(p))) {
        p <- decorate_grob(p, title = "Forest plot", footnotes = footnotes(p),
                           gp_footnotes = gpar(fontsize = 12))
      }
      grid.newpage()
      grid.draw(p)
    }))


    chunks_safe_eval()

  })

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    plot_width <- input$plot_width
    validate(need(plot_height, "need valid plot height"))
    validate(need(plot_width, "need valid plot width"))
    div(style = "overflow-x: scroll",
        plotOutput(session$ns("forest_plot"),
                   height = plot_height,
                   width = plot_width)
    )
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for the Current Reponse Forest Plot",
      rcode = get_rcode(datasets = datasets, title = "Response Forest Plot")
    )
  })

}
