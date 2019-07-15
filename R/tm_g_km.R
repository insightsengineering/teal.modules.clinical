#' teal module for Kaplan-Meier Plot from grid
#'
#' This is teal module produces a grid style KM plot for data with ADaM structure
#'
#' @inheritParams tm_t_tte
#' @param facet_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for facet plotting
#' @param plot_height vector with three elements defining selected, min and max plot height
#'
#' @importFrom survival Surv strata
#' @importFrom stats as.formula
#' @importFrom gridExtra arrangeGrob
#' @import grid
#'
#' @template author_wangh107
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ASL <- cadsl
#' ATE <- cadtte
#'
#' keys(ASL) <- c("USUBJID", "STUDYID")
#' keys(ATE) <- c("USUBJID", "STUDYID", "PARAMCD")
#'
#' arm_ref_comp <- list(
#'   ARM = list(
#'     ref = "A: Drug X",
#'     comp = c("B: Placebo", "C: Combination")
#'   ),
#'   ARMCD = list(
#'     ref = "ARM B",
#'     comp = "ARM A"
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL, ATE = ATE,
#'     code = "ASL <- cadsl
#'             ATE <- cadtte
#'             keys(ASL) <- c('USUBJID', 'STUDYID')
#'             keys(ATE) <- c('USUBJID', 'STUDYID', 'PARAMCD')",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_km(
#'       label = "KM PLOT",
#'       dataname = "ATE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(c("OS", "PFS"), "OS"),
#'       facet_var = choices_selected(c("SEX", "BMRKR2"), "BMRKR2"),
#'       strata_var = choices_selected(c("SEX", "BMRKR2"), "SEX")
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
tm_g_km <- function(label,
                    dataname,
                    arm_var,
                    arm_ref_comp = NULL,
                    paramcd,
                    facet_var,
                    strata_var,
                    plot_height = c(1200, 400, 5000),
                    pre_output = helpText("x-axes for different factes may not have the same scale"),
                    post_output = NULL) {
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(facet_var))
  stopifnot(is.choices_selected(strata_var))

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_km,
    server_args = list(
      dataname = dataname,
      arm_ref_comp = arm_ref_comp,
      label = label
    ),
    ui = ui_g_km,
    ui_args = args
  )
}


ui_g_km <- function(id, ...) {
  a <- list(...)
  ns <- NS(id)

  standard_layout(
    output = white_small_well(plot_height_output(id = ns("myplot"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code(a$dataname)),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        choices = a$arm_var$choices,
        selected = a$arm_var$selected,
        multiple = FALSE
      ),
      optionalSelectInput(
        ns("paramcd"),
        "Time to Event (Endpoint)",
        choices = a$paramcd$choices,
        selected = a$paramcd$selected,
        multiple = FALSE
      ),
      optionalSelectInput(
        ns("strata_var"),
        "Stratify by",
        choices = a$strata_var$choices,
        selected = a$strata_var$selected,
        multiple = TRUE,
        label_help = helpText("currently taken from ASL")
      ),
      optionalSelectInput(
        ns("facet_var"),
        "Facet Plots by:",
        choices = a$facet_var$choices,
        selected = a$facet_var$selected,
        multiple = TRUE,
        label_help = helpText("currently taken from ASL")
      ),
      selectInput(
        ns("ref_arm"),
        "Reference Arm",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      helpText("Reference groups automatically combined into a single group if more than one value selected."),
      selectInput(
        ns("comp_arm"),
        "Comparison Group",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      checkboxInput(
        ns("combine_comp_arms"),
        "Combine all comparison groups?",
        value = FALSE
      ),
      tags$label("Plot Settings", class = "text-primary"),
      helpText("X-axis label will be combined with variable ", tags$code("AVALU")),
      textInput(ns("xlab"), "X-axis label", "Overall survival in "),
      plot_height_input(id = ns("myplot"), value = a$plot_height),
      accordion(
        accordion_panel(
          "Additional plot settings",
          numericInput(
            inputId = ns('font_size'),
            label = "Font size",
            value = 8,
            min = 5,
            max = 15,
            step = 1,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns('show_km_table'),
            label = "Show KM table",
            value = TRUE,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns('show_coxph_table'),
            label = "Show CoxPH table",
            value = TRUE,
            width = "100%"
          )
        )
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_g_km <- function(input,
                     output,
                     session,
                     datasets,
                     dataname,
                     arm_ref_comp,
                     label) {

  init_chunks()

  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",
    asl = datasets$get_data("ASL", filtered = FALSE, reactive = FALSE),
    arm_ref_comp = arm_ref_comp,
    module = "tm_g_km"
  )

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("kmplot"), height = plot_height)
  })

  output$plot <- renderPlot({
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE) # nolint

    paramcd <- input$paramcd
    arm_var <- input$arm_var
    facet_var <- input$facet_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    strata_var <- input$strata_var
    combine_comp_arms <- input$combine_comp_arms
    xlab <- input$xlab
    tbl_fontsize <- input$font_size
    if_show_km <- input$show_km_table
    if_show_coxph <- input$show_coxph_table

    if (length(facet_var) == 0) {
      facet_var <<- NULL
    }
    if (length(strata_var) == 0) {
      strata_var <- NULL
    }

    validate_standard_inputs(
      asl = ASL_FILTERED,
      aslvars = c("USUBJID", "STUDYID", arm_var, strata_var, facet_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "CNSR", "AVALU"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate(need(is.logical(combine_comp_arms), "need combine arm information"))

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)

    chunks_reset(envir = environment())

    chunks_push(expression = bquote(ref_arm <- .(ref_arm)))
    chunks_push(expression = bquote(comp_arm <- .(comp_arm)))
    chunks_push(expression = bquote(strata_var <- .(strata_var)))
    chunks_push(expression = bquote(facet_var <- .(facet_var)))
    chunks_push(expression = bquote(combine_comp_arms <- .(combine_comp_arms)))

    chunks_push(expression = bquote(asl_vars <- unique(c("USUBJID", "STUDYID", .(arm_var), .(strata_var), .(facet_var)))))

    chunks_push(expression = bquote(asl_p <- subset(ASL_FILTERED, .(as.name(arm_var)) %in% c(ref_arm, comp_arm))))
    chunks_push(expression = bquote(anl_p <- subset(.(as.name(anl_name)), PARAMCD %in% .(paramcd))))
    chunks_push(expression = bquote(anl <- merge(asl_p[, asl_vars],
      anl_p[, c("USUBJID", "STUDYID", "AVAL", "CNSR", "AVALU")],
      all.x = FALSE, all.y = FALSE, by = c("USUBJID", "STUDYID")
    )))
    chunks_push(expression = bquote(arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])))
    chunks_push(expression = bquote(arm <- combine_levels(arm, ref_arm)))

    if (combine_comp_arms) {
      chunks_push(expression = bquote(arm <- combine_levels(arm, comp_arm)))
    }

    chunks_push(expression = bquote(anl[[.(arm_var)]] <- droplevels(arm)))
    chunks_push(expression = bquote(time_unit <- unique(anl[["AVALU"]])))
    chunks_push(expression = bquote(tbl_fontsize <- .(tbl_fontsize)))

    chunks_eval()

    validate(need(nrow(chunks_get_var("anl")) > 15, "need at least 15 data points"))
    validate(need(length(chunks_get_var("time_unit")) == 1, "Time Unit is not consistant"))

    chunks_push(expression = bquote(formula_km <- as.formula(.(paste0("Surv(AVAL, 1-CNSR) ~ ", arm_var)))))

    chunks_push(expression = bquote(formula_coxph <- as.formula(
      .(paste0(
        "Surv(AVAL, 1-CNSR) ~ ", arm_var,
        ifelse(is.null(strata_var), "", paste0(" + strata(", paste(strata_var, collapse = ","), ")"))
      ))
    )))
    chunks_push(expression = bquote(info_coxph <- .(paste0(
      "Cox Proportional Model: ",
      ifelse(is.null(strata_var),
             "Unstratified Analysis",
             paste0("Stratified by ", paste(strata_var, collapse = ","))
      )
    ))))

    if (is.null(facet_var)) {
      chunks_push(expression = bquote({
        fit_km <- survfit(formula_km, data = anl, conf.type = "plain")
        grid.newpage()
        p <- g_km(fit_km = fit_km, col = NA, draw = FALSE, xlab = paste(.(xlab), time_unit))

        .(
          if (if_show_km) {
            bquote({
              tbl_km <- t_km(fit_km)
              km_grob <- textGrob(
                label = toString(tbl_km, gap = 1),
                x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
                y = unit(1, "npc") - unit(1, "lines"),
                just = c("left", "top"),
                gp = gpar(fontfamily = "mono", fontsize = tbl_fontsize, fontface = "bold"),
                vp = vpPath("mainPlot", "kmCurve", "curvePlot")
              )
              p <- addGrob(p, km_grob)
            })
          }
        )

        .(
          if (if_show_coxph) {
            bquote({
              fit_coxph <- coxph(formula_coxph, data = anl, ties = "exact")
              tbl_coxph <- t_coxph(fit_coxph)
              text_coxph <- paste0(info_coxph, "\n", toString(tbl_coxph, gap = 1))
              coxph_grob <- textGrob(
                label = text_coxph, x = unit(1, "lines"), y = unit(1, "lines"),
                just = c("left", "bottom"),
                gp = gpar(fontfamily = "mono", fontsize = tbl_fontsize, fontface = "bold"),
                vp = vpPath("mainPlot", "kmCurve", "curvePlot")
              )
              p <- addGrob(p, coxph_grob)
            })
          }
        )

        plot <- grid.draw(p)
        plot
      }))
    } else {
      chunks_push(expression = bquote({
        facet_df <- anl[.(facet_var)]

        lab <- Map(
          function(var, x) paste0(var, "= '", x, "'"),
          .(facet_var), facet_df
        )
        lab <- Reduce(function(x, y) paste(x, y, sep = ", "), unname(lab))
        lab <- factor(unlist(lab))
        dfs <- split(anl, lab)
        max_min <- min(sapply(dfs, function(x) {
          max(x[["AVAL"]], na.rm = TRUE)
        }))
        xticks <- max(1, floor(max_min / 10))
        grid.newpage()
        pl <- Map(function(x, label) {
          if (nrow(x) < 5) {
            textGrob(paste0("Less than 5 patients in ", label, " group"))
          } else {
            x[[.(arm_var)]] <- factor(x[[.(arm_var)]])
            fit_km <- survfit(formula_km, data = x, conf.type = "plain")
            p <- g_km(
              fit_km = fit_km, col = NA, title = paste0("Kaplan - Meier Plot for: ", label),
              xticks = xticks, draw = FALSE, xlab = paste(.(xlab), time_unit)
            )

            .(
              if (if_show_km) {
                bquote({
                  tbl_km <- t_km(fit_km)
                  km_grob <- textGrob(
                    label = toString(tbl_km, gap = 1),
                    x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
                    y = unit(1, "npc") - unit(1, "lines"),
                    just = c("left", "top"),
                    gp = gpar(fontfamily = "mono", fontsize = tbl_fontsize, fontface = "bold"),
                    vp = vpPath("mainPlot", "kmCurve", "curvePlot")
                  )
                  p <- addGrob(p, km_grob)
                })
              }
            )

            .(
              if (if_show_coxph) {
                bquote({
                  fit_coxph <- coxph(formula_coxph, data = x, ties = "exact")
                  tbl_coxph <- t_coxph(fit_coxph)
                  text_coxph <- paste0(info_coxph, "\n", toString(tbl_coxph, gap = 1))
                  coxph_grob <- textGrob(
                    label = text_coxph,
                    x = unit(1, "lines"),
                    y = unit(1, "lines"),
                    just = c("left", "bottom"),
                    gp = gpar(fontfamily = "mono", fontsize = tbl_fontsize, fontface = "bold"),
                    vp = vpPath("mainPlot", "kmCurve", "curvePlot")
                  )
                  p <- addGrob(p, coxph_grob)
                })
              }
            )

            p
          }
        }, dfs, levels(lab))
        plot <- grid.draw(gridExtra::arrangeGrob(grobs = pl, ncol = 1))

        plot
      }))
    }

    p <- chunks_eval()

    chunks_validate_is_ok()

    p
  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Kaplan Meyer Plot",
      rcode = get_rcode(
        datasets = datasets,
        title = label
      )
    )
  })
}
