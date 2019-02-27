#' teal module for Kaplan-Meier Plot from grid
#'
#' This is teal module produces a grid style KM plot for data with ADaM structure
#'
#' @inheritParams tm_t_tte
#' @param facet_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for facet plotting
#' @param plot_height vector with three elements defining selected, min and max plot height
#' @param tbl_fontsize fontsize for text annotation
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
#'
#' library(random.cdisc.data)
#'
#' asl <- radsl(seed = 1)
#' ate <- radtte(asl, seed = 1)
#'
#' attr(asl, "source") <- "random.cdisc.data::radsl(seed = 1)"
#' attr(ate, "source") <- "random.cdisc.data::radtte(asl, seed = 1)"
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
#' x <- teal::init(
#'   data = list(ASL = asl, ATE = ate),
#'   modules = root_modules(
#'     tm_g_km(
#'       label = "KM PLOT",
#'       dataname = "ATE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(c("OS", "PFS"), "OS"),
#'       facet_var = choices_selected(c("SEX", "BMRKR2"), "BMRKR2"),
#'       strata_var = choices_selected(c("SEX", "BMRKR2"), "SEX"),
#'       tbl_fontsize = 12
#'     )
#'   )
#' )
#' \dontrun{
#'
#' shinyApp(ui = x$ui, server = x$server)
#' }
tm_g_km <- function(label,
                    dataname,
                    arm_var,
                    arm_ref_comp = NULL,
                    paramcd,
                    facet_var,
                    strata_var,
                    plot_height = c(1200, 400, 5000),
                    tbl_fontsize = 8,
                    pre_output = helpText("x-axes for different factes may not have the same scale"),
                    post_output = NULL,
                    code_data_processing = NULL) {
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
      tbl_fontsize = tbl_fontsize,
      code_data_processing = code_data_processing
    ),
    ui = ui_g_km,
    ui_args = args
  )
}


ui_g_km <- function(id, ...) {
  a <- list(...)
  ns <- NS(id)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"),
        "Arm Variable",
        choices = a$arm_var$choices,
        selected = a$arm_var$selected,
        multiple = FALSE
      ),
      optionalSelectInput(ns("paramcd"),
        "Time to Event (Endpoint)",
        choices = a$paramcd$choices,
        selected = a$paramcd$selected,
        multiple = FALSE
      ),
      optionalSelectInput(ns("strata_var"),
        "Stratify by",
        choices = a$strata_var$choices,
        selected = a$strata_var$selected,
        multiple = TRUE,
        label_help = helpText("currently taken from ASL")
      ),
      optionalSelectInput(ns("facet_var"),
        "Facet Plots by:",
        choices = a$facet_var$choices,
        selected = a$facet_var$selected,
        multiple = TRUE,
        label_help = helpText("currently taken from ASL")
      ),
      selectInput(ns("ref_arm"),
        "Reference Arm",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      helpText("Reference groups automatically combined into a single group if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("combine_comp_arms"), "Combine all comparison groups?", value = FALSE),
      tags$label("Plot Settings", class = "text-primary"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_g_km <- function(input, output, session, datasets, tbl_fontsize,
                     dataname, arm_ref_comp, code_data_processing) {
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

  chunks <- list(
    vars = "# No Calculated",
    data = "# No Calculated",
    facet = "# No Calculated",
    formula_km = "# No Calculated",
    formula_coxph = "# No Calculated",
    info_coxph = "# No Calculated",
    t_kmplot = "# No Calculated"
  )

  output$kmplot <- renderPlot({
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    asl_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)

    paramcd <- input$paramcd # nolint
    arm_var <- input$arm_var
    facet_var <<- input$facet_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    strata_var <- input$strata_var
    combine_comp_arms <- input$combine_comp_arms

    if (length(facet_var) == 0) facet_var <<- NULL
    if (length(strata_var) == 0) strata_var <- NULL

    for (i in seq_along(chunks)) chunks[[i]] <<- "# Not calculated"


    validate_standard_inputs(
      asl = asl_filtered,
      aslvars = c("USUBJID", "STUDYID", arm_var, strata_var, facet_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "CNSR", "AVALU"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate(need(is.logical(combine_comp_arms), "need combine arm information"))

    anl_name <- paste0(dataname, "_filtered")
    assign(anl_name, anl_filtered)

    chunks$vars <<- bquote({
      ref_arm <- .(ref_arm)
      comp_arm <- .(comp_arm)
      strata_var <- .(strata_var)
      facet_var <- .(facet_var)
      combine_comp_arms <- .(combine_comp_arms)
    })

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, strata_var, facet_var))  # nolint
    chunks$data <<- bquote({
      asl_p <- subset(asl_filtered, .(as.name(arm_var)) %in% c(ref_arm, comp_arm))
      anl_p <- subset(.(as.name(anl_name)), PARAMCD %in% .(paramcd))

      anl <- merge(asl_p[, .(asl_vars)],
        anl_p[, c("USUBJID", "STUDYID", "AVAL", "CNSR", "AVALU")],
        all.x = FALSE, all.y = FALSE, by = c("USUBJID", "STUDYID")
      )

      arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])

      arm <- combine_levels(arm, ref_arm)
      if (combine_comp_arms) {
        arm <- combine_levels(arm, comp_arm)
      }

      anl[[.(arm_var)]] <- droplevels(arm)
      time_unit <- unique(anl[["AVALU"]])
      tbl_fontsize <- .(tbl_fontsize)
    })

    eval(chunks$data)
    validate(need(nrow(anl) > 15, "need at least 15 data points"))
    validate(need(length(time_unit) == 1, "Time Unit is not consistant"))

    chunks$formula_km <<- eval(bquote({
      as.formula(paste0("Surv(AVAL, 1-CNSR) ~", .(arm_var)))
    }))

    chunks$formula_coxph <<- eval(bquote({
      as.formula(
        paste0(
          "Surv(AVAL, 1-CNSR) ~", .(arm_var),
          ifelse(is.null(strata_var), "", paste0("+ strata(", paste(.(strata_var), collapse = ","), ")"))
        )
      )
    }))

    chunks$info_coxph <<- eval(bquote({
      paste0(
        "Cox Proportional Model: ",
        ifelse(is.null(strata_var),
               "Unstratified Analysis",
               paste0("Stratified by ", paste(.(strata_var), collapse = ",")))
      )
    }))

    formula_km <- chunks$formula_km # nolint
    formula_coxph <- chunks$formula_coxph # nolint
    info_coxph <- chunks$info_coxph # nolint

    if (is.null(facet_var)) {
      chunks$t_kmplot <<- bquote({
        fit_km <- survfit(formula_km, data = anl, conf.type = "plain")
        fit_coxph <- coxph(formula_coxph, data = anl, ties = "exact")
        tbl_km <- t_km(fit_km)
        tbl_coxph <- t_coxph(fit_coxph)
        text_coxph <- paste0(info_coxph, "\n", toString(tbl_coxph, gap = 1))
        coxph_grob <- textGrob(
          label = text_coxph, x = unit(1, "lines"), y = unit(1, "lines"),
          just = c("left", "bottom"),
          gp = gpar(fontfamily = "mono", fontsize = tbl_fontsize, fontface = "bold"),
          vp = vpPath("mainPlot", "kmCurve", "curvePlot")
        )
        km_grob <- textGrob(
          label = toString(tbl_km, gap = 1),
          x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
          y = unit(1, "npc") - unit(1, "lines"),
          just = c("left", "top"),
          gp = gpar(fontfamily = "mono", fontsize = tbl_fontsize, fontface = "bold"),
          vp = vpPath("mainPlot", "kmCurve", "curvePlot")
        )
        grid.newpage()
        p <- g_km(fit_km = fit_km, col = NA, draw = FALSE, xlab = time_unit)
        p <- addGrob(p, km_grob)
        p <- addGrob(p, coxph_grob)
        grid.draw(p)
      })
    } else {
      chunks$facet <<- bquote({
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
      })

      eval(chunks$facet)

      chunks$t_kmplot <<- bquote({
        grid.newpage()
        pl <- Map(function(x, label) {
          if (nrow(x) < 5) {
            textGrob(paste0("Less than 5 patients in ", label, "group"))
          } else {
            x[[.(arm_var)]] <- factor(x[[.(arm_var)]])
            fit_km <- survfit(formula_km, data = x, conf.type = "plain")
            fit_coxph <- coxph(formula_coxph, data = x, ties = "exact")
            tbl_km <- t_km(fit_km)
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
            km_grob <- textGrob(
              label = toString(tbl_km, gap = 1),
              x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
              y = unit(1, "npc") - unit(1, "lines"),
              just = c("left", "top"),
              gp = gpar(fontfamily = "mono", fontsize = tbl_fontsize, fontface = "bold"),
              vp = vpPath("mainPlot", "kmCurve", "curvePlot")
            )


            p <- g_km(
              fit_km = fit_km, col = NA, title = paste0("Kaplan - Meier Plot for: ", label),
              xticks = xticks, draw = FALSE, xlab = time_unit
            )
            p <- addGrob(p, km_grob)
            p <- addGrob(p, coxph_grob)

            p
          }
        }, dfs, levels(lab))

        grid.draw(gridExtra::arrangeGrob(grobs = pl, ncol = 1))
      })
    }

    eval(chunks$t_kmplot)
  })

  observeEvent(input$show_rcode, {
    header <- get_rcode_header(
      title = "Kaplan Meier Plot",
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
      paste0("formula_km <- ", deparse(chunks$formula_km)),
      "",
      paste0("formula_coxph  <- ", deparse(chunks$formula_coxph)),
      "",
      paste0("info_coxph <- ", deparse(chunks$info_coxph)),
      "",
      if (!is.null(facet_var)) remove_enclosing_curly_braces(deparse(chunks$facet)),
      remove_enclosing_curly_braces(deparse(chunks$t_kmplot)),
      if (!is.null(facet_var)) {
        c(
          "",
          "# or plot each kaplan meier plot individually with",
          "# grid.newpage(); grid.draw(pl[[1]])"
        )
      }
    ), collapse = "\n")

    showModal(modalDialog(
      title = "R Code for the Current Kaplan Meier Plot",
      tags$pre(tags$code(class = "R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
}
