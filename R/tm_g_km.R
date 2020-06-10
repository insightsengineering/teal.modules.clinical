#' teal module for Kaplan-Meier Plot from grid
#'
#' This is teal module produces a grid style KM plot for data with ADaM structure
#'
#' @inheritParams tm_t_tte
#' @param facet_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for facet plotting
#' @param conf_level \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for confidence level, each within range of (0, 1).
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
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(ADSL, cached = TRUE)
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
#'     cdisc_dataset("ADSL", ADSL), cdisc_dataset("ADTTE", ADTTE),
#'     code = "ADSL <- radsl(cached = TRUE)
#'             ADTTE <- radtte(ADSL, cached = TRUE)",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_km(
#'       label = "KM PLOT",
#'       dataname = "ADTTE",
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
                    conf_level = choices_selected(c(0.8, 0.85, 0.90, 0.95, 0.99, 0.995), 0.95),
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
        multiple = FALSE,
        fixed = a$arm_var$fixed
      ),
      optionalSelectInput(
        ns("paramcd"),
        "Time to Event (Endpoint)",
        choices = a$paramcd$choices,
        selected = a$paramcd$selected,
        multiple = FALSE,
        fixed = a$paramcd$fixed
      ),
      optionalSelectInput(
        ns("strata_var"),
        "Stratify by",
        choices = a$strata_var$choices,
        selected = a$strata_var$selected,
        multiple = TRUE,
        label_help = helpText("currently taken from ADSL"),
        fixed = a$strata_var$fixed
      ),
      optionalSelectInput(
        ns("facet_var"),
        "Facet Plots by:",
        choices = a$facet_var$choices,
        selected = a$facet_var$selected,
        multiple = TRUE,
        label_help = helpText("currently taken from ADSL"),
        fixed = a$facet_var$fixed
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
      plot_height_input(id = ns("myplot"), value = a$plot_height),
      panel_group(
        panel_item(
          "Additional plot settings",
          textInput(
            inputId = ns("user_xaxis"),
            label = "Specify break intervals for x-axis"
          ),
          numericInput(
            inputId = ns("font_size"),
            label = "Font size",
            value = 8,
            min = 5,
            max = 15,
            step = 1,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns("show_km_table"),
            label = "Show KM table",
            value = TRUE,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns("show_coxph_table"),
            label = "Show CoxPH table",
            value = TRUE,
            width = "100%"
          ),
          radioButtons(
            ns("pval_method"),
            "p-value method",
            choices = c("wald", "log-rank", "likelihood"),
            selected = "log-rank"
          ),
          optionalSelectInput(ns("conf_level"),
                              "Level of Confidence",
                              a$conf_level$choices,
                              a$conf_level$selected,
                              multiple = FALSE,
                              fixed = a$conf_level$fixed
          ),
          textInput(ns("xlab"), "X-axis label", "Overall survival in ")
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
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    adsl = datasets$get_data("ADSL", filtered = FALSE, reactive = FALSE),
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
    ADSL_FILTERED <- datasets$get_data("ADSL", reactive = TRUE, filtered = TRUE) # nolint

    paramcd <- input$paramcd # nolint
    arm_var <- input$arm_var
    facet_var <- input$facet_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    strata_var <- input$strata_var
    combine_comp_arms <- input$combine_comp_arms
    pval_method <- input$pval_method # nolint
    conf_level <- as.numeric(input$conf_level) # nolint
    xlab <- input$xlab # nolint
    tbl_fontsize <- input$font_size # nolint
    if_show_km <- input$show_km_table # nolint
    if_show_coxph <- input$show_coxph_table # nolint
    xticks <- gsub(";", ",", trimws(input$user_xaxis)) %>%
      strsplit(",") %>%
      unlist() %>%
      as.numeric()
    if (length(xticks) == 0) {
      xticks <- NULL
    } else {
      validate(need(all(!is.na(xticks)), "Not all values entered were numeric"))
    }

    if (length(facet_var) == 0) {
      facet_var <<- NULL
    }
    if (length(strata_var) == 0) {
      strata_var <- NULL
    }

    validate_standard_inputs(
      adsl = ADSL_FILTERED,
      adslvars = c("USUBJID", "STUDYID", arm_var, strata_var, facet_var),
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

    chunks_push(bquote(ref_arm <- .(ref_arm)))
    chunks_push(bquote(comp_arm <- .(comp_arm)))
    chunks_push(bquote(strata_var <- .(strata_var)))
    chunks_push(bquote(facet_var <- .(facet_var)))
    chunks_push(bquote(combine_comp_arms <- .(combine_comp_arms)))

    chunks_push(bquote(adsl_vars <- unique(c("USUBJID", "STUDYID", .(arm_var), .(strata_var), .(facet_var)))))

    chunks_push(bquote(adsl_p <- subset(ADSL_FILTERED, .(as.name(arm_var)) %in% c(ref_arm, comp_arm))))
    chunks_push(bquote(anl_p <- subset(.(as.name(anl_name)), PARAMCD %in% .(paramcd))))
    chunks_push(bquote({
      anl <- merge(
        adsl_p[, adsl_vars],
        anl_p[, c("USUBJID", "STUDYID", "AVAL", "CNSR", "AVALU")],
        all.x = FALSE, all.y = FALSE, by = c("USUBJID", "STUDYID")
      )
    }))
    chunks_push(bquote(arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])))
    chunks_push(bquote(arm <- combine_levels(arm, ref_arm)))

    if (combine_comp_arms) {
      chunks_push(bquote(arm <- combine_levels(arm, comp_arm)))
    }


    chunks_push(bquote(anl[[.(arm_var)]] <- droplevels(arm)))
    chunks_push(bquote(time_unit <- unique(anl[["AVALU"]])))
    chunks_push(bquote(tbl_fontsize <- .(tbl_fontsize)))

    chunks_safe_eval()

    validate(need(nrow(chunks_get_var("anl")) > 15, "need at least 15 data points"))
    validate(need(length(chunks_get_var("time_unit")) == 1, "Time Unit is not consistant"))

    chunks_push(bquote(formula_km <- as.formula(.(paste0("Surv(AVAL, 1-CNSR) ~ ", arm_var)))))

    chunks_push(bquote({
      formula_coxph <- as.formula(
        .(paste0(
          "Surv(AVAL, 1-CNSR) ~ arm(", arm_var, ")",
          ifelse(is.null(strata_var), "", paste0(" + strata(", paste(strata_var, collapse = ","), ")"))
        ))
      )
    }))
    chunks_push(bquote({
      info_coxph <- .(paste0(
        "Cox Proportional Model: ",
        ifelse(is.null(strata_var),
               "Unstratified Analysis",
               paste0("Stratified by ", paste(strata_var, collapse = ","))
        )
      ))
    }))

    if (is.null(facet_var)) {
      chunks_push(bquote({
        fit_km <- survfit(formula_km, data = anl, conf.int = .(conf_level), conf.type = "plain")
        grid.newpage()
        p <- g_km(fit_km = fit_km, xticks = .(xticks), col = NA, draw = FALSE, xlab = paste(.(xlab), time_unit))

        .(
          if (if_show_km) {
            bquote({
              tbl_km <- t_km(formula_km, data = anl, conf_level = .(conf_level), conf.type = "plain")
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
              tbl_coxph <- t_coxph_pairwise(
                formula_coxph,
                data = anl,
                conf_level = .(conf_level),
                pval_method = .(pval_method),
                ties = "exact"
              )
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
      chunks_push(bquote({
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
        xticks <- .(xticks)
        if (is.null(xticks)){
          xticks <- max(1, floor(max_min / 10))
        } else {
          xticks <- .(xticks)
        }

        grid.newpage()
        pl <- Map(function(x, label) {
          if (nrow(x) < 5) {
            textGrob(paste0("Less than 5 patients in ", label, " group"))
          } else {
            x[[.(arm_var)]] <- factor(x[[.(arm_var)]])
            fit_km <- survfit(formula_km, data = x, conf.int = .(conf_level), conf.type = "plain")
            p <- g_km(
              fit_km = fit_km, col = NA, title = paste0("Kaplan - Meier Plot for: ", label),
              xticks = xticks, draw = FALSE, xlab = paste(.(xlab), time_unit)
            )

            .(
              if (if_show_km) {
                bquote({
                  tbl_km <- t_km(formula_km, data = x, conf_level = .(conf_level), conf.type = "plain")
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
                  tbl_coxph <- t_coxph_pairwise(
                    formula_coxph,
                    data = x,
                    conf_level = .(conf_level),
                    pval_method = .(pval_method),
                    ties = "exact"
                  )
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

    p <- chunks_safe_eval()

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
