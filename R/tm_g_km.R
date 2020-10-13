#' teal module for Kaplan-Meier Plot from grid
#'
#' This is teal module produces a grid style KM plot for data with ADaM structure
#'
#' @inheritParams tm_t_tte
#' @inheritParams shared_params
#' @param facet_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for facet plotting
#' @param conf_level \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for confidence level, each within range of (0, 1).
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
#' ADTTE <- radtte(cached = TRUE)
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
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADTTE", ADTTE, code = "ADTTE <- radtte(cached = TRUE)"),
#'     check = TRUE
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
                    plot_height = c(1200L, 400L, 5000L),
                    plot_width = NULL,
                    pre_output = NULL,
                    post_output = NULL) {
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(facet_var))
  stopifnot(is.choices_selected(strata_var))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_km,
    server_args = list(
      dataname = dataname,
      arm_ref_comp = arm_ref_comp,
      label = label,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_g_km,
    ui_args = args
  )
}

ui_g_km <- function(id, ...) {
  a <- list(...)
  ns <- NS(id)

  standard_layout(
    output = white_small_well(plot_with_settings_ui(id = ns("myplot"), height = a$plot_height, width = a$plot_width)),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code(a$dataname)),
      optionalSelectInput(
        ns("paramcd"),
        "Time to Event (Endpoint)",
        choices = a$paramcd$choices,
        selected = a$paramcd$selected,
        multiple = FALSE,
        fixed = a$paramcd$fixed
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
      # arm related parameters
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        choices = a$arm_var$choices,
        selected = a$arm_var$selected,
        multiple = FALSE,
        fixed = a$arm_var$fixed
      ),
      div(
        class = "arm-comp-box",
        tags$label("Compare Arms"),
        shinyWidgets::switchInput(inputId = ns("compare_arms"), value = !is.null(a$arm_ref_comp), size = "mini"),
        conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          div(
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
            optionalSelectInput(
              ns("strata_var"),
              "Stratify by",
              choices = a$strata_var$choices,
              selected = a$strata_var$selected,
              multiple = TRUE,
              label_help = helpText("currently taken from ADSL"),
              fixed = a$strata_var$fixed
            )
          )
        )
      ),
      helpText("X-axis label will be combined with variable ", tags$code("AVALU")),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        panel_group(
          panel_item(
            "Comparison settings",
            radioButtons(
              ns("pval_method_coxph"),
              label = HTML(paste("p-value method for ",
                                 tags$span(style="color:darkblue", "Coxph"), # nolint
                                 " (Hazard Ratio)",
                                 sep = "")
              ),
              choices = c("wald", "log-rank", "likelihood"),
              selected = "log-rank"
            ),
            radioButtons(
              ns("ties_coxph"),
              label = HTML(paste("Ties for ",
                                 tags$span(style="color:darkblue", "Coxph"), # nolint
                                 " (Hazard Ratio)",
                                 sep = "")
              ),
              choices = c("exact", "breslow", "efron"),
              selected = "exact"
            )
          )
        )
      ),
      panel_group(
        panel_item(
          "Additional plot settings",
          checkboxInput(
            inputId = ns("scale_x_axis"),
            label = "Scale multiple X axis range",
            value = FALSE,
            width = "100%"
          ),
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
                     label,
                     plot_height,
                     plot_width) {

  init_chunks()

  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_g_km",
    on_off = reactive(input$compare_arms)
  )

  plot_r <- reactive({
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint

    paramcd <- input$paramcd # nolint
    arm_var <- input$arm_var
    facet_var <- input$facet_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    strata_var <- input$strata_var
    combine_comp_arms <- input$combine_comp_arms
    compare_arms <- input$compare_arms
    pval_method_coxph <- input$pval_method_coxph # nolint
    ties_coxph <- input$ties_coxph # nolint
    conf_level <- as.numeric(input$conf_level) # nolint
    xlab <- input$xlab # nolint
    tbl_fontsize <- input$font_size # nolint
    if_show_km <- input$show_km_table # nolint
    xticks <- gsub(";", ",", trimws(input$user_xaxis)) %>%
      strsplit(",") %>%
      unlist() %>%
      as.numeric()
    scale_x_axis <- input$scale_x_axis # nolint
    if (length(xticks) == 0) {
      xticks <- NULL
    } else {
      validate(need(all(!is.na(xticks)), "Not all values entered were numeric"))
      validate(need(all(xticks >= 0), "All break intervals for x-axis must be non-negative"))
      validate(need(any(xticks > 0), "At least one break interval for x-axis must be positive"))
    }

    if (length(facet_var) == 0) {
      facet_var <<- NULL
    }
    if (length(strata_var) == 0) {
      strata_var <- NULL
    }

    # validate inputs
    validate_args <- list(
      adsl = ADSL_FILTERED,
      adslvars = c("USUBJID", "STUDYID", arm_var, strata_var, facet_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "CNSR", "AVALU"),
      arm_var = arm_var
    )

    if (length(unique(ADSL_FILTERED[[arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      if (compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = ref_arm))
      }
    } else {
      if (compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = ref_arm, comp_arm = comp_arm))
      }
    }

    do.call(what = "validate_standard_inputs", validate_args)

    validate(need(is.logical(combine_comp_arms), "need combine arm information"))

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)

    chunks_reset(envir = environment())

    chunks_push(bquote(ref_arm <- .(ref_arm)))
    chunks_push(bquote(comp_arm <- .(comp_arm)))
    chunks_push(bquote(strata_var <- .(strata_var)))
    chunks_push(bquote(facet_var <- .(facet_var)))
    chunks_push(bquote(combine_comp_arms <- .(combine_comp_arms)))
    chunks_push(bquote(scale_x_axis <- .(scale_x_axis)))

    chunks_push(bquote(adsl_vars <- unique(c("USUBJID", "STUDYID", .(arm_var), .(strata_var), .(facet_var)))))

    if (isFALSE(compare_arms) || length(unique(ADSL_FILTERED[[arm_var]])) == 1) {
      chunks_push(bquote(adsl_p <- ADSL_FILTERED))
    } else{
      chunks_push(bquote(adsl_p <- subset(ADSL_FILTERED, .(as.name(arm_var)) %in% c(ref_arm, comp_arm))))
    }

    chunks_push(bquote(anl_p <- subset(.(as.name(anl_name)), PARAMCD %in% .(paramcd))))
    chunks_push(bquote({
      anl <- merge(
        adsl_p[, adsl_vars],
        anl_p[, c("USUBJID", "STUDYID", "AVAL", "CNSR", "AVALU")],
        all.x = FALSE, all.y = FALSE, by = c("USUBJID", "STUDYID")
      )
    }))

    if (isFALSE(compare_arms) || length(unique(ADSL_FILTERED[[arm_var]])) == 1) {
      chunks_push(bquote(arm <- as.factor(anl[[.(arm_var)]])))
    } else {
      chunks_push(bquote(arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])))
      chunks_push(bquote(arm <- combine_levels(arm, ref_arm)))
      if (combine_comp_arms) {
        chunks_push(bquote(arm <- combine_levels(arm, comp_arm)))
      }
      chunks_push(bquote(anl[[.(arm_var)]] <- droplevels(arm)))
    }

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
      }))

      if (if_show_km) {
        chunks_push(bquote({
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
        }))
      }

      if (isTRUE(compare_arms) && length(unique(ADSL_FILTERED[[arm_var]])) > 1){
        chunks_push(bquote({
          tbl_coxph <- t_coxph_pairwise(
            formula_coxph,
            data = anl,
            conf_level = .(conf_level),
            pval_method = .(pval_method_coxph),
            ties = .(ties_coxph)
          )
          text_coxph <- paste0(info_coxph, "\n", toString(tbl_coxph, gap = 1))
          coxph_grob <- textGrob(
            label = text_coxph, x = unit(1, "lines"), y = unit(1, "lines"),
            just = c("left", "bottom"),
            gp = gpar(fontfamily = "mono", fontsize = tbl_fontsize, fontface = "bold"),
            vp = vpPath("mainPlot", "kmCurve", "curvePlot")
          )
          p <- addGrob(p, coxph_grob)
        }))
      }

      chunks_push(bquote({
        grid.draw(p)
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
        max_time <- NULL
        if (scale_x_axis) {
          max_time <- max(sapply(dfs, function(x) {
            max(x[["AVAL"]], na.rm = TRUE)
          }))
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
              xticks = xticks, max_time = max_time, draw = FALSE, xlab = paste(.(xlab), time_unit)
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
              if (isTRUE(compare_arms) && length(unique(ADSL_FILTERED[[arm_var]])) > 1) {
                bquote({
                  tbl_coxph <- t_coxph_pairwise(
                    formula_coxph,
                    data = x,
                    conf_level = .(conf_level),
                    pval_method = .(pval_method_coxph),
                    ties = .(ties_coxph)
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
        g_final <- gridExtra::arrangeGrob(grobs = pl, ncol = 1)
        grid.draw(g_final)
      }))
    }

    chunks_safe_eval()

    chunks_get_var("g_final")
  })

  # Insert the plot into a plot with settings module from teal.devel
  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width
  )

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Kaplan Meier Plot",
      rcode = get_rcode(
        datasets = datasets,
        datanames = union("ADSL", dataname),
        title = label
      )
    )
  })
}
