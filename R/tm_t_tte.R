#' Time To Event Table Teal Module
#'
#' @inheritParams teal.devel::standard_layout
#' @param label menu item label of the module in the teal app
#' @param dataname (\code{character}) analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as \code{arm_var}
#' @param arm_ref_comp (\code{\link[teal]{choices_selected}}) optional, if specified it must be a named list with each
#'   element corresponding to an arm variable in \code{ADSL} and the element must
#'   be another list with the elements named \code{ref} and \code{comp} that the
#'   defined the default reference and comparison arms when the arm variable is
#'   changed.
#' @param paramcd \code{\link[teal]{choices_selected}} object with all available choices and preselected option for
#' variable names that can be used as \code{PARAMCD} variable
#' @param strata_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for stratification
#' @param time_points \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used \code{\link[tern]{t_tte}}
#' @param time_unit (\code{character}) with unit of \code{dataname$AVAL}, please use singular e.g. month instead
#'   of months
#' @param event_desc_var (\code{character}) variable name with the event description information,
#'   optional
#'
#' @details This module produces a response summary table that is similar to
#'   STREAM template \code{ttet01}. The core functionality is based on
#'   \code{\link[tern]{t_tte}} from the \code{tern} package.\cr
#' This modules expects that the analysis data has the following variables
#'
#' \tabular{ll}{
#'  \code{AVAL} \tab time to event\cr
#'  \code{CNSR} \tab boolean or 0,1 is element in \code{AVAL} censored\cr
#'  \code{PARAMCD} \tab variable used to filter for endpoint (e.g. OS), after
#'  filtering for \code{paramcd} one observation per patient is expected
#' }
#'
#' The arm variables, stratification variables and taken from the \code{ADSL}
#' data.
#'
#'
#' @template author_waddella
#'
#' @export
#' @import magrittr
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' arm_ref_comp = list(
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   ),
#'    ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   )
#' )
#'
#' app <- init(
#'     data = cdisc_data(
#'       cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'       cdisc_dataset("ADTTE", ADTTE, code = "ADTTE <- radtte(cached = TRUE)"),
#'       check = TRUE
#'     ),
#'     modules = root_modules(
#'         tm_t_tte(
#'             label = "Time To Event Table",
#'             dataname = 'ADTTE',
#'             arm_var = choices_selected(c("ARM", "ARMCD", "ACTARMCD"), "ARM"),
#'             arm_ref_comp = arm_ref_comp,
#'             paramcd = choices_selected(value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"),
#'             strata_var = choices_selected(c("SEX", "BMRKR2"), "SEX"),
#'             time_points = choices_selected(c(6, 8), 6),
#'             time_unit = "month",
#'             event_desc_var = "EVNTDESC"
#'         )
#'     )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_tte <- function(label,
                     dataname,
                     arm_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     strata_var,
                     time_points,
                     time_unit = "months",
                     event_desc_var = NULL,
                     pre_output = NULL,
                     post_output = NULL
                     ) {

  stopifnot(length(dataname) == 1)
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(strata_var))
  stopifnot(is.choices_selected(time_points))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_tte,
    ui = ui_t_tte,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      arm_ref_comp = arm_ref_comp,
      time_unit = time_unit,
      event_desc_var = event_desc_var,
      label = label
    ),
    filters = dataname
  )
}

#' @import teal.devel
ui_t_tte <- function(id, ...) {

  a <- list(...) # module args

  ns <- NS(id)

  standard_layout(
    output = white_small_well(uiOutput(ns("tte_table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("paramcd"),
                          "Select Endpoint",
                          a$paramcd$choices,
                          a$paramcd$selected,
                          multiple = FALSE,
                          fixed = a$paramcd$fixed
      ),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var$choices,
                          a$arm_var$selected,
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
              "Reference Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            helpText("Multiple reference groups are automatically combined into a single group."),
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
              a$strata_var$choices,
              a$strata_var$selected,
              multiple = TRUE,
              label_help = helpText("from ", tags$code("ADSL")),
              fixed = a$strata_var$fixed
            )
          )
        )
      ),
      optionalSelectInput(ns("time_points"),
                          "Time Points",
                          a$time_points$choices,
                          a$time_points$selected,
                          multiple = TRUE,
                          fixed = a$time_points$fixed
      ),
      if (!is.null(a$event_desc_var)) {
        helpText("Event Description Variable: ", tags$code(a$event_desc_var))
      },
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
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
          ),
          numericInput(
            inputId = ns("conf_level_coxph"),
            label = HTML(paste("Confidence Level for ",
                               tags$span(style="color:darkblue", "Coxph"), # nolint
                               " (Hazard Ratio)", sep = "")
            ),
            value = 0.95,
            min = 0.01,
            max = 0.99,
            step = 0.01,
            width = "100%"
          ),
          numericInput(
            inputId = ns("conf_level_ztest"),
            label = HTML(paste("Confidence Level for ",
                               tags$span(style="color:darkblue", "Z-test"), # nolint
                               " (Difference in Event Free Rate)",
                               sep = "")
            ),
            value = 0.95,
            min = 0.01,
            max = 0.99,
            step = 0.01,
            width = "100%"
          )
        )
      ),
      panel_item(
        "Additional table settings",
        numericInput(
          inputId = ns("conf_level_survfit"),
          label = HTML(paste("Confidence Level for ",
                             tags$span(style="color:darkblue", "Survfit"), # nolint
                             " (KM Median Estimate & Event Free Rate)",
                             sep = "")
          ),
          value = 0.95,
          min = 0.01,
          max = 0.99,
          step = 0.01,
          width = "100%"
        ),
        radioButtons(
          ns("conf_type_survfit"),
          "Confidence Level Type for Survfit",
          choices = c("plain", "log", "log-log"),
          selected = "plain"
        ),
        sliderInput(
          inputId = ns("probs_survfit"),
          label = "KM Estimate Percentiles",
          min = 0.01,
          max = 0.99,
          value = c(0.25, 0.75),
          width = "100%"
        )
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @import teal.devel
#' @importFrom rtables as_html
srv_t_tte <- function(input,
                      output,
                      session,
                      datasets,
                      dataname,
                      arm_ref_comp,
                      time_unit,
                      event_desc_var,
                      label) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",    # from UI
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte",
    on_off = reactive(input$compare_arms)
  )

  # Create output
  table_reactive <- reactive({
    # resolve all reactive expressions
    # nolint start
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE)
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE)
    # nolint end

    paramcd <- input$paramcd # nolint
    strata_var <- input$strata_var
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_comp_arms <- input$combine_comp_arms
    compare_arms <- input$compare_arms

    time_points <- as.numeric(input$time_points)
    pval_method_coxph <- input$pval_method_coxph # nolint
    conf_level <- c(survfit = input$conf_level_survfit, ztest = input$conf_level_ztest) # nolint
    conf_type_survfit <- input$conf_type_survfit # nolint
    probs_survfit <- input$probs_survfit
    ties_coxph <- input$ties_coxph # nolint
    conf_level_coxph <- input$conf_level_coxph # nolint


    if (length(strata_var) == 0) {
      strata_var <- NULL
    }

    time_points <- if (length(time_points) == 0) {
      NULL
    } else {
      sort(time_points)
    }

    # validate inputs
    validate_args <- list(
      adsl = ADSL_FILTERED,
      adslvars = c("USUBJID", "STUDYID", arm_var, strata_var),
      anl = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "CNSR", event_desc_var),
      arm_var = arm_var
    )

    # validate arm levels
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
    validate(need(!is.null(probs_survfit), "Must select percentiles for KM median estimate"))

    # do analysis

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED)

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, strata_var)) # nolint
    anl_vars <- unique(c("USUBJID", "STUDYID", "AVAL", "CNSR", event_desc_var)) # nolint

    ## Now comes the analysis code
    chunks_push(bquote(ref_arm <- .(ref_arm)))
    chunks_push(bquote(comp_arm <- .(comp_arm)))
    chunks_push(bquote(strata_var <- .(strata_var)))
    chunks_push(bquote(combine_comp_arms <- .(combine_comp_arms)))

    if (isFALSE(compare_arms) || length(unique(ADSL_FILTERED[[arm_var]])) == 1) {
      chunks_push(bquote(adsl_p <- ADSL_FILTERED))
    } else{
      chunks_push(bquote(adsl_p <- subset(ADSL_FILTERED, .(as.name(arm_var)) %in% c(ref_arm, comp_arm))))
    }
    chunks_push(bquote(anl_endpoint <- subset(.(as.name(anl_name)), PARAMCD == .(paramcd))))

    chunks_push(bquote({
      anl <- merge(
        x = adsl_p[, .(adsl_vars)],
        y = anl_endpoint[, .(anl_vars)],
        all.x = FALSE, all.y = FALSE,
        by = c("USUBJID", "STUDYID")
      )
    }))

    if (isFALSE(compare_arms) || length(unique(ADSL_FILTERED[[arm_var]])) == 1) {
      chunks_push(bquote(arm <- as.factor(anl[[.(arm_var)]])))
      chunks_push(bquote(anl[[.(arm_var)]] <- droplevels(arm)))
    } else {
      chunks_push(bquote(arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])))
      chunks_push(bquote(arm <- combine_levels(arm, ref_arm)))
      if (combine_comp_arms) {
        chunks_push(bquote(arm <- combine_levels(arm, comp_arm)))
      }
      chunks_push(bquote(anl[[.(arm_var)]] <- droplevels(arm)))
    }

    chunks_safe_eval()

    validate(need(nrow(chunks_get_var("anl")) > 10, "need at least 10 data points"))
    validate(need(
      !all(chunks_get_var("anl")$CNSR == 1),
      "No events have been observed for the selected subgroup/subpopulation"))

    if (isFALSE(compare_arms) || length(unique(ADSL_FILTERED[[arm_var]])) == 1) {
      chunks_push(bquote({
        tbl <- t_tte(
          formula = .(as.formula(
            paste0(
              "Surv(AVAL, !CNSR) ~ arm(", arm_var, ")"
            )
          )),
          data = anl,
          col_N = table(anl[[.(arm_var)]]),
          event_descr = if (is.null(.(event_desc_var))) NULL else as.factor(anl[[.(event_desc_var)]]),
          time_points = .(time_points),
          time_unit = .(time_unit),
          conf_level = .(conf_level),
          conf_type_survfit = .(conf_type_survfit),
          probs_survfit = .(probs_survfit),
          comparison = NULL
        )
        tbl
      }))
    } else {
      chunks_push(bquote({
        tbl <- t_tte(
          formula = .(as.formula(
            paste0(
              "Surv(AVAL, !CNSR) ~ arm(", arm_var, ")",
              if (length(strata_var) == 0) {
                ""
              } else {
                paste0(" + strata(", paste(strata_var, collapse = ", "), ")")
              }
            )
          )),
          data = anl,
          col_N = table(anl[[.(arm_var)]]),
          event_descr = if (is.null(.(event_desc_var))) NULL else as.factor(anl[[.(event_desc_var)]]),
          time_points = .(time_points),
          time_unit = .(time_unit),
          conf_level = .(conf_level),
          conf_type_survfit = .(conf_type_survfit),
          probs_survfit = .(probs_survfit),
          comparison = control_coxph(pval_method = .(pval_method_coxph),
                                     ties = .(ties_coxph),
                                     conf = .(conf_level_coxph))
        )
        tbl
      }))
    }

  })

  output$tte_table <- renderUI({
    table_reactive()

    chunks_safe_eval()

    tbl <- chunks_get_var("tbl")
    as_html(tbl)
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Cross Table",
      rcode = get_rcode(
        datasets = datasets,
        datanames = union("ADSL", dataname),
        title = label
      )
    )
  })

}
