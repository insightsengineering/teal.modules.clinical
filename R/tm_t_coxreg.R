#' Cox Multiple Regression Model
#'
#' Teal module to program the simple effects of a multi-variable Cox regression
#' models such as: `Surv(time, event) ~ Arm + Cov1 + ... `.
#' This provides the `COXT02` standard output.
#'
#' @inheritParams teal.devel::standard_layout
#' @param label Menu item label of the module in the teal app.
#' @param dataname `character` analysis data used in teal module, needs to be
#'   available in the list passed to the `data` argument of
#'   \code{\link[teal]{init}}. Note that the data is expected to be in vertical
#'   form with the `PARAMCD` variable filtering to one observation per patient.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available
#'   choices and preselected option for variable names that can be used as
#'   `arm_var`
#' @param arm_ref_comp (\code{\link[teal]{choices_selected}}) optional, if
#'   specified it must be a named list with each element corresponding to an arm
#'   variable in `ADSL` and the element must be another list with the elements
#'   named `ref` and `comp` that the defined the default reference and
#'   comparison arms when the arm variable is changed.
#' @param paramcd \code{\link[teal]{choices_selected}} object with all available
#'   choices and preselected option for variable names that can be used as
#'   `PARAMCD` variable.
#' @param cov_var \code{\link[teal]{choices_selected}} object with all available
#'   choices and preselected option for variable names that can be used as
#'   covariates in the model.
#' @param strata_var \code{\link[teal]{choices_selected}} object with all
#'   available choices and preselected option for variable names that can be
#'   used for stratification
#' @param conf_level \code{\link[teal]{choices_selected}} object with all
#'   available choices and preselected option for variable names that can be
#'   used for confidence level for computation of the confidence intervals.
#'
#' @details
#' This modules expects that the analysis data has the following variables
#'
#' \tabular{ll}{
#'  `AVAL` \tab time to event\cr
#'  `CNSR` \tab boolean or 0,1 is element in `AVAL` censored\cr
#'  `PARAMCD` \tab variable used to filter for endpoint (e.g. OS), after
#'  filtering for `paramcd` one observation per patient is expected
#' }
#'
#' The arm variables, stratification and covariate variables are taken from the
#' `ADSL` data.
#'
#' @section Note:
#' The likelihood ratio test is not supported for model including strata,
#' Wald test will be substituted.
#'
#' @export
#' @import magrittr
#'
#' @md
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL  <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#' arm_ref_comp = list(
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   ),
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADTTE", ADTTE),
#'     code = "ADSL  <- radsl(cached = TRUE);  ADTTE <- radtte(cached = TRUE)"
#'   ),
#'   modules = root_modules(
#'     tm_t_coxreg(
#'       label = "Cox Reg.",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(c("ARM", "ARMCD", "ACTARMCD"), "ARM"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"),
#'       strata_var = choices_selected(c("COUNTRY", "STRATA1", "STRATA2"), "STRATA1"),
#'       cov_var    = choices_selected(
#'         c("AGE", "BMRKR1", "BMRKR2", "REGION1"), "AGE"
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_coxreg <- function(label,
                        dataname,
                        arm_var,
                        arm_ref_comp = NULL,
                        paramcd,
                        cov_var,
                        strata_var,
                        conf_level = choices_selected(
                          c(0.8, 0.85, 0.90, 0.95, 0.99, 0.995),
                          0.95, keep_order = TRUE
                        ),
                        pre_output = NULL,
                        post_output = NULL
) {


  stopifnot(length(dataname) == 1)
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(cov_var))
  stopifnot(is.choices_selected(strata_var))
  stopifnot(is.choices_selected(conf_level))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_coxreg,
    ui = ui_t_coxreg,
    ui_args = args,
    server_args = list(
      arm_ref_comp = arm_ref_comp,
      dataname = dataname,
      label = label
    ),
    filters = dataname
  )
}

#' @import teal.devel
ui_t_coxreg <- function(id, ...) {

  a <- list(...) # module args

  ns <- NS(id)

  standard_layout(
    output = white_small_well(uiOutput(ns("coxreg_table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("paramcd"),
        "Select Endpoint",
        a$paramcd$choices,
        a$paramcd$selected,
        multiple = FALSE,
        fixed = a$paramcd$fixed
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
        "Reference Group",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      helpText("Multiple reference groups are combined into a single group."),
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
        ns("cov_var"),
        "Covariates",
        a$cov_var$choices,
        a$cov_var$selected,
        multiple = TRUE,
        label_help = helpText("from ", tags$code("ADSL")),
        fixed = a$cov_var$fixed
      ),
      optionalSelectInput(
        ns("strata_var"),
        "Stratify by",
        a$strata_var$choices,
        a$strata_var$selected,
        multiple = TRUE,
        label_help = helpText("from ", tags$code("ADSL")),
        fixed = a$strata_var$fixed
      ),
      panel_group(
        panel_item(
          "Additional table settings",
          radioButtons(
            ns("pval_method"),
            label = HTML(
              paste(
                "p-value method for ",
                tags$span(style="color:darkblue", "Coxph"), # nolint
                " (Hazard Ratio)", sep = ""
              )
            ),
            choices = c("wald", "likelihood"),
            selected = "wald"
          ),
          radioButtons(
            ns("ties"),
            label = HTML(
              paste(
                "Ties for ",
                tags$span(style="color:darkblue", "Coxph"), # nolint
                " (Hazard Ratio)", sep = "")
            ),
            choices = c("exact", "breslow", "efron"),
            selected = "exact"
          ),
          numericInput(
            inputId = ns("conf_level"),
            label = HTML(
              paste(
                "Confidence level for ",
                tags$span(style="color:darkblue", "Coxph"), # nolint
                " (Hazard Ratio)", sep = ""
              )
            ),
            value = 0.95,
            min = 0.01,
            max = 0.99,
            step = 0.01,
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

#' @import teal.devel
#' @importFrom rtables as_html
srv_t_coxreg <- function(input,
                         output,
                         session,
                         datasets,
                         dataname,
                         arm_ref_comp,
                         label) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default ----
  #   comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",  # from UI
    adsl = datasets$get_data("ADSL", filtered = FALSE),
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte"
  )

  # Create output
  table_reactive <- reactive({

    ## resolve all reactive expressions ----

    # nolint start
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE)
    ANL_FILTERED  <- datasets$get_data(
      dataname, filtered = TRUE
    )
    # nolint end

    paramcd <- input$paramcd # nolint
    cov_var <- input$cov_var
    strata_var <- input$strata_var
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_comp_arms <- input$combine_comp_arms
    pval_method <- input$pval_method # nolint
    ties <- input$ties
    conf_level <- input$conf_level

    if (length(strata_var) == 0) strata_var <- NULL
    if (length(cov_var) == 0)    cov_var    <- NULL


    ## validate your input values ----
    validate_standard_inputs(
      adsl = ADSL_FILTERED,
      adslvars = c("USUBJID", "STUDYID", arm_var, strata_var, cov_var),
      anl = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "CNSR"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    validate(need(is.logical(combine_comp_arms), "need combine arm information"))
    validate(need(!is.null(ties), "Must select ties for Coxph"))
    validate(
      need(
        conf_level >= 0 & conf_level <= 1,
        "Confidence interval must be in the range 0 to 1."
      )
    )

    ## Analysis ----

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED)

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, strata_var, cov_var)) # nolint
    anl_vars <- unique(c("USUBJID", "STUDYID", "AVAL", "CNSR")) # nolint

    chunks_push(bquote(ref_arm <- .(ref_arm)))
    chunks_push(bquote(comp_arm <- .(comp_arm)))
    chunks_push(bquote(strata_var <- .(strata_var)))
    chunks_push(bquote(combine_comp_arms <- .(combine_comp_arms)))
    chunks_push(
      bquote(
        adsl_p <- subset(
          ADSL_FILTERED, .(as.name(arm_var)) %in% c(ref_arm, comp_arm)
        )
      )
    )
    chunks_push(
      bquote(
        anl_endpoint <- subset(.(as.name(anl_name)), PARAMCD == .(paramcd))
      )
    )
    chunks_push(
      bquote({
        anl <- merge(
          x = adsl_p[, .(adsl_vars)],      all.x = FALSE,
          y = anl_endpoint[, .(anl_vars)], all.y = FALSE,
          by = c("USUBJID", "STUDYID")
        )
      })
    )
    chunks_push(bquote(arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1])))
    chunks_push(bquote(arm <- combine_levels(arm, ref_arm)))
    if (combine_comp_arms) {
      chunks_push(bquote(arm <- combine_levels(arm, comp_arm)))
    }
    chunks_push(bquote(anl[[.(arm_var)]] <- droplevels(arm)))

    form <- stats::as.formula(paste0("Surv(AVAL, !CNSR) ~ ", arm_var, ""))
    if (length(strata_var) > 0) form <- stats::update.formula(
      old = form,
      new = paste0(" ~.  + strata(", paste(strata_var, collapse = ", "), ")")
    )
    if (length(cov_var) > 0) form <- stats::update.formula(
      old = form, new = paste(" ~ . + ", paste(cov_var, collapse = " + "))
    )

    chunks_push(
      bquote({
        tbl <- t_coxreg(
          formula = .(form),
          data = anl,
          conf_level  = .(conf_level),
          pval_method = .(pval_method),
          ties = .(ties)
        )
        tbl
      })
    )

    chunks_safe_eval()
    invisible()

  })

  output$coxreg_table <- renderUI({
    table_reactive()
    tbl <- chunks_get_var("tbl")
    as_html(tbl)
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "(Multi-variable) Cox proportional hazard regression model",
      rcode = get_rcode(
        datasets = datasets,
        title = label
      )
    )
  })

}
