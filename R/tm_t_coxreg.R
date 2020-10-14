#' Cox Regression Model
#'
#' Teal module to fit Cox univariate or multivariate models consistent with
#' `COXT01` and `COXT02` standard outputs, respectively.
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
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
#' @param multivariate If `FALSE`, the univariate approach is be computed
#'   (equivalent to `COXT01` standard) instead of the multivariate model
#'   (equivalent to `COXT02` standard).
#' @param conf_level \code{\link[teal]{choices_selected}} object with all
#'   available choices and preselected option for variable names that can be
#'   used for confidence level for computation of the confidence intervals.
#'
#' @details
#' The Cox Proportional Hazards (PH) model is the most commonly used method to
#' estimate the magnitude of
#' the effect in survival analysis. It assumes proportional hazards: the ratio
#' of the hazards between groups (e.g., two arms) is constant over time.
#' This ratio is referred to as the "hazard ratio" (HR) and is one of the most
#' commonly reported metrics to describe the effect size in survival analysis.
#' For further information about the Cox Proportional Hazards Model, check
#' "Statistical Analysis of Clinical Trials Data with R", NEST team.
#'
#' This modules expects that the analysis data has the following variables:
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
#' - The likelihood ratio test is not supported for model including strata,
#'   Wald test will be substituted.
#' - Multivariate is the default choice for backward compatibility.
#'
#' @export
#' @import magrittr
#' @importFrom stats setNames
#'
#' @md
#'
#' @examples
#' ## First example
#' ## =============
#' ## The example below is based on the usual approach involving creation of
#' ## a random CDISC dataset and then running the application.
#'
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
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- radsl(cached = TRUE)'),
#'     cdisc_dataset("ADTTE", ADTTE, code = 'ADTTE <- radtte(cached = TRUE)'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_coxreg(
#'       label = "Cox Reg.",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(c("ARM", "ARMCD", "ACTARMCD"), "ARM"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"
#'       ),
#'       strata_var = choices_selected(
#'         c("COUNTRY", "STRATA1", "STRATA2"), "STRATA1"
#'       ),
#'       cov_var    = choices_selected(
#'         c("AGE", "BMRKR1", "BMRKR2", "REGION1"), "AGE"
#'       ),
#'       multivariate = TRUE
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' ## Second example
#' ## ==============
#' ## This time, a synthetic pair of ADTTE/ADSL is fabricated for a Cox regression
#' ## where ties and pval_method matter.
#'
#' ## Dataset fabrication
#' ## -------------------
#'
#' ADTTE <- data.frame(
#'   STUDYID = "LUNG",
#'   AVAL    = c(        4,   3,   1,   1,   2,   2,   3,   1,   2),
#'   CNSR    = c(        1,   1,   1,   0,   1,   1,   0,   0,   0),
#'   ARMCD   = factor(
#'     c(                0,   1,   1,   1,   1,   0,   0,   0,   0),
#'     labels = c("ARM A", "ARM B")
#'   ),
#'   SEX     = factor(
#'     c(                0,   0,   0,   0,   1,   1,   1,   1,   1),
#'     labels = c("F", "M")
#'   ),
#'   INST    = factor(c("A", "A", "B", "B", "A", "B", "A", "B", "A")),
#' stringsAsFactors = FALSE)
#' ADTTE <- rbind(ADTTE, ADTTE, ADTTE, ADTTE)
#' ADTTE <- dplyr::as_tibble(ADTTE)
#' set.seed(1)
#' ADTTE$INST <- sample(ADTTE$INST)
#' ADTTE$AGE <- sample(seq(5, 75, 5), size = nrow(ADTTE), replace = TRUE)
#' ADTTE$USUBJID <- paste("sub", 1:nrow(ADTTE), ADTTE$INST, sep = "-")
#' ADTTE$PARAM <- ADTTE$PARAMCD <- "OS"
#' ADSL <- subset(
#'   ADTTE, select = c("USUBJID", "STUDYID", "ARMCD", "SEX", "INST", "AGE")
#' )
#'
#' ## Teal application
#' ## ================
#' ## Note that the R code exported by `Show R Code` does not include the data
#' ## preprocessing. You will need to create the dataset as above before
#' ## running the exported R code.
#'
#' arm_ref_comp = list(ARMCD = list(ref = "ARM A", comp = c("ARM B")))
#' app <- init(
#'   data = cdisc_data(
#'     dataset(
#'       dataname = "ADSL", data = ADSL,
#'       keys = keys(primary = c("USUBJID"), foreign = NULL, parent  = NULL)
#'     ),
#'     dataset(
#'       dataname = "ADTTE", data = ADTTE,
#'       keys = keys(
#'         primary = c("USUBJID", "PARAMCD"), foreign = c("USUBJID"),
#'         parent = "ADSL"
#'       )
#'     )
#'   ),
#'   modules = root_modules(
#'     tm_t_coxreg(
#'       label = "Cox Reg.",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(c("ARMCD"), "ARMCD"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"
#'       ),
#'       strata_var = choices_selected(c("INST"), NULL),
#'       cov_var = choices_selected(c("SEX", "AGE"), "SEX"),
#'       multivariate = TRUE
#'     )
#'   )
#' )
#'
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
                        multivariate = TRUE,
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
      radioButtons(
        ns("coxreg_type"),
        label = tags$label("Type of regression", class = "text-primary"),
        choices = c("univariate", "multivariate"),
        selected = ifelse(a$multivariate, "multivariate", "univariate"),
        inline   = FALSE
      ),
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
      uiOutput(outputId = ns("ui_inter")),
      uiOutput(ns("interaction_input")),
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
          uiOutput(outputId = ns("ui_pval_method")),
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
#' @importFrom utils.nest as_num
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
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte"
  )

  # The estimation of an interaction effect is an option that should be
  # accessible only in the context of standard `COXT01` (univariate).
  output$ui_inter <- renderUI({
    if (input$coxreg_type == "univariate") {
      tagList(
        checkboxInput(
          session$ns("include_interactions"),
          "Interaction terms",
          value = FALSE
        )
      )
    } else if (input$coxreg_type != "univariate") {
      return(NULL)
    }
  })

  # For every numeric covariate, the numeric level for the Hazard Ration
  # estimation is proposed only if the covariate is included in the model:
  # for this purpose, a function and a UI-rendered output.
  open_textinput <- function(x, anl) {

    y <- signif(stats::median(anl[[x]], na.rm = TRUE), 3)
    y <- tagList(
      textInput(
        session$ns(paste0("interact_", x)),
        label = paste0("Hazard Ratios for ", x, " at:"),
        value = as.character(y)
      )
    )

    return(y)
  }

  output$interaction_input <- renderUI({

    # The idea is to exclude cases when increments are not necessary and
    # finally accessing the UI-rendering function defined above.
    if (is.null(input$include_interactions)) {
      return(NULL)
    } else if (input$coxreg_type != "univariate") {
      return(NULL)
    } else if (!input$include_interactions | is.null(input$cov_var)) {
      return(NULL)
    } else {
      anl <- datasets$get_data(dataname, filtered = FALSE)
      cov_is_numeric <- vapply(
        anl[input$cov_var], FUN = is.numeric, FUN.VALUE = TRUE
      )
      interaction_var <- input$cov_var[cov_is_numeric]
    }

    if (length(interaction_var) == 0) {
      return(NULL)
    } else {
      y <- lapply(
        X = interaction_var,
        FUN = open_textinput,
        anl = anl
      )
      return(y)
    }

  })

  output$ui_pval_method <- renderUI({

    if (is.null(input$strata_var)) {
      tagList(

        radioButtons(
          session$ns("pval_method"),
          label = HTML(
            paste(
              "p-value method for ",
              tags$span(style="color:darkblue", "Coxph"), # nolint
              " (Hazard Ratio)", sep = ""
            )
          ),
          choices = c("wald", "likelihood"),
          selected = "wald"
        )
      )
    } else {
      NULL
    }

  })

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
    validate(need(paramcd, "'Select Endpoint' field is empty."))
    cov_var <- input$cov_var
    strata_var <- input$strata_var
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_comp_arms <- input$combine_comp_arms
    pval_method <- if (is.null(input$pval_method)) {
      "wald"
    } else {
      input$pval_method
    } # nolint
    ties <- input$ties
    conf_level <- input$conf_level
    multivariate <- input$coxreg_type == "multivariate"
    include_interactions <- any(input$include_interactions)

    # Preparation of the `increment` argument. This is where the output of
    #  UI-rendered `interaction_input` is processed. Basically it:
    #  (1) screens for all inputs with names starting by "interact_",
    #  (2) processes the text input to keep only numeric values,
    #  (3) renders a list conformable with `increments` as in `tern::t_coxreg`.
    if (include_interactions) {

      pattern <- "interact_"
      increments <- as.list(
        grep(pattern = pattern, x = names(input), value = TRUE)
      )
      increments <- setNames(nm = increments)
      increments <- lapply(
        increments, function(x) {
          y <- input[[x]]
          y <- unlist(utils.nest::as_num(y))
          return(y)
        }
      )
      names(increments) <- gsub(
        pattern = pattern, replacement = "", x = names(increments)
      )

    } else {
      increments <- NULL
    }

    if (length(strata_var) == 0) strata_var <- NULL
    if (length(cov_var) == 0)    cov_var    <- NULL

    ## validate input values ----
    validate_standard_inputs(
      adsl = ADSL_FILTERED,
      adslvars = c("USUBJID", "STUDYID", arm_var, strata_var, cov_var),
      anl = ANL_FILTERED,
      anlvars = c("USUBJID", "STUDYID",  "PARAMCD", "AVAL", "CNSR"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    if (!is.null(strata_var) & !is.null(cov_var)) {

      validate(
        need(
          expr = !any(strata_var %in% cov_var),
          message = paste(
            "A stratification variable is also selected as a covariate",
            "and blocks the model fit."
          )
        )
      )

    }

    validate(
      need(is.logical(combine_comp_arms), "Need combine arm information.")
    )
    validate(need(!is.null(ties), "Select ties for Coxph."))
    validate(
      need(
        conf_level >= 0 & conf_level <= 1,
        "Confidence interval must be in the range 0 to 1."
      )
    )

    validate(need(!is.null(cov_var), "Select at least one covariate."))


    ## Analysis ----
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED)

    chunks_reset(envir = environment())

    adsl_vars <- unique(
      c("USUBJID", "STUDYID", arm_var, strata_var, cov_var)
    ) # nolint
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
    chunks_push(
      bquote(arm <- relevel(as.factor(anl[[.(arm_var)]]), ref_arm[1]))
    )
    chunks_push(bquote(arm <- combine_levels(arm, ref_arm)))
    if (combine_comp_arms) {
      chunks_push(bquote(arm <- combine_levels(arm, comp_arm)))
    }
    chunks_push(bquote(anl[[.(arm_var)]] <- droplevels(arm)))

    chunks_safe_eval()
    validate(need(any(chunks_get_var("anl")[["CNSR"]] == 0), "No observed events in the data"))

    # Preparation of the formula for `tern::t_coxreg` in three steps.
    ## (1/3) Prepare the original formula ...
    form <- if (!multivariate) {
      stats::as.formula(
        paste0("Surv(AVAL, !CNSR) ~ pairwise(", arm_var, ")")
      )
    } else {
      stats::as.formula(
        paste0("Surv(AVAL, !CNSR) ~ ", arm_var, "")
      )
    }

    ## (2/3) ... then deal with candidate covariates if any ...
    form <- if (!multivariate & !include_interactions & length(cov_var) > 0) {
      form <- stats::update.formula(
        old = form,
        new = paste(" ~ . + univariate(", paste(cov_var, collapse = ", "), ")")
      )
    } else if (!multivariate & include_interactions & length(cov_var) > 0) {
      stats::update.formula(
        old = form,
        new = paste(" ~ . * univariate(", paste(cov_var, collapse = ", "), ")")
      )
    } else if (multivariate & length(cov_var) > 0) {
      stats::update.formula(
        old = form,
        new = paste(" ~ . + ", paste(cov_var, collapse = " + "))
      )
    }

    ## (3/3) ... finally add the strata if any.
    if (length(strata_var) > 0) {
      form <- stats::update.formula(
        old = form,
        new = paste0(" ~.  + strata(", paste(strata_var, collapse = ", "), ")")
      )
    }

    chunks_push(
      bquote({
        tbl <- tern::t_coxreg(
          formula = .(form),
          data = anl,
          conf_level  = .(conf_level),
          pval_method = .(pval_method),
          ties = .(ties),
          simplify = FALSE,
          increments = .(increments)
        )
        tbl
      })
    )

    chunks_safe_eval()
    invisible()

  })

  output$coxreg_table <- renderUI({
    # Note that COXT02 (multivariate) returns a single `rtable` object while
    # COXT01 (univariate) returns a list of `rtable` (one level per treatment).
    table_reactive()
    tbl <- chunks_get_var("tbl")
    if (input$coxreg_type == "univariate") {
      lapply(tbl, as_html)
    } else if (input$coxreg_type != "univariate") {
      as_html(tbl)
    }
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "(Multi-variable) Cox proportional hazard regression model",
      rcode = get_rcode(
        datasets = datasets,
        datanames = union("ADSL", dataname),
        title = label
      )
    )
  })

}
