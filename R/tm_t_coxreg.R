#' Template for Cox Regression
#'
#' Creates a valid expression for Cox regression analysis.
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @inheritParams tm_t_coxreg
#' @param control (`list`)\cr list of settings for the analysis,
#'   see [control_coxreg()].
#'
#' @importFrom broom tidy
#' @importFrom stats relevel
#'
#' @seealso [tm_t_coxreg()]
#'
template_coxreg <- function(dataname,
                            cov_var,
                            arm_var,
                            ref_arm,
                            comp_arm,
                            paramcd,
                            at = list(),
                            strata = NULL,
                            combine_comp_arms = FALSE,
                            multivariate = FALSE,
                            control = control_coxreg()) {

  y <- list()

  data_pipe <- list()
  data_list <- list()

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = df  %>%
        filter(PARAMCD == paramcd) %>%
        mutate(event = 1 - CNSR),
      env = list(
        df = as.name(dataname),
        paramcd = paramcd
      )
    )
  )

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      filter(arm_var %in% c(ref_arm, comp_arm)),
      env = list(
        arm_var = as.name(arm_var),
        ref_arm = ref_arm,
        comp_arm = comp_arm
      )
    )
  )

  data_pipe <- add_expr(
    data_pipe,
    substitute_names(
      expr = mutate(arm_var = droplevels(relevel(arm_var, ref_arm))),
      names = list(
        arm_var = as.name(arm_var)
      ),
      others = list(
        ref_arm = ref_arm
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- data_pipe,
      env = list(data_pipe = pipe_expr(data_pipe))
    )
  )

  if (combine_comp_arms) {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = anl$arm_var <- combine_levels(
          x = anl$arm_var,
          levels = comp_arm
        ),
        env = list(
          arm_var = as.name(arm_var),
          comp_arm = comp_arm
        )
      )
    )
  }

  data_list <- add_expr(
    data_list,
    substitute(
      expr = variables <- list(
        time = "AVAL", event = "event", arm = arm_var, covariates = cov_var
      ),
      env = list(
        arm_var = arm_var,
        cov_var = cov_var
      )
    )
  )

  if (!is.null(strata)) {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = variables$strata <- strata_var,
        env = list(
          strata_var = strata
        )
      )
    )
  }

  data_list <- if (multivariate) {
    add_expr(
      data_list,
      substitute(
        model <- fit_coxreg_multivar(
          variables = variables,
          data = anl,
          control = control
        ),
        env = list(
          control = control
          )
        )
      )
  } else {
    add_expr(
      data_list,
      substitute(
        model <- fit_coxreg_univar(
          variables = variables,
          data = anl,
          control = control,
          at = at
        ),
      env = list(
        at = at,
        control = control
        )
      )
    )
  }

  data_list <- add_expr(
    data_list,
    quote(df <- broom::tidy(model))
    )

  y$data <- bracket_expr(data_list)

  layout_list <- list()

  layout_list <- add_expr(layout_list, quote(basic_table()))

  if (!multivariate) {
    layout_list <- add_expr(
      layout_list,
      quote(split_rows_by("effect"))
    )
  }

  layout_list <- add_expr(
    layout_list,
    quote(split_rows_by("term", child_labels = "hidden"))
  )

  vars <- c("n", "hr", "ci", "pval")

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = summarize_coxreg(
        multivar = multivariate,
        conf_level = conf_level,
        vars = vars
      ),
      env = list(
        multivariate = multivariate,
        conf_level = control$conf_level,
        vars = if (!is_empty(at)) c(vars, "pval_inter") else vars
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- quote(result <- build_table(lyt = lyt, df = df))

  y
}

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
#' ADTTE <- base::rbind(ADTTE, ADTTE, ADTTE, ADTTE)
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
#'
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
                        post_output = NULL) {
  stopifnot(
    length(dataname) == 1,
    is.choices_selected(arm_var),
    is.choices_selected(paramcd),
    is.choices_selected(cov_var),
    is.choices_selected(strata_var),
    is.choices_selected(conf_level)
    )

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
#'
ui_t_coxreg <- function(id, ...) {

  a <- list(...) # module args

  ns <- NS(id)

  standard_layout(
    output = white_small_well(uiOutput(ns("as_html"))),
    encoding = div(
      radioButtons(
        ns("type"),
        label = tags$label("Type of Regression:", class = "text-primary"),
        choices = c("Univariate", "Multivariate"),
        selected = if (a$multivariate) "Multivariate" else "Univariate"
      ),
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", code(a$dataname)),
      optionalSelectInput(
        ns("paramcd"),
        "Select Endpoint",
        a$paramcd$choices,
        a$paramcd$selected,
        fixed = a$paramcd$fixed
      ),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        a$arm_var$choices,
        a$arm_var$selected,
        fixed = a$arm_var$fixed
      ),
      selectInput(
        ns("ref_arm"),
        "Reference Group",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        ns("comp_arm"),
        "Comparison Group",
        choices = NULL,
        multiple = TRUE
      ),
      conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'Multivariate'"),
        checkboxInput(
          ns("combine_comp_arms"),
          "Combine all comparison groups?"
        )
      ),
      optionalSelectInput(
        ns("cov_var"),
        "Covariates",
        a$cov_var$choices,
        a$cov_var$selected,
        multiple = TRUE,
        label_help = helpText("from ", code("ADSL")),
        fixed = a$cov_var$fixed
      ),
      conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'Univariate'"),
        checkboxInput(
          ns("interactions"),
          "Interaction terms"
        )
      ),
      uiOutput(ns("interaction_input")),
        optionalSelectInput(
          ns("strata_var"),
          "Stratify by",
          a$strata_var$choices,
          a$strata_var$selected,
          multiple = TRUE,
          label_help = helpText("from ", code("ADSL")),
          fixed = a$strata_var$fixed
        ),
      panel_group(
        panel_item(
          "Additional table settings",
          conditionalPanel(
            condition = paste0("input['", ns("strata_var"), "'] != ''"),
            radioButtons(
              ns("pval_method"),
              label = p(
                "p-value method for",
                span(style = "color:darkblue", "Coxph"),
                "(Hazard Ratio)"
              ),
              choices = c("wald", "likelihood"),
              selected = "wald"
            )
          ),
          radioButtons(
            ns("ties"),
            label = p(
              "Ties for ",
              span(style = "color:darkblue", "Coxph"),
              " (Hazard Ratio)",
              sep = ""
              ),
            choices = c("exact", "breslow", "efron"),
            selected = "exact"
          ),
          numericInput(
            inputId = ns("conf_level"),
            label = p(
              "Confidence level for ",
              span(style = "color:darkblue", "Coxph"),
              " (Hazard Ratio)",
              sep = ""
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
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @import teal.devel
#' @importFrom rtables as_html
#'
srv_t_coxreg <- function(input,
                         output,
                         session,
                         datasets,
                         dataname,
                         arm_ref_comp,
                         label) {

  init_chunks()

  # Observer to update reference and comparison arm input options.
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte"
  )

  ## render conditional strata levels input UI ----
  open_textinput <- function(x, anl) {
    # For every numeric covariate, the numeric level for the Hazard Ration
    # estimation is proposed only if the covariate is included in the model:
    # for this purpose, a function and a UI-rendered output.
    med <- signif(stats::median(anl[[x]], na.rm = TRUE), 3)
    textInput(
      session$ns(paste0("interact_", x)),
      label = paste("Hazard Ratios for", x, "at:"),
      value = as.character(med)
    )
  }

  output$interaction_input <- renderUI({
    # exclude cases when increments are not necessary and
    # finally accessing the UI-rendering function defined above.
    if (!is.null(input$interactions) &&
        input$interactions &&
        !is.null(input$cov_var)) {
      anl <- datasets$get_data(dataname, filtered = FALSE)

      cov_is_numeric <- vapply(anl[input$cov_var], is.numeric, logical(1))
      interaction_var <- input$cov_var[cov_is_numeric]
      print(interaction_var)

      if (length(interaction_var) != 0) {
        lapply(interaction_var, open_textinput, anl = anl)
      }
    }
  })

  ## Prepare the call evaluation environment ----
  prepared_env <- reactive({
    # does not require parent dataset for column counts
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    validate(
      need(input$arm_var, "Please select an arm variable"),
      need(input$paramcd, "Please select an endpoint"),
      need(input$ref_arm, "Please select a reference arm"),
      need(input$comp_arm, "Please select a comparison arm")
      )
    validate_has_data(anl_filtered, min_nrow = 1)
    # covariate variables are always needed in fit_coxreg_univar
    # not only with interactions selected
    validate(need(input$cov_var, "Please select an interaction variable"))
    validate(
      need(
        c(input$ref_arm, input$comp_arm) %in% levels(anl_filtered[[input$arm_var]]),
        "Arm variable is updating"
        )
      )

    e <- new.env()
    e[[paste0(dataname, "_FILTERED")]] <- anl_filtered
    e
  })

  at <- reactive({
    res <- lapply(
      input$cov_var,
      function(x) {
        cov <- input[[paste0("interact_", x)]]
        if (!is.null(cov)) {
          vec <- strsplit(cov, split = ",")
          as.numeric(unlist(vec))
        }
      })
    setNames(res, input$cov_var)
  })

  call_template <- function(comp_arm) {
    template_coxreg(
      dataname = paste0(dataname, "_FILTERED"),
      cov_var = input$cov_var,
      at = if (!is.null(input$interactions) && input$interactions) at() else list(),
      arm_var = input$arm_var,
      ref_arm = input$ref_arm,
      comp_arm = comp_arm,
      strata = input$strata_var,
      combine_comp_arms = input$combine_comp_arms,
      paramcd = input$paramcd,
      multivariate = input$type == "Multivariate",
      control = control_coxreg(
        pval_method = input$pval_method,
        ties = input$ties,
        conf_level = input$conf_level,
        interaction = if_null(input$interactions, FALSE)
      )
    )
  }

  ## generate table call with template and render table ----
  output$as_html <- renderUI({
    chunks_reset(envir = prepared_env())

    calls <- if (input$type != "Multivariate") {
      lapply(input$comp_arm, call_template)
    } else {
      list(call_template(input$comp_arm))
    }

    res <- lapply(
      calls,
      function(call) {
        mapply(expr = call, chunks_push)
        chunks_safe_eval()
        chunks_get_var("result")
      })

    div(lapply(res, as_html))
  })

  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = "R Code for the Current (Multi-variable) Cox proportional hazard regression model",
    code_header = label
  )
}
