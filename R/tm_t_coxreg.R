#' Template: Cox Regression Univariate
#'
#' Creates a valid expression for Cox regression analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis, see [control_coxreg()].
#' @param at (`list` of `numeric`)\cr when the candidate covariate is a `numeric`, use `at`
#' to specify the value of the covariate at which the effect should be estimated.
#' @param append (`logical`)\cr if the result should be appended to the previous one.
#'
#' @seealso [tm_t_coxreg()]
#'
template_coxreg_u <- function(dataname,
                              cov_var,
                              arm_var,
                              cnsr_var,
                              aval_var,
                              ref_arm,
                              comp_arm,
                              paramcd,
                              at = list(),
                              strata_var = NULL,
                              combine_comp_arms = FALSE,
                              control = control_coxreg(),
                              append = FALSE,
                              basic_table_args = teal.devel::basic_table_args(
                                title = paste0("Multi-Variable Cox Regression for ", paramcd)
                              )) {
  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  data_pipe <- list()
  data_list <- list()

  data_pipe <- add_expr(
    data_pipe,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  if (combine_comp_arms) {
    data_pipe <- add_expr(
      data_pipe,
      utils.nest::substitute_names(
        expr = dplyr::mutate(arm_var = combine_levels(x = arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
  }

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = dplyr::mutate(event = 1 - cnsr_var),
      env = list(cnsr_var = as.name(cnsr_var))
    )
  )

  data_pipe <- add_expr(data_pipe, quote(df_explicit_na(na_level = "")))

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- data_pipe,
      env = list(data_pipe = pipe_expr(data_pipe))
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = variables <- list(
        time = aval_var, event = "event", arm = arm_var, covariates = cov_var
      ),
      env = list(
        aval_var = aval_var,
        arm_var = arm_var,
        cov_var = cov_var
      )
    )
  )

  if (!is.null(strata_var)) {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = variables$strata <- strata_var,
        env = list(
          strata_var = strata_var
        )
      )
    )
  }

  data_list <-
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

  data_list <- add_expr(
    data_list,
    quote(df <- broom::tidy(model))
  )

  y$data <- bracket_expr(data_list)

  layout_list <- list()

  layout_list <- add_expr(layout_list, teal.devel::parse_basic_table_args(basic_table_args))
  layout_list <- add_expr(
    layout_list,
    quote(rtables::split_rows_by("effect"))
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::append_topleft(paramcd) %>%
        rtables::split_rows_by("term", child_labels = "hidden"),
      env = list(paramcd = paramcd)
    )
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
        multivariate = FALSE,
        conf_level = control$conf_level,
        vars = if (control$interaction) c(vars, "pval_inter") else vars
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- if (append) {
    quote(result <- c(result, rtables::build_table(lyt = lyt, df = df)))
  } else {
    quote(result <- rtables::build_table(lyt = lyt, df = df))
  }

  y
}

#' Template: Cox Regression Multivariate
#'
#' Creates a valid expression for Cox regression analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis,
#'   see [control_coxreg()].
#' @param at (`list` of `numeric`)\cr when the candidate covariate is a
#'  `numeric`, use `at` to specify the value of the covariate at which the
#'  effect should be estimated.
#'
#' @seealso [tm_t_coxreg()]
#'
template_coxreg_m <- function(dataname,
                              cov_var,
                              arm_var,
                              cnsr_var,
                              aval_var,
                              ref_arm,
                              comp_arm,
                              paramcd,
                              at = list(),
                              strata_var = NULL,
                              combine_comp_arms = FALSE,
                              control = control_coxreg(),
                              basic_table_args = teal.devel::basic_table_args(
                                title = paste0("Cox Regression for ", paramcd)
                              )) {
  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  data_pipe <- list()
  data_list <- list()

  data_pipe <- add_expr(
    data_pipe,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  if (combine_comp_arms) {
    data_pipe <- add_expr(
      data_pipe,
      utils.nest::substitute_names(
        expr = dplyr::mutate(arm_var = combine_levels(x = arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
  }

  data_pipe <- add_expr(
    data_pipe,
    substitute(
      expr = dplyr::mutate(event = 1 - cnsr_var),
      env = list(cnsr_var = as.name(cnsr_var))
    )
  )

  data_pipe <- add_expr(data_pipe, quote(df_explicit_na(na_level = "")))

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- data_pipe,
      env = list(data_pipe = pipe_expr(data_pipe))
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = variables <- list(
        time = aval_var, event = "event", arm = arm_var, covariates = cov_var
      ),
      env = list(
        aval_var = aval_var,
        arm_var = arm_var,
        cov_var = cov_var
      )
    )
  )

  if (!is.null(strata_var)) {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = variables$strata <- strata_var,
        env = list(strata_var = strata_var)
      )
    )
  }

  data_list <- add_expr(
    data_list,
    substitute(
      model <- fit_coxreg_multivar(
        variables = variables,
        data = anl,
        control = control
      ),
      env = list(control = control)
    )
  )

  data_list <- add_expr(
    data_list,
    quote(df <- broom::tidy(model))
  )

  y$data <- bracket_expr(data_list)

  layout_list <- list()

  layout_list <- add_expr(layout_list, teal.devel::parse_basic_table_args(basic_table_args))

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::append_topleft(paramcd) %>%
        rtables::split_rows_by("term", child_labels = "hidden"),
      env = list(paramcd = paramcd)
    )
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
        multivariate = TRUE,
        conf_level = control$conf_level,
        vars = if (control$interaction) c(vars, "pval_inter") else vars
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- quote({
    result <- rtables::build_table(lyt = lyt, df = df)
    result
  })

  y
}

#' Teal Module: Cox Regression Model
#'
#' Teal module to fit Cox univariate or multivariate models consistent with
#' `COXT01` and `COXT02` standard outputs, respectively.
#'
#' @inheritParams module_arguments
#' @param multivariate (`logical`)\cr
#'   If `FALSE`, the univariate approach is used
#'   (equivalent to `COXT01` standard) instead of the multivariate model
#'   (equivalent to `COXT02` standard).
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
#'
#' @examples
#' ## First example
#' ## =============
#' ## The example below is based on the usual approach involving creation of
#' ## a random CDISC dataset and then running the application.
#'
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#' arm_ref_comp <- list(
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
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL  <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADTTE", ADTTE, code = 'ADTTE <- synthetic_cdisc_data("latest")$adtte'),
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
#'       cov_var = choices_selected(
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
#'   AVAL = c(4, 3, 1, 1, 2, 2, 3, 1, 2),
#'   CNSR = c(1, 1, 1, 0, 1, 1, 0, 0, 0),
#'   ARMCD = factor(
#'     c(0, 1, 1, 1, 1, 0, 0, 0, 0),
#'     labels = c("ARM A", "ARM B")
#'   ),
#'   SEX = factor(
#'     c(0, 0, 0, 0, 1, 1, 1, 1, 1),
#'     labels = c("F", "M")
#'   ),
#'   INST = factor(c("A", "A", "B", "B", "A", "B", "A", "B", "A")),
#'   stringsAsFactors = FALSE
#' )
#' ADTTE <- base::rbind(ADTTE, ADTTE, ADTTE, ADTTE)
#' ADTTE <- dplyr::as_tibble(ADTTE)
#' set.seed(1)
#' ADTTE$INST <- sample(ADTTE$INST)
#' ADTTE$AGE <- sample(seq(5, 75, 5), size = nrow(ADTTE), replace = TRUE)
#' ADTTE$USUBJID <- paste("sub", 1:nrow(ADTTE), ADTTE$INST, sep = "-")
#' ADTTE$PARAM <- ADTTE$PARAMCD <- "OS"
#' ADSL <- subset(
#'   ADTTE,
#'   select = c("USUBJID", "STUDYID", "ARMCD", "SEX", "INST", "AGE")
#' )
#'
#' ## Teal application
#' ## ================
#' ## Note that the R code exported by `Show R Code` does not include the data
#' ## preprocessing. You will need to create the dataset as above before
#' ## running the exported R code.
#'
#' arm_ref_comp <- list(ARMCD = list(ref = "ARM A", comp = c("ARM B")))
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset(
#'       dataname = "ADSL",
#'       x = ADSL
#'     ),
#'     cdisc_dataset(
#'       dataname = "ADTTE",
#'       x = ADTTE
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_coxreg <- function(label,
                        dataname,
                        parentname = ifelse(
                          inherits(arm_var, "data_extract_spec"),
                          teal.devel::datanames_input(arm_var),
                          "ADSL"
                        ),
                        arm_var,
                        arm_ref_comp = NULL,
                        paramcd,
                        cov_var,
                        strata_var,
                        aval_var = choices_selected(variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE),
                        cnsr_var = choices_selected(variable_choices(dataname, "CNSR"), "CNSR", fixed = TRUE),
                        multivariate = TRUE,
                        conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_coxreg")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  utils.nest::stop_if_not(
    is.choices_selected(conf_level),
    list(
      is.null(pre_output) || inherits(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || inherits(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = parentname, multiple = TRUE)
  )

  module(
    label = label,
    server = srv_t_coxreg,
    ui = ui_t_coxreg,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        arm_ref_comp = arm_ref_comp,
        dataname = dataname,
        parentname = parentname,
        label = label,
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_coxreg <- function(id, ...) {
  a <- list(...) # module args
  is_single_dataset_value <- teal.devel::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$strata_var,
    a$aval_var,
    a$cnsr_var,
    a$cov_var
  )

  ns <- NS(id)

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(teal.devel::table_with_settings_ui(ns("table"))),
    encoding = div(
      radioButtons(
        ns("type"),
        label = tags$label("Type of Regression:", class = "text-primary"),
        choices = c(
          "Separate models for comparison groups with one covariate at a time" = "Univariate",
          "One model with all comparison groups and covariates" = "Multivariate"
        ),
        selected = dplyr::if_else(a$multivariate, "Multivariate", "Univariate")
      ),
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(
        a[c("arm_var", "paramcd", "subgroup_var", "strata_var", "aval_var", "cnsr_var", "cov_var")]
      ),
      teal.devel::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
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
      teal.devel::data_extract_ui(
        id = ns("cov_var"),
        label = "Covariates",
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'Univariate'"),
        checkboxInput(
          ns("interactions"),
          "Interaction terms"
        )
      ),
      uiOutput(ns("interaction_input")),
      teal.devel::data_extract_ui(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
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
          optionalSelectInput(
            inputId = ns("conf_level"),
            label = p(
              "Confidence level for ",
              span(style = "color:darkblue", "Coxph"),
              " (Hazard Ratio)",
              sep = ""
            ),
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          )
        )
      )
    ),
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_coxreg <- function(input,
                         output,
                         session,
                         datasets,
                         dataname,
                         parentname,
                         arm_var,
                         paramcd,
                         strata_var,
                         aval_var,
                         cnsr_var,
                         cov_var,
                         arm_ref_comp,
                         label,
                         basic_table_args) {
  stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

  # Observer to update reference and comparison arm input options.
  teal.devel::arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    dataname = parentname,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_coxreg"
  )

  anl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(
      arm_var = arm_var,
      paramcd = paramcd,
      strata_var = strata_var,
      aval_var = aval_var,
      cnsr_var = cnsr_var,
      cov_var = cov_var
    ),
    merge_function = "dplyr::inner_join"
  )

  ## render conditional strata levels input UI  ----
  open_textinput <- function(x, anl) {
    # For every numeric covariate, the numeric level for the Hazard Ration
    # estimation is proposed only if the covariate is included in the model:
    # for this purpose, a function and a UI-rendered output.
    textInput(
      session$ns(paste0("interact_", x)),
      label = paste("Hazard Ratios for", x, "at (comma delimited):"),
      value = as.character(stats::median(anl$data()[[x]]))
    )
  }

  output$interaction_input <- renderUI({
    # exclude cases when increments are not necessary and
    # finally accessing the UI-rendering function defined above.
    if (!is.null(input$interactions) && input$interactions) {
      anl_m <- anl_merged()
      input_cov_var <- as.vector(anl_m$columns_source$cov_var)

      anl <- datasets$get_data(dataname, filtered = FALSE)
      cov_is_numeric <- vapply(anl[input_cov_var], is.numeric, logical(1))
      interaction_var <- input_cov_var[cov_is_numeric]
      if (length(interaction_var) > 0 && length(input_cov_var) > 0) {
        lapply(interaction_var, open_textinput, anl = anl_m)
      }
    }
  })

  ## Prepare the call evaluation environment ----
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]
    input_cov_var <- as.vector(anl_m$columns_source$cov_var)
    anl <- datasets$get_data(dataname, filtered = FALSE)
    cov_is_numeric <- vapply(anl[input_cov_var], is.numeric, logical(1))
    interaction_var <- input_cov_var[cov_is_numeric]

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var),
      arm_var = input_arm_var,
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      min_nrow = 4
    )

    #  validate arm levels
    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
    }

    validate(need(
      input$conf_level >= 0 && input$conf_level <= 1,
      "Please choose a confidence level between 0 and 1"
    ))

    teal.devel::validate_no_intersection(
      input_arm_var,
      input_strata_var,
      "`Treatment` and `Strata` variables should not be overlapped."
    )
    teal.devel::validate_no_intersection(
      input_arm_var,
      input_cov_var,
      "`Treatment` and `Covariate` variables should not be overlapped."
    )
    teal.devel::validate_no_intersection(
      input_strata_var,
      input_cov_var,
      "`Strata` and `Covariate` variables should not be overlapped."
    )

    do.call(what = "validate_standard_inputs", validate_args)

    arm_n <- base::table(anl_m$data()[[input_arm_var]])
    anl_arm_n <- if (input$combine_comp_arms) {
      c(sum(arm_n[input$ref_arm]), sum(arm_n[input$comp_arm]))
    } else {
      c(sum(arm_n[input$ref_arm]), arm_n[input$comp_arm])
    }
    validate(need(
      all(anl_arm_n >= 2),
      "Each treatment group should have at least 2 records."
    ))

    # validate p-value method
    if (length(input_strata_var) > 0) {
      validate(need(
        input$pval_method == "wald",
        "Only Wald tests are supported for models with strata."
      ))
    }

    if (input$type == "Multivariate") {
      validate(need(
        input$interactions == FALSE,
        "Interaction is only supported for univariate models."
      ))
    }

    if (!is.null(input$interactions) && input$interactions) {
      validate(need(
        (length(input_cov_var) > 0),
        "If interactions are selected at least one covariate should be specified."
      ))
    }

    if (!is.null(input$interactions) && input$interactions && length(interaction_var) > 0) {
      validate(need(
        (sum(sapply(at(), utils.nest::is_empty)) == 0),
        "Please specify all the interaction levels."
      ))
    }

    validate(need(utils.nest::is_character_single(input_aval_var), "Analysis variable should be a single column."))
    validate(need(utils.nest::is_character_single(input_cnsr_var), "Censor variable should be a single column."))

    # validate covariate has at least two levels
    validate(
      need(
        all(vapply(anl_m$data()[input_cov_var], FUN = function(x) {
          length(unique(x)) > 1
        }, logical(1))),
        "All covariate needs to have at least two levels"
      )
    )

    NULL
  })

  at <- reactive({
    anl_m <- anl_merged()
    input_cov_var <- as.vector(anl_m$columns_source$cov_var)
    anl <- datasets$get_data(dataname, filtered = FALSE)
    cov_is_numeric <- vapply(anl[input_cov_var], is.numeric, logical(1))
    interaction_var <- input_cov_var[cov_is_numeric]
    if (length(interaction_var) > 0 && length(input_cov_var) > 0) {
      res <- lapply(
        interaction_var,
        function(x) {
          cov <- input[[paste0("interact_", x)]]
          if (!is.null(cov)) {
            vec <- strsplit(cov, split = ",")
            as.numeric(unlist(vec))
          }
        }
      )
      stats::setNames(res, interaction_var)
    }
  })


  call_template <- function(comp_arm, anl, paramcd, multivariate, basic_table_args = NULL) {
    strata_var <- as.vector(anl$columns_source$strata_var)
    strata_var <- if (length(strata_var) != 0) strata_var else NULL
    cov_var <- as.vector(anl$columns_source$cov_var)
    cov_var <- if (length(cov_var) > 0) cov_var else NULL

    at <- if (!is.null(input$interactions) && input$interactions) at() else list()
    arm_var <- as.vector(anl$columns_source$arm_var)
    cnsr_var <- as.vector(anl$columns_source$cnsr_var)
    aval_var <- as.vector(anl$columns_source$aval_var)
    ref_arm <- input$ref_arm
    combine_comp_arms <- input$combine_comp_arms
    control <- control_coxreg(
      pval_method = input$pval_method,
      ties = input$ties,
      conf_level = as.numeric(input$conf_level),
      interaction = utils.nest::if_null(input$interactions, FALSE)
    )

    if (multivariate) {
      template_coxreg_m(
        dataname = "ANL",
        cov_var = cov_var,
        at = at,
        arm_var = arm_var,
        cnsr_var = cnsr_var,
        aval_var = aval_var,
        ref_arm = ref_arm,
        comp_arm = comp_arm,
        paramcd = paramcd,
        strata_var = strata_var,
        combine_comp_arms = combine_comp_arms,
        control = control,
        basic_table_args = basic_table_args
      )
    } else {
      template_coxreg_u(
        dataname = "ANL",
        cov_var = cov_var,
        at = at,
        arm_var = arm_var,
        cnsr_var = cnsr_var,
        aval_var = aval_var,
        ref_arm = ref_arm,
        comp_arm = comp_arm,
        paramcd = paramcd,
        strata_var = strata_var,
        combine_comp_arms = combine_comp_arms,
        control = control,
        append = TRUE,
        basic_table_args = basic_table_args
      )
    }
  }

  ## generate table call with template and render table ----
  table <- reactive({
    validate_checks()

    teal.devel::chunks_reset()
    anl_m <- anl_merged()
    teal.devel::chunks_push_data_merge(anl_m)
    teal.devel::chunks_push_new_line()

    ANL <- teal.devel::chunks_get_var("ANL") # nolint
    paramcd <- as.character(unique(ANL[[unlist(paramcd$filter)["vars_selected"]]]))
    multivariate <- input$type == "Multivariate"

    if (input$type == "Multivariate") {
      main_title <- paste0("Multi-Variable Cox Regression for ", paramcd)
      all_basic_table_args <- teal.devel::resolve_basic_table_args(
        user_table = basic_table_args,
        module_table = teal.devel::basic_table_args(title = main_title)
      )
      mapply(
        expr = call_template(input$comp_arm, anl_m, paramcd, multivariate, all_basic_table_args),
        teal.devel::chunks_push
      )
      teal.devel::chunks_safe_eval()
      teal.devel::chunks_get_var("result")
    } else {
      main_title <- paste0("Cox Regression for ", paramcd)
      all_basic_table_args <- teal.devel::resolve_basic_table_args(
        user_table = basic_table_args,
        module_table = teal.devel::basic_table_args(title = main_title)
      )
      teal.devel::chunks_push(quote(result <- list()))
      lapply(input$comp_arm, function(x) {
        mapply(expr = call_template(x, anl_m, paramcd, multivariate, NULL), teal.devel::chunks_push)
      })
      teal.devel::chunks_push(substitute(
        expr = {
          final_table <- rtables::rbindl_rtables(result, check_headers = TRUE)
          rtables::main_title(final_table) <- title
          rtables::main_footer(final_table) <- footer
          rtables::prov_footer(final_table) <- p_footer
          rtables::subtitles(final_table) <- subtitle
          final_table
        },
        env = list(
          title = all_basic_table_args$title,
          footer = `if`(is.null(all_basic_table_args$main_footer), "", all_basic_table_args$main_footer),
          p_footer = `if`(is.null(all_basic_table_args$prov_footer), "", all_basic_table_args$prov_footer),
          subtitle = `if`(is.null(all_basic_table_args$subtitles), "", all_basic_table_args$subtitles)
        )
      ))
      teal.devel::chunks_safe_eval()
      teal.devel::chunks_get_var("final_table")
    }
  })

  callModule(
    teal.devel::table_with_settings_srv,
    id = "table",
    table_r = table
  )

  callModule(
    module = teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(
      list(arm_var, paramcd, strata_var, aval_var, cnsr_var, cov_var)
    ),
    modal_title = "R Code for the Current (Multi-variable) Cox proportional hazard regression model",
    code_header = label
  )
}
