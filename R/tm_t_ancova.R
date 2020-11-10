#' ANCOVA Teal Module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @name tm_t_ancova
#'
NULL

#' Template for ANCOVA summary
#'
#' Creates a valid expression for analysis of variance summary table.
#'
template_ancova <- function(parentname, # nousage
                            dataname,
                            arm_var,
                            ref_arm,
                            cov_var,
                            aval_var,
                            avisit,
                            paramcd,
                            conf_level = 0.95
) {

  y <- list()

  # Data processing.
  y$data <- substitute(
    expr = anl <- df %>%
      filter(AVISIT %in% avisit & PARAMCD %in% paramcd) %>%
      droplevels(),
    env = list(
      df = as.name(dataname),
      avisit = avisit,
      paramcd = paramcd
    )
  )

  # Build layout.
  layout_list <- list()

  layout_list <- add_expr(layout_list, substitute(basic_table()))

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(var = arm_var, ref_group = ref_arm),
      env = list(
        arm_var = arm_var,
        ref_arm = ref_arm
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_rows_by("AVISIT", split_fun = drop_split_levels)
    )
  )

  if (length(unique(paramcd)) > 1) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by("PARAMCD") %>%
          summarize_ancova(
            vars = aval_var,
            variables = list(arm = arm_var, covariates = cov_var),
            conf_level = conf_level,
            var_labels = "Adjusted mean",
            show_labels = "hidden"
          ),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          cov_var = cov_var,
          conf_level = conf_level
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        summarize_ancova(
          vars = aval_var,
          variables = list(arm = arm_var, covariates = NULL),
          conf_level = conf_level,
          var_labels = "Unadjusted comparison",
          .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means")
        ) %>%
        summarize_ancova(
          vars = aval_var,
          variables = list(arm = arm_var, covariates = cov_var),
          conf_level = conf_level,
          var_labels = paste0(
            "Adjusted comparison (", paste(cov_var, collapse = " + "), ")"
          )
        ),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          cov_var = cov_var,
          conf_level = conf_level
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Build table.
  col_counts <- substitute(
    expr = table(droplevels(parentname$arm_var)),
    env = list(parentname = as.name(parentname), arm_var = arm_var)
  )
  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl, col_counts = col_counts),
    env = list(col_counts = col_counts)
  )

  y
}

#' @describeIn tm_t_ancova Teal module for analysis of variance
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @inheritParams tm_t_tte
#'
#' @details This module produces an analysis of variance summary table that is
#' similar to STREAM template `aovt01`. This modules expects that the analysis
#' data has the following variables:
#'
#' \tabular{ll}{
#'  `AVISIT` \tab variable used to filter for analysis visits.\cr
#'  `PARAMCD` \tab variable used to filter for endpoints, after filtering for
#'  `paramcd` and `avisit`, one observation per patient is expected for the analysis
#'  to be meaningful.
#' }
#'
#' @export
#' @import magrittr
#'
#' @examples
#'
#' # Preparation of the test case.
#' library(dplyr)
#' library(random.cdisc.data)
#' adsl <- radsl(cached = TRUE)
#' adqs <- radqs(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADQS", adqs),
#'     code =
#'       "ADSL <- radsl(cached = TRUE)
#'        ADQS <- radqs(cached = TRUE)"
#'   ),
#'   modules = root_modules(
#'     tm_t_ancova(
#'       label = "ANCOVA table",
#'       dataname = "ADQS",
#'       avisit = choices_selected(
#'         choices = value_choices(adqs, "AVISIT"),
#'         selected = "WEEK 1 DAY 8"
#'       ),
#'       aval_var = choices_selected(c("CHG", "AVAL"), "CHG"),
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARMCD"),
#'       cov_var = choices_selected(
#'         choices = variable_choices(adqs, subset = c("BASE", "STRATA1", "SEX")),
#'         selected = "STRATA1"
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(adqs, "PARAMCD", "PARAM"),
#'         selected = "FKSI-FWB"
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_ancova <- function(label,
                        dataname,
                        arm_var,
                        arm_ref_comp = NULL,
                        aval_var,
                        cov_var,
                        avisit,
                        paramcd,
                        pre_output = NULL,
                        post_output = NULL
) {

  stopifnot(length(dataname) == 1)
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(aval_var))
  stopifnot(is.choices_selected(cov_var))
  stopifnot(is.choices_selected(avisit))
  stopifnot(is.choices_selected(paramcd))

  args <- c(as.list(environment()))

  module(
    label = label,
    ui = ui_ancova,
    ui_args = args,
    server = srv_ancova,
    server_args = args,
    filters = dataname
  )
}

#' @noRd
ui_ancova <- function(id,
                      datasets,
                      dataname,
                      ...) {
  args <- list(...)
  ns <- NS(id)

  standard_layout(
    output = white_small_well(uiOutput(ns("as_html"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(
        ns("avisit"), "Analysis Visit",
        choices = args$avisit$choices,
        selected = args$avisit$selected,
        multiple = TRUE,
        fixed = args$avisit$fixed
      ),
      optionalSelectInput(
        ns("paramcd"), "Select Endpoint",
        choices = args$paramcd$choices,
        selected = args$paramcd$selected,
        multiple = TRUE,
        fixed = args$paramcd$fixed
      ),
      optionalSelectInput(
        ns("aval_var"), "Analysis Variable",
        choices = args$aval_var$choices,
        selected = args$aval_var$selected,
        multiple = FALSE,
        fixed = args$aval_var$fixed
      ),
      optionalSelectInput(
        ns("arm_var"), "Arm Variable",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected,
        multiple = FALSE,
        fixed = args$arm_var$fixed
      ),
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
        ns("cov_var"), "Covariates",
        choices = args$cov_var$choices,
        selected = args$cov_var$selected,
        multiple = TRUE,
        fixed = args$cov_var$fixed
      ),
      numericInput(
        inputId = ns("conf_level"),
        label = HTML(paste("Confidence Level")),
        value = 0.95,
        min = 0.01,
        max = 0.99,
        step = 0.01,
        width = "100%"
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @noRd
srv_ancova <- function(input,
                       output,
                       session,
                       datasets,
                       dataname,
                       arm_ref_comp,
                       label,
                       ...) {

  init_chunks()

  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_ancova"
  )

  # Prepare the analysis environment (filter data, check data, populate envir).
  prepared_env <- reactive({

    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    # Validate inputs.
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input$arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", input$aval_var, input$cov_var),
      arm_var = input$arm_var
    )

    # Validate arm levels - comparison is needed for this module.
    validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
    do.call(what = "validate_standard_inputs", validate_args)

    # Send data where the analysis lives.
    anl_name <- paste0(dataname, "_FILTERED")

    e <- new.env()
    e$ADSL_FILTERED <- adsl_filtered # nolint
    assign(anl_name, anl_filtered, envir = e)
    e
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_ancova(
      parentname = "ADSL_FILTERED",
      dataname = paste0(dataname, "_FILTERED"),
      arm_var = input$arm_var,
      ref_arm = input$ref_arm,
      cov_var = input$cov_var,
      aval_var = input$aval_var,
      avisit = input$avisit,
      paramcd = input$paramcd,
      conf_level = as.numeric(input$conf_level)
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Output to render.
  output$as_html <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  # Render R code.
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "ANCOVA",
      rcode = get_rcode(datasets = datasets, title = label)
    )
  })
}
