#' ANCOVA Teal Module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @inheritParams tm_t_tte
#' @name tm_t_ancova
#'
NULL

#' Template for ANCOVA summary
#'
#' Creates a valid expression for analysis of variance summary table.
#'
template_ancova <- function(anl_name = "ANL",
                            parent_name = "ADSL_FILTERED",
                            arm_var,
                            ref_arm = NULL,
                            comp_arm = NULL,
                            combine_comp_arms = FALSE,
                            aval_var,
                            cov_var,
                            paramcd,
                            conf_level = 0.95
) {

  assert_that(
    is.string(anl_name),
    is.string(parent_name),
    is.string(arm_var),
    is.flag(combine_comp_arms),
    is.string(aval_var),
    is.character(cov_var)
  )

  y <- list()

  # Data processing.
  data_list <- list()
  anl_list <- list()
  parent_list <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  anl_list <- add_expr(
    anl_list,
    substitute(
      expr = anl %>%
        filter(arm_var %in% arm_vals) %>%
        droplevels(),
      env = list(
        anl = as.name(anl_name),
        arm_var = as.name(arm_var),
        arm_vals = c(ref_arm, comp_arm)
      )
    )
  )

  parent_list <- add_expr(
    parent_list,
    substitute(
      parent %>%
        filter(arm_var %in% arm_vals) %>%
        droplevels(),
      env = list(
        parent = as.name(parent_name),
        arm_var = as.name(arm_var),
        arm_vals = c(ref_arm, comp_arm)
      )
    )
  )

  if (length(ref_arm) > 1) {
    anl_list <- add_expr(
      anl_list,
      substitute_names(
        expr = mutate(
          arm_var = combine_levels(arm_var, levels = ref_arm, new_level = ref_arm_val)
        ),
        names = list(
          arm_var = as.name(arm_var)
        ),
        others = list(
          ref_arm = ref_arm,
          ref_arm_val = ref_arm_val
        )
      )
    )
    parent_list <- add_expr(
      parent_list,
      substitute_names(
        expr = mutate(
          arm_var = combine_levels(arm_var, levels = ref_arm, new_level = ref_arm_val)
        ),
        names = list(
          arm_var = as.name(arm_var)
        ),
        others = list(
          ref_arm = ref_arm,
          ref_arm_val = ref_arm_val
        )
      )
    )
  } else {
    anl_list <- add_expr(
      anl_list,
      substitute_names(
        expr = mutate(
          arm_var = relevel(arm_var, ref = ref_arm)
        ),
        names = list(
          arm_var = as.name(arm_var)
        ),
        others = list(
          ref_arm = ref_arm,
          comp_arm = comp_arm
        )
      )
    )
    parent_list <- add_expr(
      parent_list,
      substitute_names(
        expr = mutate(
          arm_var = relevel(arm_var, ref = ref_arm)
        ),
        names = list(
          arm_var = as.name(arm_var)
        ),
        others = list(
          ref_arm = ref_arm,
          comp_arm = comp_arm
        )
      )
    )
  }

  if (combine_comp_arms) {
    anl_list <- add_expr(
      anl_list,
      substitute_names(
        expr = mutate(
          arm_var = combine_levels(arm_var, levels = comp_arm)
        ),
        names = list(
          arm_var = as.name(arm_var)
        ),
        others = list(
          comp_arm = comp_arm
        )
      )
    )
    parent_list <- add_expr(
      parent_list,
      substitute_names(
        expr = mutate(
          arm_var = combine_levels(arm_var, levels = comp_arm)
        ),
        names = list(
          arm_var = as.name(arm_var)
        ),
        others = list(
          comp_arm = comp_arm
        )
      )
    )
  }

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- anl_list,
      env = list(
        anl = as.name(anl_name),
        anl_list = pipe_expr(anl_list)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parent <- parent_list,
      env = list(
        parent = as.name(parent_name),
        parent_list = pipe_expr(parent_list)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Build layout.
  layout_list <- list()

  layout_list <- add_expr(layout_list, quote(basic_table()))

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(var = arm_var, ref_group = ref_group) %>%
        split_rows_by("AVISIT", split_fun = drop_split_levels),
      env = list(
        arm_var = arm_var,
        ref_group = paste(ref_arm, collapse = "/")
      )
    )
  )

  if (length(unique(paramcd)) > 1) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
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
    expr = table(parent$arm_var),
    env = list(parent = as.name(parent_name), arm_var = arm_var)
  )

  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl, col_counts = col_counts),
    env = list(
      anl = as.name(anl_name),
      col_counts = col_counts
    )
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
#' similar to STREAM template `aovt01` when multiple endpoints are selected.
#' When a single endpoint is selected, both unadjusted and adjusted comparison
#' would be provided. This modules expects that the analysis data has the
#' following variables:
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
#' arm_ref_comp = list(
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   ),
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
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
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, c("ARM", "ACTARMCD")),
#'         selected = "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       aval_var = choices_selected(
#'         choices = variable_choices(adqs, c("CHG", "AVAL")),
#'         selected = "CHG"
#'       ),
#'       cov_var = choices_selected(
#'         choices = variable_choices(adqs, c("BASE", "STRATA1", "SEX")),
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
                        parent_name = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
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
  stopifnot(is.cs_or_des(arm_var))
  stopifnot(is.cs_or_des(aval_var))
  stopifnot(is.cs_or_des(cov_var))
  stopifnot(is.cs_or_des(avisit))
  stopifnot(is.cs_or_des(paramcd))

  # Convert choices-selected to data_extract_spec.
  if (is.choices_selected(arm_var)) {
    arm_var <- cs_to_des_select(arm_var, dataname = parent_name, multiple = FALSE)
  }
  if (is.choices_selected(aval_var)) {
    aval_var <- cs_to_des_select(aval_var, dataname = dataname, multiple = FALSE)
  }
  if (is.choices_selected(cov_var)) {
    cov_var <- cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE)
  }
  if (is.choices_selected(avisit)) {
    avisit <- cs_to_des_filter(avisit, dataname = dataname, multiple = TRUE)
  }
  if (is.choices_selected(paramcd)) {
    paramcd <- cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE)
  }

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = arm_var,
    aval_var = aval_var,
    cov_var = cov_var,
    avisit = avisit,
    paramcd = paramcd
  )

  module(
    label = label,
    ui = ui_ancova,
    ui_args = args,
    server = srv_ancova,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parent_name = parent_name,
        arm_ref_comp = arm_ref_comp,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @import teal.devel
#'
ui_ancova <- function(id, ...) {

  args <- list(...)
  is_single_dataset_value <- is_single_dataset(
    args$arm_var, args$aval_var, args$cov_var, args$avisit, args$paramcd
  )

  ns <- NS(id)

  standard_layout(
    output = white_small_well(uiOutput(ns("as_html"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("arm_var", "aval_var", "cov_var", "avisit", "paramcd")]),
      data_extract_input(
        id = ns("avisit"),
        label = "Analysis Visit",
        data_extract_spec = args$avisit,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = args$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = args$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = args$arm_var,
        is_single_dataset = is_single_dataset_value
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
      data_extract_input(
        id = ns("cov_var"),
        label = "Covariates",
        data_extract_spec = args$cov_var,
        is_single_dataset = is_single_dataset_value
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
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @import teal.devel
#' @importFrom rtables as_html
#'
srv_ancova <- function(input,
                       output,
                       session,
                       datasets,
                       dataname,
                       parent_name,
                       arm_var,
                       arm_ref_comp,
                       aval_var,
                       cov_var,
                       paramcd,
                       avisit,
                       label) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel.
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parent_name),
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_ancova"
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, aval_var, cov_var, avisit, paramcd),
    input_id = c("arm_var", "aval_var", "cov_var", "avisit", "paramcd"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = "arm_var",
    anl_name = "ANL_ADSL"
  )

  # Prepare the analysis environment (filter data, check data, populate envir).
  validate_checks <- reactive({

    adsl_filtered <- datasets$get_data(parent_name, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_cov_var <- as.vector(anl_m$columns_source$cov_var)

    # Validate PARAMCD and AVISIT input values.
    input_names <- names(input)
    input_avisit <- input_names[grepl("avisit", input_names) & grepl("filter", input_names)]
    input_paramcd <- input_names[grepl("paramcd", input_names) & grepl("filter", input_names)]
    validate(need(
      all(input[[input_avisit]] %in% unique(anl_filtered[["AVISIT"]])),
      "Analysis visit value does not exists in AVISIT variable in analysis data."
    ))
    validate(need(
      all(input[[input_paramcd]] %in% unique(anl_filtered[["PARAMCD"]])),
      "Endpoint value does not exists in PARAMCD variable in analysis data."
    ))

    # Validate inputs.
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "AVISIT", "PARAMCD", input_aval_var, input_cov_var),
      arm_var = input_arm_var
    )
    validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
    do.call(what = "validate_standard_inputs", validate_args)

    # Other validations.
    validate(need(
      !is_empty(input_aval_var),
      "Analysis variable cannot be empty."
    ))
    validate(need(
      length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) > 1,
      "ANCOVA table needs at least 2 arm groups to make comparisons."
    ))

  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 10)

    my_calls <- template_ancova(
      parent_name = "ANL_ADSL",
      anl_name = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      combine_comp_arms = input$combine_comp_arms,
      aval_var = as.vector(anl_m$columns_source$aval_var),
      cov_var = as.vector(anl_m$columns_source$cov_var),
      paramcd = unique(ANL[["PARAMCD"]]),
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
  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm_var, aval_var, cov_var, avisit, paramcd)
    ),
    modal_title = label
  )
}
