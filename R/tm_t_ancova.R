#' Template: ANCOVA summary
#'
#' Creates a valid expression for analysis of variance summary table.
#' @inheritParams template_arguments
#'
#' @seealso [tm_t_ancova()]
#'
template_ancova <- function(dataname = "ANL",
                            parentname = "ADSL_FILTERED",
                            arm_var,
                            ref_arm = NULL,
                            comp_arm = NULL,
                            combine_comp_arms = FALSE,
                            aval_var,
                            cov_var,
                            paramcd_levels = "EXAMPLE",
                            paramcd_var = "PARAMCD",
                            visit_var = "AVISIT",
                            conf_level = 0.95
) {

  assert_that(
    is.string(dataname),
    is.string(parentname),
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
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val,
      drop = FALSE
    )
  )
  anl_list <- add_expr(anl_list, quote(droplevels()))

  parent_list <- add_expr(
    parent_list,
    prepare_arm(
      dataname = parentname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val,
      drop = FALSE
    )
  )
  parent_list <- add_expr(parent_list, quote(droplevels()))

  if (combine_comp_arms) {
    anl_list <- add_expr(
      anl_list,
      substitute_names(
        expr = mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
    parent_list <- add_expr(
      parent_list,
      substitute_names(
        expr = mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
  }

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- anl_list,
      env = list(
        anl = as.name(dataname),
        anl_list = pipe_expr(anl_list)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parent <- parent_list,
      env = list(
        parent = as.name(parentname),
        parent_list = pipe_expr(parent_list)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Build layout.
  y$layout_prep <- quote(split_fun <- drop_split_levels)

  layout_list <- list()

  layout_list <- add_expr(layout_list, quote(basic_table()))

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(var = arm_var, ref_group = ref_group) %>%
        add_colcounts() %>%
        split_rows_by(visit_var, split_fun = split_fun) %>%
        append_varlabels(dataname, visit_var),
      env = list(
        arm_var = arm_var,
        ref_group = paste(ref_arm, collapse = "/"),
        visit_var = visit_var,
        dataname = as.name(dataname)
      )
    )
  )

  if (length(paramcd_levels) > 1) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by(paramcd_var, split_fun = split_fun) %>%
          append_varlabels(dataname, paramcd_var, indent = TRUE) %>%
          summarize_ancova(
            vars = aval_var,
            variables = list(arm = arm_var, covariates = cov_var),
            conf_level = conf_level,
            var_labels = "Adjusted mean",
            show_labels = "hidden"
          ),
        env = list(
          paramcd_var = paramcd_var,
          aval_var = aval_var,
          arm_var = arm_var,
          cov_var = cov_var,
          conf_level = conf_level,
          dataname = as.name(dataname)
        )
      )
    )
  } else {
    # Only one entry in `paramcd_levels` here.
    layout_list <- add_expr(
      layout_list,
      substitute(
        append_topleft(paste0("  ", paramcd_levels)) %>%
        summarize_ancova(
          vars = aval_var,
          variables = list(arm = arm_var, covariates = NULL),
          conf_level = conf_level,
          var_labels = "Unadjusted comparison",
          .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
          table_names = "unadjusted_comparison"
        ) %>%
          summarize_ancova(
            vars = aval_var,
            variables = list(arm = arm_var, covariates = cov_var),
            conf_level = conf_level,
            var_labels = paste0(
              "Adjusted comparison (", paste(cov_var, collapse = " + "), ")"
            ),
            table_names = "adjusted_comparison"
          ),
        env = list(
          paramcd_levels = paramcd_levels,
          aval_var = aval_var,
          arm_var = arm_var,
          cov_var = cov_var,
          conf_level = conf_level,
          dataname = as.name(dataname)
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Build table.
  y$table <- substitute(
    expr = {
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      result
    },
    env = list(
      anl = as.name(dataname),
      parent = as.name(parentname)
    )
  )

  y
}

#' Teal Module: ANCOVA Teal Module
#'
#' @inheritParams module_arguments
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
                        parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                        arm_var,
                        arm_ref_comp = NULL,
                        aval_var,
                        cov_var,
                        avisit,
                        paramcd,
                        conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                        pre_output = NULL,
                        post_output = NULL
) {

  stop_if_not(
    is_character_single(dataname),
    is_character_single(parentname),
    is.choices_selected(conf_level),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
      ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
      )
    )

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE),
    avisit = cs_to_des_filter(avisit, dataname = dataname, multiple = TRUE, include_vars = TRUE),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE)
  )

  module(
    label = label,
    ui = ui_ancova,
    ui_args = c(data_extract_list, args),
    server = srv_ancova,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_ancova <- function(id, ...) {

  a <- list(...)
  is_single_dataset_value <- is_single_dataset(
    a$arm_var, a$aval_var, a$cov_var, a$avisit, a$paramcd
  )

  ns <- NS(id)

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "aval_var", "cov_var", "avisit", "paramcd")]),
      data_extract_input(
        id = ns("avisit"),
        label = "Analysis Visit",
        data_extract_spec = a$avisit,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
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
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      optionalSelectInput(
        inputId = ns("conf_level"),
        label = HTML(paste("Confidence Level")),
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_ancova <- function(input,
                       output,
                       session,
                       datasets,
                       dataname,
                       parentname,
                       arm_var,
                       arm_ref_comp,
                       aval_var,
                       cov_var,
                       paramcd,
                       avisit,
                       label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel.
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    dataname = parentname,
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

    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_cov_var <- as.vector(anl_m$columns_source$cov_var)
    input_avisit <- unlist(avisit$filter)["vars"]
    input_paramcd <- unlist(paramcd$filter)["vars"]

    # Validate inputs.
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_avisit, input_aval_var, input_cov_var),
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

    validate(need(
      input$conf_level >= 0 && input$conf_level <= 1,
      "Please choose a confidence level between 0 and 1"
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

    paramcd_levels <- unique(ANL[[unlist(paramcd$filter)["vars"]]])

    my_calls <- template_ancova(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      combine_comp_arms = input$combine_comp_arms,
      aval_var = as.vector(anl_m$columns_source$aval_var),
      cov_var = as.vector(anl_m$columns_source$cov_var),
      paramcd_levels = paramcd_levels,
      paramcd_var = unlist(paramcd$filter)["vars"],
      visit_var = unlist(avisit$filter)["vars"],
      conf_level = as.numeric(input$conf_level)
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Output to render.
  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  callModule(
    table_with_settings_srv,
    id = "table",
    table_r = table
  )

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
