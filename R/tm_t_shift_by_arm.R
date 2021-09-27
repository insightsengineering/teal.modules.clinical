#' Template: Shift by Arm
#'
#' @inheritParams template_arguments
#' @param aval_var (`character`)\cr the variable name for the analysis reference range indicator.
#' @param base_var (`character`)\cr the variable name for the baseline reference range indicator.
#' @param add_total (`logical`)\cr
#'   whether to include row with total number of patients.
#'
#' @seealso [tm_t_shift_by_arm()]
#'
template_shift_by_arm <- function(dataname,
                                  parentname,
                                  arm_var = "ARM",
                                  paramcd = "PARAMCD",
                                  visit_var = "AVISIT",
                                  treatment_flag_var = "ONTRTFL",
                                  treatment_flag = "Y",
                                  aval_var = "ANRIND",
                                  base_var = "BNRIND",
                                  na_level = "<Missing>",
                                  add_total = FALSE) {

  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(visit_var),
    is.string(paramcd),
    is.string(aval_var),
    is.string(base_var),
    is.string(na_level),
    is.string(treatment_flag_var),
    is.string(treatment_flag),
    is.flag(add_total)
  )

  y <- list()

  # Start data steps.
  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- df_explicit_na(parentname, na_level = na_level),
      env = list(parentname = as.name(parentname), na_level = na_level)
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      expr = dataname <- df_explicit_na(dataname, na_level = na_level) %>%
        dplyr::filter(treatment_flag_var == treatment_flag),
      env = list(
        dataname = as.name(dataname),
        na_level = na_level,
        treatment_flag_var = as.name(treatment_flag_var),
        treatment_flag = treatment_flag
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = attr(dataname$base_var, "label") <- "Baseline Assessment",
      env = list(dataname = as.name(dataname), base_var = base_var)
    )
  )

  y$data <- bracket_expr(data_list)

  # Start layout steps.
  layout_list <- list()

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = basic_table() %>%
          split_cols_by(visit_var, split_fun = drop_split_levels) %>% # temprary solution for over arching column
          split_cols_by(aval_var) %>%
          split_rows_by(
            arm_var,
            split_fun = add_overall_level("All Patients", first = FALSE),
            label_pos = "topleft",
            split_label = obj_label(dataname$arm_var)) %>%
          add_rowcounts() %>%
          summarize_vars(base_var, denom = "N_row", na_level = na_level, na.rm = FALSE, .stats = "count_fraction") %>%
          append_varlabels(dataname, base_var, indent = 1L),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          base_var = base_var,
          dataname = as.name(dataname),
          visit_var = visit_var,
          na_level = na_level
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = basic_table() %>%
          split_cols_by(visit_var, split_fun = drop_split_levels) %>% # temprary solution for over arching column
          split_cols_by(aval_var) %>%
          split_rows_by(
            arm_var,
            split_fun = drop_split_levels,
            label_pos = "topleft",
            split_label = obj_label(dataname$arm_var)) %>%
          add_rowcounts() %>%
          summarize_vars(base_var, denom = "N_row", na_level = na_level, na.rm = FALSE, .stats = "count_fraction") %>%
          append_varlabels(dataname, base_var, indent = 1L),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          base_var = base_var,
          dataname = as.name(dataname),
          visit_var = visit_var,
          na_level = na_level
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- substitute(
    expr = {
      result <- build_table(lyt = lyt, df = dataname)
      result
    },
    env = list(dataname = as.name(dataname))
  )

  y

}

#' Teal Module: Shift by Arm
#'
#' @inheritParams module_arguments
#' @inheritParams template_shift_by_arm
#' @param add_total (`logical`)\cr
#'   whether to include row with total number of patients.
#'
#' @export
#' @examples
#'
#' library(dplyr)
#' library(tern)
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adeg <- synthetic_cdisc_data("latest")$adeg
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADEG", adeg, code = 'ADEG <- synthetic_cdisc_data("latest")$adeg'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_shift_by_arm(
#'       label = "Shift by Arm Table",
#'       dataname = "ADEG",
#'       arm_var = choices_selected(
#'         variable_choices(adsl, subset = c("ARM", "ARMCD")), selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'       value_choices(adeg, "PARAMCD"), selected = "HR"
#'       ),
#'       visit_var = choices_selected(
#'       value_choices(adeg, "AVISIT"), selected = "POST-BASELINE MINIMUM"
#'       ),
#'       aval_var = choices_selected(
#'       variable_choices(adeg, subset = "ANRIND"), selected = "ANRIND", fixed = TRUE
#'       ),
#'       base_var = choices_selected(
#'         variable_choices(adeg, subset = "BNRIND"), selected = "BNRIND", fixed = TRUE
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_shift_by_arm <- function(label,
                              dataname,
                              parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                              arm_var,
                              paramcd,
                              visit_var,
                              aval_var,
                              base_var,
                              treatment_flag_var = choices_selected(
                                variable_choices(dataname, subset = "ONTRTFL"), selected = "ONTRTFL", fixed = TRUE
                              ),
                              treatment_flag = choices_selected(
                                value_choices(dataname, "ONTRTFL"), selected = "Y", fixed = TRUE
                              ),
                              na_level = "<Missing>",
                              add_total = FALSE,
                              pre_output = NULL,
                              post_output = NULL) {

  stop_if_not(
    is_character_single(dataname),
    is_character_single(parentname),
    is.choices_selected(treatment_flag),
    is.choices_selected(treatment_flag_var),
    is_character_single(na_level),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    visit_var = cs_to_des_filter(visit_var, dataname = dataname),
    treatment_flag_var = cs_to_des_select(treatment_flag_var, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    base_var = cs_to_des_select(base_var, dataname = dataname)
  )

  args <- as.list(environment())

  module(
    label = label,
    server = srv_shift_by_arm,
    ui = ui_shift_by_arm,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        na_level = na_level
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )

}

#' @noRd
ui_shift_by_arm <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  is_single_dataset_value <- is_single_dataset(
    a$id_var,
    a$arm_var,
    a$paramcd,
    a$visit_var,
    a$treatment_flag_var,
    a$treatment_flag,
    a$aval_var,
    a$base_var
  )

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c(
        "arm_var", "paramcd_var", "paramcd", "aval_var", "base_var", "visit_var", "treamtment_flag_var"
      )]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("visit_var"),
        label = "Select Visit",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval_var"),
        label = "Select Analysis Range Indicator Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("base_var"),
        label = "Select Baseline Reference Range Indicator Variable",
        data_extract_spec = a$base_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      panel_group(
        panel_item(
          "Additional Variables Info",
          data_extract_input(
            id = ns("treatment_flag_var"),
            label = "On Treatment Flag Variable",
            data_extract_spec = a$treatment_flag_var,
            is_single_dataset = is_single_dataset_value
          ),
          optionalSelectInput(
            ns("treatment_flag"),
            "Value Indicating On Treatment",
            a$treatment_flag$choices,
            a$treatment_flag$selected,
            multiple = FALSE,
            fixed = a$treatment_flag$fixed
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_shift_by_arm <- function(input,
                             output,
                             session,
                             datasets,
                             dataname,
                             parentname,
                             arm_var,
                             paramcd,
                             visit_var,
                             treatment_flag_var,
                             aval_var,
                             base_var,
                             label,
                             na_level,
                             add_total) {

  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, paramcd, visit_var, aval_var, base_var, treatment_flag_var),
    input_id = c("arm_var", "paramcd", "visit_var", "aval_var", "base_var", "treatment_flag_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  # validate inputs
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    anl_m_rowcount <- NROW(anl_m$data())
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_base_var <- as.vector(anl_m$columns_source$base_var)
    input_treatment_flag_var <- as.vector(anl_m$columns_source$treatment_flag_var)

    validate(
      need(input_arm_var, "Please select a treatment variable"),
      need(
        anl_m_rowcount > 0,
        paste0(
          "Please make sure the analysis dataset is not empty or\n",
          "endpoint parameter and analysis visit are selected."
          )
        ),
      need(input_treatment_flag_var, "Please select an on treatment flag variable."),
      need(input$treatment_flag, "Please select indicator value for on treatment records.")
    )

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_aval_var, input_base_var),
      arm_var = input_arm_var
    )
  })

  # generate r code for the analysis
  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    my_calls <- template_shift_by_arm(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      paramcd = unlist(paramcd$filter)["vars_selected"],
      treatment_flag_var = as.vector(anl_m$columns_source$treatment_flag_var),
      treatment_flag = input$treatment_flag,
      aval_var = as.vector(anl_m$columns_source$aval_var),
      base_var = as.vector(anl_m$columns_source$base_var),
      na_level = na_level,
      add_total = input$add_total
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("result")
  })

  callModule(
    table_with_settings_srv,
    id = "table",
    table_r = table
  )

  # Render R code.
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(arm_var, paramcd, visit_var, aval_var, base_var)),
    modal_title = "R Code for Shift Table by Arm",
    code_header = label
  )
}
