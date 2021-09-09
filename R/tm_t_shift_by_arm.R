#' Template: Shift by Arm
#'
#' @inheritParams template_arguments
#' @param visit (`character`)\cr variable designating the analysis visit.
#' @param paramcd (`character`)\cr variable designating the parameter code.
#' @param treatment_flag_var (`character`)\cr name of the on treatment flag variable.
#' @param treatment_flag (`character`)\cr name of the value indicating on treatment
#'   records in `treatment_flag_var`.
#' @param anrind_var (`character`)\cr the variable name for the analysis reference range indicator.
#' @param bnrind_var (`character`)\cr the variable name for the baseline reference range indicator.
#'
#' @seealso [tm_t_shift_by_arm()]
#'
template_shift_by_arm <- function(dataname,
                                  parentname,
                                  arm_var = "ARM",
                                  paramcd = "PARAMCD",
                                  visit = "AVISIT",
                                  treatment_flag_var = "ONTRTFL",
                                  treatment_flag = "Y",
                                  anrind_var = "ANRIND",
                                  bnrind_var = "BNRIND",
                                  na_level = "<Missing>") {

  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(visit),
    is.string(paramcd),
    is.string(anrind_var),
    is.string(bnrind_var)
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
        filter(treatment_flag_var == treatment_flag),
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
      expr = attr(dataname$bnrind_var, "label") <- "Baseline Reference Range Indicator",
      env = list(dataname = as.name(dataname), bnrind_var = bnrind_var)
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        mutate(col_label = visit),
      env = list(
        df = as.name(dataname),
        visit = as.name(visit)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Start layout steps.
  layout_list <- list()

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = basic_table() %>%
        split_cols_by("col_label", split_fun = drop_split_levels) %>% # temprary solution for over arching column
        split_cols_by(anrind_var) %>%
        split_rows_by(
          arm_var,
          split_fun = drop_split_levels,
          label_pos = "topleft",
          split_label = obj_label(dataname$arm_var)) %>%
        add_rowcounts() %>%
        summarize_vars(bnrind_var, denom = "N_row") %>%
        append_varlabels(dataname, bnrind_var, indent = 1L),
      env = list(
        anrind_var = anrind_var,
        arm_var = arm_var,
        bnrind_var = bnrind_var,
        dataname = as.name(dataname)
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- substitute(
    expr = {
      result <- build_table(lyt = lyt, df = anl)
      result
    }
  )

  y

}

#' Teal Module: Shift by Arm
#'
#' @inheritParams module_arguments
#' @inheritParams template_shift_by_arm
#' @param arm_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table. If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param treatment_flag_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr on treatment flag variable.
#' @param treatment_flag ([teal::choices_selected()] or [teal::data_extract_spec])\cr value indicating on treatment
#'   records in `treatment_flag_var`.
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
#'       anrind_var = choices_selected(
#'       variable_choices(adeg, subset = "ANRIND"), selected = "ANRIND", fixed = TRUE
#'       ),
#'       bnrind_var = choices_selected(
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
                              paramcd = choices_selected(
                                value_choices(dataname, "PARAMCD"), selected = "HR"
                              ),
                              visit = choices_selected(
                                value_choices(dataname, "AVISIT"), selected = "POST-BASELINE MINIMUM"
                              ),
                              treatment_flag_var = choices_selected(
                                variable_choices(dataname, subset = "ONTRTFL"), selected = "ONTRTFL", fixed = TRUE
                              ),
                              treatment_flag = choices_selected(
                                value_choices(dataname, "ONTRTFL"), selected = "Y", fixed = TRUE
                              ),
                              anrind_var = choices_selected(
                                variable_choices(dataname, subset = "ANRIND"), selected = "ANRIND", fixed = TRUE
                              ),
                              bnrind_var = choices_selected(
                                variable_choices(dataname, subset = "BNRIND"), selected = "BNRIND", fixed = TRUE
                              ),
                              na_level = "<Missing>",
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
    visit = cs_to_des_filter(visit, dataname = dataname),
    treatment_flag_var = cs_to_des_select(treatment_flag_var, dataname = dataname),
    anrind_var = cs_to_des_select(anrind_var, dataname = dataname),
    bnrind_var = cs_to_des_select(bnrind_var, dataname = dataname)
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
    a$visit,
    a$treatment_flag_var,
    a$treatment_flag,
    a$anrind_var,
    a$bnrind_var
  )

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c(
        "arm_var", "paramcd_var", "paramcd", "anrind_var", "bnrind_var", "visit_var", "visit", "treamtment_flag_var"
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
        id = ns("visit"),
        label = "Select Visit",
        data_extract_spec = a$visit,
        is_single_dataset = is_single_dataset_value
      ),
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
          ),
          data_extract_input(
            id = ns("anrind_var"),
            label = "Select Analysis Range Indicator Variable",
            data_extract_spec = a$anrind_var,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("bnrind_var"),
            label = "Select Baseline Reference Range Indicator Variable",
            data_extract_spec = a$bnrind_var,
            is_single_dataset = is_single_dataset_value
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
                             visit,
                             treatment_flag_var,
                             anrind_var,
                             bnrind_var,
                             label,
                             na_level) {

  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, paramcd, visit, anrind_var, bnrind_var, treatment_flag_var),
    input_id = c("arm_var", "paramcd", "visit", "anrind_var", "bnrind_var", "treatment_flag_var"),
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
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_anrind_var <- as.vector(anl_m$columns_source$input_anrind_var)
    input_bnrind_var <- as.vector(anl_m$columns_source$input_bnrind_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]
    input_visit <- unlist(visit$filter)["vars_selected"]
    input_treatment_flag_var <- as.vector(anl_m$columns_source$treatment_flag_var)

    validate(
      need(input_arm_var, "Please select a treatment variable"),
      need(input_paramcd, "Please select a endpoint parameter"),
      need(input_visit, "Please select an analysis visit"),
      need(input_treatment_flag_var, "Please select an on treatment flag variable."),
      need(input$treatment_flag, "Please select indicator value for on treatment records.")
    )

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_anrind_var, input_bnrind_var),
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
      anrind_var = as.vector(anl_m$columns_source$anrind_var),
      bnrind_var = as.vector(anl_m$columns_source$bnrind_var),
      na_level = na_level
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
    datanames = get_extract_datanames(list(arm_var, paramcd, visit, anrind_var, bnrind_var)),
    modal_title = "R Code for Shift Table by Arm",
    code_header = label
  )
}
