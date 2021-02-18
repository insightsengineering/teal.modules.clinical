#' Template: Abnormality Summary Table
#'
#' @inheritParams template_arguments
#' @param exclude_base_abn (`logical`)\cr whether to exclude patients who had abnormal values at baseline.
#'
#' @seealso [tm_t_abnormality()]
#'
template_abnormality <- function(parentname,
                                 dataname,
                                 arm_var,
                                 by_vars,
                                 abnormal,
                                 grade = "ANRIND",
                                 baseline_var = "BNRIND",
                                 treatment_flag_var = "ONTRTFL",
                                 treatment_flag = "Y",
                                 add_total = FALSE,
                                 exclude_base_abn = FALSE) {
  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        filter(treatment_flag_var == treatment_flag & !is.na(grade)),
      env = list(
        df = as.name(dataname),
        grade = as.name(grade),
        treatment_flag_var = as.name(treatment_flag_var),
        treatment_flag = treatment_flag
      )
    )
  )

  y$data <- bracket_expr(data_list)

  y$layout_prep <- quote(split_fun <- drop_split_levels)
  layout_list <- list()

  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = basic_table() %>%
          split_cols_by(
            var = arm_var,
            split_fun = add_overall_level("All Patients", first = FALSE)
          ) %>%
          add_colcounts(),
        env = list(arm_var = arm_var)
      )
    } else {
      substitute(
        expr = basic_table() %>%
          split_cols_by(var = arm_var) %>%
          add_colcounts(),
        env = list(arm_var = arm_var)
      )
    }
  )

  for (by_var in by_vars) {
    split_label <- substitute(
      expr = var_labels(dataname)[[by_var]],
      env = list(
        dataname = as.name(dataname),
        by_var = by_var
      )
    )
    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by(
          by_var,
          split_label = split_label,
          visible_label = TRUE,
          split_fun = split_fun
        ),
        env = list(
          by_var = by_var,
          split_label = split_label
        )
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = count_abnormal(
        var = grade,
        abnormal = abnormal,
        variables = list(id = usubjid, baseline = baseline_var),
        exclude_base_abn = exclude_base_abn
      ) %>%
        append_varlabels(dataname, grade),
      env = list(
        grade = grade,
        abnormal = setNames(abnormal, tolower(abnormal)),
        usubjid = "USUBJID",
        baseline_var = baseline_var,
        exclude_base_abn = exclude_base_abn,
        dataname = as.name(dataname)
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent) %>%
        prune_table()
      result
    },
    env = list(parent = as.name(parentname))
  )

  y
}


#' Teal Module: Abnormality Summary Table
#'
#' @inheritParams module_arguments
#' @inheritParams template_abnormality
#' @param grade ([teal::choices_selected()] or [teal::data_extract_spec])\cr object with all available
#'   choices and preselected option for variable names that can be used to
#'   specify the abnormality grade. Variable must be factor.
#' @param abnormal ([teal::choices_selected()] or [teal::data_extract_spec])\cr indicating abnormality grade.
#' @param baseline_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#'   variable for baseline abnormality grade.
#' @param treatment_flag_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr on treatment flag variable.
#' @param treatment_flag ([teal::choices_selected()] or [teal::data_extract_spec])\cr value indicating on treatment
#'   records in `treatment_flag_var`.
#'
#' @note Patients with the same abnormality at baseline as on the treatment visit can be
#'   excluded in accordance with GDSR specifications by using `exclude_base_abn`.
#'
#' @export
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' adsl <- radsl(cached = TRUE)
#' adlb <- radlb(cached = TRUE) %>%
#'   mutate(
#'     ONTRTFL = case_when(
#'       AVISIT %in% c("SCREENING", "BASELINE") ~ "",
#'       TRUE ~ "Y"
#'     )
#'   ) %>%
#'   var_relabel(
#'     ONTRTFL = "On Treatment Record Flag"
#'   )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADLB", adlb,
#'       code = 'ADLB <- radlb(cached = TRUE) %>%
#'                 mutate(
#'                   ONTRTFL = case_when(
#'                     AVISIT %in% c("SCREENING", "BASELINE") ~ "",
#'                     TRUE ~ "Y"
#'                   )
#'                 ) %>%
#'                 var_relabel(
#'                   ONTRTFL = "On Treatment Record Flag"
#'                 )'
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_abnormality(
#'       label = "Abnormality Table",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       by_vars = choices_selected(
#'         choices = variable_choices(adlb, subset = c("LBCAT", "PARAM", "AVISIT")),
#'         selected = c("LBCAT", "PARAM"),
#'         keep_order = TRUE
#'       ),
#'       grade = choices_selected(
#'         choices = variable_choices(adlb, subset = "ANRIND"),
#'         selected = "ANRIND",
#'         fixed = TRUE
#'       ),
#'       abnormal = c("LOW", "HIGH"),
#'       exclude_base_abn = FALSE
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_abnormality <- function(label,
                             dataname,
                             parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                             arm_var,
                             by_vars,
                             grade,
                             abnormal,
                             id_var = choices_selected(
                               variable_choices(dataname, subset = "USUBJID"), selected = "USUBJID", fixed = TRUE
                             ),
                             baseline_var = choices_selected(
                               variable_choices(dataname, subset = "BNRIND"), selected = "BNRIND", fixed = TRUE
                             ),
                             treatment_flag_var = choices_selected(
                               variable_choices(dataname, subset = "ONTRTFL"), selected = "ONTRTFL", fixed = TRUE
                             ),
                             treatment_flag = choices_selected(
                               value_choices(dataname, "ONTRTFL"), selected = "Y", fixed = TRUE
                             ),
                             exclude_base_abn = FALSE,
                             pre_output = NULL,
                             post_output = NULL) {
  stop_if_not(
    is.string(dataname),
    is.choices_selected(arm_var),
    is.choices_selected(by_vars),
    is.choices_selected(grade),
    is_character_vector(abnormal),
    is.choices_selected(id_var),
    is.choices_selected(baseline_var),
    is.choices_selected(treatment_flag),
    is.choices_selected(treatment_flag_var),
    is_logical_single(exclude_base_abn),
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
    id_var = cs_to_des_select(id_var, dataname = dataname),
    by_vars = cs_to_des_select(by_vars, dataname = dataname, multiple = TRUE),
    grade = cs_to_des_select(grade, dataname = dataname),
    baseline_var = cs_to_des_select(baseline_var, dataname = dataname),
    treatment_flag_var = cs_to_des_select(treatment_flag_var, dataname = dataname)
  )

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_abnormality,
    server = srv_t_abnormality,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        abnormal = abnormal,
        label = label
        )
      ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_abnormality <- function(id, ...) {

  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$id_var,
    a$by_vars,
    a$grade,
    a$baseline_var,
    a$treatment_flag_var,
    a$treatment_flag
    )

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "id_var", "by_vars", "grade", "baseline_var", "treatment_flag_var")]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = TRUE),
      data_extract_input(
        id = ns("by_vars"),
        label = "Row By Variable",
        data_extract_spec = a$by_vars,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("grade"),
        label = "Grade Variable",
        data_extract_spec = a$grade,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        ns("abnormal_values"),
        "Abnormality Indicator",
        choices = c("LOW", "HIGH"),
        selected = c("LOW", "HIGH"),
        multiple = TRUE
      ),
      checkboxInput(
        ns("exclude_base_abn"),
        "Exclude subjects whose baseline grade is the same as abnormal grade",
        value = a$exclude_base_abn
      ),
      panel_group(
        panel_item(
          "Additional Variables Info",
          data_extract_input(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_input(
            id = ns("baseline_var"),
            label = "Baseline Grade Variable",
            data_extract_spec = a$baseline_var,
            is_single_dataset = is_single_dataset_value
          ),
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
srv_t_abnormality <- function(input,
                              output,
                              session,
                              datasets,
                              dataname,
                              parentname,
                              abnormal,
                              arm_var,
                              id_var,
                              by_vars,
                              grade,
                              baseline_var,
                              treatment_flag_var,
                              label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  # Update UI choices depending on selection of previous options
  observe({
    anl <- datasets$get_data(dataname, filtered = FALSE)

    validate_has_elements(input$grade, "Please select a grade variable")
    choices <- unique(anl[[input$grade]][!is.na(anl[[input$grade]])])

    updateSelectInput(
      session,
      "abnormal_values",
      choices = choices,
      selected = if (is.null(abnormal) |
                     length(intersect(abnormal, choices)) <= 0) {
        choices[1]
      } else {
        intersect(abnormal, choices)
      }
    )
  })

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, id_var, by_vars, grade, baseline_var, treatment_flag_var),
    input_id = c("arm_var", "id_var", "by_vars", "grade", "baseline_var", "treatment_flag_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    validate_has_data(adsl_filtered, 1)
    validate_has_data(anl_filtered, 1)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_by_vars <- as.vector(anl_m$columns_source$by_vars)
    input_grade <- as.vector(anl_m$columns_source$grade)
    input_baseline_var <- as.vector(anl_m$columns_source$baseline_var)
    input_treatment_flag_var <- as.vector(anl_m$columns_source$treatment_flag_var)

    validate(
      need(input_arm_var, "Please select a treatment variable."),
      need(input_grade, "Please select a grade variable."),
      need(input$abnormal_values, "Please select an abnormality indicator."),
      need(input_id_var, "Please select a subject identifier."),
      need(input_baseline_var, "Please select a baseline grade variable."),
      need(input_treatment_flag_var, "Please select an on treatment flag variable."),
      need(input$treatment_flag, "Please select indicator value for on treatment records.")
    )
    # validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_id_var, input_by_vars, input_grade),
      arm_var = input_arm_var,
      min_nrow = 10
    )
  })

  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    my_calls <- template_abnormality(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      by_vars = as.vector(anl_m$columns_source$by_vars),
      abnormal = input$abnormal_values,
      grade = as.vector(anl_m$columns_source$grade),
      baseline_var = as.vector(anl_m$columns_source$baseline_var),
      treatment_flag_var = as.vector(anl_m$columns_source$treatment_flag_var),
      treatment_flag = input$treatment_flag,
      add_total = input$add_total,
      exclude_base_abn = input$exclude_base_abn
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  output$table <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  # Render R code.
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm_var, id_var, by_vars, grade)
      ),
    modal_title = "R Code for Abnormality Table",
    code_header = label
  )
}
