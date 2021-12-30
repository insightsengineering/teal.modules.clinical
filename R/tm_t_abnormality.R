#' Template: Abnormality Summary Table
#'
#' @inheritParams template_arguments
#' @param exclude_base_abn (`logical`)\cr whether to exclude patients who had abnormal values at baseline.
#' @param grade (`character`)\cr name of the variable that can be used to
#'   specify the abnormality grade. Variable must be factor.
#' @param abnormal (`named list`)\cr indicating abnormality direction and grades.
#' @param baseline_var (`character`)\cr
#'   name of the variable for baseline abnormality grade.
#' @param na_level (`character`)\cr the NA level in the input dataset, default to `"<Missing>"`.
#'
#' @seealso [tm_t_abnormality()]
#'
template_abnormality <- function(parentname,
                                 dataname,
                                 arm_var,
                                 id_var = "USUBJID",
                                 by_vars,
                                 abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
                                 grade = "ANRIND",
                                 baseline_var = "BNRIND",
                                 treatment_flag_var = "ONTRTFL",
                                 treatment_flag = "Y",
                                 add_total = FALSE,
                                 exclude_base_abn = FALSE,
                                 drop_arm_levels = TRUE,
                                 na_level = "<Missing>",
                                 basic_table_args = teal.devel::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(id_var),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    is.character(by_vars),
    is.list(abnormal),
    assertthat::is.string(grade),
    assertthat::is.string(baseline_var),
    assertthat::is.string(treatment_flag_var),
    assertthat::is.string(treatment_flag),
    assertthat::is.flag(add_total),
    assertthat::is.flag(exclude_base_abn),
    assertthat::is.flag(drop_arm_levels)
  )

  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        dplyr::filter(treatment_flag_var == treatment_flag & !is.na(grade) & grade != na_level),
      env = list(
        df = as.name(dataname),
        grade = as.name(grade),
        treatment_flag_var = as.name(treatment_flag_var),
        treatment_flag = treatment_flag,
        na_level = na_level
      )
    )
  )

  data_list <- add_expr(
    data_list,
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = arm_var,
      drop_arm_levels = drop_arm_levels
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      dataname <- df_explicit_na(dataname, na_level = na_level),
      env = list(dataname = as.name("anl"), na_level = na_level))
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- df_explicit_na(parentname, na_level = na_level),
      env = list(parentname = as.name(parentname), na_level = na_level))
  )

  y$data <- bracket_expr(data_list)

  # layout start
  prep_list <- list()
  prep_list <- add_expr(
    prep_list,
    substitute(
      # Define the map for layout using helper function h_map_for_count_abnormal
      map <- h_map_for_count_abnormal(
        df = dataname,
        variables = list(anl = grade, split_rows = by_vars),
        abnormal = abnormal,
        method = "default",
        na_level = na_level
      ),
      env = list(dataname = as.name("anl"), by_vars = by_vars, grade = grade, abnormal = abnormal, na_level = na_level)
    )
  )

  y$layout_prep <- bracket_expr(prep_list)

  parsed_basic_table_args <- teal.devel::parse_basic_table_args(
    teal.devel::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.devel::basic_table_args(main_footer = "by variables without observed abnormalities are excluded.")
    )
  )

  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = expr_basic_table_args %>%
          split_cols_by(
            var = arm_var,
            split_fun = add_overall_level("All Patients", first = FALSE)
          ) %>%
          add_colcounts(),
        env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
      )
    } else {
      substitute(
        expr = expr_basic_table_args %>%
          split_cols_by(var = arm_var) %>%
          add_colcounts(),
        env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
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
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map)
        ),
        env = list(
          by_var = by_var,
          split_label = split_label,
          map = as.name("map")
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
        variables = list(id = id_var, baseline = baseline_var),
        exclude_base_abn = exclude_base_abn
      ) %>%
        append_varlabels(dataname, grade, indent = indent_space),
      env = list(
        grade = grade,
        abnormal = abnormal,
        id_var = id_var,
        baseline_var = baseline_var,
        exclude_base_abn = exclude_base_abn,
        dataname = as.name(dataname),
        by_vars = by_vars,
        indent_space = length(by_vars)
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
#' @param abnormal (`named list`)\cr defined by user to indicate what abnormalities are to be displayed.
#' @param baseline_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#'   variable for baseline abnormality grade.
#' @param na_level (`character`)\cr the NA level in the input dataset, default to `"<Missing>"`.
#'
#' @note Patients with the same abnormality at baseline as on the treatment visit can be
#'   excluded in accordance with GDSR specifications by using `exclude_base_abn`.
#'
#' @export
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adlb <- synthetic_cdisc_data("latest")$adlb %>%
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
#'     cdisc_dataset("ADSL", adsl, code = "ADSL <- synthetic_cdisc_data('latest')$adsl"),
#'     cdisc_dataset("ADLB", adlb,
#'       code = "ADLB <- synthetic_cdisc_data('latest')$adlb %>%
#'                 mutate(
#'                   ONTRTFL = case_when(
#'                     AVISIT %in% c('SCREENING', 'BASELINE') ~ '',
#'                     TRUE ~ 'Y'
#'                   )
#'                 ) %>%
#'                 var_relabel(
#'                   ONTRTFL = 'On Treatment Record Flag'
#'                 )"
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
#'       add_total = FALSE,
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
#'       abnormal = list(low = "LOW", high = "HIGH"),
#'       exclude_base_abn = FALSE
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_abnormality <- function(label,
                             dataname,
                             parentname = ifelse(inherits(arm_var, "data_extract_spec"), teal.devel::datanames_input(arm_var), "ADSL"),
                             arm_var,
                             by_vars,
                             grade,
                             abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
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
                             add_total = TRUE,
                             exclude_base_abn = FALSE,
                             drop_arm_levels = TRUE,
                             pre_output = NULL,
                             post_output = NULL,
                             na_level = "<Missing>",
                             basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_abnormality")
  utils.nest::stop_if_not(
    assertthat::is.string(dataname),
    is.choices_selected(arm_var),
    assertthat::is.flag(add_total),
    is.choices_selected(by_vars),
    is.choices_selected(grade),
    utils.nest::is_character_list(abnormal, min_length = 2, max_length = 2),
    is.choices_selected(id_var),
    is.choices_selected(baseline_var),
    is.choices_selected(treatment_flag),
    is.choices_selected(treatment_flag_var),
    utils.nest::is_logical_single(exclude_base_abn),
    assertthat::is.flag(drop_arm_levels),
    list(
      is.null(pre_output) || inherits(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
      ),
    list(
      is.null(post_output) || inherits(post_output, "shiny.tag"),
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

  checkmate::assert_class(basic_table_args, "basic_table_args")

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
        label = label,
        na_level = na_level,
        basic_table_args = basic_table_args
        )
      ),
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_abnormality <- function(id, ...) {

  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- teal.devel::is_single_dataset(
    a$arm_var,
    a$id_var,
    a$by_vars,
    a$grade,
    a$baseline_var,
    a$treatment_flag_var,
    a$treatment_flag
    )

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(teal.devel::table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(a[c("arm_var", "id_var", "by_vars", "grade", "baseline_var", "treatment_flag_var")]),
      teal.devel::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.devel::data_extract_ui(
        id = ns("by_vars"),
        label = "Row By Variable",
        data_extract_spec = a$by_vars,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("grade"),
        label = "Grade Variable",
        data_extract_spec = a$grade,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(
        ns("exclude_base_abn"),
        "Exclude subjects whose baseline grade is the same as abnormal grade",
        value = a$exclude_base_abn
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          )
        )
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional Variables Info",
          teal.devel::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.devel::data_extract_ui(
            id = ns("baseline_var"),
            label = "Baseline Grade Variable",
            data_extract_spec = a$baseline_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.devel::data_extract_ui(
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
    forms = teal.devel::get_rcode_ui(ns("rcode")),
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
                              add_total,
                              drop_arm_levels,
                              label,
                              na_level,
                              basic_table_args) {
  stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

  # Update UI choices depending on selection of previous options
  observeEvent(input$grade, {
    anl <- datasets$get_data(dataname, filtered = FALSE)

    validate_has_elements(input$grade, "Please select a grade variable")
    choices <- unique(anl[[input$grade]][!is.na(anl[[input$grade]])])
  })

  anl_selectors <- teal.devel::data_extract_multiple_srv(
    list(
      arm_var = arm_var,
      id_var = id_var,
      by_vars = by_vars,
      grade = grade,
      baseline_var = baseline_var,
      treatment_flag_var = treatment_flag_var
    ),
    datasets = datasets
  )

  anl_merged <- teal.devel::data_merge_srv(
    selector_list = anl_selectors,
    datasets = datasets,
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var = arm_var),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_by_vars <- anl_selectors()$by_vars()$select_ordered
    input_grade <- as.vector(anl_m$columns_source$grade)
    input_baseline_var <- as.vector(anl_m$columns_source$baseline_var)
    input_treatment_flag_var <- as.vector(anl_m$columns_source$treatment_flag_var)

    validate(
      need(input_arm_var, "Please select a treatment variable."),
      need(input_grade, "Please select a grade variable."),
      need(input_id_var, "Please select a subject identifier."),
      need(input_baseline_var, "Please select a baseline grade variable."),
      need(input_treatment_flag_var, "Please select an on treatment flag variable."),
      need(input$treatment_flag, "Please select indicator value for on treatment records.")
    )
    # validate inputs
    teal.devel::validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_id_var, input_by_vars, input_grade),
      arm_var = input_arm_var
    )
  })

  call_preparation <- reactive({
    validate_checks()

    teal.devel::chunks_reset()
    anl_m <- anl_merged()
    teal.devel::chunks_push_data_merge(anl_m)
    teal.devel::chunks_push_new_line()

    anl_adsl <- adsl_merged()
    teal.devel::chunks_push_data_merge(anl_adsl)
    teal.devel::chunks_push_new_line()

    my_calls <- template_abnormality(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      by_vars = anl_selectors()$by_vars()$select_ordered,
      id_var = as.vector(anl_m$columns_source$id_var),
      abnormal = abnormal,
      grade = as.vector(anl_m$columns_source$grade),
      baseline_var = as.vector(anl_m$columns_source$baseline_var),
      treatment_flag_var = as.vector(anl_m$columns_source$treatment_flag_var),
      treatment_flag = input$treatment_flag,
      add_total = input$add_total,
      exclude_base_abn = input$exclude_base_abn,
      drop_arm_levels = input$drop_arm_levels,
      na_level = na_level,
      basic_table_args = basic_table_args
    )
    mapply(expression = my_calls, teal.devel::chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    teal.devel::chunks_safe_eval()
    teal.devel::chunks_get_var("result")
  })

  callModule(
    teal.devel::table_with_settings_srv,
    id = "table",
    table_r = table
  )

  # Render R code.
  callModule(
    module = teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(
      list(arm_var, id_var, by_vars, grade)
      ),
    modal_title = "R Code for Abnormality Table",
    code_header = label
  )
}
