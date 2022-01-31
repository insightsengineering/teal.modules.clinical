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
                                  na.rm = FALSE, # nolint
                                  na_level = "<Missing>",
                                  add_total = FALSE,
                                  basic_table_args = teal.devel::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(visit_var),
    assertthat::is.string(paramcd),
    assertthat::is.string(aval_var),
    assertthat::is.string(base_var),
    assertthat::is.flag(na.rm),
    assertthat::is.string(na_level),
    assertthat::is.string(treatment_flag_var),
    assertthat::is.string(treatment_flag),
    assertthat::is.flag(add_total)
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

  parsed_basic_table_args <- teal.devel::parse_basic_table_args(
    teal.devel::resolve_basic_table_args(
      user_table = basic_table_args
    )
  )

  # Start layout steps.
  layout_list <- list()

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(visit_var, split_fun = drop_split_levels) %>% # temp solution for over arching column
          rtables::split_cols_by(aval_var) %>%
          rtables::split_rows_by(
            arm_var,
            split_fun = add_overall_level("All Patients", first = FALSE),
            label_pos = "topleft",
            split_label = obj_label(dataname$arm_var)
          ) %>%
          add_rowcounts() %>%
          summarize_vars(base_var, denom = "N_row", na_level = na_level, na.rm = na.rm, .stats = "count_fraction") %>%
          append_varlabels(dataname, base_var, indent = 1L),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          base_var = base_var,
          dataname = as.name(dataname),
          visit_var = visit_var,
          na.rm = na.rm,
          na_level = na_level,
          expr_basic_table_args = parsed_basic_table_args
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(visit_var, split_fun = drop_split_levels) %>% # temp solution for over arching column
          rtables::split_cols_by(aval_var) %>%
          rtables::split_rows_by(
            arm_var,
            split_fun = drop_split_levels,
            label_pos = "topleft",
            split_label = obj_label(dataname$arm_var)
          ) %>%
          add_rowcounts() %>%
          summarize_vars(base_var, denom = "N_row", na_level = na_level, na.rm = na.rm, .stats = "count_fraction") %>%
          append_varlabels(dataname, base_var, indent = 1L),
        env = list(
          aval_var = aval_var,
          arm_var = arm_var,
          base_var = base_var,
          dataname = as.name(dataname),
          visit_var = visit_var,
          na.rm = na.rm,
          na_level = na_level,
          expr_basic_table_args = parsed_basic_table_args
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
      result <- rtables::build_table(lyt = lyt, df = dataname)
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
#'         variable_choices(adsl, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(adeg, "PARAMCD"),
#'         selected = "HR"
#'       ),
#'       visit_var = choices_selected(
#'         value_choices(adeg, "AVISIT"),
#'         selected = "POST-BASELINE MINIMUM"
#'       ),
#'       aval_var = choices_selected(
#'         variable_choices(adeg, subset = "ANRIND"),
#'         selected = "ANRIND", fixed = TRUE
#'       ),
#'       base_var = choices_selected(
#'         variable_choices(adeg, subset = "BNRIND"),
#'         selected = "BNRIND", fixed = TRUE
#'       ),
#'       useNA = "ifany"
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_shift_by_arm <- function(label,
                              dataname,
                              parentname = ifelse(
                                inherits(arm_var, "data_extract_spec"),
                                teal.devel::datanames_input(arm_var),
                                "ADSL"
                              ),
                              arm_var,
                              paramcd,
                              visit_var,
                              aval_var,
                              base_var,
                              treatment_flag_var = choices_selected(
                                variable_choices(dataname, subset = "ONTRTFL"),
                                selected = "ONTRTFL", fixed = TRUE
                              ),
                              treatment_flag = choices_selected(
                                value_choices(dataname, "ONTRTFL"),
                                selected = "Y", fixed = TRUE
                              ),
                              useNA = c("ifany", "no"), # nolint
                              na_level = "<Missing>",
                              add_total = FALSE,
                              pre_output = NULL,
                              post_output = NULL,
                              basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_shift_by_arm")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  useNA <- match.arg(useNA) # nolint
  checkmate::assert_string(na_level)
  checkmate::assert_class(treatment_flag, "choices_selected")
  checkmate::assert_class(treatment_flag_var, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    visit_var = cs_to_des_filter(visit_var, dataname = dataname),
    treatment_flag_var = cs_to_des_select(treatment_flag_var, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    base_var = cs_to_des_select(base_var, dataname = dataname)
  )

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
        na_level = na_level,
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_shift_by_arm <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  is_single_dataset_value <- teal.devel::is_single_dataset(
    a$id_var,
    a$arm_var,
    a$paramcd,
    a$visit_var,
    a$treatment_flag_var,
    a$treatment_flag,
    a$aval_var,
    a$base_var
  )

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(teal.devel::table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(a[c(
        "arm_var", "paramcd_var", "paramcd", "aval_var", "base_var", "visit_var", "treamtment_flag_var"
      )]),
      teal.devel::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("visit_var"),
        label = "Select Visit",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("aval_var"),
        label = "Select Analysis Range Indicator Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("base_var"),
        label = "Select Baseline Reference Range Indicator Variable",
        data_extract_spec = a$base_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients row", value = a$add_total),
      radioButtons(
        ns("useNA"),
        label = "Display NA counts",
        choices = c("ifany", "no"),
        selected = a$useNA
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional Variables Info",
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
srv_shift_by_arm <- function(id,
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
                             add_total,
                             basic_table_args) {
  stopifnot(is_cdisc_data(datasets))
  moduleServer(id, function(input, output, session) {
    teal.devel::init_chunks()

    anl_merged <- teal.devel::data_merge_module(
      datasets = datasets,
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        visit_var = visit_var,
        aval_var = aval_var,
        base_var = base_var,
        treatment_flag_var = treatment_flag_var
      ),
      merge_function = "dplyr::inner_join"
    )

    adsl_merged <- teal.devel::data_merge_module(
      datasets = datasets,
      data_extract = list(arm_var = arm_var),
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

      teal.devel::validate_standard_inputs(
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

      teal.devel::chunks_reset()
      anl_m <- anl_merged()
      teal.devel::chunks_push_data_merge(anl_m)
      teal.devel::chunks_push_new_line()

      anl_adsl <- adsl_merged()
      teal.devel::chunks_push_data_merge(anl_adsl)
      teal.devel::chunks_push_new_line()

      my_calls <- template_shift_by_arm(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        paramcd = unlist(paramcd$filter)["vars_selected"],
        treatment_flag_var = as.vector(anl_m$columns_source$treatment_flag_var),
        treatment_flag = input$treatment_flag,
        aval_var = as.vector(anl_m$columns_source$aval_var),
        base_var = as.vector(anl_m$columns_source$base_var),
        na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE), # nolint
        na_level = na_level,
        add_total = input$add_total,
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

    teal.devel::table_with_settings_srv(
      id = "table",
      table_r = table
    )

    # Render R code.
    teal.devel::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.devel::get_extract_datanames(list(arm_var, paramcd, visit_var, aval_var, base_var)),
      modal_title = "R Code for Shift Table by Arm",
      code_header = label
    )
  })
}
