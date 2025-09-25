#' Template: Abnormality Summary Table
#'
#' Creates a valid expression to generate a table to summarize abnormality.
#'
#' @inheritParams template_arguments
#' @param exclude_base_abn (`logical`)\cr whether to exclude patients who had abnormal values at baseline.
#' @param grade (`character`)\cr name of the variable used to
#'   specify the abnormality grade. Variable must be factor.
#' @param abnormal (`named list`)\cr indicating abnormality direction and grades.
#' @param baseline_var (`character`)\cr
#'   name of the variable specifying baseline abnormality grade.
#' @param na_level (`character`)\cr the NA level in the input dataset, defaults to `"<Missing>"`.
#' @param tbl_title (`character`)\cr Title with label of variables from by bars
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_abnormality()]
#' @keywords internal
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
                                 total_label = default_total_label(),
                                 exclude_base_abn = FALSE,
                                 drop_arm_levels = TRUE,
                                 na_level = tern::default_na_str(),
                                 basic_table_args = teal.widgets::basic_table_args(),
                                 tbl_title) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(id_var)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::check_character(by_vars)
  checkmate::check_list(abnormal)
  checkmate::assert_string(grade)
  checkmate::assert_string(baseline_var)
  checkmate::assert_string(treatment_flag_var)
  checkmate::assert_string(treatment_flag)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(exclude_base_abn)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_string(tbl_title)

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
      dataname <- tern::df_explicit_na(dataname, na_level = na_level),
      env = list(dataname = as.name("anl"), na_level = na_level)
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- tern::df_explicit_na(parentname, na_level = na_level),
      env = list(parentname = as.name(parentname), na_level = na_level)
    )
  )

  y$data <- bracket_expr(data_list)

  # layout start
  prep_list <- list()
  prep_list <- add_expr(
    prep_list,
    substitute(
      # Define the map for layout using helper function h_map_for_count_abnormal
      map <- tern::h_map_for_count_abnormal(
        df = dataname,
        variables = list(anl = grade, split_rows = by_vars),
        abnormal = abnormal,
        method = "default",
        na_str = na_level
      ),
      env = list(dataname = as.name("anl"), by_vars = by_vars, grade = grade, abnormal = abnormal, na_level = na_level)
    )
  )

  y$layout_prep <- bracket_expr(prep_list)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        show_colcounts = TRUE,
        title = tbl_title,
        main_footer = "Variables without observed abnormalities are excluded."
      )
    )
  )

  layout_list <- list()


  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(
            var = arm_var,
            split_fun = rtables::add_overall_level(total_label, first = FALSE)
          ),
        env = list(
          arm_var = arm_var,
          total_label = total_label,
          expr_basic_table_args = parsed_basic_table_args
        )
      )
    } else {
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(var = arm_var),
        env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
      )
    }
  )

  for (by_var in by_vars) {
    split_label <- substitute(
      expr = teal.data::col_labels(dataname, fill = FALSE)[[by_var]],
      env = list(
        dataname = as.name(dataname),
        by_var = by_var
      )
    )
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::split_rows_by(
          by_var,
          split_label = split_label,
          label_pos = "topleft",
          split_fun = rtables::trim_levels_to_map(map = map)
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
      expr = tern::count_abnormal(
        var = grade,
        abnormal = abnormal,
        variables = list(id = id_var, baseline = baseline_var),
        .indent_mods = 4L,
        exclude_base_abn = exclude_base_abn
      ) %>%
        tern::append_varlabels(dataname, grade, indent = indent_space),
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
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent) %>%
        rtables::prune_table()
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' teal Module: Abnormality Summary Table
#'
#' This module produces a table to summarize abnormality.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_abnormality
#' @param grade ([teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for variable names that can be used to
#'   specify the abnormality grade. Variable must be factor.
#' @param abnormal (`named list`)\cr defined by user to indicate what abnormalities are to be displayed.
#' @param baseline_var ([teal.transform::choices_selected()])\cr
#'   variable for baseline abnormality grade.
#' @param na_level (`character`)\cr the NA level in the input dataset, default to `"<Missing>"`.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`TableTree` - output of `rtables::build_table()`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_abnormality(
#'    ..., # arguments for module
#'    decorators = list(
#'      table = teal_transform_module(...) # applied only to `table` output
#'    )
#' )
#' ```
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.clinical")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @note Patients with the same abnormality at baseline as on the treatment visit can be
#'   excluded in accordance with GDSR specifications by using `exclude_base_abn`.
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#'
#' data <- teal_data()
#' data <- within(data, {
#'   library(teal.modules.clinical)
#'   library(formatters)
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl
#'   ADLB <- tmc_ex_adlb %>%
#'     mutate(
#'       ONTRTFL = case_when(
#'         AVISIT %in% c("SCREENING", "BASELINE") ~ "",
#'         TRUE ~ "Y"
#'       ) %>% with_label("On Treatment Record Flag")
#'     )
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_abnormality(
#'       label = "Abnormality Table",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADSL, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       add_total = FALSE,
#'       by_vars = choices_selected(
#'         choices = variable_choices(ADLB, subset = c("LBCAT", "PARAM", "AVISIT")),
#'         selected = c("LBCAT", "PARAM"),
#'         keep_order = TRUE
#'       ),
#'       baseline_var = choices_selected(
#'         variable_choices(ADLB, subset = "BNRIND"),
#'         selected = "BNRIND", fixed = TRUE
#'       ),
#'       grade = choices_selected(
#'         choices = variable_choices(ADLB, subset = "ANRIND"),
#'         selected = "ANRIND",
#'         fixed = TRUE
#'       ),
#'       abnormal = list(low = "LOW", high = "HIGH"),
#'       exclude_base_abn = FALSE
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_abnormality <- function(label,
                             dataname,
                             parentname = ifelse(
                               inherits(arm_var, "data_extract_spec"),
                               teal.transform::datanames_input(arm_var),
                               "ADSL"
                             ),
                             arm_var,
                             by_vars,
                             grade,
                             abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
                             id_var = teal.transform::choices_selected(
                               teal.transform::variable_choices(dataname, subset = "USUBJID"),
                               selected = "USUBJID", fixed = TRUE
                             ),
                             baseline_var = teal.transform::choices_selected(
                               teal.transform::variable_choices(dataname, subset = "BNRIND"),
                               selected = "BNRIND", fixed = TRUE
                             ),
                             treatment_flag_var = teal.transform::choices_selected(
                               teal.transform::variable_choices(dataname, subset = "ONTRTFL"),
                               selected = "ONTRTFL", fixed = TRUE
                             ),
                             treatment_flag = teal.transform::choices_selected("Y"),
                             add_total = TRUE,
                             total_label = default_total_label(),
                             exclude_base_abn = FALSE,
                             drop_arm_levels = TRUE,
                             pre_output = NULL,
                             post_output = NULL,
                             na_level = tern::default_na_str(),
                             basic_table_args = teal.widgets::basic_table_args(),
                             transformators = list(),
                             decorators = list()) {
  message("Initializing tm_t_abnormality")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_list(abnormal, types = "character", len = 2)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(by_vars, "choices_selected")
  checkmate::assert_class(grade, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(baseline_var, "choices_selected")
  checkmate::assert_class(treatment_flag_var, "choices_selected")
  checkmate::assert_class(treatment_flag, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_flag(exclude_base_abn)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    by_vars = cs_to_des_select(by_vars, dataname = dataname, multiple = TRUE, ordered = TRUE),
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
        treatment_flag = treatment_flag,
        label = label,
        total_label = total_label,
        na_level = na_level,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_t_abnormality <- function(id, ...) {
  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$id_var,
    a$by_vars,
    a$grade,
    a$baseline_var,
    a$treatment_flag_var,
    a$treatment_flag
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(
        a[c("arm_var", "id_var", "by_vars", "grade", "baseline_var", "treatment_flag_var")]
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.transform::data_extract_ui(
        id = ns("by_vars"),
        label = "Row By Variable",
        data_extract_spec = a$by_vars,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
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
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          )
        )
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          teal.transform::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.transform::data_extract_ui(
            id = ns("baseline_var"),
            label = "Baseline Grade Variable",
            data_extract_spec = a$baseline_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.transform::data_extract_ui(
            id = ns("treatment_flag_var"),
            label = "On Treatment Flag Variable",
            data_extract_spec = a$treatment_flag_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.widgets::optionalSelectInput(
            ns("treatment_flag"),
            label = "Value Indicating On Treatment",
            multiple = FALSE,
            fixed_on_single = TRUE
          )
        )
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_abnormality <- function(id,
                              data,
                              dataname,
                              parentname,
                              abnormal,
                              arm_var,
                              id_var,
                              by_vars,
                              grade,
                              baseline_var,
                              treatment_flag_var,
                              treatment_flag,
                              add_total,
                              total_label,
                              drop_arm_levels,
                              label,
                              na_level,
                              basic_table_args,
                              decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        id_var = id_var,
        by_vars = by_vars,
        grade = grade,
        baseline_var = baseline_var,
        treatment_flag_var = treatment_flag_var
      ),
      datasets = data,
      select_validation_rule = list(
        arm_var = shinyvalidate::sv_required(
          "Please select a treatment variable."
        ),
        by_vars = shinyvalidate::sv_required(
          "Please select a Row By Variable."
        ),
        id_var = shinyvalidate::sv_required(
          "Please select a subject identifier."
        ),
        grade = shinyvalidate::sv_required(
          "Please select a grade variable."
        ),
        baseline_var = shinyvalidate::sv_required(
          "Please select a baseline grade variable."
        ),
        treatment_flag_var = shinyvalidate::sv_required(
          "Please select indicator value for on treatment records."
        )
      )
    )

    isolate({
      resolved <- teal.transform::resolve_delayed(treatment_flag, as.list(data()))
      teal.widgets::updateOptionalSelectInput(
        session = session,
        inputId = "treatment_flag",
        choices = resolved$choices,
        selected = resolved$selected
      )
    })

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("treatment_flag", shinyvalidate::sv_required(
        "Please select indicator value for on treatment records."
      ))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Abnormality Summary Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    validate_checks <- reactive({
      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      teal::validate_inputs(iv_r())

      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_id_var <- names(merged$anl_input_r()$columns_source$id_var)
      input_by_vars <- names(merged$anl_input_r()$columns_source$by_vars)
      input_grade <- names(merged$anl_input_r()$columns_source$grade)
      input_baseline_var <- names(merged$anl_input_r()$columns_source$baseline_var)
      input_treatment_flag_var <- names(merged$anl_input_r()$columns_source$treatment_flag_var)

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_id_var, input_by_vars, input_grade),
        arm_var = input_arm_var
      )
    })

    all_q <- reactive({
      validate_checks()

      by_vars_names <- merged$anl_input_r()$columns_source$by_vars
      by_vars_labels <- as.character(sapply(by_vars_names, function(name) {
        attr(merged$anl_q()[["ANL"]][[name]], "label")
      }))

      tbl_title <- ifelse(
        length(by_vars_labels) == 1,
        paste("Laboratory Abnormality summary by", by_vars_labels),
        paste(paste("Laboratory Abnormality summary by", paste(by_vars_labels, collapse = ", ")))
      )

      my_calls <- template_abnormality(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = as.vector(merged$anl_input_r()$columns_source$arm_var),
        by_vars = merged$anl_input_r()$columns_source$by_vars,
        id_var = as.vector(merged$anl_input_r()$columns_source$id_var),
        abnormal = abnormal,
        grade = as.vector(merged$anl_input_r()$columns_source$grade),
        baseline_var = as.vector(merged$anl_input_r()$columns_source$baseline_var),
        treatment_flag_var = as.vector(merged$anl_input_r()$columns_source$treatment_flag_var),
        treatment_flag = input$treatment_flag,
        add_total = input$add_total,
        total_label = total_label,
        exclude_base_abn = input$exclude_base_abn,
        drop_arm_levels = input$drop_arm_levels,
        na_level = na_level,
        basic_table_args = basic_table_args,
        tbl_title = tbl_title
      )

      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    # Outputs to render.
    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_table_q())))
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = label
    )

    decorated_table_q
  })
}
