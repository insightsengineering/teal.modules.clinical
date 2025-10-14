#' Template: Laboratory test results with highest grade post-baseline
#'
#' Creates a valid expression to generate a table to summarize abnormality by grade.
#'
#' @inheritParams template_arguments
#' @param atoxgr_var (`character`)\cr name of the variable indicating
#' Analysis Toxicity Grade.
#' @param worst_high_flag_var (`character`)\cr name of the variable indicating
#' Worst High Grade flag
#' @param worst_low_flag_var (`character`)\cr name of the variable indicating
#' Worst Low Grade flag
#' @param worst_flag_indicator (`character`)\cr flag value indicating the worst grade.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_abnormality_by_worst_grade()]
#' @keywords internal
template_abnormality_by_worst_grade <- function(parentname, # nolint: object_length.
                                                dataname,
                                                arm_var,
                                                id_var = "USUBJID",
                                                paramcd = "PARAMCD",
                                                atoxgr_var = "ATOXGR",
                                                worst_high_flag_var = "WGRHIFL",
                                                worst_low_flag_var = "WGRLOFL",
                                                worst_flag_indicator = "Y",
                                                add_total = FALSE,
                                                total_label = default_total_label(),
                                                drop_arm_levels = TRUE,
                                                basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(id_var)
  checkmate::assert_string(paramcd)
  checkmate::assert_string(atoxgr_var)
  checkmate::assert_string(worst_high_flag_var)
  checkmate::assert_string(worst_low_flag_var)
  checkmate::assert_string(worst_flag_indicator)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_arm_levels)

  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl_labels <- teal.data::col_labels(df, fill = FALSE),
      env = list(
        df = as.name(dataname)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        dplyr::mutate(
          # Changed the following prepo step methodology as not
          # all cases have grade = 4 (realized with nsdl real data)
          GRADE_DIR = factor(
            dplyr::case_when(
              as.numeric(as.character(atoxgr_var)) < 0 ~ "LOW",
              atoxgr_var == "0" ~ "ZERO",
              as.numeric(as.character(atoxgr_var)) > 0 ~ "HIGH"
            ),
            levels = c("LOW", "ZERO", "HIGH")
          ),
          # Changed the following prepo step methodology as not
          # all cases have grade = 4 (realized with nsdl real data)
          GRADE_ANL = factor(
            abs(
              as.numeric(
                as.character(atoxgr_var)
              )
            )
          )
        ) %>%
        dplyr::filter(worst_low_flag_var == worst_flag_indicator | worst_high_flag_var == worst_flag_indicator) %>%
        droplevels(),
      env = list(
        df = as.name(dataname),
        worst_low_flag_var = as.name(worst_low_flag_var),
        worst_high_flag_var = as.name(worst_high_flag_var),
        worst_flag_indicator = worst_flag_indicator,
        atoxgr_var = as.name(atoxgr_var)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    quote(
      expr = teal.data::col_labels(anl) <- c(
        anl_labels,
        GRADE_DIR = "   Direction of Abnormality",
        GRADE_ANL = "Highest Grade"
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
      expr = if (is.null(rtables::obj_label(anl[[paramcd]]))) {
        stop("Please specify label for ", paramcd)
      },
      env = list(
        paramcd = paramcd
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # map creation

  prep_list <- list()

  prep_list <- add_expr(
    prep_list,
    substitute(
      expr = map <- expand.grid(
        PARAM = levels(anl[[paramcd]]),
        GRADE_DIR = c("LOW", "HIGH"),
        GRADE_ANL = as.character(1:4),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::arrange(paramcd, desc(GRADE_DIR), GRADE_ANL),
      env = list(
        paramcd = paramcd
      )
    )
  )

  y$layout_prep <- bracket_expr(prep_list)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(show_colcounts = TRUE)
    )
  )

  # layout start
  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(
            var = arm_var,
            split_fun = rtables::add_overall_level(label = total_label, first = FALSE)
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

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_rows_by(
        paramcd,
        label_pos = "topleft",
        split_label = rtables::obj_label(anl[[paramcd]])
      ) %>%
        tern::summarize_num_patients(
          var = id_var,
          required = "GRADE_ANL",
          .stats = "unique_count"
        ) %>%
        rtables::split_rows_by(
          "GRADE_DIR",
          label_pos = "topleft",
          split_fun = rtables::trim_levels_to_map(map = map),
          split_label = rtables::obj_label(anl$GRADE_DIR)
        ) %>%
        tern::count_abnormal_by_worst_grade(
          var = "GRADE_ANL",
          variables = list(id = id_var, param = paramcd, grade_dir = "GRADE_DIR"),
          .indent_mods = 4L
        ) %>%
        rtables::append_topleft("                                  Highest Grade"),
      env = list(
        paramcd = paramcd,
        id_var = id_var
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' teal Module: Laboratory test results with highest grade post-baseline
#'
#' This module produces a table to summarize laboratory test results with highest grade post-baseline

#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_abnormality_by_worst_grade
#' @param atoxgr_var ([teal.transform::choices_selected()])\cr
#' object with all available choices and preselected option
#' for variable names that can be used as Analysis Toxicity Grade.
#' @param worst_high_flag_var ([teal.transform::choices_selected()])\cr
#' object with all available choices and preselected option for variable names that can be used as Worst High
#' Grade flag.
#' @param worst_low_flag_var ([teal.transform::choices_selected()])\cr
#' object with all available choices and preselected option for variable names that can be used as Worst Low Grade flag.
#' @param worst_flag_indicator ([teal.transform::choices_selected()])\cr
#' value indicating worst grade.
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
#' tm_t_abnormality_by_worst_grade(
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
#' @export
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' library(dplyr)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADLB <- tmc_ex_adlb %>%
#'     filter(!AVISIT %in% c("SCREENING", "BASELINE"))
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_abnormality_by_worst_grade(
#'       label = "Laboratory Test Results with Highest Grade Post-Baseline",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADSL, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(ADLB, "PARAMCD", "PARAM"),
#'         selected = c("ALT", "CRP", "IGA")
#'       ),
#'       add_total = FALSE
#'     )
#'   ),
#'   filter = teal_slices(
#'     teal_slice("ADSL", "SAFFL", selected = "Y"),
#'     teal_slice("ADLB", "ONTRTFL", selected = "Y")
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_abnormality_by_worst_grade <- function(label, # nolint: object_length.
                                            dataname,
                                            parentname = ifelse(
                                              inherits(arm_var, "data_extract_spec"),
                                              teal.transform::datanames_input(arm_var),
                                              "ADSL"
                                            ),
                                            arm_var,
                                            id_var = teal.transform::choices_selected(
                                              teal.transform::variable_choices(
                                                dataname,
                                                subset = "USUBJID"
                                              ),
                                              selected = "USUBJID", fixed = TRUE
                                            ),
                                            paramcd,
                                            atoxgr_var = teal.transform::choices_selected(
                                              teal.transform::variable_choices(
                                                dataname,
                                                subset = "ATOXGR"
                                              ),
                                              selected = "ATOXGR", fixed = TRUE
                                            ),
                                            worst_high_flag_var = teal.transform::choices_selected(
                                              teal.transform::variable_choices(
                                                dataname,
                                                subset = "WGRHIFL"
                                              ),
                                              selected = "WGRHIFL", fixed = TRUE
                                            ),
                                            worst_low_flag_var = teal.transform::choices_selected(
                                              teal.transform::variable_choices(
                                                dataname,
                                                subset = "WGRLOFL"
                                              ),
                                              selected = "WGRLOFL", fixed = TRUE
                                            ),
                                            worst_flag_indicator = teal.transform::choices_selected("Y"),
                                            add_total = TRUE,
                                            total_label = default_total_label(),
                                            drop_arm_levels = TRUE,
                                            pre_output = NULL,
                                            post_output = NULL,
                                            basic_table_args = teal.widgets::basic_table_args(),
                                            transformators = list(),
                                            decorators = list()) {
  message("Initializing tm_t_abnormality_by_worst_grade")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(total_label)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(atoxgr_var, "choices_selected")
  checkmate::assert_class(worst_high_flag_var, "choices_selected")
  checkmate::assert_class(worst_low_flag_var, "choices_selected")
  checkmate::assert_class(worst_flag_indicator, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE),
    atoxgr_var = cs_to_des_select(atoxgr_var, dataname = dataname),
    worst_high_flag_var = cs_to_des_select(worst_high_flag_var, dataname = dataname),
    worst_low_flag_var = cs_to_des_select(worst_low_flag_var, dataname = dataname)
  )

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_abnormality_by_worst_grade,
    server = srv_t_abnormality_by_worst_grade,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        worst_flag_indicator = worst_flag_indicator,
        total_label = total_label,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_t_abnormality_by_worst_grade <- function(id, ...) { # nolint: object_length.

  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$id_var,
    a$paramcd,
    a$atoxgr_var,
    a$worst_high_flag_var,
    a$worst_low_flag_var,
    a$worst_flag_indicator
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(
        a[c(
          "arm_var",
          "id_var",
          "paramcd",
          "atoxgr_var",
          "worst_high_flag_var",
          "worst_low_flag_var",
          "worst_flag_indicator"
        )]
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = FALSE),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Lab Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("atoxgr_var"),
        label = "Analysis toxicity grade",
        data_extract_spec = a$atoxgr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("worst_low_flag_var"),
        label = "Worst low flag variable",
        data_extract_spec = a$worst_low_flag_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("worst_high_flag_var"),
        label = "Worst high flag variable",
        data_extract_spec = a$worst_high_flag_var,
        is_single_dataset = is_single_dataset_value
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          teal.transform::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.widgets::optionalSelectInput(
            ns("worst_flag_indicator"),
            label = "Value Indicating Worst Grade",
            multiple = FALSE,
            fixed_on_single = TRUE
          ),
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          )
        )
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_abnormality_by_worst_grade <- function(id, # nolint: object_length.
                                             data,
                                             dataname,
                                             parentname,
                                             id_var,
                                             arm_var,
                                             paramcd,
                                             atoxgr_var,
                                             worst_flag_indicator,
                                             worst_low_flag_var,
                                             worst_high_flag_var,
                                             add_total,
                                             total_label,
                                             drop_arm_levels,
                                             label,
                                             basic_table_args,
                                             decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    isolate({
      resolved <- teal.transform::resolve_delayed(worst_flag_indicator, as.list(data()))
      teal.widgets::updateOptionalSelectInput(
        session = session,
        inputId = "worst_flag_indicator",
        choices = resolved$choices,
        selected = resolved$selected
      )
    })

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        id_var = id_var,
        paramcd = paramcd,
        atoxgr_var = atoxgr_var,
        worst_high_flag_var = worst_high_flag_var,
        worst_low_flag_var = worst_low_flag_var
      ),
      datasets = data,
      select_validation_rule = list(
        arm_var = shinyvalidate::sv_required("Please select a treatment variable."),
        id_var = shinyvalidate::sv_required("Please select a Subject Identifier."),
        atoxgr_var = shinyvalidate::sv_required("Please select Analysis Toxicity Grade variable."),
        worst_low_flag_var = shinyvalidate::sv_required("Please select the Worst Low Grade flag variable."),
        worst_high_flag_var = shinyvalidate::sv_required("Please select the Worst High Grade flag variable.")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("Please select at least one Laboratory parameter.")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
      iv$add_rule(
        "worst_flag_indicator",
        ~ if (length(.) == 0) {
          "Please select the value indicating worst grade."
        }
      )
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data,
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
          teal.reporter::teal_card("# Laboratory Test Results Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
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
      anl <- merged$anl_q()[["ANL"]]

      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_paramcd_var <- names(merged$anl_input_r()$columns_source$paramcd)
      input_atoxgr <- names(merged$anl_input_r()$columns_source$atoxgr_var)
      input_worst_high_flag_var <- names(merged$anl_input_r()$columns_source$worst_high_flag_var)
      input_worst_low_flag_var <- names(merged$anl_input_r()$columns_source$worst_low_flag_var)

      teal::validate_inputs(iv_r())

      if (length(input_paramcd_var) > 0) {
        validate(
          need(
            is.factor(anl[[input_paramcd_var]]),
            "Parameter variable should be a factor."
          )
        )
      }

      if (length(input_atoxgr) > 0) {
        validate(
          need(
            all(as.character(unique(anl[[input_atoxgr]])) %in% as.character(c(-4:4))),
            "All grade values should be within -4:4 range."
          ),
          need(
            is.factor(anl[[input_atoxgr]]),
            "Grade variable should be a factor."
          ),
          need(
            all(sapply(1:4, function(y) any(abs(as.numeric(as.character(anl[[input_atoxgr]]))) == y))),
            paste(
              "To display the table there must be at least one record for",
              "each highest grade (in either direction).\n\n",
              "Please remove filter(s) or select a different lab parameter."
            )
          )
        )
      }

      if (length(input_atoxgr) > 0) {
        validate(
          need(
            is.factor(anl[[input_atoxgr]]),
            "Treatment variable should be a factor."
          ),
        )
      }

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_paramcd_var,
          input_atoxgr, input_worst_high_flag_var,
          input_worst_low_flag_var
        ),
        arm_var = input_arm_var
      )
    })

    all_q <- reactive({
      validate_checks()

      my_calls <- template_abnormality_by_worst_grade(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = names(merged$anl_input_r()$columns_source$arm_var),
        id_var = names(merged$anl_input_r()$columns_source$id_var),
        paramcd = names(merged$anl_input_r()$columns_source$paramcd),
        atoxgr_var = names(merged$anl_input_r()$columns_source$atoxgr_var),
        worst_high_flag_var = names(merged$anl_input_r()$columns_source$worst_high_flag_var),
        worst_low_flag_var = names(merged$anl_input_r()$columns_source$worst_low_flag_var),
        worst_flag_indicator = input$worst_flag_indicator,
        add_total = input$add_total,
        total_label = total_label,
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = basic_table_args
      )

      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
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

    decorated_table_q
  })
}
