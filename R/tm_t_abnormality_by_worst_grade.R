#' Template: Laboratory test results with highest grade post-baseline
#' @inheritParams template_arguments
#' @param atoxgr_var (`character`)\cr the variable name indicating
#' Analysis Toxicity Grade.
#' @param worst_high_flag_var (`character`)\cr the variable name indicating
#' Worst High Grade flag
#' @param worst_low_flag_var (`character`)\cr the variable name indicating
#' Worst Low Grade flag
#' @param worst_flag_indicator (`character`)\cr value indicating worst grade.
#'
#' @seealso [tm_t_abnormality_by_worst_grade()]
#' @keywords internal
template_abnormality_by_worst_grade <- function(parentname, # nolint
                                                dataname,
                                                arm_var,
                                                id_var = "USUBJID",
                                                paramcd = "PARAMCD",
                                                atoxgr_var = "ATOXGR",
                                                worst_high_flag_var = "WGRHIFL",
                                                worst_low_flag_var = "WGRLOFL",
                                                worst_flag_indicator = "Y",
                                                add_total = FALSE,
                                                total_label = "All Patients",
                                                drop_arm_levels = TRUE,
                                                basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(id_var),
    assertthat::is.string(paramcd),
    assertthat::is.string(atoxgr_var),
    assertthat::is.string(worst_high_flag_var),
    assertthat::is.string(worst_low_flag_var),
    assertthat::is.string(worst_flag_indicator),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label),
    assertthat::is.flag(drop_arm_levels)
  )

  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl_labels <- formatters::var_labels(df, fill = FALSE),
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
      expr = formatters::var_labels(anl) <- c(
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
      expr = if (is.null(obj_label(anl[[paramcd]]))) {
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
    teal.widgets::resolve_basic_table_args(user_table = basic_table_args)
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
            split_fun = add_overall_level(label = total_label, first = FALSE)
          ) %>%
          rtables::add_colcounts(),
        env = list(
          arm_var = arm_var,
          total_label = total_label,
          expr_basic_table_args = parsed_basic_table_args
        )
      )
    } else {
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(var = arm_var) %>%
          rtables::add_colcounts(),
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
        split_label = obj_label(anl[[paramcd]])
      ) %>%
        summarize_num_patients(
          var = id_var,
          required = "GRADE_ANL",
          .stats = "unique_count"
        ) %>%
        rtables::split_rows_by(
          "GRADE_DIR",
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map),
          split_label = obj_label(anl$GRADE_DIR)
        ) %>%
        count_abnormal_by_worst_grade(
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      result
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' Teal Module: Laboratory test results with highest grade post-baseline
#'
#' @inheritParams module_arguments
#' @inheritParams template_abnormality_by_worst_grade
#' @param atoxgr_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' object with all available choices and preselected option
#' for variable names that can be used as Analysis Toxicity Grade.
#' @param worst_high_flag_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' object with all available choices and preselected option for variable names that can be used as Worst High
#' Grade flag.
#' @param worst_low_flag_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' object with all available choices and preselected option for variable names that can be used as Worst Low Grade flag.
#' @param worst_flag_indicator ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' value indicating worst grade.
#' @seealso [template_abnormality_by_worst_grade()]
#'
#' @export
#'
#' @examples
#' adsl <- tmc_ex_adsl
#' adlb <- tmc_ex_adlb %>%
#'   dplyr::filter(!AVISIT %in% c("SCREENING", "BASELINE"))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADLB", adlb)
#'   ),
#'   modules = modules(
#'     tm_t_abnormality_by_worst_grade(
#'       label = "Laboratory Test Results with Highest Grade Post-Baseline",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(adlb, "PARAMCD", "PARAM"),
#'         selected = c("ALT", "CRP", "IGA")
#'       ),
#'       add_total = FALSE
#'     )
#'   ),
#'   filter = (
#'     list(
#'       ADSL = list(SAFFL = "Y"),
#'       ADLB = list(ONTRTFL = "Y")
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_abnormality_by_worst_grade <- function(label, # nolint
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
                                            worst_flag_indicator = teal.transform::choices_selected(
                                              teal.transform::value_choices(
                                                dataname,
                                                var_choices = "WGRLOFL"
                                              ),
                                              selected = "Y", fixed = TRUE
                                            ),
                                            add_total = TRUE,
                                            total_label = "All Patients",
                                            drop_arm_levels = TRUE,
                                            pre_output = NULL,
                                            post_output = NULL,
                                            basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_abnormality_by_worst_grade")
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
        total_label = total_label,
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_abnormality_by_worst_grade <- function(id, ...) { # nolint

  ns <- shiny::NS(id)
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
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
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
      shiny::checkboxInput(ns("add_total"), "Add All Patients column", value = FALSE),
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
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional table settings",
          teal.transform::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.widgets::optionalSelectInput(
            ns("worst_flag_indicator"),
            label = "Value Indicating Worst Grade",
            choices = a$worst_flag_indicator$choices,
            selected = a$worst_flag_indicator$selected,
            multiple = FALSE,
            fixed = a$worst_flag_indicator$fixed
          ),
          shiny::checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          )
        )
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_abnormality_by_worst_grade <- function(id, # nolint
                                             data,
                                             reporter,
                                             filter_panel_api,
                                             dataname,
                                             parentname,
                                             id_var,
                                             arm_var,
                                             paramcd,
                                             atoxgr_var,
                                             worst_low_flag_var,
                                             worst_high_flag_var,
                                             add_total,
                                             total_label,
                                             drop_arm_levels,
                                             label,
                                             basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
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
        worst_high_flag_var = shinyvalidate::sv_required("Please select the Worst High Grade flag variable."),
        worst_flag_indicator = shinyvalidate::sv_required("Please select the value indicating worst grade.")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("Please select at least one Laboratory parameter.")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data,
      join_keys = get_join_keys(data),
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      join_keys = get_join_keys(data),
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    validate_checks <- shiny::reactive({
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
        shiny::validate(
          shiny::need(
            is.factor(anl[[input_paramcd_var]]),
            "Parameter variable should be a factor."
          )
        )
      }

      if (length(input_atoxgr) > 0) {
        shiny::validate(
          shiny::need(
            all(as.character(unique(anl[[input_atoxgr]])) %in% as.character(c(-4:4))),
            "All grade values should be within -4:4 range."
          ),
          shiny::need(
            is.factor(anl[[input_atoxgr]]),
            "Grade variable should be a factor."
          ),
          shiny::need(
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
        shiny::validate(
          shiny::need(
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

    all_q <- shiny::reactive({
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

      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
    })

    # Outputs to render.
    table_r <- shiny::reactive(all_q()[["result"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(all_q())))
    )

    # Render R code.
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Laboratory Test Results Table")
        card$append_text("Laboratory Test Results Table", "header2")
        card$append_text("Laboratory test results with highest grade post-baseline Table", "header3")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(all_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
