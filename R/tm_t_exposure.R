#' Template: Exposure Table for Risk management plan
#'
#' @inheritParams template_arguments
#' @param row_by_var (`character`)\cr
#'   variable name used to split the rows.
#' @param col_by_var (`character`)\cr
#'   variable name used to split the columns.
#' @param avalu_var (`character`)\cr
#'   name of the analysis unit variable.
#' @param drop_levels (`flag`)\cr
#'   whether empty rows should be removed from the table.
#' @param paramcd_label (`character`)\cr
#'   the column from the dataset where the value will be used to label the argument `paramcd`.
#' @seealso [tm_t_exposure()]
#' @keywords internal
#'
template_exposure <- function(parentname,
                              dataname,
                              id_var,
                              paramcd,
                              paramcd_label = NULL,
                              row_by_var,
                              col_by_var,
                              add_total = FALSE,
                              total_label = "Total",
                              drop_levels = TRUE,
                              na_level = "<Missing>",
                              aval_var,
                              avalu_var,
                              basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(row_by_var),
    assertthat::is.string(col_by_var) || length(col_by_var) == 0,
    assertthat::is.string(paramcd),
    assertthat::is.string(id_var),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label),
    assertthat::is.string(na_level),
    assertthat::is.string(aval_var),
    assertthat::is.string(avalu_var) || length(avalu_var) == 0,
    assertthat::is.flag(drop_levels)
  )

  y <- list()
  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- dataname,
      env = list(
        dataname = as.name(dataname)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      dataname <- df_explicit_na(dataname, na_level = na_level),
      env = list(
        dataname = as.name("anl"),
        na_level = na_level
      )
    )
  )
  y$data <- bracket_expr(data_list)

  # layout start
  y$layout_prep <- quote(split_fun <- drop_split_levels)

  if (is.null(paramcd_label)) {
    paramcd_label <- paramcd
  }

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        main_footer = paste0("* Patient Time is the sum of ", paramcd_label)
      )
    )
  )

  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  if (length(col_by_var) > 0) {
    if (add_total) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          rtables::split_cols_by(col_by_var, split_fun = add_overall_level(total_label, first = FALSE)),
          env = list(
            col_by_var = col_by_var,
            total_label = total_label
          )
        )
      )
    } else {
      layout_list <- add_expr(
        layout_list,
        substitute(
          rtables::split_cols_by(col_by_var),
          env = list(
            col_by_var = col_by_var
          )
        )
      )
    }
  }

  layout_list <- add_expr(
    layout_list,
    quote(rtables::add_colcounts())
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_patients_exposure_in_cols(
        var = aval_var, col_split = TRUE,
        .labels = c(
          n_patients = "Number of Patients",
          sum_exposure = ifelse(
            avalu_var == " ",
            paste("Sum of", paramcd),
            paste("Sum of", paramcd, sprintf("(%s)", avalu_var))
          )
        ),
        custom_label = "Total Number of Patients and Patient Time*"
      ),
      env = list(
        aval_var = aval_var,
        avalu_var = avalu_var,
        paramcd = paramcd
      )
    )
  )

  split_label <- substitute(
    expr = formatters::var_labels(dataname[row_by_var], fill = TRUE),
    env = list(
      dataname = as.name(dataname),
      row_by_var = row_by_var
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      analyze_patients_exposure_in_cols(
        var = row_by_var,
        col_split = FALSE
      ) %>%
        append_topleft(c(split_label)),
      env = list(
        row_by_var = row_by_var,
        split_label = split_label
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

  if (drop_levels) {
    y$table <- substitute(
      expr = {
        result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
        rtables::prune_table(result)
      },
      env = list(parent = as.name(parentname))
    )
  }
  y
}

#' Teal module: Exposure Table for Risk management plan
#'
#' @inheritParams module_arguments
#' @inheritParams template_exposure
#' @param row_by_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all available choices and preselected option for
#'   variable names that can be used to split rows.
#' @param col_by_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all available choices and preselected option for
#'   variable names that can be used to split columns.
#' @param parcat ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all available choices and preselected option for
#'   parameter category values.
#' @param avalu_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with the analysis unit variable.
#' @param paramcd_label (`character`)\cr
#'   the column from the dataset where the value will be used to label the argument `paramcd`.
#'
#' @export
#'
#' @examples
#' adsl <- tmc_ex_adsl
#' adex <- tmc_ex_adex
#'
#' set.seed(1, kind = "Mersenne-Twister")
#' labels <- formatters::var_labels(adex, fill = FALSE)
#' adex <- adex %>%
#'   dplyr::distinct(USUBJID, .keep_all = TRUE) %>%
#'   dplyr::mutate(
#'     PARAMCD = "TDURD",
#'     PARAM = "Overall duration (days)",
#'     AVAL = sample(x = seq(1, 200), size = dplyr::n(), replace = TRUE),
#'     AVALU = "Days"
#'   ) %>%
#'   dplyr::bind_rows(adex)
#' formatters::var_labels(adex) <- labels
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADEX", adex)
#'   ),
#'   modules = modules(
#'     tm_t_exposure(
#'       label = "Duration of Exposure Table",
#'       dataname = "ADEX",
#'       paramcd = choices_selected(
#'         choices = value_choices(adex, "PARAMCD", "PARAM"),
#'         selected = "TDURD"
#'       ),
#'       col_by_var = choices_selected(
#'         choices = variable_choices(adex, subset = c("SEX", "ARM")),
#'         selected = "SEX"
#'       ),
#'       row_by_var = choices_selected(
#'         choices = variable_choices(adex, subset = c("RACE", "REGION1", "STRATA1", "SEX")),
#'         selected = "RACE"
#'       ),
#'       parcat = choices_selected(
#'         choices = value_choices(adex, "PARCAT2"),
#'         selected = "Drug A"
#'       ),
#'       add_total = FALSE
#'     )
#'   ),
#'   filter = list(
#'     ADSL = list(SAFFL = "Y")
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_exposure <- function(label,
                          dataname,
                          parentname = ifelse(
                            inherits(col_by_var, "data_extract_spec"),
                            teal.transform::datanames_input(col_by_var),
                            "ADSL"
                          ),
                          row_by_var,
                          col_by_var,
                          paramcd = teal.transform::choices_selected(
                            choices = teal.transform::value_choices(dataname, "PARAMCD", "PARAM"),
                            selected = "TDURD"
                          ),
                          paramcd_label = "PARAM",
                          id_var = teal.transform::choices_selected(
                            teal.transform::variable_choices(dataname, subset = "USUBJID"),
                            selected = "USUBJID",
                            fixed = TRUE
                          ),
                          parcat,
                          aval_var = teal.transform::choices_selected(
                            teal.transform::variable_choices(dataname, subset = "AVAL"),
                            selected = "AVAL",
                            fixed = TRUE
                          ),
                          avalu_var = teal.transform::choices_selected(
                            teal.transform::variable_choices(dataname, subset = "AVALU"),
                            selected = "AVALU",
                            fixed = TRUE
                          ),
                          add_total,
                          total_label = "All Patients",
                          na_level = "<Missing>",
                          pre_output = NULL,
                          post_output = NULL,
                          basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_exposure")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(row_by_var, "choices_selected")
  checkmate::assert_class(col_by_var, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(parcat, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(avalu_var, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  data_extract_list <- list(
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    row_by_var = cs_to_des_select(row_by_var, dataname = dataname),
    col_by_var = cs_to_des_select(col_by_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    parcat = cs_to_des_filter(parcat, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    avalu_var = cs_to_des_select(avalu_var, dataname = dataname)
  )

  args <- as.list(environment())
  module(
    label = label,
    ui = ui_t_exposure,
    server = srv_t_exposure,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        total_label = total_label,
        na_level = na_level,
        basic_table_args = basic_table_args,
        paramcd_label = paramcd_label
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}


#' @noRd
ui_t_exposure <- function(id, ...) {
  ns <- shiny::NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$paramcd,
    a$col_by_var,
    a$row_by_var,
    a$id_var,
    a$parcat,
    a$aval_var,
    a$avalu_var
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c(
        "paramcd", "col_by_var", "row_by_var", "id_var", "parcat", "aval_var", "avalu_var"
      )]),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select the Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("parcat"),
        label = "Select the Parameter Category",
        data_extract_spec = a$parcat,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("col_by_var"),
        label = "Select Column by Variable",
        data_extract_spec = a$col_by_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("row_by_var"),
        label = "Select Row by Variable",
        data_extract_spec = a$row_by_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional Variables Info",
          teal.transform::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.transform::data_extract_ui(
            id = ns("aval_var"),
            label = "Analysis Value Variable",
            data_extract_spec = a$aval_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.transform::data_extract_ui(
            id = ns("avalu_var"),
            label = "Analysis Value Unit Variable",
            data_extract_spec = a$avalu_var,
            is_single_dataset = is_single_dataset_value
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
srv_t_exposure <- function(id,
                           data,
                           reporter,
                           filter_panel_api,
                           dataname,
                           parentname,
                           paramcd,
                           paramcd_label,
                           id_var,
                           row_by_var,
                           col_by_var,
                           parcat,
                           aval_var,
                           avalu_var,
                           na_level,
                           label,
                           total_label,
                           basic_table_args = basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")
  shiny::moduleServer(id, function(input, output, session) {
    rule_intersection <- function(other) {
      function(value) {
        others <- selector_list()[[other]]()$select
        if (length(intersect(value, others)) > 0L) {
          "Column by and row by variables should not be the same."
        }
      }
    }

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        id_var = id_var,
        paramcd = paramcd,
        row_by_var = row_by_var,
        col_by_var = col_by_var,
        parcat = parcat,
        aval_var = aval_var,
        avalu_var = avalu_var
      ),
      datasets = data,
      select_validation_rule = list(
        id_var = shinyvalidate::sv_required("Subject Identifier is required"),
        col_by_var = shinyvalidate::compose_rules(
          shinyvalidate::sv_optional(),
          rule_intersection("row_by_var")
        ),
        row_by_var = shinyvalidate::compose_rules(
          shinyvalidate::sv_required("Please select a row by variable."),
          rule_intersection("col_by_var")
        ),
        aval_var = shinyvalidate::sv_required("Please select an analysis variable."),
        avalu_var = shinyvalidate::sv_required("Please select an analysis unit variable.")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("Please select a parameter value."),
        parcat = shinyvalidate::sv_required("Please select a parameter category value.")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      join_keys = get_join_keys(data),
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      join_keys = get_join_keys(data),
      data_extract = list(col_by_var = col_by_var),
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

      teal::validate_inputs(iv_r())

      input_paramcd <- unlist(paramcd$filter)["vars_selected"]
      input_id_var <- names(merged$anl_input_r()$columns_source$id_var)
      input_row_by_var <- names(merged$anl_input_r()$columns_source$row_by_var)
      input_col_by_var <- names(merged$adsl_input_r()$columns_source$col_by_var)
      input_parcat <- unlist(parcat$filter)["vars_selected"]
      input_aval_var <- names(merged$anl_input_r()$columns_source$aval_var)
      input_avalu_var <- names(merged$anl_input_r()$columns_source$avalu_var)

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_col_by_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_id_var, input_paramcd,
          input_row_by_var, input_parcat, input_aval_var, input_avalu_var
        ),
        arm_var = NULL,
        need_arm = FALSE
      )
      NULL
    })

    all_q <- shiny::reactive({
      validate_checks()

      anl_filtered <- merged$anl_q()[[dataname]]
      input_avalu_var <- as.character(
        unique(merged$anl_q()[["ANL"]][[names(merged$anl_input_r()$columns_source$avalu_var)[1]]])
      )
      input_paramcd <- as.character(
        unique(merged$anl_q()[["ANL"]][[names(merged$anl_input_r()$columns_source$paramcd)[1]]])
      )

      if (is.null(paramcd_label)) {
        input_paramcd_label <- input_paramcd
      } else {
        paramcd <- names(merged$anl_input_r()$columns_source$paramcd)
        paramcd_map_list <- c(paramcd, paramcd_label)
        paramcd_map <- unique(anl_filtered[paramcd_map_list])
        input_paramcd_label <- as.character(paramcd_map[paramcd_map[1] == input_paramcd, 2])
      }

      basic_table_args$title <- "Duration of Exposure Table"
      basic_table_args$subtitles <-
        paste("Parameter Category:", merged$anl_input_r()$filter_info$parcat[[1]]$selected[[1]])

      my_calls <- template_exposure(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        id_var <- names(merged$anl_input_r()$columns_source$id_var),
        paramcd <- input_paramcd,
        paramcd_label = input_paramcd_label,
        row_by_var <- names(merged$anl_input_r()$columns_source$row_by_var),
        col_by_var <- names(merged$anl_input_r()$columns_source$col_by_var),
        add_total = input$add_total,
        total_label = total_label,
        drop_levels = TRUE,
        na_level = na_level,
        aval_var <- names(merged$anl_input_r()$columns_source$aval_var),
        avalu_var <- input_avalu_var,
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
        card$set_name("Exposure for Risk Management Plan Table")
        card$append_text("Exposure for Risk Management Plan Table", "header2")
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
