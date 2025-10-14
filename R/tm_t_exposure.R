#' Template: Exposure Table for Risk management plan
#'
#' Creates a valid expression to generate exposure table for risk management plan.
#'
#' @inheritParams template_arguments
#' @param row_by_var (`character`)\cr variable name used to split the values by rows.
#' @param col_by_var (`character`)\cr variable name used to split the values by columns.
#' @param drop_levels (`flag`)\cr whether empty rows should be removed from the table.
#' @param paramcd_label (`character`)\cr the column from the `dataname` dataset where the
#'   value will be used to label the argument `paramcd`.
#' @param add_total_row (`flag`)\cr whether a "total" level should be added after the others which includes all the
#'   levels that constitute the split. A custom label can be set for this level via the `total_row_label` argument.
#' @param total_row_label (`character`)\cr string to display as total row label if row is
#'   enabled (see `add_total_row`).
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_exposure()]
#'
#' @keywords internal
template_exposure <- function(parentname,
                              dataname,
                              id_var,
                              paramcd,
                              paramcd_label = NULL,
                              row_by_var,
                              col_by_var = NULL,
                              add_total = FALSE,
                              total_label = "Total",
                              add_total_row = TRUE,
                              total_row_label = "Total number of patients and patient time*",
                              drop_levels = TRUE,
                              na_level = tern::default_na_str(),
                              aval_var,
                              avalu_var,
                              basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(row_by_var)
  checkmate::assert_character(col_by_var, null.ok = TRUE)
  checkmate::assert_string(paramcd)
  checkmate::assert_string(id_var)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(add_total_row)
  checkmate::assert_string(total_row_label)
  checkmate::assert_string(na_level)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(avalu_var, null.ok = TRUE)
  checkmate::assert_flag(drop_levels)

  y <- list()
  data_list <- list()

  if (length(col_by_var) == 0) {
    col_by_var <- NULL
  }

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
      dataname <- tern::df_explicit_na(dataname, na_level = na_str),
      env = list(
        dataname = as.name("anl"),
        na_str = na_level
      )
    )
  )
  y$data <- bracket_expr(data_list)

  # layout start
  y$layout_prep <- quote(split_fun <- rtables::drop_split_levels)

  if (is.null(paramcd_label)) {
    paramcd_label <- paramcd
  }

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        show_colcounts = TRUE,
        main_footer = paste0("* Patient time is the sum of ", paramcd_label)
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
          rtables::split_cols_by(col_by_var, split_fun = rtables::add_overall_level(total_label, first = FALSE)),
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
    substitute(
      tern::analyze_patients_exposure_in_cols(
        var = row_by_var,
        ex_var = aval_var,
        col_split = TRUE,
        add_total_level = add_total_row,
        na_str = na_str,
        .labels = c(
          n_patients = "Number of Patients",
          sum_exposure = ifelse(
            avalu_var == " ",
            paste("Sum of", paramcd),
            paste("Sum of", paramcd, sprintf("(%s)", avalu_var))
          )
        ),
        custom_label = total_row_label
      ),
      env = list(
        row_by_var = row_by_var,
        aval_var = aval_var,
        add_total_row = add_total_row,
        na_str = na_level,
        avalu_var = avalu_var,
        paramcd = paramcd,
        total_row_label = total_row_label
      )
    )
  )

  split_label <- substitute(
    expr = teal.data::col_labels(dataname[row_by_var], fill = TRUE),
    env = list(
      dataname = as.name(dataname),
      row_by_var = row_by_var
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      tern::analyze_patients_exposure_in_cols(
        var = row_by_var,
        col_split = FALSE,
        na_str = na_str
      ) %>%
        rtables::append_topleft(c(split_label)),
      env = list(
        row_by_var = row_by_var,
        na_str = na_level,
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
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
    },
    env = list(parent = as.name(parentname))
  )

  if (drop_levels) {
    y$table <- substitute(
      expr = {
        table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
        table <- rtables::prune_table(table)
      },
      env = list(parent = as.name(parentname))
    )
  }
  y
}

#' teal Module: Exposure Table for Risk management plan
#'
#' The module produces an exposure table for risk management plan.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_exposure
#' @param row_by_var ([teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for
#'   variable names that can be used to split rows.
#' @param col_by_var ([teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for
#'   variable names that can be used to split columns.
#' @param parcat ([teal.transform::choices_selected()])\cr
#'   object with all available choices and preselected option for
#'   parameter category values.
#' @param paramcd_label (`character`)\cr the column from the dataset where the value will be used to
#'   label the argument `paramcd`.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`ElementaryTable` as created from `rtables::build_table`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_exposure(
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
#'   ADEX <- tmc_ex_adex
#'
#'   set.seed(1, kind = "Mersenne-Twister")
#'   .labels <- col_labels(ADEX, fill = FALSE)
#'   ADEX <- ADEX %>%
#'     distinct(USUBJID, .keep_all = TRUE) %>%
#'     mutate(
#'       PARAMCD = "TDURD",
#'       PARAM = "Overall duration (days)",
#'       AVAL = sample(x = seq(1, 200), size = n(), replace = TRUE),
#'       AVALU = "Days"
#'     ) %>%
#'     bind_rows(ADEX)
#'   col_labels(ADEX) <- .labels
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_exposure(
#'       label = "Duration of Exposure Table",
#'       dataname = "ADEX",
#'       paramcd = choices_selected(
#'         choices = value_choices(data[["ADEX"]], "PARAMCD", "PARAM"),
#'         selected = "TDURD"
#'       ),
#'       col_by_var = choices_selected(
#'         choices = variable_choices(data[["ADEX"]], subset = c("SEX", "ARM")),
#'         selected = "SEX"
#'       ),
#'       row_by_var = choices_selected(
#'         choices = variable_choices(data[["ADEX"]], subset = c("RACE", "REGION1", "STRATA1", "SEX")),
#'         selected = "RACE"
#'       ),
#'       parcat = choices_selected(
#'         choices = value_choices(data[["ADEX"]], "PARCAT2"),
#'         selected = "Drug A"
#'       ),
#'       add_total = FALSE
#'     )
#'   ),
#'   filter = teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y"))
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
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
                          total_label = default_total_label(),
                          add_total_row = TRUE,
                          total_row_label = "Total number of patients and patient time*",
                          na_level = tern::default_na_str(),
                          pre_output = NULL,
                          post_output = NULL,
                          basic_table_args = teal.widgets::basic_table_args(),
                          transformators = list(),
                          decorators = list()) {
  message("Initializing tm_t_exposure")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_class(row_by_var, "choices_selected")
  checkmate::assert_class(col_by_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(parcat, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(avalu_var, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(add_total_row)
  checkmate::assert_string(total_row_label)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

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
        total_row_label = total_row_label,
        na_level = na_level,
        basic_table_args = basic_table_args,
        paramcd_label = paramcd_label,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}


#' @keywords internal
ui_t_exposure <- function(id, ...) {
  ns <- NS(id)
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
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
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
      checkboxInput(ns("add_total_row"), "Add Total row", value = a$add_total_row),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
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
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_exposure <- function(id,
                           data,
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
                           total_row_label,
                           basic_table_args = basic_table_args,
                           decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
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

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(col_by_var = col_by_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Exposure"),
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

    all_q <- reactive({
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
        id_var = names(merged$anl_input_r()$columns_source$id_var),
        paramcd = input_paramcd,
        paramcd_label = input_paramcd_label,
        row_by_var = names(merged$anl_input_r()$columns_source$row_by_var),
        col_by_var = names(merged$anl_input_r()$columns_source$col_by_var),
        add_total = input$add_total,
        total_label = total_label,
        add_total_row = input$add_total_row,
        total_row_label = total_row_label,
        drop_levels = TRUE,
        na_level = na_level,
        aval_var = names(merged$anl_input_r()$columns_source$aval_var),
        avalu_var = input_avalu_var,
        basic_table_args = basic_table_args
      )
      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Outputs to render.
    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
