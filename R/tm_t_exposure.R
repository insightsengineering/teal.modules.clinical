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
#' @param row_by_var ([teal.picks::variables()])\cr
#'   variable names that can be used to split rows (`dataname`).
#' @param col_by_var ([teal.picks::variables()])\cr
#'   variable names that can be used to split columns (`parentname`).
#' @param paramcd ([teal.picks::variables()])\cr variable used to filter by parameter (`dataname`);
#'   `values()` is added internally.
#' @param id_var ([teal.picks::variables()])\cr subject identifier (`dataname`).
#' @param parcat ([teal.picks::variables()])\cr parameter category column on `dataname`; `values()` is added internally.
#' @param aval_var ([teal.picks::variables()])\cr analysis value variable (`dataname`).
#' @param avalu_var ([teal.picks::variables()])\cr analysis value unit variable (`dataname`).
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
#'       paramcd = variables(choices = "PARAMCD"),
#'       col_by_var = variables(
#'         choices = c("SEX", "ARM"),
#'         selected = "SEX"
#'       ),
#'       row_by_var = variables(
#'         choices = c("RACE", "REGION1", "STRATA1", "SEX"),
#'         selected = "RACE"
#'       ),
#'       parcat = variables(choices = "PARCAT2"),
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
                          parentname = "ADSL",
                          row_by_var,
                          col_by_var,
                          paramcd = variables(choices = "PARAMCD"),
                          paramcd_label = "PARAM",
                          id_var = variables(choices = "USUBJID"),
                          parcat,
                          aval_var = variables(choices = "AVAL"),
                          avalu_var = variables(choices = "AVALU"),
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
  checkmate::assert_class(row_by_var, "variables")
  checkmate::assert_class(col_by_var, "variables")
  checkmate::assert_class(paramcd, "variables")
  checkmate::assert_class(id_var, "variables")
  checkmate::assert_class(parcat, "variables")
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(avalu_var, "variables")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(add_total_row)
  checkmate::assert_string(total_row_label)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  paramcd <- picks(datasets(dataname, dataname), paramcd, values())
  row_by_var <- picks(datasets(dataname, dataname), row_by_var)
  col_by_var <- picks(datasets(parentname, parentname), col_by_var)
  id_var <- picks(datasets(dataname, dataname), id_var)
  parcat <- picks(datasets(dataname, dataname), parcat, values())
  aval_var <- picks(datasets(dataname, dataname), aval_var)
  avalu_var <- picks(datasets(dataname, dataname), avalu_var)

  args <- as.list(environment())
  module(
    label = label,
    ui = ui_t_exposure,
    server = srv_t_exposure,
    ui_args = args[names(args) %in% names(formals(ui_t_exposure))],
    server_args = args[names(args) %in% names(formals(srv_t_exposure))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}


#' @keywords internal
ui_t_exposure <- function(id,
                          paramcd,
                          parcat,
                          col_by_var,
                          row_by_var,
                          id_var,
                          aval_var,
                          avalu_var,
                          add_total_row,
                          add_total,
                          pre_output,
                          post_output,
                          decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(tags$label("Select the Parameter"), picks_ui(ns("paramcd"), paramcd)),
      tags$div(tags$label("Select the Parameter Category"), picks_ui(ns("parcat"), parcat)),
      tags$div(tags$label("Select Column by Variable"), picks_ui(ns("col_by_var"), col_by_var)),
      tags$div(tags$label("Select Row by Variable"), picks_ui(ns("row_by_var"), row_by_var)),
      checkboxInput(ns("add_total_row"), "Add Total row", value = add_total_row),
      checkboxInput(ns("add_total"), "Add All Patients column", value = add_total),
      teal::ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          tags$div(tags$label("Subject Identifier"), picks_ui(ns("id_var"), id_var)),
          tags$div(tags$label("Analysis Value Variable"), picks_ui(ns("aval_var"), aval_var)),
          tags$div(tags$label("Analysis Value Unit Variable"), picks_ui(ns("avalu_var"), avalu_var))
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
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
                           basic_table_args,
                           decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- picks_srv(
      id = "",
      picks = list(
        id_var = id_var,
        paramcd = paramcd,
        row_by_var = row_by_var,
        col_by_var = col_by_var,
        parcat = parcat,
        aval_var = aval_var,
        avalu_var = avalu_var
      ),
      data = data
    )

    anl_selectors <- selectors
    adsl_selectors <- selectors["col_by_var"]

    data_with_card <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj
    })
    merged_anl <- merge_srv(
      "merge_anl", data = data_with_card, selectors = anl_selectors, output_name = "ANL"
    )
    merged_adsl_anl <- merge_srv(
      "merge_adsl_anl", data = merged_anl$data, selectors = adsl_selectors, output_name = "ANL_ADSL"
    )
    anl_q <- merged_adsl_anl$data

    validate_checks <- reactive({
      input_id <- as.vector(anl_selectors$id_var()$variables$selected)
      input_row <- as.vector(anl_selectors$row_by_var()$variables$selected)
      input_col <- as.vector(anl_selectors$col_by_var()$variables$selected)
      input_aval <- as.vector(anl_selectors$aval_var()$variables$selected)
      input_avalu <- as.vector(anl_selectors$avalu_var()$variables$selected)

      validate(
        need(length(input_id) >= 1L, "Subject Identifier is required"),
        need(length(input_row) >= 1L, "Please select a row by variable."),
        need(length(input_aval) >= 1L, "Please select an analysis variable."),
        need(length(input_avalu) >= 1L, "Please select an analysis unit variable."),
        need(
          length(intersect(input_row, input_col)) == 0L,
          "Column by and row by variables should not be the same."
        )
      )

      pc_param <- anl_selectors$paramcd()
      pc_vals <- if (is.null(pc_param$values)) character(0) else pc_param$values$selected
      validate(need(length(pc_vals) >= 1L, "Please select a parameter value."))

      pc_parcat <- anl_selectors$parcat()
      parcat_vals <- if (is.null(pc_parcat$values)) character(0) else pc_parcat$values$selected
      validate(need(length(parcat_vals) >= 1L, "Please select a parameter category value."))

      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_paramcd_var <- as.vector(anl_selectors$paramcd()$variables$selected)
      input_parcat_var <- as.vector(anl_selectors$parcat()$variables$selected)

      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_col),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_id, input_paramcd_var,
          input_row, input_parcat_var, input_aval, input_avalu
        ),
        arm_var = NULL,
        need_arm = FALSE
      )
      NULL
    })

    all_q <- reactive({
      validate_checks()

      anl_filtered <- anl_q()[[dataname]]
      anl_merged <- anl_q()[["ANL"]]

      input_avalu_name <- as.vector(anl_selectors$avalu_var()$variables$selected)[[1]]
      input_avalu_var <- as.character(unique(anl_merged[[input_avalu_name]]))

      input_paramcd_name <- as.vector(anl_selectors$paramcd()$variables$selected)[[1]]
      input_paramcd <- as.character(unique(anl_merged[[input_paramcd_name]]))

      if (is.null(paramcd_label)) {
        input_paramcd_label <- input_paramcd
      } else {
        paramcd_col <- input_paramcd_name
        paramcd_map_list <- c(paramcd_col, paramcd_label)
        paramcd_map <- unique(anl_filtered[paramcd_map_list])
        input_paramcd_label <- as.character(
          paramcd_map[paramcd_map[[paramcd_col]] == input_paramcd[1], paramcd_label, drop = TRUE][1]
        )
      }

      parcat_pick <- anl_selectors$parcat()
      parcat_sel <- if (is.null(parcat_pick$values)) character(0) else parcat_pick$values$selected[[1]]

      basic_table_args$title <- "Duration of Exposure Table"
      basic_table_args$subtitles <- paste("Parameter Category:", parcat_sel)

      input_col_names <- as.vector(anl_selectors$col_by_var()$variables$selected)

      my_calls <- template_exposure(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        id_var = as.vector(anl_selectors$id_var()$variables$selected),
        paramcd = input_paramcd,
        paramcd_label = input_paramcd_label,
        row_by_var = as.vector(anl_selectors$row_by_var()$variables$selected),
        col_by_var = input_col_names,
        add_total = input$add_total,
        total_label = total_label,
        add_total_row = input$add_total_row,
        total_row_label = total_row_label,
        drop_levels = TRUE,
        na_level = na_level,
        aval_var = as.vector(anl_selectors$aval_var()$variables$selected),
        avalu_var = input_avalu_var,
        basic_table_args = basic_table_args
      )
      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Outputs to render.
    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
