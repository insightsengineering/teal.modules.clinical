#' Template: Patient Profile Medical History
#'
#' Creates a valid expression to generate a patient profile medical history report using ADaM datasets.
#'
#' @inheritParams template_arguments
#' @param mhterm (`character`)\cr name of the reported term for the medical history variable.
#' @param mhbodsys (`character`)\cr name of the body system or organ class variable.
#' @param mhdistat (`character`)\cr name of the status of the disease variable.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_pp_medical_history()]
#'
#' @keywords internal
template_medical_history <- function(dataname = "ANL",
                                     mhterm = "MHTERM",
                                     mhbodsys = "MHBODSYS",
                                     mhdistat = "MHDISTAT",
                                     patient_id = NULL) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(mhterm)
  checkmate::assert_string(mhbodsys)
  checkmate::assert_string(mhdistat)

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      labels <- teal.data::col_labels(dataname, fill = FALSE)[c(mhbodsys_char, mhterm_char, mhdistat_char)]
      mhbodsys_label <- labels[mhbodsys_char]

      result_raw <-
        dataname %>%
        dplyr::select(mhbodsys, mhterm, mhdistat) %>%
        dplyr::arrange(mhbodsys) %>%
        dplyr::mutate_if(is.character, as.factor) %>%
        dplyr::mutate_if(is.factor, function(x) tern::explicit_na(x, "UNKNOWN")) %>%
        dplyr::distinct() %>%
        `colnames<-`(labels)

      table <- rtables::basic_table() %>%
        rtables::split_cols_by_multivar(colnames(result_raw)[2:3]) %>%
        rtables::split_rows_by(
          colnames(result_raw)[1],
          split_fun = rtables::drop_split_levels
        ) %>%
        rtables::split_rows_by(
          colnames(result_raw)[2],
          split_fun = rtables::drop_split_levels,
          child_labels = "hidden"
        ) %>%
        rtables::analyze_colvars(function(x) x[seq_along(x)]) %>%
        rtables::build_table(result_raw)

      rtables::main_title(table) <- paste("Patient ID:", patient_id)
    }, env = list(
      dataname = as.name(dataname),
      mhbodsys = as.name(mhbodsys),
      mhterm = as.name(mhterm),
      mhdistat = as.name(mhdistat),
      mhbodsys_char = mhbodsys,
      mhterm_char = mhterm,
      mhdistat_char = mhdistat,
      patient_id = patient_id
    ))
  )

  y$table <- bracket_expr(table_list)

  y
}

#' teal Module: Patient Profile Medical History
#'
#' This module produces a patient profile medical history report using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_medical_history
#' @param mhterm ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `MHTERM` variable from `dataname`.
#' @param mhbodsys ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `MHBODSYS` variable from `dataname`.
#' @param mhdistat ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `MHDISTAT` variable from `dataname`.
#'
#' @inherit module_arguments return
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
#' tm_t_pp_medical_history(
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
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADMH <- tmc_ex_admh
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADMH <- data[["ADMH"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_pp_medical_history(
#'       label = "Medical History",
#'       dataname = "ADMH",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       mhterm = choices_selected(
#'         choices = variable_choices(ADMH, c("MHTERM")),
#'         selected = "MHTERM"
#'       ),
#'       mhbodsys = choices_selected(
#'         choices = variable_choices(ADMH, "MHBODSYS"),
#'         selected = "MHBODSYS"
#'       ),
#'       mhdistat = choices_selected(
#'         choices = variable_choices(ADMH, "MHDISTAT"),
#'         selected = "MHDISTAT"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_pp_medical_history <- function(label,
                                    dataname = "ADMH",
                                    parentname = "ADSL",
                                    patient_col = "USUBJID",
                                    mhterm = NULL,
                                    mhbodsys = NULL,
                                    mhdistat = NULL,
                                    pre_output = NULL,
                                    post_output = NULL,
                                    transformators = list(),
                                    decorators = list()) {
  message("Initializing tm_t_pp_medical_history")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(mhterm, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(mhbodsys, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(mhdistat, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  assert_decorators(decorators, "table")

  args <- as.list(environment())
  data_extract_list <- list(
    mhterm = `if`(is.null(mhterm), NULL, cs_to_des_select(mhterm, dataname = dataname)),
    mhbodsys = `if`(is.null(mhbodsys), NULL, cs_to_des_select(mhbodsys, dataname = dataname)),
    mhdistat = `if`(is.null(mhdistat), NULL, cs_to_des_select(mhdistat, dataname = dataname))
  )

  module(
    label = label,
    ui = ui_t_medical_history,
    ui_args = c(data_extract_list, args),
    server = srv_t_medical_history,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        patient_col = patient_col,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_t_medical_history <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$mhterm,
    ui_args$mhbodsys,
    ui_args$mhdistat
  )

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(ui_args[c("mhterm", "mhbodsys", "mhdistat")]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("mhterm"),
        label = "Select MHTERM variable:",
        data_extract_spec = ui_args$mhterm,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("mhbodsys"),
        label = "Select MHBODSYS variable:",
        data_extract_spec = ui_args$mhbodsys,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("mhdistat"),
        label = "Select MHDISTAT variable:",
        data_extract_spec = ui_args$mhdistat,
        is_single_dataset = is_single_dataset_value
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(ui_args$decorators, "table"))
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

#' @keywords internal
srv_t_medical_history <- function(id,
                                  data,
                                  dataname,
                                  parentname,
                                  patient_col,
                                  mhterm,
                                  mhbodsys,
                                  mhdistat,
                                  label,
                                  decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    patient_id <- reactive(input$patient_id)

    # Init
    patient_data_base <- reactive(unique(data()[[parentname]][[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session, "patient_id",
      choices = patient_data_base(), selected = patient_data_base()[1]
    )

    observeEvent(patient_data_base(),
      handlerExpr = {
        teal.widgets::updateOptionalSelectInput(
          session,
          "patient_id",
          choices = patient_data_base(),
          selected = if (length(patient_data_base()) == 1) {
            patient_data_base()
          } else {
            intersect(patient_id(), patient_data_base())
          }
        )
      },
      ignoreInit = TRUE
    )

    # Medical history tab ----
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(mhterm = mhterm, mhbodsys = mhbodsys, mhdistat = mhdistat),
      datasets = data,
      select_validation_rule = list(
        mhterm = shinyvalidate::sv_required("Please select MHTERM variable."),
        mhbodsys = shinyvalidate::sv_required("Please select MHBODSYS variable."),
        mhdistat = shinyvalidate::sv_required("Please select MHDISTAT variable.")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select a patient"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::left_join"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Patient Medical History Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>% teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    # Generate r code for the analysis.
    all_q <- reactive({
      teal::validate_inputs(iv_r())

      validate(
        need(
          nrow(anl_q()[["ANL"]][anl_q()[["ANL"]][[patient_col]] == patient_id(), ]) > 0,
          "Patient has no data about medical history."
        )
      )

      my_calls <- template_medical_history(
        dataname = "ANL",
        mhterm = input[[extract_input("mhterm", dataname)]],
        mhbodsys = input[[extract_input("mhbodsys", dataname)]],
        mhdistat = input[[extract_input("mhdistat", dataname)]],
        patient_id = patient_id()
      )

      obj <- teal.code::eval_code(
        anl_q(),
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
      obj %>% teal.code::eval_code(as.expression(unlist(my_calls)))
    })

    # Decoration of table output.
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
