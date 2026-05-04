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
#' @param mhterm ([teal.picks::variables] or [teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `MHTERM` variable from `dataname`.
#' @param mhbodsys ([teal.picks::variables] or [teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `MHBODSYS` variable from `dataname`.
#' @param mhdistat ([teal.picks::variables] or [teal.transform::choices_selected()])\cr object with all
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
#'       mhterm = variables("MHTERM", fixed = TRUE),
#'       mhbodsys = variables("MHBODSYS", fixed = TRUE),
#'       mhdistat = variables("MHDISTAT", fixed = TRUE),
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

  mhterm <- deprecate_pick_variables_arg(mhterm, "mhterm", null.ok = TRUE)
  mhbodsys <- deprecate_pick_variables_arg(mhbodsys, "mhbodsys", null.ok = TRUE)
  mhdistat <- deprecate_pick_variables_arg(mhdistat, "mhdistat", null.ok = TRUE)

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  assert_decorators(decorators, "table")

  # Build picks bound to the dataset
  if (!is.null(mhterm)) mhterm <- teal.picks::picks(datasets(dataname, dataname), mhterm)
  if (!is.null(mhbodsys)) mhbodsys <- teal.picks::picks(datasets(dataname, dataname), mhbodsys)
  if (!is.null(mhdistat)) mhdistat <- teal.picks::picks(datasets(dataname, dataname), mhdistat)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_medical_history,
    ui_args = args[names(args) %in% names(formals(ui_t_medical_history))],
    server = srv_t_medical_history,
    server_args = args[names(args) %in% names(formals(srv_t_medical_history))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_t_medical_history <- function(id,
                                 mhterm,
                                 mhbodsys,
                                 mhdistat,
                                 pre_output,
                                 post_output,
                                 decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      if (!is.null(mhterm)) {
        tags$div(
          tags$label("Select MHTERM variable:"),
          teal.picks::picks_ui(ns("mhterm"), mhterm)
        )
      },
      if (!is.null(mhbodsys)) {
        tags$div(
          tags$label("Select MHBODSYS variable:"),
          teal.picks::picks_ui(ns("mhbodsys"), mhbodsys)
        )
      },
      if (!is.null(mhdistat)) {
        tags$div(
          tags$label("Select MHDISTAT variable:"),
          teal.picks::picks_ui(ns("mhdistat"), mhdistat)
        )
      },
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table"))
    ),
    pre_output = pre_output,
    post_output = post_output
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

    # Patient selector initialisation
    patient_data_base <- reactive(unique(data()[[parentname]][[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session,
      "patient_id",
      choices = patient_data_base(),
      selected = patient_data_base()[1]
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

    # Build selector list — only include non-NULL picks
    picks_list <- Filter(Negate(is.null), list(
      mhterm   = mhterm,
      mhbodsys = mhbodsys,
      mhdistat = mhdistat
    ))

    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId = "mhterm-variables-selected",
        condition = !is.null(selectors$mhterm()$variables$selected),
        message = "Please select MHTERM variable."
      )
      teal:::validate_input(
        inputId = "mhbodsys-variables-selected",
        condition = !is.null(selectors$mhbodsys()$variables$selected),
        message = "Please select MHBODSYS variable."
      )
      teal:::validate_input(
        inputId = "mhdistat-variables-selected",
        condition = !is.null(selectors$mhdistat()$variables$selected),
        message = "Please select MHDISTAT variable."
      )
      teal:::validate_input(
        inputId = "patient_id",
        condition = !is.null(input$patient_id) && length(input$patient_id) > 0,
        message = "Please select a patient"
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's output(s)")
      )
      obj
    })

    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data = validated_q,
      selectors = selectors,
      join_fun = "dplyr::left_join",
      output_name = "ANL"
    )

    all_q <- reactive({
      obj <- anl_inputs$data()

      validate(
        need(
          nrow(obj[["ANL"]][obj[["ANL"]][[patient_col]] == patient_id(), ]) > 0,
          "Patient has no data about medical history."
        )
      )

      my_calls <- template_medical_history(
        dataname = "ANL",
        mhterm = anl_inputs$variables()$mhterm,
        mhbodsys = anl_inputs$variables()$mhbodsys,
        mhdistat = anl_inputs$variables()$mhdistat,
        patient_id = patient_id()
      )

      obj <- teal.code::eval_code(
        obj,
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          },
          env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      obj |> teal.code::eval_code(as.expression(unlist(my_calls)))
    })

    # Decoration of table output
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
