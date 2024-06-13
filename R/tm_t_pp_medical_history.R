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
        dplyr::mutate_if(is.factor, function(x) explicit_na(x, "UNKNOWN")) %>%
        dplyr::distinct() %>%
        `colnames<-`(labels)

      result <- rtables::basic_table() %>%
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

      main_title(result) <- paste("Patient ID:", patient_id)

      result
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
#' @inheritParams template_medical_history
#' @param mhterm ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `MHTERM` variable from `dataname`.
#' @param mhbodsys ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `MHBODSYS` variable from `dataname`.
#' @param mhdistat ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `MHDISTAT` variable from `dataname`.
#'
#' @inherit module_arguments return
#'
#' @examples
#' ADSL <- tmc_ex_adsl
#' ADMH <- tmc_ex_admh
#'
#' app <- init(
#'   data = cdisc_data(
#'     ADSL = ADSL,
#'     ADMH = ADMH,
#'     code = "
#'       ADSL <- tmc_ex_adsl
#'       ADMH <- tmc_ex_admh
#'     "
#'   ),
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
                                    post_output = NULL) {
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
        patient_col = patient_col
      )
    ),
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
    output = tags$div(tmctable_with_settings_ui()),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
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
      )
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
                                  reporter,
                                  filter_panel_api,
                                  dataname,
                                  parentname,
                                  patient_col,
                                  mhterm,
                                  mhbodsys,
                                  mhdistat,
                                  label) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    tmc_track_shiny_input_changes()
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
      data() %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

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

      teal.code::eval_code(
        anl_q(),
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      ) %>%
        teal.code::eval_code(as.expression(my_calls))
    })

    table_r <- reactive(all_q()[["result"]])

    tmctable_with_settings_srv(table_r = table_r)

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Patient Medical History Table",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(teal.code::get_code(all_q()))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
