#' Template: Medical History
#'
#' Creates medical history template.
#'
#' @inheritParams template_arguments
#' @param mhterm (`character`)\cr name of the reported name for medical history variable.
#' @param mhbodsys (`character`)\cr name of the body system or organ class variable.
#' @param mhdistat (`character`)\cr name of the status of the disease variable.
#' @keywords internal
#'
template_medical_history <- function(dataname = "ANL",
                                     mhterm = "MHTERM",
                                     mhbodsys = "MHBODSYS",
                                     mhdistat = "MHDISTAT") {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(mhterm),
    assertthat::is.string(mhbodsys),
    assertthat::is.string(mhdistat)
  )

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      labels <- formatters::var_labels(dataname, fill = FALSE)[c(mhbodsys_char, mhterm_char, mhdistat_char)]
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

      result
    }, env = list(
      dataname = as.name(dataname),
      mhbodsys = as.name(mhbodsys),
      mhterm = as.name(mhterm),
      mhdistat = as.name(mhdistat),
      mhbodsys_char = mhbodsys,
      mhterm_char = mhterm,
      mhdistat_char = mhdistat
    ))
  )

  y$table <- bracket_expr(table_list)

  y
}

#' Teal Module: Patient Medical History Teal Module
#'
#' This teal module produces a patient medical history report using `ADaM` datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param mhterm ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   `MHTERM` column of the `ADMH` dataset.
#' @param mhbodsys ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   `MHBODSYS` column of the `ADMH` dataset.
#' @param mhdistat ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   `MHDISTAT` column of the `ADMH` dataset.
#'
#' @export
#'
#' @examples
#' adsl <- tmc_ex_adsl
#' admh <- tmc_ex_admh
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADMH", admh)
#'   ),
#'   modules = modules(
#'     tm_t_pp_medical_history(
#'       label = "Medical History",
#'       dataname = "ADMH",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       mhterm = choices_selected(
#'         choices = variable_choices(admh, c("MHTERM")),
#'         selected = "MHTERM"
#'       ),
#'       mhbodsys = choices_selected(
#'         choices = variable_choices(admh, "MHBODSYS"),
#'         selected = "MHBODSYS"
#'       ),
#'       mhdistat = choices_selected(
#'         choices = variable_choices(admh, "MHDISTAT"),
#'         selected = "MHDISTAT"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_pp_medical_history <- function(label,
                                    dataname = "ADMH",
                                    parentname = "ADSL",
                                    patient_col = "USUBJID",
                                    mhterm = NULL,
                                    mhbodsys = NULL,
                                    mhdistat = NULL,
                                    pre_output = NULL,
                                    post_output = NULL) {
  logger::log_info("Initializing tm_t_pp_medical_history")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
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
    datanames = "all"
  )
}

ui_t_medical_history <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$mhterm,
    ui_args$mhbodsys,
    ui_args$mhdistat
  )

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = shiny::div(
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
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
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}


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
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    patient_id <- shiny::reactive(input$patient_id)

    # Init
    patient_data_base <- shiny::reactive(unique(data[[parentname]]()[[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session, "patient_id",
      choices = patient_data_base(), selected = patient_data_base()[1]
    )

    shiny::observeEvent(patient_data_base(),
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

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select a patient"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      join_keys = get_join_keys(data),
      selector_list = selector_list,
      merge_function = "dplyr::left_join"
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    all_q <- shiny::reactive({
      teal::validate_inputs(iv_r())

      shiny::validate(
        shiny::need(
          nrow(anl_q()[["ANL"]][anl_q()[["ANL"]][[patient_col]] == patient_id(), ]) > 0,
          "Patient has no data about medical history."
        )
      )

      my_calls <- template_medical_history(
        dataname = "ANL",
        mhterm = input[[extract_input("mhterm", dataname)]],
        mhbodsys = input[[extract_input("mhbodsys", dataname)]],
        mhdistat = input[[extract_input("mhdistat", dataname)]]
      )

      teal.code::eval_code(
        anl_q(),
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      ) %>%
        teal.code::eval_code(as.expression(my_calls))
    })

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

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Patient Medical History Table")
        card$append_text("Patient Medical History Table", "header2")
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
