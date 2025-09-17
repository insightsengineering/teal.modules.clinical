#' Template: Patient Profile Basic Info
#'
#' Creates a valid expression to generate a patient profile basic info report using ADaM datasets.
#'
#' @inheritParams template_arguments
#' @param vars (`character`)\cr names of the variables to be shown in the table.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_pp_basic_info()]
#'
#' @keywords internal
template_basic_info <- function(dataname = "ANL",
                                vars,
                                patient_id = NULL) {
  checkmate::assert_string(dataname)
  checkmate::assert_character(vars, min.len = 1)

  y <- list()
  y$table <- list()

  table_list <- add_expr(
    list(),
    substitute(
      expr = {
        values <- dataname %>%
          dplyr::select(vars) %>%
          # we are sure that only one row
          utils::head(1) %>%
          t()

        key <- teal.data::col_labels(dataname, fill = TRUE)[rownames(values)]

        table_data <-
          data.frame(var = rownames(values), key = key, value = values) %>%
          dplyr::select(var, key, value) %>%
          dplyr::rename(` ` = var, `  ` = key, `   ` = value)

        table <- rtables::df_to_tt(table_data)
        table
      }, env = list(
        dataname = as.name(dataname),
        vars = vars,
        patient_id = patient_id
      )
    )
  )
  y$table <- bracket_expr(table_list)

  y
}

#' teal Module: Patient Profile Basic Info
#'
#' This module produces a patient profile basic info report using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_basic_info
#' @param vars ([teal.transform::choices_selected()])\cr  object with all
#'   available choices and preselected option for variables from `dataname` to show in the table.
#'
#' @inherit module_arguments return
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
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_pp_basic_info(
#'       label = "Basic Info",
#'       dataname = "ADSL",
#'       patient_col = "USUBJID",
#'       vars = choices_selected(
#'         choices = variable_choices(ADSL),
#'         selected = c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT")
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_pp_basic_info <- function(label,
                               dataname = "ADSL",
                               patient_col = "USUBJID",
                               vars = NULL,
                               pre_output = NULL,
                               post_output = NULL,
                               transformators = list(),
                               decorators = lifecycle::deprecated()) {
  message("Initializing tm_t_pp_basic_info")

  if (lifecycle::is_present(decorators)) {
    lifecycle::deprecate_warn(
      when = "0.11.0",
      what = "tm_t_pp_laboratory(decorators)",
      details = "Decorators functionality was removed from this module. The `decorators` argument will be ignored."
    )
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(vars, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  args <- as.list(environment())
  data_extract_list <- list(
    vars = `if`(is.null(vars), NULL, cs_to_des_select(vars, dataname = dataname, multiple = TRUE))
  )

  module(
    label = label,
    ui = ui_t_basic_info,
    ui_args = c(data_extract_list, args),
    server = srv_t_basic_info,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        label = label,
        patient_col = patient_col
      )
    ),
    transformators = transformators,
    datanames = dataname
  )
}

#' @keywords internal
ui_t_basic_info <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(ui_args$vars)

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      htmlOutput(ns("title")),
      DT::DTOutput(outputId = ns("basic_info_table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(ui_args[c("vars")]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("vars"),
        label = "Select variable:",
        data_extract_spec = ui_args$vars,
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
srv_t_basic_info <- function(id,
                             data,
                             dataname,
                             patient_col,
                             vars,
                             label) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    patient_id <- reactive(input$patient_id)

    # Init
    patient_data_base <- reactive(unique(data()[[dataname]][[patient_col]]))
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

    # Basic Info tab ----
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(vars = vars),
      datasets = data,
      select_validation_rule = list(
        vars = shinyvalidate::sv_required("Please select basic info variables")
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
          teal.reporter::teal_card("# Patient Profile Basic Info Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    all_q <- reactive({
      teal::validate_inputs(iv_r())
      my_calls <- template_basic_info(
        dataname = "ANL",
        vars = anl_inputs()$columns_source$vars,
        patient_id = patient_id()
      )

      obj <- teal.code::eval_code(
        anl_q(),
        substitute(
          expr = {
            pt_id <- patient_id
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

    table_r <- reactive({
      q <- req(all_q())

      list(
        html = DT::datatable(
          data = q[["table_data"]],
          options = list(
            lengthMenu = list(list(-1, 5, 10, 25), list("All", "5", "10", "25"))
          )
        ),
        report = q[["table"]]
      )
    })

    output$title <- renderText({
      paste("<h5><b>Patient ID:", all_q()[["pt_id"]], "</b></h5>")
    })

    output$basic_info_table <- DT::renderDataTable(table_r()[["html"]])

    all_q
  })
}
