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
#' @param vars (`teal.picks::variables` or `teal.transform::choices_selected`)
#'
#' object specifying available choices and preselected variables from
#' `dataname` to show in the table.
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
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_pp_basic_info.picks(
#'       label = "Basic Info",
#'       dataname = "ADSL",
#'       patient_col = "USUBJID",
#'       vars = teal.picks::variables(
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
                               vars = teal.picks::variables(selected = NULL),
                               pre_output = NULL,
                               post_output = NULL,
                               transformators = list()) {
  message("Initializing tm_t_pp_basic_info")

  # Compatibility layer: convert choices_selected to teal.picks
  if (inherits(vars, "choices_selected")) {
    vars <- teal.picks::as.picks(vars)
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(vars, "variables", null.ok = TRUE)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  vars <- teal.picks::picks(datasets(dataname), vars)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_basic_info,
    ui_args = args[names(args) %in% names(formals(ui_t_basic_info))],
    server = srv_t_basic_info,
    server_args = args[names(args) %in% names(formals(srv_t_basic_info))],
    transformators = transformators,
    datanames = dataname
  )
}

#' @keywords internal
ui_t_basic_info <- function(id,
                            vars,
                            pre_output,
                            post_output) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      htmlOutput(ns("title")),
      DT::DTOutput(outputId = ns("basic_info_table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      tags$div(
        tags$label("Select variable:"),
        teal.picks::picks_ui(ns("vars"), vars)
      )
    ),
    pre_output = pre_output,
    post_output = post_output
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

    # Init patient selector
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

    selectors <- teal.picks::picks_srv(
      picks = list(vars = vars),
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId = "vars-variables-selected",
        condition = !is.null(selectors$vars()$variables$selected),
        message = "Please select basic info variables"
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

      my_calls <- template_basic_info(
        dataname = "ANL",
        vars = anl_inputs$variables()$vars,
        patient_id = patient_id()
      )

      obj <- teal.code::eval_code(
        obj,
        substitute(
          expr = {
            pt_id <- patient_id
            ANL <- ANL[ANL[[patient_col]] == pt_id, ]
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
