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

        key <- col_labels(dataname, fill = TRUE)[rownames(values)]

        result <-
          data.frame(var = rownames(values), key = key, value = values) %>%
          dplyr::select(var, key, value) %>%
          dplyr::rename(` ` = var, `  ` = key, `   ` = value)

        result <- rlistings::as_listing(
          result,
          default_formatting = list(all = fmt_config(align = "left"))
        )
        main_title(result) <- paste("Patient ID:", patient_id)

        result
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
#' @inheritParams template_basic_info
#' @param vars ([teal.transform::choices_selected()])\cr  object with all
#'   available choices and preselected option for variables from `dataname` to show in the table.
#'
#' @inherit module_arguments return
#'
#' @examples
#' ADSL <- tmc_ex_adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     ADSL = ADSL,
#'     code = "ADSL <- tmc_ex_adsl"
#'   ),
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
                               post_output = NULL) {
  logger::log_info("Initializing tm_t_pp_basic_info")
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
    datanames = dataname
  )
}

#' @keywords internal
ui_t_basic_info <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(ui_args$vars)

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = shiny::tags$div(
      shiny::htmlOutput(ns("title")),
      DT::DTOutput(outputId = ns("basic_info_table"))
    ),
    encoding = shiny::tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
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
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

#' @keywords internal
srv_t_basic_info <- function(id,
                             data,
                             reporter,
                             filter_panel_api,
                             dataname,
                             patient_col,
                             vars,
                             label) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  shiny::moduleServer(id, function(input, output, session) {
    patient_id <- shiny::reactive(input$patient_id)

    # Init
    patient_data_base <- shiny::reactive(unique(data()[[dataname]][[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session,
      "patient_id",
      choices = patient_data_base(),
      selected = patient_data_base()[1]
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

    # Basic Info tab ----
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(vars = vars),
      datasets = data,
      select_validation_rule = list(
        vars = shinyvalidate::sv_required("Please select basic info variables")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select a patient"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::left_join"
    )

    anl_q <- shiny::reactive({
      data() %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    all_q <- shiny::reactive({
      teal::validate_inputs(iv_r())
      my_calls <- template_basic_info(
        dataname = "ANL",
        vars = anl_inputs()$columns_source$vars,
        patient_id = patient_id()
      )

      teal.code::eval_code(
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
      ) %>%
        teal.code::eval_code(as.expression(my_calls))
    })

    output$title <- shiny::renderText({
      paste("<h5><b>Patient ID:", all_q()[["pt_id"]], "</b></h5>")
    })

    table_r <- shiny::reactive(all_q()[["result"]])

    output$basic_info_table <- DT::renderDataTable(
      expr = table_r(),
      options = list(
        lengthMenu = list(list(-1, 5, 10, 25), list("All", "5", "10", "25"))
      )
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
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Patient Profile Basic Info Table",
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
