#' Template: Patient Profile Laboratory Table
#'
#' Creates a valid expression to generate a patient profile laboratory table using ADaM datasets.
#'
#' @inheritParams template_arguments
#' @param paramcd (`character`)\cr name of the parameter code variable.
#' @param param (`character`)\cr name of the parameter variable.
#' @param timepoints (`character`)\cr name of time variable.
#' @param anrind (`character`)\cr name of the analysis reference range indicator variable.
#' @param round_value (`numeric`)\cr number of decimal places to round to.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_pp_laboratory()]
#'
#' @keywords internal
template_laboratory <- function(dataname = "ANL",
                                paramcd = "PARAMCD",
                                param = "PARAM",
                                anrind = "ANRIND",
                                timepoints = "ADY",
                                aval_var = "AVAL",
                                avalu_var = "AVALU",
                                patient_id = NULL,
                                round_value = 0L) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(paramcd)
  checkmate::assert_string(param)
  checkmate::assert_string(anrind)
  checkmate::assert_string(timepoints)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(avalu_var)
  checkmate::assert_integer(round_value, lower = 0)

  y <- list()
  y$table <- list()

  table_lab_list <- add_expr(
    list(),
    substitute(
      expr = {
        dataname[, aval_char] <- round(dataname[, aval_char], round_value)
        labor_table_base <- dataname %>%
          dplyr::select(timepoints, paramcd, param, aval_var, avalu_var, anrind) %>%
          dplyr::arrange(timepoints) %>%
          dplyr::select(-timepoints) %>%
          dplyr::group_by(paramcd, param) %>%
          dplyr::mutate(INDEX = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(aval_anrind = paste(aval_var, anrind)) %>%
          dplyr::select(-c(aval_var, anrind))

        table_data <- labor_table_base %>%
          as.data.frame() %>%
          stats::reshape(
            direction = "wide",
            idvar = c(paramcd_char, param_char, avalu_char),
            v.names = "aval_anrind",
            timevar = "INDEX"
          )
        colnames(table_data)[-c(1:3)] <- unique(labor_table_base$INDEX)

        table_data[[param_char]] <- teal.modules.clinical::clean_description(table_data[[param_char]])

        table <- rlistings::as_listing(
          table_data,
          key_cols = NULL,
          default_formatting = list(all = formatters::fmt_config(align = "left"))
        )
        rtables::main_title(table) <- paste("Patient ID:", patient_id)
        table

        table_data_html <- labor_table_base %>%
          dplyr::mutate(aval_anrind_col = color_lab_values(aval_anrind)) %>%
          dplyr::select(-aval_anrind) %>%
          as.data.frame() %>%
          stats::reshape(
            direction = "wide",
            idvar = c(paramcd_char, param_char, avalu_char),
            v.names = "aval_anrind_col",
            timevar = "INDEX"
          )

        colnames(table_data_html)[-c(1:3)] <- unique(labor_table_base$INDEX)
        table_data_html[[param_char]] <- teal.modules.clinical::clean_description(table_data_html[[param_char]])
      },
      env = list(
        dataname = as.name(dataname),
        param = as.name(param),
        param_char = param,
        paramcd = as.name(paramcd),
        paramcd_char = paramcd,
        aval_var = as.name(aval_var),
        aval_char = aval_var,
        avalu_var = as.name(avalu_var),
        avalu_char = avalu_var,
        timepoints = as.name(timepoints),
        anrind = as.name(anrind),
        patient_id = patient_id,
        round_value = round_value
      )
    )
  )

  y$table <- bracket_expr(table_lab_list)
  y
}

#' teal Module: Patient Profile Laboratory Table
#'
#' This module produces a patient profile laboratory table using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_laboratory
#' @inheritParams template_arguments
#' @param param ([teal.picks::variables] or [teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `PARAM` variable from `dataname`.
#' @param timepoints ([teal.picks::variables] or [teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the time variable from `dataname`.
#' @param anrind ([teal.picks::variables] or [teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `ANRIND` variable from `dataname`. Variable should have the
#'   following 3 levels: `"HIGH"`, `"LOW"`, and `"NORMAL"`.
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
#'   ADLB <- tmc_ex_adlb
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_pp_laboratory(
#'       label = "Vitals",
#'       dataname = "ADLB",
#'       patient_col = "USUBJID",
#'       timepoints = variables("ADY", fixed = TRUE),
#'       aval_var = variables("AVAL", fixed = TRUE),
#'       avalu_var = variables("AVALU", fixed = TRUE),
#'       param = variables("PARAM", fixed = TRUE),
#'       paramcd = variables("PARAMCD", fixed = TRUE),
#'       anrind = variables("ANRIND", fixed = TRUE),
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_pp_laboratory <- function(label,
                               dataname = "ADLB",
                               parentname = "ADSL",
                               patient_col = "USUBJID",
                               timepoints = NULL,
                               aval = lifecycle::deprecated(),
                               aval_var = NULL,
                               avalu = lifecycle::deprecated(),
                               avalu_var = NULL,
                               param = NULL,
                               paramcd = NULL,
                               anrind = NULL,
                               pre_output = NULL,
                               post_output = NULL,
                               transformators = list(),
                               decorators = lifecycle::deprecated()) {
  message("Initializing tm_t_pp_laboratory")

  if (lifecycle::is_present(decorators)) {
    lifecycle::deprecate_warn(
      when = "0.11.0",
      what = "tm_t_pp_laboratory(decorators)",
      details = "Decorators functionality was removed from this module. The `decorators` argument will be ignored."
    )
  }

  if (lifecycle::is_present(aval)) {
    lifecycle::deprecate_stop(
      when = "0.8.16",
      what = "tm_t_pp_laboratory(aval)",
      with = "tm_t_pp_laboratory(aval_var)"
    )
  }

  if (lifecycle::is_present(avalu)) {
    lifecycle::deprecate_stop(
      when = "0.8.16",
      what = "tm_t_pp_laboratory(avalu)",
      with = "tm_t_pp_laboratory(avalu_var)"
    )
  }

  timepoints <- deprecate_pick_variables_arg(timepoints, "timepoints", null.ok = TRUE)
  aval_var <- deprecate_pick_variables_arg(aval_var, "aval_var", null.ok = TRUE)
  avalu_var <- deprecate_pick_variables_arg(avalu_var, "avalu_var", null.ok = TRUE)
  param <- deprecate_pick_variables_arg(param, "param", null.ok = TRUE)
  paramcd <- deprecate_pick_variables_arg(paramcd, "paramcd", null.ok = TRUE)
  anrind <- deprecate_pick_variables_arg(anrind, "anrind", null.ok = TRUE)

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)

  # Build picks bound to the dataset
  if (!is.null(timepoints)) timepoints <- teal.picks::picks(datasets(dataname, dataname), timepoints)
  if (!is.null(aval_var)) aval_var <- teal.picks::picks(datasets(dataname, dataname), aval_var)
  if (!is.null(avalu_var)) avalu_var <- teal.picks::picks(datasets(dataname, dataname), avalu_var)
  if (!is.null(param)) param <- teal.picks::picks(datasets(dataname, dataname), param)
  if (!is.null(paramcd)) paramcd <- teal.picks::picks(datasets(dataname, dataname), paramcd)
  if (!is.null(anrind)) anrind <- teal.picks::picks(datasets(dataname, dataname), anrind)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_laboratory,
    ui_args = args[names(args) %in% names(formals(ui_g_laboratory))],
    server = srv_g_laboratory,
    server_args = args[names(args) %in% names(formals(srv_g_laboratory))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_laboratory <- function(id,
                            timepoints,
                            aval_var,
                            avalu_var,
                            param,
                            paramcd,
                            anrind,
                            pre_output,
                            post_output) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tags$div(
      htmlOutput(ns("title")),
      DT::DTOutput(outputId = ns("lab_values_table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      if (!is.null(paramcd)) {
        tags$div(
          tags$label("Select PARAMCD variable:"),
          teal.picks::picks_ui(ns("paramcd"), paramcd)
        )
      },
      if (!is.null(param)) {
        tags$div(
          tags$label("Select PARAM variable:"),
          teal.picks::picks_ui(ns("param"), param)
        )
      },
      if (!is.null(timepoints)) {
        tags$div(
          tags$label("Select timepoints variable:"),
          teal.picks::picks_ui(ns("timepoints"), timepoints)
        )
      },
      if (!is.null(aval_var)) {
        tags$div(
          tags$label("Select AVAL variable:"),
          teal.picks::picks_ui(ns("aval_var"), aval_var)
        )
      },
      if (!is.null(avalu_var)) {
        tags$div(
          tags$label("Select AVALU variable:"),
          teal.picks::picks_ui(ns("avalu_var"), avalu_var)
        )
      },
      if (!is.null(anrind)) {
        tags$div(
          tags$label("Select ANRIND variable:"),
          teal.picks::picks_ui(ns("anrind"), anrind)
        )
      },
      selectInput(
        inputId = ns("round_value"),
        label = "Select number of decimal places for rounding:",
        choices = NULL
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_laboratory <- function(id,
                             data,
                             dataname,
                             parentname,
                             patient_col,
                             timepoints,
                             aval_var,
                             avalu_var,
                             param,
                             paramcd,
                             anrind,
                             label) {
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
      timepoints = timepoints,
      aval_var   = aval_var,
      avalu_var  = avalu_var,
      param      = param,
      paramcd    = paramcd,
      anrind     = anrind
    ))

    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data = data
    )

    # Populate round_value choices from the aval column once data is available.
    # The aval column name comes from the default/fixed picks selection.
    observeEvent(data(), once = TRUE, {
      aval_values <- isolate(data())[[dataname]][, selectors$aval_var()$variables$selected]
      decimal_nums <- aval_values[trunc(aval_values) != aval_values]
      max_decimal <- max(nchar(gsub("([0-9]+).([0-9]+)", "\\2", decimal_nums)))

      updateSelectInput(
        session,
        "round_value",
        choices = seq(0L, max_decimal),
        selected = min(4L, max_decimal)
      )
    })

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId = "timepoints-variables-selected",
        condition = !is.null(selectors$timepoints()$variables$selected),
        message = "Please select timepoints variable."
      )
      teal:::validate_input(
        inputId = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message = "Please select AVAL variable."
      )
      teal:::validate_input(
        inputId = "avalu_var-variables-selected",
        condition = !is.null(selectors$avalu_var()$variables$selected),
        message = "Please select AVALU variable."
      )
      teal:::validate_input(
        inputId = "param-variables-selected",
        condition = !is.null(selectors$param()$variables$selected),
        message = "Please select PARAM variable."
      )
      teal:::validate_input(
        inputId = "paramcd-variables-selected",
        condition = !is.null(selectors$paramcd()$variables$selected),
        message = "Please select PARAMCD variable."
      )
      teal:::validate_input(
        inputId = "anrind-variables-selected",
        condition = !is.null(selectors$anrind()$variables$selected),
        message = "Please select ANRIND variable."
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

      labor_calls <- template_laboratory(
        dataname = "ANL",
        timepoints = anl_inputs$variables()$timepoints,
        aval_var = anl_inputs$variables()$aval_var,
        avalu_var = anl_inputs$variables()$avalu_var,
        param = anl_inputs$variables()$param,
        paramcd = anl_inputs$variables()$paramcd,
        anrind = anl_inputs$variables()$anrind,
        patient_id = patient_id(),
        round_value = as.integer(input$round_value)
      )

      obj <- teal.code::eval_code(
        obj,
        substitute(
          expr = {
            pt_id <- patient_id
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          },
          env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      obj <- obj |> teal.code::eval_code(as.expression(labor_calls))
      # Remove table_data_html — only needed for the display, not the report
      teal.reporter::teal_card(obj) <- utils::head(teal.reporter::teal_card(obj), -3)
      obj
    })

    output$title <- renderText({
      req(all_q())
      paste("<h5><b>Patient ID:", all_q()[["pt_id"]], "</b></h5>")
    })

    output$lab_values_table <- DT::renderDataTable(
      expr = {
        q <- req(all_q())
        table_html <- DT::datatable(
          data = q[["table_data_html"]],
          escape = FALSE,
          options = list(
            lengthMenu = list(list(-1, 5, 10, 25), list("All", "5", "10", "25")),
            scrollX = TRUE
          )
        )
        table_html$dependencies <- c(
          table_html$dependencies,
          list(rmarkdown::html_dependency_bootstrap("default"))
        )
        table_html
      }
    )

    all_q
  })
}
