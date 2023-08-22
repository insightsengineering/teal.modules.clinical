#' Template: Adverse Events Tab
#'
#' Creates an adverse events template call.
#'
#' @inheritParams template_arguments
#' @param aeterm (`character`)\cr name of the reported term for the adverse event variable.
#' @param tox_grade (`character`)\cr name of the standard toxicity grade variable.
#' @param causality (`character`)\cr name of the causality variable.
#' @param outcome (`character`)\cr name of outcome of adverse event variable.
#' @param action (`character`)\cr name of action taken with study treatment variable.
#' @param time (`character`)\cr name of study day of start of adverse event variable.
#' @param decod (`character`)\cr name of dictionary derived term variable.
#' @param patient_id (`character`)\cr patient ID.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#' @keywords internal
#'
template_adverse_events <- function(dataname = "ANL",
                                    aeterm = "AETERM",
                                    tox_grade = "AETOXGR",
                                    causality = "AEREL",
                                    outcome = "AEOUT",
                                    action = "AEACN",
                                    time = "ASTDY",
                                    decod = NULL,
                                    patient_id,
                                    font_size = 12L,
                                    ggplot2_args = teal.widgets::ggplot2_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(aeterm),
    assertthat::is.string(tox_grade),
    assertthat::is.string(causality),
    assertthat::is.string(outcome),
    assertthat::is.string(action),
    assertthat::is.string(time) || is.null(time),
    assertthat::is.string(decod) || is.null(decod),
    assertthat::is.string(patient_id),
    is.numeric(font_size)
  )

  y <- list()

  y$table <- list()
  y$chart <- list()

  table_list <- add_expr(
    list(),
    substitute(
      expr = {
        table <- dataname %>%
          dplyr::select(
            aeterm, tox_grade, causality, outcome, action, time, decod
          ) %>%
          dplyr::arrange(dplyr::desc(tox_grade)) %>%
          `colnames<-`(get_labels(dataname)$column_labels[vars])
        table
      },
      env = list(
        dataname = as.name(dataname),
        aeterm = as.name(aeterm),
        tox_grade = as.name(tox_grade),
        causality = as.name(causality),
        outcome = as.name(outcome),
        action = as.name(action),
        time = as.name(time),
        decod = `if`(is.null(decod), NULL, as.name(decod)),
        vars = c(aeterm, tox_grade, causality, outcome, action, time, decod)
      )
    )
  )

  parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
    teal.widgets::resolve_ggplot2_args(
      user_plot = ggplot2_args,
      module_plot = teal.widgets::ggplot2_args(
        labs = list(y = "Adverse Events", title = paste0("Patient ID: ", patient_id)),
        theme = list(
          text = substitute(ggplot2::element_text(size = font), list(font = font_size[1])),
          axis.text.y = quote(ggplot2::element_blank()),
          axis.ticks.y = quote(ggplot2::element_blank()),
          legend.position = "right",
          panel.grid.minor = quote(ggplot2::element_line(
            linewidth = 0.5,
            linetype = "dotted",
            colour = "grey"
          )),
          panel.grid.major = quote(ggplot2::element_line(
            linewidth = 0.5,
            linetype = "dotted",
            colour = "grey"
          ))
        )
      )
    )
  )

  chart_list <- add_expr(
    list(),
    substitute(
      expr = plot <- dataname %>%
        dplyr::select(aeterm, time, tox_grade, causality) %>%
        dplyr::mutate(ATOXGR = as.character(tox_grade)) %>%
        dplyr::arrange(dplyr::desc(ATOXGR)) %>%
        dplyr::mutate(ATOXGR = dplyr::case_when(
          ATOXGR == "." ~ "UNKNOWN",
          TRUE ~ ATOXGR
        )) %>%
        ggplot2::ggplot(ggplot2::aes(
          fill = ATOXGR, color = aeterm, y = aeterm, x = time
        )) +
        ggrepel::geom_label_repel(
          ggplot2::aes(label = aeterm),
          color = "black",
          hjust = "right",
          size = font_size_var[1] / 3.5,
          show.legend = FALSE
        ) +
        ggplot2::scale_fill_manual(values = c(
          "1" = "#E2264633",
          "2" = "#E2264666",
          "3" = "#E2264699",
          "4" = "#E22646CC",
          "5" = "#E22646FF",
          "UNKNOWN" = "#ACADB1FF"
        )) +
        ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 1.2)) +
        ggplot2::xlim(1, 1.2 * max(dataname[[time_var]])) +
        ggplot2::geom_point(color = "black", size = 2, shape = 24, position = ggplot2::position_nudge(y = -0.15)) +
        labs +
        themes,
      env = list(
        dataname = as.name(dataname),
        aeterm = as.name(aeterm),
        time = as.name(time),
        tox_grade = as.name(tox_grade),
        causality = as.name(causality),
        time_var = time,
        font_size_var = font_size,
        patient_id = patient_id,
        labs = parsed_ggplot2_args$labs,
        themes = parsed_ggplot2_args$theme
      )
    )
  )

  chart_list <- add_expr(
    expr_ls = chart_list,
    new_expr = quote(print(plot))
  )

  y$table <- bracket_expr(table_list)
  y$chart <- bracket_expr(chart_list)

  y
}

#' Teal Module: Patient Profile Adverse Events Teal Module
#'
#' This teal module produces a patient profile adverse events table and plot using `ADaM` datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param aeterm
#' ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AETERM} column of the `ADAE` dataset.
#' @param tox_grade ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AETOXGR} column of the `ADAE` dataset.
#' @param causality ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AEREL} column of the `ADAE` dataset.
#' @param outcome ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AEOUT} column of the `ADAE` dataset.
#' @param action ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AEACN} column of the `ADAE` dataset.
#' @param time ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{ASTDY} column of the `ADAE` dataset.
#' @param decod ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AEDECOD} column of the `ADAE` dataset.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#'
#' @export
#'
#' @examples
#' library(nestcolor)
#'
#' adae <- tmc_ex_adae
#' adsl <- tmc_ex_adsl %>% dplyr::filter(USUBJID %in% adae$USUBJID)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAE", adae)
#'   ),
#'   modules = modules(
#'     tm_g_pp_adverse_events(
#'       label = "Adverse Events",
#'       dataname = "ADAE",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       aeterm = choices_selected(
#'         choices = variable_choices(adae, "AETERM"),
#'         selected = "AETERM"
#'       ),
#'       tox_grade = choices_selected(
#'         choices = variable_choices(adae, "AETOXGR"),
#'         selected = "AETOXGR"
#'       ),
#'       causality = choices_selected(
#'         choices = variable_choices(adae, "AEREL"),
#'         selected = "AEREL"
#'       ),
#'       outcome = choices_selected(
#'         choices = variable_choices(adae, "AEOUT"),
#'         selected = "AEOUT"
#'       ),
#'       action = choices_selected(
#'         choices = variable_choices(adae, "AEACN"),
#'         selected = "AEACN"
#'       ),
#'       time = choices_selected(
#'         choices = variable_choices(adae, "ASTDY"),
#'         selected = "ASTDY"
#'       ),
#'       decod = NULL
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_pp_adverse_events <- function(label,
                                   dataname = "ADAE",
                                   parentname = "ADSL",
                                   patient_col = "USUBJID",
                                   aeterm = NULL,
                                   tox_grade = NULL,
                                   causality = NULL,
                                   outcome = NULL,
                                   action = NULL,
                                   time = NULL,
                                   decod = NULL,
                                   font_size = c(12L, 12L, 25L),
                                   plot_height = c(700L, 200L, 2000L),
                                   plot_width = NULL,
                                   pre_output = NULL,
                                   post_output = NULL,
                                   ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_pp_adverse_events")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_numeric(font_size, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(font_size[1], lower = font_size[2], upper = font_size[3], .var.name = "font_size")
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  args <- as.list(environment())
  data_extract_list <- list(
    aeterm = `if`(is.null(aeterm), NULL, cs_to_des_select(aeterm, dataname = dataname)),
    tox_grade = `if`(is.null(tox_grade), NULL, cs_to_des_select(tox_grade, dataname = dataname)),
    causality = `if`(is.null(causality), NULL, cs_to_des_select(causality, dataname = dataname)),
    outcome = `if`(is.null(outcome), NULL, cs_to_des_select(outcome, dataname = dataname)),
    action = `if`(is.null(action), NULL, cs_to_des_select(action, dataname = dataname)),
    time = `if`(is.null(time), NULL, cs_to_des_select(time, dataname = dataname)),
    decod = `if`(is.null(decod), NULL, cs_to_des_select(decod, dataname = dataname))
  )

  module(
    label = label,
    ui = ui_g_adverse_events,
    ui_args = c(data_extract_list, args),
    server = srv_g_adverse_events,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        patient_col = patient_col,
        plot_height = plot_height,
        plot_width = plot_width,
        ggplot2_args = ggplot2_args
      )
    ),
    datanames = "all"
  )
}

ui_g_adverse_events <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$aeterm,
    ui_args$tox_grade,
    ui_args$causality,
    ui_args$outcome,
    ui_args$action,
    ui_args$time,
    ui_args$decod
  )

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = shiny::div(
      teal.widgets::get_dt_rows(ns("table"), ns("table_rows")),
      DT::DTOutput(outputId = ns("table")),
      teal.widgets::plot_with_settings_ui(id = ns("chart"))
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(ui_args[c(
        "aeterm", "tox_grade", "causality", "outcome",
        "action", "time", "decod"
      )]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("aeterm"),
        label = "Select AETERM variable:",
        data_extract_spec = ui_args$aeterm,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("tox_grade"),
        label = "Select AETOXGR variable:",
        data_extract_spec = ui_args$tox_grade,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("causality"),
        label = "Select AEREL variable:",
        data_extract_spec = ui_args$causality,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("outcome"),
        label = "Select AEOUT variable:",
        data_extract_spec = ui_args$outcome,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("action"),
        label = "Select AEACN variable:",
        data_extract_spec = ui_args$action,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("time"),
        label = "Select ASTDY variable:",
        data_extract_spec = ui_args$time,
        is_single_dataset = is_single_dataset_value
      ),
      `if`(
        is.null(ui_args$decod),
        NULL,
        teal.transform::data_extract_ui(
          id = ns("decod"),
          label = "Select DECOD variable:",
          data_extract_spec = ui_args$decod,
          is_single_dataset = is_single_dataset_value
        )
      ),
      teal.widgets::panel_item(
        title = "Plot settings",
        collapsed = TRUE,
        teal.widgets::optionalSliderInputValMinMax(
          ns("font_size"),
          "Font Size",
          ui_args$font_size,
          ticks = FALSE, step = 1
        )
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


srv_g_adverse_events <- function(id,
                                 data,
                                 filter_panel_api,
                                 reporter,
                                 dataname,
                                 parentname,
                                 patient_col,
                                 aeterm,
                                 tox_grade,
                                 causality,
                                 outcome,
                                 action,
                                 time,
                                 decod,
                                 plot_height,
                                 plot_width,
                                 label,
                                 ggplot2_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    patient_id <- shiny::reactive(input$patient_id)

    # Init
    patient_data_base <- shiny::reactive(unique(data[[parentname]]()[[patient_col]]))
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

    # Adverse events tab ----
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = Filter(
        Negate(is.null),
        list(
          aeterm = aeterm,
          tox_grade = tox_grade,
          causality = causality,
          outcome = outcome,
          action = action,
          time = time,
          decod = decod
        )
      ),
      datasets = data,
      select_validation_rule = list(
        aeterm = shinyvalidate::sv_required("Please select AETERM variable."),
        tox_grade = shinyvalidate::sv_required("Please select AETOXGR variable."),
        causality = shinyvalidate::sv_required("Please select AEREL variable."),
        outcome = shinyvalidate::sv_required("Please select AEOUT variable."),
        action = shinyvalidate::sv_required("Please select AEACN variable."),
        time = shinyvalidate::sv_required("Please select ASTDY variable."),
        decod = shinyvalidate::sv_required("Please select ANRIND variable.")
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
      join_keys = get_join_keys(data)
    )

    anl_q <- shiny::reactive(
      teal.code::eval_code(
        teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)), as.expression(anl_inputs()$expr)
      )
    )

    all_q <- shiny::reactive({
      teal::validate_inputs(iv_r())
      anl_m <- anl_inputs()
      qenv <- anl_q()
      ANL <- qenv[["ANL"]] # nolint

      teal::validate_has_data(ANL[ANL[[patient_col]] == input$patient_id, ], min_nrow = 1)

      qenv2 <- teal.code::eval_code(
        qenv,
        substitute(
          expr = ANL <- ANL[ANL[[patient_col]] == patient_id, ], # nolint
          env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )

      calls <- template_adverse_events(
        dataname = "ANL",
        aeterm = input[[extract_input("aeterm", dataname)]],
        tox_grade = input[[extract_input("tox_grade", dataname)]],
        causality = input[[extract_input("causality", dataname)]],
        outcome = input[[extract_input("outcome", dataname)]],
        action = input[[extract_input("action", dataname)]],
        time = input[[extract_input("time", dataname)]],
        decod = input[[extract_input("decod", dataname)]],
        patient_id = patient_id(),
        font_size = input[["font_size"]],
        ggplot2_args = ggplot2_args
      )

      teal.code::eval_code(qenv2, as.expression(calls))
    })
    output$table <- DT::renderDataTable(
      expr = teal.code::dev_suppress(all_q()[["table"]]),
      options = list(pageLength = input$table_rows)
    )

    plot_r <- shiny::reactive({
      shiny::req(iv_r()$is_valid())
      all_q()[["plot"]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "chart",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
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
        card$set_name("Patient Profile Adverse Events Plot")
        card$append_text("Patient Profile Adverse Events Plot", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
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
