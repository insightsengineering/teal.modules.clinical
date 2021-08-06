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
                                    font_size = 12L) {
  assert_that(
    is.string(dataname),
    is.string(aeterm),
    is.string(tox_grade),
    is.string(causality),
    is.string(outcome),
    is.string(action),
    is.string(time) || is.null(time),
    is.string(decod) || is.null(decod),
    is.string(patient_id),
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
          select(
            aeterm, tox_grade, causality, outcome, action, time, decod
          ) %>%
          arrange(desc(tox_grade)) %>%
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
        decod = if_not_null(decod, as.name(decod)),
        vars = c(aeterm, tox_grade, causality, outcome, action, time, decod)
      )
    )
  )

  chart_list <- add_expr(
    list(),
    substitute(
      expr = chart <- dataname %>%
        select(aeterm, time, tox_grade, causality) %>%
        mutate(ATOXGR = as.character(tox_grade)) %>%
        arrange(desc(ATOXGR)) %>%
        mutate(ATOXGR = case_when(
          ATOXGR == "." ~ "UNKNOWN",
          TRUE ~ ATOXGR
        )) %>%
        ggplot(aes(
          fill = ATOXGR, color = aeterm, y = aeterm, x = time
        )) +
        ggrepel::geom_label_repel(
          aes(label = aeterm),
          color = "black",
          hjust = "left",
          size = font_size_var[1] / 3.5
        ) +
        scale_fill_manual(values = c(
          "1" = "#E2264633",
          "2" = "#E2264666",
          "3" = "#E2264699",
          "4" = "#E22646CC",
          "5" = "#E22646FF",
          "UNKNOWN" = "#ACADB1FF"
        )) +
        scale_y_discrete(expand = expansion(add = 1.2)) +
        xlim(1, 1.2 * max(dataname[[time_var]])) +
        geom_point(color = "black", size = 2, shape = 24, position = position_nudge(y = -0.15)) +
        ylab("Adverse Events") +
        theme(
          text = element_text(size = font_size_var[1]),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_line(
            size = 0.5,
            linetype = "dotted",
            colour = "grey"
          ),
          panel.grid.minor = element_line(
            size = 0.5,
            linetype = "dotted",
            colour = "grey"
          )
        ) +
        theme(legend.position = "none") +
        ggtitle(paste0("Patient ID: ", patient_id)),
      env = list(
        dataname = as.name(dataname),
        aeterm = as.name(aeterm),
        time = as.name(time),
        tox_grade = as.name(tox_grade),
        causality = as.name(causality),
        time_var = time,
        font_size_var = font_size,
        patient_id = patient_id
      )
    )
  )

  chart_list <- add_expr(
    expr_ls = chart_list,
    new_expr = quote(print(chart))
  )

  y$table <- bracket_expr(table_list)
  y$chart <- bracket_expr(chart_list)

  y
}

#' Teal Module: Patient Profile Adverse Events Teal Module
#'
#' This teal module produces a patient profile adverse events table and plot using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`) value patient ID column to be used.
#' @param aeterm
#' ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AETERM} column of the ADAE dataset.
#' @param tox_grade ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AETOXGR} column of the
#' ADAE dataset.
#' @param causality ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AEREL} column of the
#' ADAE dataset.
#' @param outcome ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AEOUT} column of the
#' ADAE dataset.
#' @param action ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AEACN} column of the ADAE dataset.
#' @param time ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{ASTDY} column of the ADAE dataset.
#' @param decod ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AEDECOD} column of the
#' ADAE dataset.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data('latest')$adsl"),
#'     cdisc_dataset("ADAE", ADAE, code = "ADAE <- synthetic_cdisc_data('latest')$adae"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_pp_adverse_events(
#'       label = "Adverse events",
#'       dataname = "ADAE",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       aeterm = choices_selected(
#'         choices = variable_choices(ADAE, "AETERM"),
#'         selected = "AETERM"
#'       ),
#'       tox_grade = choices_selected(
#'         choices = variable_choices(ADAE, "AETOXGR"),
#'         selected = "AETOXGR"
#'       ),
#'       causality = choices_selected(
#'         choices = variable_choices(ADAE, "AEREL"),
#'         selected = "AEREL"
#'       ),
#'       outcome = choices_selected(
#'         choices = variable_choices(ADAE, "AEOUT"),
#'         selected = "AEOUT"
#'       ),
#'       action = choices_selected(
#'         choices = variable_choices(ADAE, "AEACN"),
#'         selected = "AEACN"
#'       ),
#'       time = choices_selected(
#'         choices = variable_choices(ADAE, "ASTDY"),
#'         selected = "ASTDY"
#'       ),
#'       decod = NULL
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
                                   plot_width = c(900L, 200L, 2000L),
                                   pre_output = NULL,
                                   post_output = NULL) {
  assert_that(is_character_single(label))
  assert_that(is_character_single(dataname))
  assert_that(is_character_single(parentname))
  assert_that(is_character_single(patient_col))
  assert_that(is.null(pre_output) || is(pre_output, "shiny.tag"),
    msg = "pre_output should be either null or shiny.tag type of object"
  )
  assert_that(is.null(post_output) || is(post_output, "shiny.tag"),
    msg = "post_output should be either null or shiny.tag type of object"
  )

  check_slider_input(font_size, allow_null = FALSE)
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())
  data_extract_list <- list(
    aeterm = if_not_null(aeterm, cs_to_des_select(aeterm, dataname = dataname)),
    tox_grade = if_not_null(tox_grade, cs_to_des_select(tox_grade, dataname = dataname)),
    causality = if_not_null(causality, cs_to_des_select(causality, dataname = dataname)),
    outcome = if_not_null(outcome, cs_to_des_select(outcome, dataname = dataname)),
    action = if_not_null(action, cs_to_des_select(action, dataname = dataname)),
    time = if_not_null(time, cs_to_des_select(time, dataname = dataname)),
    decod = if_not_null(decod, cs_to_des_select(decod, dataname = dataname))
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
        plot_width = plot_width
      )
    ),
    filters = "all"
  )
}

ui_g_adverse_events <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- is_single_dataset(
    ui_args$aeterm,
    ui_args$tox_grade,
    ui_args$causality,
    ui_args$outcome,
    ui_args$action,
    ui_args$time,
    ui_args$decod
  )

  ns <- NS(id)
  standard_layout(
    output = div(
      get_dt_rows(ns("table"), ns("table_rows")),
      DT::DTOutput(outputId = ns("table")),
      plot_with_settings_ui(id = ns("chart"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(ui_args[c(
        "aeterm", "tox_grade", "causality", "outcome",
        "action", "time", "decod"
      )]),
      optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = T)
      ),
      data_extract_input(
        id = ns("aeterm"),
        label = "Select AETERM variable:",
        data_extract_spec = ui_args$aeterm,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("tox_grade"),
        label = "Select AETOXGR variable:",
        data_extract_spec = ui_args$tox_grade,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("causality"),
        label = "Select AEREL variable:",
        data_extract_spec = ui_args$causality,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("outcome"),
        label = "Select AEOUT variable:",
        data_extract_spec = ui_args$outcome,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("action"),
        label = "Select AEACN variable:",
        data_extract_spec = ui_args$action,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("time"),
        label = "Select ASTDY variable:",
        data_extract_spec = ui_args$time,
        is_single_dataset = is_single_dataset_value
      ),
      if_not_null(
        ui_args$decod,
        data_extract_input(
          id = ns("decod"),
          label = "Select DECOD variable:",
          data_extract_spec = ui_args$decod,
          is_single_dataset = is_single_dataset_value
        )
      ),
      panel_item(
        title = "Plot settings",
        collapsed = TRUE,
        optionalSliderInputValMinMax(ns("font_size"), "Font Size", ui_args$font_size, ticks = FALSE, step = 1)
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}


srv_g_adverse_events <- function(input,
                                 output,
                                 session,
                                 datasets,
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
                                 label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  patient_id <- reactive(input$patient_id)

  # Init
  patient_data_base <- reactive(unique(datasets$get_data(parentname, filtered = TRUE)[[patient_col]]))
  updateOptionalSelectInput(session, "patient_id", choices = patient_data_base(), selected = patient_data_base()[1])

  observeEvent(patient_data_base(), {
    updateOptionalSelectInput(
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
  ae_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(aeterm, tox_grade, causality, outcome, action, time, decod),
    input_id = c("aeterm", "tox_grade", "causality", "outcome", "action", "time", "decod")
  )

  calls <- reactive({
    validate(need(patient_id(), "Please select a patient."))
    validate_has_data(ae_merged_data()$data()[ae_merged_data()$data()[[patient_col]] == input$patient_id, ], 1)

    validate(
      need(
        input[[extract_input("aeterm", dataname)]],
        "Please select AETERM variable."
      ),
      need(
        input[[extract_input("tox_grade", dataname)]],
        "Please select AETOXGR variable."
      ),
      need(
        input[[extract_input("causality", dataname)]],
        "Please select AEREL variable."
      ),
      need(
        input[[extract_input("outcome", dataname)]],
        "Please select AEOUT variable."
      ),
      need(
        input[[extract_input("action", dataname)]],
        "Please select AEACN variable."
      ),
      need(
        input[[extract_input("time", dataname)]],
        "Please select ASTDY variable."
      )
    )

    stack <- chunks$new()
    stack$reset()

    chunks_push_data_merge(ae_merged_data(), chunks = stack)

    stack$push(substitute(
      expr = {
        ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
      }, env = list(
        patient_col = patient_col,
        patient_id = patient_id()
      )
    ))


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
      font_size = input[["font_size"]]
    )

    lapply(calls, chunks_push, chunks = stack)
    chunks_safe_eval(chunks = stack)
    stack
  })
  output$table <- DT::renderDataTable({
    chunks_reset()
    chunks_push_chunks(calls())
    chunks_get_var("table")
    },
    options = list(pageLength = input$table_rows)
  )

  chart <- reactive({
    chunks_reset()
    chunks_push_chunks(calls())
    chunks_get_var("chart")
  })

  callModule(
    plot_with_settings_srv,
    id = "chart",
    plot_r = chart,
    height = plot_height,
    width = plot_width
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(
      aeterm, tox_grade, causality, outcome, action, time, decod
    )),
    modal_title = label
  )
}
