#' Teal module: Event rates adjusted for patient-years
#'
#' @name events_patyear
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#'
NULL

#' @describeIn events_patyear create the expression corresponding to the analysis.
#'
#' @param control (`list`)\cr list of settings for the analysis.
#' @param event_indicator (`numeric`)\cr code for event as opposed to censoring.
#'
template_events_patyear <- function(dataname,
                                    parentname,
                                    arm_var,
                                    paramcd,
                                    control = control_incidence_rate(),
                                    event_indicator = 0,
                                    add_total = TRUE
) {

  # initialize
  y <- list()

  # data
  y$data <- substitute(
    expr = anl <- df %>%
      filter(PARAMCD == paramcd) %>%
      mutate(is_event = CNSR == event_indicator),
    env = list(
      df = as.name(dataname),
      paramcd = paramcd,
      event_indicator = event_indicator
    )
  )

  # layout
  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = basic_table() %>%
        split_cols_by(var = arm_var) %>%
        add_colcounts(),
      env = list(arm_var = arm_var)
    )
  )
  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        add_overall_col(label = "All Patients")
      )
    )
  }
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = estimate_incidence_rate(
        vars = "AVAL",
        is_event = "is_event",
        control = control_incidence_rate(
          conf_level = conf_level,
          conf_type = conf_type,
          time_unit = time_unit
        )
      ),
      env = list(
        conf_level = control$conf_level,
        conf_type = control$conf_type,
        time_unit = control$time_unit
      )
    )
  )
  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # table
  col_counts <- substitute(
    expr = table(parentname$arm_var),
    env = list(parentname = as.name(parentname), arm_var = arm_var)
  )
  if (add_total) {
    col_counts <- substitute(
      expr = c(col_counts, "All Patients" = sum(col_counts))
    )
  }
  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl, col_counts = col_counts),
    env = list(col_counts = col_counts)
  )

  y

}


#' @noRd
ui_events_patyear <- function(id,
                              datasets,
                              dataname,
                              ...) {
  ns <- NS(id)
  args <- list(...)
  standard_layout(
    output = white_small_well(uiOutput(ns("patyear_table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(
        ns("paramcd"),
        "Select an Event Type Parameter",
        choices = args$paramcd$choices,
        selected = args$paramcd$selected,
        multiple = FALSE,
        fixed = args$paramcd$fixed
      ),
      selectInput(
        ns("event_indicator"),
        "Select an Event Indicator",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected,
        multiple = FALSE,
        fixed = args$arm_var$fixed
      ),
      optionalSelectInput(
        ns("time_unit"),
        "Time Unit for AE Rate",
        choices = c(0.1, 1, 10, 100, 1000),
        selected = 100,
        multiple = FALSE,
        fixed = FALSE
      ),
      numericInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        value = 0.95,
        min = 0.01,
        max = 0.99,
        step = 0.01,
        width = "100%"
      ),
      optionalSelectInput(
        ns("conf_method"),
        "CI Method",
        choices = c("Normal approximation", "Normal approximation for log rate", "Exact", "Bayr's method"),
        selected = "Normal approximation",
        multiple = FALSE,
        fixed = FALSE
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = args$add_total)
    ),
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    )
  )
}


#' @noRd
srv_events_patyear <- function(input,
                               output,
                               session,
                               datasets,
                               dataname) {

  init_chunks()
  observe({
    anl <- datasets$get_data(dataname, filtered = FALSE)
    paramcd <- input$paramcd
    event_choices <- unique(anl$CNSR[anl$PARAMCD == paramcd]) %>% sort #nolint
    updateSelectInput(
      session, "event_indicator",
      choices = event_choices,
      selected = event_choices[1]
    )
  })

  # Prepare the analysis environment (filter data, check data, populate envir).
  prepared_env <- reactive({

    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    # Validate through assertions.
    assert_that(
      teal_enough_rows(data = adsl_filtered, min_nrow = 15),
      teal_enough_rows(data = anl_filtered, min_nrow = 15),
      teal_has_element(str = input$arm_var, label = "ARM")
    )

    # Send data where the analysis lives.
    e <- new.env()
    e$ADSL_FILTERED <- adsl_filtered # nolint
    anl_name <- paste0(dataname, "_FILTERED")
    e[[anl_name]] <- anl_filtered
    e
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_events_patyear(
      dataname = paste0(dataname, "_FILTERED"),
      parentname = "ADSL_FILTERED",
      arm_var = input$arm_var,
      paramcd = input$paramcd,
      control = control_incidence_rate(
        conf_level = as.numeric(input$conf_level), # nolint
        conf_type = if (input$conf_method == "Normal approximation") {
          "normal"
        } else if (input$conf_method == "Normal approximation for log rate") {
          "normal_log"
        } else if (input$conf_method == "Exact") {
          "exact"
        } else {
          "byar"
        },
        time_unit = as.numeric(input$time_unit)
      ),
      event_indicator = input$event_indicator,
      add_total = input$add_total
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  output$patyear_table <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })
  # Render R code.
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Event Rate adjusted for patient-year at risk",
      rcode = get_rcode(
        datasets = datasets,
        datanames = dataname,
        title = "Event Rate adjusted for patient-year Table"
      )
    )
  })

}

#' @describeIn events_patyear Teal module for incidence rate.
#'
#' @export
#' @examples
#' # Preparation of the test case.
#' library(dplyr)
#' library(random.cdisc.data)
#' adsl <- radsl(cached = TRUE)
#' adaette <- radaette(cached = TRUE)
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAETTE", adaette),
#'     code =
#'       "ADSL <- radsl(cached = TRUE)
#'     ADAETTE <- radaette(cached = TRUE)"
#'   ),
#'   modules = root_modules(
#'     tm_t_events_patyear(
#'       label = "AE rate adjusted for patient-years at risk Table",
#'       dataname = "ADAETTE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARMCD"),
#'       paramcd = choices_selected(levels(adaette$PARAMCD), "AETTE1")
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_events_patyear <- function(label,
                                dataname,
                                arm_var,
                                paramcd) {
  args <- c(as.list(environment()))
  module(
    label = label,
    ui = ui_events_patyear,
    ui_args = args,
    server = srv_events_patyear,
    server_args = list(
      dataname = dataname
    ),
    filters = dataname
  )
}
