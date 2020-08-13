#' teal module for event rates adjusted for patient-years
#'
#' @description This module produces an event rate adjusted for patient-years at risk table that matches the
#'   STREAM template \code{aet05} and \code{aet05_all}
#' @inheritParams tm_t_rsp
#' @param conf_level \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for confidence level for computation of the confidence intervals.
#' @param event_type  named (\code{character} value) to specify the type of event that is summarized,
#'    (e.g. c("AE" = "adverse event")). Default is "event".
#' @export
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADAETTE <- radaette(cached = TRUE)
#' arm_vars <- c("ACTARM", "ARM")
#' cs_arm_vars <- choices_selected(choices = variable_choices(ADSL, subset = arm_vars),
#' selected = "ARM")
#' cs_params <- choices_selected(choices = value_choices(ADAETTE, "PARAMCD", "PARAM"))
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADAETTE", ADAETTE),
#'     code = "ADSL <- radsl(cached = TRUE)
#'             ADAETTE <- radaette(cached = TRUE)",
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_t_events_patyear(
#'       label = "AE Rate adjusted for patient-years at risk Table",
#'       dataname = "ADAETTE",
#'       arm_var = cs_arm_vars,
#'       paramcd = cs_params,
#'       event_type = c("AE" = "adverse event")
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
                                paramcd,
                                conf_level = choices_selected(c(0.8, 0.85, 0.90, 0.95, 0.99, 0.995),
                                                              0.95, keep_order = TRUE),
                                event_type = "event",
                                pre_output = NULL,
                                post_output = NULL) {

  stop_if_not(list(is_character_single(label), "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is_character_vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))

  args <- as.list(environment())
  module(
    label = label,
    server = srv_t_events_patyear,
    ui = ui_t_events_patyear,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      event_type = event_type
    ),
    filters = dataname
  )
}

#' UI part for event rate adjusted for patient-years table teal module
#' @param id namespace id
#' @noRd
ui_t_events_patyear <- function(id, ...) {

  ns <- NS(id)

  a <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("patyear_table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("paramcd"),
        "Select an Event Type Parameter",
        choices = a$paramcd$choices,
        selected = a$paramcd$selected,
        multiple = FALSE,
        fixed = a$paramcd$fixed
      ),
      selectInput(
        ns("events_indicator"),
        "Select a Censor Indicator",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
      ),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        choices = a$arm_var$choices,
        selected = a$arm_var$selected,
        multiple = FALSE,
        fixed = a$arm_var$fixed
      ),
      optionalSelectInput(
        ns("conf_level"),
        "Confidence Level",
        choices = a$conf_level$choices,
        selected = a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      ),
      optionalSelectInput(
        ns("conf_method"),
        "CI Method",
        choices = c("Normal approximation", "Exact", "Bayr's method"),
        selected = "Normal approximation",
        multiple = FALSE,
        fixed = FALSE
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total)

    ),
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

#' server part of tm_t_events_patyear
#'
#' @noRd

srv_t_events_patyear <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 event_type) {

  init_chunks()
  observe({
    anl <- datasets$get_data(dataname, filtered = FALSE)
    paramcd <- input$paramcd

    events_choices <- unique(anl$CNSR[anl$PARAMCD == paramcd]) %>% sort

    updateSelectInput(
      session, "events_indicator",
      choices = events_choices,
      selected = events_choices[1]
    )
  })
  output$patyear_table <- renderUI({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
    paramcd <- input$paramcd
    events_indicator <- input$events_indicator
    arm_var <- input$arm_var
    conf_level <- as.numeric(input$conf_level) # nolint
    conf_method <- input$conf_method

    validate(need(length(conf_method) == 1, "CI method selection can not be NULL"))
    conf_method <- if (conf_method == "Normal approximation"){
      "normal"
    } else if (conf_method == "Exact"){
      "exact"
    } else {
      "byar"
    }
    add_total <- input$add_total

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", "CNSR", "AVAL", "AVALU"),
      arm_var = arm_var,
      min_n_levels_armvar = NULL
    )
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)
    adsl_name <- "ADSL_FILTERED"
    assign(adsl_name, adsl_filtered)
    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var)) # nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALU", "PARAMCD", "CNSR") # nolint
    validate_in(events_indicator, anl_filtered$CNSR, "event indicator does not exist")
    validate(need(!is.null(paramcd), "Event selection can not be NULL"))

    chunks_reset(envir = environment())

    chunks_push(bquote({
      anl_endpoint <- subset(
        .(as.name(anl_name)),
        PARAMCD == .(paramcd)
      )
    }))
    chunks_push(bquote({
      anl <- merge(
        x = .(as.name(adsl_name))[, .(adsl_vars), drop = FALSE],
        y = anl_endpoint[, .(anl_vars), drop = FALSE],
        all.x = FALSE,
        all.y = FALSE,
        by = c("USUBJID", "STUDYID")
      )
    }))
    chunks_safe_eval()
    data_anl <- chunks_get_var("anl")
    time_unit <- unique(data_anl$AVALU) %>% toupper()
    validate(
      need(length(time_unit) == 1, "Time Unit not consistent"),
      need(grepl("YEAR", time_unit), "Time Unit is not YEAR")
    )

    total <- if (add_total) "All Patients" else NULL # nolint
    chunks_push(bquote({
      tbl <- t_events_patyear(
        events =  anl[["CNSR"]] == .(events_indicator),
        time_in_years =  anl[["AVAL"]],
        col_by = as.factor(anl[[.(arm_var)]]),
        col_N = table(.(as.name(adsl_name))[[.(arm_var)]]),
        total = .(total),
        conf_level = .(conf_level),
        conf_method = .(conf_method),
        lambda = 100,
        event_type = .(event_type)
      )
      tbl
    }))
    chunks_safe_eval()
    tbl <- chunks_get_var("tbl")
    as_html(tbl)

  })
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Event Rate adjusted for patient-year at risk",
      rcode = get_rcode(
        datasets = datasets,
        datanames = union("ADSL", dataname),
        title = "Event Rate adjusted for patient-year Table"
      )
    )
  })
}
