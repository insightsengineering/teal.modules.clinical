#' teal Module: Multiple Events by Term (teal.picks implementation)
#'
#' This module produces a table of multiple events by term using `teal.picks` for
#' variable selection instead of `teal.transform::choices_selected`.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_mult_events
#' @param arm_var ([teal.picks::variables()])\cr variable selector for the arm/treatment variable.
#' @param seq_var ([teal.picks::variables()])\cr variable selector for the sequence number variable.
#' @param hlt ([teal.picks::variables()])\cr variable selector for the high-level term(s).
#' @param llt ([teal.picks::variables()])\cr variable selector for the low-level term.
#' @param title_text (`string`)\cr first part of the dynamic table title.
#'
#' @keywords internal
#' @export
tm_t_mult_events.picks <- function(label,
                                   dataname,
                                   parentname = "ADSL",
                                   arm_var = teal.picks::variables(c("ARM", "ARMCD"), selected = "ARM"),
                                   seq_var = teal.picks::variables("CMSEQ", fixed = TRUE),
                                   hlt = teal.picks::variables(
                                     c("ATC1", "ATC2", "ATC3", "ATC4"),
                                     selected = c("ATC1", "ATC2", "ATC3", "ATC4"),
                                     multiple = TRUE,
                                     ordered = TRUE
                                   ),
                                   llt = teal.picks::variables("CMDECOD"),
                                   add_total = TRUE,
                                   total_label = default_total_label(),
                                   na_level = tern::default_na_str(),
                                   event_type = "event",
                                   title_text = "Concomitant Medications",
                                   drop_arm_levels = TRUE,
                                   pre_output = NULL,
                                   post_output = NULL,
                                   basic_table_args = teal.widgets::basic_table_args(),
                                   transformators = list(),
                                   decorators = list()) {
  message("Initializing tm_t_mult_events.picks")

  # Compatibility: accept choices_selected and convert to picks
  for (arg in c("arm_var", "seq_var", "hlt", "llt")) {
    if (inherits(get(arg), "choices_selected")) {
      assign(arg, teal.picks::as.picks(get(arg)))
    }
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(seq_var, "variables")
  checkmate::assert_class(hlt, "variables")
  checkmate::assert_class(llt, "variables")
  checkmate::assert_string(event_type)
  checkmate::assert_string(title_text)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  # Build picks objects bound to their datasets
  arm_var <- teal.picks::picks(datasets(parentname), arm_var)
  seq_var <- teal.picks::picks(datasets(dataname), seq_var)
  hlt     <- teal.picks::picks(datasets(dataname), hlt)
  llt     <- teal.picks::picks(datasets(dataname), llt)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_mult_events_byterm.picks,
    ui = ui_t_mult_events_byterm.picks,
    ui_args = args[names(args) %in% names(formals(ui_t_mult_events_byterm.picks))],
    server_args = args[names(args) %in% names(formals(srv_t_mult_events_byterm.picks))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_t_mult_events_byterm.picks <- function(id,
                                          arm_var,
                                          seq_var,
                                          hlt,
                                          llt,
                                          add_total,
                                          drop_arm_levels,
                                          pre_output,
                                          post_output,
                                          decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Treatment Variable:"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      ),
      tags$div(
        tags$label("Event High Level Term:"),
        teal.picks::picks_ui(ns("hlt"), hlt)
      ),
      tags$div(
        tags$label("Event Low Level Term:"),
        teal.picks::picks_ui(ns("llt"), llt)
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = add_total),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = drop_arm_levels
          )
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          tags$div(
            tags$label("Analysis Sequence Number:"),
            teal.picks::picks_ui(ns("seq_var"), seq_var)
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_t_mult_events_byterm.picks <- function(id,
                                           data,
                                           dataname,
                                           parentname,
                                           arm_var,
                                           seq_var,
                                           hlt,
                                           llt,
                                           event_type,
                                           title_text,
                                           label,
                                           total_label,
                                           na_level,
                                           basic_table_args,
                                           decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- teal.picks::picks_srv(
      picks = list(
        arm_var = arm_var,
        seq_var = seq_var,
        hlt     = hlt,
        llt     = llt
      ),
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId   = "arm_var-variables-selected",
        condition = !is.null(selectors$arm_var()$variables$selected),
        message   = "Please select a treatment variable."
      )
      teal:::validate_input(
        inputId   = "llt-variables-selected",
        condition = !is.null(selectors$llt()$variables$selected),
        message   = "Please select a \"LOW LEVEL TERM\" variable."
      )

      obj
    })

    # Main ANL merge — includes all four selectors via inner join
    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data       = validated_q,
      selectors  = selectors,
      join_fun   = "dplyr::inner_join",
      output_name = "ANL"
    )

    # ADSL-side merge — only arm_var to build ANL_ADSL
    adsl_inputs <- teal.picks::merge_srv(
      "adsl_inputs",
      data       = validated_q,
      selectors  = selectors["arm_var"],
      output_name = "ANL_ADSL"
    )

    validate_checks <- reactive({
      anl_q         <- anl_inputs$data()
      adsl_filtered <- anl_q[[parentname]]
      anl_filtered  <- anl_q[[dataname]]

      input_arm_var <- anl_inputs$variables()$arm_var
      input_seq_var <- anl_inputs$variables()$seq_var
      input_hlt     <- anl_inputs$variables()$hlt
      input_llt     <- anl_inputs$variables()$llt

      teal:::validate_input(
        inputId   = "arm_var-variables-selected",
        condition = is.factor(adsl_filtered[[input_arm_var]]),
        message   = "Treatment variable is not a factor."
      )
      teal:::validate_input(
        inputId   = "seq_var-variables-selected",
        condition = is.integer(anl_filtered[[input_seq_var]]),
        message   = "Analysis sequence variable is not an integer."
      )

      validate_standard_inputs(
        adsl     = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl      = anl_filtered,
        anlvars  = c("USUBJID", "STUDYID", input_seq_var, input_hlt, input_llt),
        arm_var  = input_arm_var
      )
    })

    all_q <- reactive({
      validate_checks()

      obj       <- anl_inputs$data()
      anl_data  <- c(obj, adsl_inputs$data())

      input_hlt <- anl_inputs$variables()$hlt
      input_llt <- anl_inputs$variables()$llt

      hlt_labels <- vapply(input_hlt, function(x) rtables::obj_label(obj[["ANL"]][[x]]), character(1))
      llt_labels <- vapply(input_llt, function(x) rtables::obj_label(obj[["ANL"]][[x]]), character(1))

      bta <- basic_table_args
      if (is.null(bta$title)) {
        bta$title <- paste(
          title_text, "by",
          paste(hlt_labels, collapse = ", "),
          "and",
          paste(llt_labels, collapse = ", ")
        )
      }

      my_calls <- template_mult_events(
        dataname        = "ANL",
        parentname      = "ANL_ADSL",
        arm_var         = anl_inputs$variables()$arm_var,
        seq_var         = anl_inputs$variables()$seq_var,
        hlt             = if (length(input_hlt) > 0) input_hlt else NULL,
        llt             = input_llt,
        add_total       = input$add_total,
        total_label     = total_label,
        na_level        = na_level,
        event_type      = event_type,
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = bta
      )

      teal.reporter::teal_card(anl_data) <- c(teal.reporter::teal_card(anl_data), "### Table")
      teal.code::eval_code(anl_data, as.expression(unlist(my_calls)))
    })

    decorated_table_q <- srv_decorate_teal_data(
      id         = "decorator",
      data       = all_q,
      decorators = select_decorators(decorators, "table"),
      expr       = table
    )

    table_r <- reactive(decorated_table_q()[["table"]])
    teal.widgets::table_with_settings_srv(id = "table", table_r = table_r)

    decorated_table_q
  })
}
