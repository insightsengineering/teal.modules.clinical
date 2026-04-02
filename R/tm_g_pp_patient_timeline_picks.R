#' teal Module: Patient Profile Timeline Plot (teal.picks)
#'
#' This module produces a patient profile timeline [ggplot2::ggplot()] type plot using ADaM datasets.
#' This is the `teal.picks` implementation of [tm_g_pp_patient_timeline()].
#'
#' The module handles variables from two datasets (`dataname_adae` and `dataname_adcm`) and supports
#' both absolute dates and relative study days on the x-axis.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_patient_timeline
#' @param aeterm (`teal.picks::variables`)\cr variable for AE reported term from `dataname_adae`.
#' @param cmdecod (`teal.picks::variables` or `NULL`)\cr variable for medication standardized term from `dataname_adcm`.
#' @param aetime_start (`teal.picks::variables` or `NULL`)\cr variable for AE start datetime from `dataname_adae`.
#' @param aetime_end (`teal.picks::variables` or `NULL`)\cr variable for AE end datetime from `dataname_adae`.
#' @param dstime_start (`teal.picks::variables` or `NULL`)\cr variable for medication start datetime from `dataname_adcm`.
#' @param dstime_end (`teal.picks::variables` or `NULL`)\cr variable for medication end datetime from `dataname_adcm`.
#' @param aerelday_start (`teal.picks::variables` or `NULL`)\cr variable for AE relative start day from `dataname_adae`.
#' @param aerelday_end (`teal.picks::variables` or `NULL`)\cr variable for AE relative end day from `dataname_adae`.
#' @param dsrelday_start (`teal.picks::variables` or `NULL`)\cr variable for medication relative start day from `dataname_adcm`.
#' @param dsrelday_end (`teal.picks::variables` or `NULL`)\cr variable for medication relative end day from `dataname_adcm`.
#'
#' @inherit module_arguments return
#'
#' @export
tm_g_pp_patient_timeline.picks <- function(label,
                                            dataname_adcm = "ADCM",
                                            dataname_adae = "ADAE",
                                            parentname    = "ADSL",
                                            patient_col   = "USUBJID",
                                            aeterm        = teal.picks::variables("AETERM",   fixed = TRUE),
                                            cmdecod       = teal.picks::variables("CMDECOD",  fixed = TRUE),
                                            aetime_start  = teal.picks::variables("ASTDTM",   fixed = TRUE),
                                            aetime_end    = teal.picks::variables("AENDTM",   fixed = TRUE),
                                            dstime_start  = teal.picks::variables("CMASTDTM", fixed = TRUE),
                                            dstime_end    = teal.picks::variables("CMAENDTM", fixed = TRUE),
                                            aerelday_start = teal.picks::variables("ASTDY",   fixed = TRUE),
                                            aerelday_end   = teal.picks::variables("AENDY",   fixed = TRUE),
                                            dsrelday_start = teal.picks::variables("ASTDY",   fixed = TRUE),
                                            dsrelday_end   = teal.picks::variables("AENDY",   fixed = TRUE),
                                            font_size   = c(12L, 12L, 25L),
                                            plot_height = c(700L, 200L, 2000L),
                                            plot_width  = NULL,
                                            pre_output  = NULL,
                                            post_output = NULL,
                                            ggplot2_args = teal.widgets::ggplot2_args(),
                                            transformators = list(),
                                            decorators = list()) {
  message("Initializing tm_g_pp_patient_timeline.picks")

  # Compatibility: coerce choices_selected
  for (arg in c(
    "aeterm", "cmdecod",
    "aetime_start", "aetime_end", "dstime_start", "dstime_end",
    "aerelday_start", "aerelday_end", "dsrelday_start", "dsrelday_end"
  )) {
    val <- get(arg)
    if (!is.null(val) && inherits(val, "choices_selected")) assign(arg, teal.picks::as.picks(val))
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname_adcm)
  checkmate::assert_string(dataname_adae)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  for (arg in c("aeterm", "cmdecod",
                "aetime_start", "aetime_end", "dstime_start", "dstime_end",
                "aerelday_start", "aerelday_end", "dsrelday_start", "dsrelday_end")) {
    checkmate::assert_class(get(arg), "variables", null.ok = TRUE)
  }
  checkmate::assert_numeric(font_size, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(font_size[1], lower = font_size[2], upper = font_size[3], .var.name = "font_size")
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  assert_decorators(decorators, "plot")

  xor_error_string <- function(x, y) {
    paste("Both `", x, "` and `", y, "` need to be provided or both need to be `NULL`.")
  }
  if (xor(is.null(aetime_start),   is.null(aetime_end)))   stop(xor_error_string("aetime_start",   "aetime_end"))
  if (xor(is.null(dstime_start),   is.null(dstime_end)))   stop(xor_error_string("dstime_start",   "dstime_end"))
  if (xor(is.null(aerelday_start), is.null(aerelday_end))) stop(xor_error_string("aerelday_start", "aerelday_end"))
  if (xor(is.null(dsrelday_start), is.null(dsrelday_end))) stop(xor_error_string("dsrelday_start", "dsrelday_end"))
  if (is.null(aeterm) && is.null(cmdecod)) {
    stop("At least one of 'aeterm' or 'cmdecod' needs to be provided.")
  }

  # Build picks — AE variables bound to dataname_adae, CM variables to dataname_adcm
  picks_adae_vars <- Filter(Negate(is.null), list(
    aeterm        = aeterm,
    aetime_start  = aetime_start,
    aetime_end    = aetime_end,
    aerelday_start = aerelday_start,
    aerelday_end   = aerelday_end
  ))
  picks_adcm_vars <- Filter(Negate(is.null), list(
    cmdecod      = cmdecod,
    dstime_start = dstime_start,
    dstime_end   = dstime_end,
    dsrelday_start = dsrelday_start,
    dsrelday_end   = dsrelday_end
  ))

  picks_adae <- if (length(picks_adae_vars) > 0) {
    do.call(teal.picks::picks, c(list(teal.picks::datasets(dataname_adae)), picks_adae_vars))
  } else NULL

  picks_adcm <- if (length(picks_adcm_vars) > 0) {
    do.call(teal.picks::picks, c(list(teal.picks::datasets(dataname_adcm)), picks_adcm_vars))
  } else NULL

  # Combine into a single picks list for the module
  picks_timeline <- c(
    if (!is.null(picks_adae)) as.list(picks_adae) else list(),
    if (!is.null(picks_adcm)) as.list(picks_adcm) else list()
  )

  args <- as.list(environment())

  module(
    label      = label,
    ui         = ui_g_patient_timeline.picks,
    ui_args    = args[names(args) %in% names(formals(ui_g_patient_timeline.picks))],
    server     = srv_g_patient_timeline.picks,
    server_args = args[names(args) %in% names(formals(srv_g_patient_timeline.picks))],
    transformators = transformators,
    datanames  = c(dataname_adcm, dataname_adae, parentname)
  )
}

#' @keywords internal
ui_g_patient_timeline.picks <- function(id,
                                         picks_timeline,
                                         picks_adae,
                                         picks_adcm,
                                         aeterm,
                                         cmdecod,
                                         aetime_start,
                                         aetime_end,
                                         dstime_start,
                                         dstime_end,
                                         aerelday_start,
                                         aerelday_end,
                                         dsrelday_start,
                                         dsrelday_end,
                                         font_size,
                                         pre_output,
                                         post_output,
                                         decorators) {
  ns <- NS(id)
  has_relday  <- !is.null(aerelday_start) || !is.null(dsrelday_start)
  has_abstime <- !is.null(aetime_start)   || !is.null(dstime_start)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("patient_timeline_plot")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      if (!is.null(cmdecod)) {
        tags$div(
          tags$label("Select Medication standardized term variable:"),
          teal.picks::picks_ui(ns("cmdecod"), picks_timeline["cmdecod"])
        )
      },
      if (!is.null(aeterm)) {
        tags$div(
          tags$label("Select AE reported term variable:"),
          teal.picks::picks_ui(ns("aeterm"), picks_timeline["aeterm"])
        )
      },
      if (has_relday || has_abstime) {
        tagList(
          checkboxInput(ns("relday_x_axis"),
            label = "Use relative days on the x-axis",
            value = has_relday
          ),
          if (has_relday) {
            conditionalPanel(
              "input.relday_x_axis == true", ns = ns,
              if (!is.null(aerelday_start)) {
                tagList(
                  tags$div(
                    tags$label("Select AE relative start day variable:"),
                    teal.picks::picks_ui(ns("aerelday_start"), picks_timeline["aerelday_start"])
                  ),
                  tags$div(
                    tags$label("Select AE relative end day variable:"),
                    teal.picks::picks_ui(ns("aerelday_end"), picks_timeline["aerelday_end"])
                  )
                )
              },
              if (!is.null(dsrelday_start)) {
                tagList(
                  tags$div(
                    tags$label("Select Medication relative start day variable:"),
                    teal.picks::picks_ui(ns("dsrelday_start"), picks_timeline["dsrelday_start"])
                  ),
                  tags$div(
                    tags$label("Select Medication relative end day variable:"),
                    teal.picks::picks_ui(ns("dsrelday_end"), picks_timeline["dsrelday_end"])
                  )
                )
              }
            )
          }
        )
      } else {
        shinyjs::hidden(checkboxInput(ns("relday_x_axis"), label = "", value = FALSE))
      },
      if (has_abstime) {
        conditionalPanel(
          "input.relday_x_axis == false", ns = ns,
          if (!is.null(aetime_start)) {
            tagList(
              tags$div(
                tags$label("Select ASTDTM variable:"),
                teal.picks::picks_ui(ns("aetime_start"), picks_timeline["aetime_start"])
              ),
              tags$div(
                tags$label("Select AENDTM variable:"),
                teal.picks::picks_ui(ns("aetime_end"), picks_timeline["aetime_end"])
              )
            )
          },
          if (!is.null(dstime_start)) {
            tagList(
              tags$div(
                tags$label("Select TRTSDTM variable:"),
                teal.picks::picks_ui(ns("dstime_start"), picks_timeline["dstime_start"])
              ),
              tags$div(
                tags$label("Select TRTEDTM variable:"),
                teal.picks::picks_ui(ns("dstime_end"), picks_timeline["dstime_end"])
              )
            )
          }
        )
      },
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"), "Font Size", font_size, ticks = FALSE, step = 1
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_patient_timeline.picks <- function(id,
                                          data,
                                          dataname_adae,
                                          dataname_adcm,
                                          parentname,
                                          patient_col,
                                          picks_timeline,
                                          picks_adae,
                                          picks_adcm,
                                          aeterm,
                                          cmdecod,
                                          aetime_start,
                                          aetime_end,
                                          dstime_start,
                                          dstime_end,
                                          aerelday_start,
                                          aerelday_end,
                                          dsrelday_start,
                                          dsrelday_end,
                                          plot_height,
                                          plot_width,
                                          label,
                                          ggplot2_args,
                                          decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    patient_id <- reactive(input$patient_id)

    # Init patient selector
    patient_data_base <- reactive(unique(data()[[parentname]][[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session, "patient_id",
      choices  = patient_data_base(),
      selected = patient_data_base()[1]
    )
    observeEvent(patient_data_base(), ignoreInit = TRUE, {
      teal.widgets::updateOptionalSelectInput(
        session, "patient_id",
        choices  = patient_data_base(),
        selected = if (length(patient_data_base()) == 1) {
          patient_data_base()
        } else {
          intersect(patient_id(), patient_data_base())
        }
      )
    })

    # Build selector IDs — only include variables that were provided
    selector_ids <- names(Filter(Negate(is.null), list(
      aeterm = aeterm, cmdecod = cmdecod,
      aetime_start = aetime_start, aetime_end = aetime_end,
      dstime_start = dstime_start, dstime_end = dstime_end,
      aerelday_start = aerelday_start, aerelday_end = aerelday_end,
      dsrelday_start = dsrelday_start, dsrelday_end = dsrelday_end
    )))

    selectors <- teal.picks::picks_srv(
      picks = picks_timeline[selector_ids],
      data  = data
    )

    validated_q <- reactive({
      obj <- req(data())

      validate_input(
        inputId   = "patient_id",
        condition = !is.null(input$patient_id) && nzchar(input$patient_id),
        message   = "Please select a patient."
      )
      if (!is.null(aeterm)) {
        validate_input(
          inputId   = "aeterm-variables-selected",
          condition = length(selectors$aeterm()$variables$selected) > 0,
          message   = "Please select AE term variable."
        )
      }
      if (!is.null(cmdecod)) {
        validate_input(
          inputId   = "cmdecod-variables-selected",
          condition = length(selectors$cmdecod()$variables$selected) > 0,
          message   = "Please select Medication term variable."
        )
      }

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's output(s)")
      )
      obj
    })

    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data        = validated_q,
      selectors   = selectors,
      output_name = "ANL"
    )

    all_q <- reactive({
      vars <- anl_inputs$variables()
      ANL  <- anl_inputs$data()[["ANL"]]

      p_time_data_pat <- ANL[ANL[[patient_col]] == patient_id(), ]

      use_relday <- isTRUE(input$relday_x_axis)

      if (!use_relday) {
        abs_ae_cols  <- c(vars$aetime_start,  vars$aetime_end)
        abs_cm_cols  <- c(vars$dstime_start,  vars$dstime_end)
        abs_ae_cols  <- abs_ae_cols[!is.null(abs_ae_cols)]
        abs_cm_cols  <- abs_cm_cols[!is.null(abs_cm_cols)]
        validate(need(
          (length(abs_ae_cols) > 0 &&
             sum(stats::complete.cases(p_time_data_pat[, abs_ae_cols, drop = FALSE])) > 0) ||
          (length(abs_cm_cols) > 0 &&
             sum(stats::complete.cases(p_time_data_pat[, abs_cm_cols, drop = FALSE])) > 0),
          "Selected patient has no valid absolute dates. Consider relaxing filters."
        ))
      } else {
        rel_ae_cols <- c(vars$aerelday_start, vars$aerelday_end)
        rel_cm_cols <- c(vars$dsrelday_start, vars$dsrelday_end)
        rel_ae_cols <- rel_ae_cols[!is.null(rel_ae_cols)]
        rel_cm_cols <- rel_cm_cols[!is.null(rel_cm_cols)]
        validate(need(
          (length(rel_ae_cols) > 0 &&
             sum(stats::complete.cases(p_time_data_pat[, rel_ae_cols, drop = FALSE])) > 0) ||
          (length(rel_cm_cols) > 0 &&
             sum(stats::complete.cases(p_time_data_pat[, rel_cm_cols, drop = FALSE])) > 0),
          "Selected patient has no valid relative days. Consider relaxing filters."
        ))
      }

      patient_timeline_calls <- template_patient_timeline(
        dataname       = "ANL",
        aeterm         = vars$aeterm,
        aetime_start   = vars$aetime_start,
        aetime_end     = vars$aetime_end,
        dstime_start   = vars$dstime_start,
        dstime_end     = vars$dstime_end,
        cmdecod        = vars$cmdecod,
        aerelday_start = vars$aerelday_start,
        aerelday_end   = vars$aerelday_end,
        dsrelday_start = vars$dsrelday_start,
        dsrelday_end   = vars$dsrelday_end,
        font_size      = input[["font_size"]],
        relative_day   = use_relday,
        patient_id     = patient_id(),
        ggplot2_args   = ggplot2_args
      )

      qenv <- teal.code::eval_code(
        anl_inputs$data(),
        substitute(
          expr = { ANL <- ANL[ANL[[patient_col]] == patient_id, ] },
          env  = list(patient_col = patient_col, patient_id = patient_id())
        )
      )
      obj <- qenv
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, as.expression(patient_timeline_calls))
    })

    decorated_all_q <- srv_decorate_teal_data(
      "decorator",
      data       = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr       = plot
    )

    plot_r <- reactive(decorated_all_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id     = "patient_timeline_plot",
      plot_r = plot_r,
      height = plot_height,
      width  = plot_width
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
