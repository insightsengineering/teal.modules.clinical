#' Template: Patient Profile Timeline Plot
#'
#' Creates a valid expression to generate a patient profile timeline [ggplot2::ggplot()] plot using ADaM datasets.
#'
#' @inheritParams template_adverse_events
#' @inheritParams template_arguments
#' @param aetime_start (`character`)\cr name of start date/time of adverse event variable.
#' @param aetime_end (`character`)\cr name of end date/time of adverse event variable.
#' @param dstime_start (`character`)\cr name of date/time of first exposure to treatment variable.
#' @param dstime_end (`character`)\cr name of date/time of last exposure to treatment variable.
#' @param aerelday_start (`character`)\cr name of adverse event study start day variable.
#' @param aerelday_end (`character`)\cr name of adverse event study end day variable.
#' @param dsrelday_start (`character`)\cr name of concomitant medications study start day variable.
#' @param dsrelday_end (`character`)\cr name of concomitant medications study day start variable.
#' @param relative_day (`logical`)\cr whether to use relative days (`TRUE`) or absolute dates (`FALSE`).
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_pp_patient_timeline()]
#'
#' @keywords internal
template_patient_timeline <- function(dataname = "ANL",
                                      aeterm = "AETERM",
                                      aetime_start = "ASTDTM",
                                      aetime_end = "AENDTM",
                                      dstime_start = "CMASTDTM",
                                      dstime_end = "CMAENDTM",
                                      cmdecod = "CMDECOD",
                                      aerelday_start = NULL,
                                      aerelday_end = NULL,
                                      dsrelday_start = NULL,
                                      dsrelday_end = NULL,
                                      relative_day = FALSE,
                                      patient_id,
                                      font_size = 12L,
                                      ggplot2_args = teal.widgets::ggplot2_args()) {
  # Note: The variables used for aetime_start, aetime_end, dstime_start and dstime_end are to be
  # updated after random.cdisc.data updates.

  checkmate::assert_string(dataname)
  checkmate::assert_string(aeterm, null.ok = TRUE)
  checkmate::assert_string(aetime_start, null.ok = TRUE)
  checkmate::assert_string(aetime_end, null.ok = TRUE)
  checkmate::assert_string(dstime_start, null.ok = TRUE)
  checkmate::assert_string(dstime_end, null.ok = TRUE)
  checkmate::assert_string(cmdecod, null.ok = TRUE)
  checkmate::assert_string(aerelday_start, null.ok = TRUE)
  checkmate::assert_string(dsrelday_start, null.ok = TRUE)
  checkmate::assert_number(font_size)
  checkmate::assert_flag(relative_day)
  checkmate::assert_string(patient_id)

  chart_list <- list()
  if (!relative_day) {
    parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
      teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args,
        module_plot = teal.widgets::ggplot2_args(
          labs = list(title = paste0("Patient ID: ", patient_id), x = "Absolute Study Dates"),
          theme = list(
            plot.title = substitute(
              ggplot2::element_text(hjust = 0, size = font_size_var),
              list(font_size_var = font_size)
            ),
            axis.text = substitute(
              ggplot2::element_text(size = font_size_var, face = "bold", colour = "black"),
              list(font_size_var = font_size)
            ),
            axis.title = substitute(
              ggplot2::element_text(size = font_size_var, face = "bold", colour = "black"),
              list(font_size_var = font_size)
            ),
            text = substitute(ggplot2::element_text(size = font_size_var), list(font_size_var = font_size))
          )
        )
      )
    )

    chart_list <- add_expr(
      chart_list,
      new_expr = quote({
        posixct_origin <- "1970-01-01 00:00.00 UTC"
        med_chart <- NULL
        ae_chart <- NULL
      })
    )

    if (all(vapply(list(cmdecod, dstime_start, dstime_end), Negate(is.null), logical(1)))) {
      chart_list <- add_expr(
        chart_list,
        substitute(
          expr = {
            med_chart <- dataname %>%
              dplyr::select(dstime_start, dstime_end, cmdecod) %>%
              dplyr::distinct()

            colnames(med_chart) <- c("start", "end", "event")
            med_chart$group <- "Medication"
          },
          env = list(
            dataname = as.name(dataname),
            dstime_start = `if`(length(dstime_start), as.name(dstime_start), dstime_start),
            dstime_end = `if`(length(dstime_end), as.name(dstime_end), dstime_end),
            cmdecod = `if`(length(cmdecod), as.name(cmdecod), cmdecod)
          )
        )
      )
    }

    if (all(vapply(list(aeterm, aetime_start, aetime_end), Negate(is.null), logical(1)))) {
      chart_list <- add_expr(
        chart_list,
        substitute(
          expr = {
            ae_chart <- dataname %>%
              dplyr::select(aetime_start, aetime_end, aeterm) %>%
              dplyr::distinct()
            colnames(ae_chart) <- c("start", "end", "event")
            ae_chart$group <- "Adverse Events"
          },
          env = list(
            dataname = as.name(dataname),
            aeterm = `if`(length(aeterm), as.name(aeterm), aeterm),
            aetime_start = `if`(length(aetime_start), as.name(aetime_start), aetime_start),
            aetime_end = `if`(length(aetime_end), as.name(aetime_end), aetime_end)
          )
        )
      )
    }

    chart_list <- add_expr(
      chart_list,
      substitute(
        expr = {
          vistime_data <- dplyr::bind_rows(list(ae_chart, med_chart))
          # in some cases, dates are converted to numeric so this is a step to convert them back
          vistime_data$start <- as.POSIXct(vistime_data$start, origin = posixct_origin)
          vistime_data$end <- as.POSIXct(vistime_data$end, origin = posixct_origin)
          vistime_data <- vistime_data %>%
            dplyr::filter(stats::complete.cases(.[, c("start", "end", "event")])) %>%
            dplyr::filter(!is.na(format(.data$start))) %>%
            dplyr::filter(!is.na(format(.data$end)))

          if (nrow(vistime_data) == 0 || all(is.na(format(c(vistime_data$start, vistime_data$end))))) {
            empty_plot_label <- "Empty Plot (either due to filtering or missing values).\n Consider relaxing filters."
            df <- data.frame(
              x = 0,
              y = 0,
              label = empty_plot_label
            )
            patient_timeline_plot <- ggplot2::ggplot(
              data = df,
              ggplot2::aes(
                x = x,
                y = y,
                label = label
              )
            ) +
              ggplot2::geom_label() +
              ggplot2::theme_void()
          } else {
            patient_timeline_plot <- vistime::gg_vistime(
              vistime_data,
              col.event = "event",
              col.group = "group",
              show_labels = FALSE
            ) +
              ggrepel::geom_text_repel(
                mapping = ggplot2::aes(label = event),
                size = font_size_var / 3.5,
                color = "black",
                direction = "x",
                nudge_x = 0.5,
                segment.size = 0.1
              ) +
              ggplot2::scale_x_datetime(labels = scales::date_format("%b-%Y")) + labs + themes
          }
          plot <- patient_timeline_plot
        },
        env = list(
          font_size_var = font_size,
          labs = parsed_ggplot2_args$labs,
          themes = parsed_ggplot2_args$theme
        )
      )
    )
  } else {
    parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
      teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args,
        module_plot = teal.widgets::ggplot2_args(
          labs = list(title = paste0("Patient ID: ", patient_id), x = "Relative Study Days", y = ""),
          theme = list(
            plot.title = substitute(
              ggplot2::element_text(hjust = 0, size = font_size_var),
              list(font_size_var = font_size)
            ),
            axis.text = substitute(
              ggplot2::element_text(size = font_size_var, face = "bold", colour = "black"),
              list(font_size_var = font_size)
            ),
            axis.title = substitute(
              ggplot2::element_text(size = font_size_var, face = "bold", colour = "black"),
              list(font_size_var = font_size)
            ),
            text = substitute(ggplot2::element_text(size = font_size_var), list(font_size_var = font_size)),
            legend.position = "none"
          )
        )
      ),
      ggtheme = "classic"
    )

    chart_list <- add_expr(
      chart_list,
      new_expr = quote({
        med_chart <- NULL
        ae_chart <- NULL
      })
    )

    if (length(c(dsrelday_start, dsrelday_end, cmdecod)) == 3) {
      chart_list <- add_expr(
        chart_list,
        substitute(
          expr = {
            med_chart <- dataname %>%
              dplyr::select(dsrelday_start_var, dsrelday_end_var, cmdecod) %>%
              dplyr::distinct() %>%
              dplyr::rename(start = dsrelday_start_var, end = dsrelday_end_var, event = cmdecod) %>%
              dplyr::mutate(group = "Medication")
          },
          env = list(
            dataname = as.name(dataname),
            cmdecod = cmdecod,
            dsrelday_start_var = dsrelday_start,
            dsrelday_end_var = dsrelday_end
          )
        )
      )
    }

    if (length(c(aerelday_start, aerelday_end, aeterm)) == 3) {
      chart_list <- add_expr(
        chart_list,
        substitute(
          expr = {
            ae_chart <- dataname %>%
              dplyr::select(aerelday_start_var, aerelday_end_var, aeterm) %>%
              dplyr::distinct() %>%
              dplyr::rename(start = aerelday_start_var, end = aerelday_end_var, event = aeterm) %>%
              dplyr::mutate(group = "Adverse Events")
          },
          env = list(
            dataname = as.name(dataname),
            aeterm = aeterm,
            aerelday_start_var = aerelday_start,
            aerelday_end_var = aerelday_end
          )
        )
      )
    }

    chart_list <- add_expr(
      chart_list,
      substitute(
        expr = {
          vistime_data <- dplyr::bind_rows(list(ae_chart, med_chart))
          vistime_data <- vistime_data %>%
            dplyr::filter(stats::complete.cases(.[, c("start", "end", "event")])) %>%
            dplyr::filter(!is.na(format(.data$start))) %>%
            dplyr::filter(!is.na(format(.data$end))) %>%
            dplyr::mutate(color = make.unique(group))

          if (nrow(vistime_data) == 0 || all(is.na(format(c(vistime_data$start, vistime_data$end))))) {
            empty_plot_label <- "Empty Plot (either due to filtering or missing values).\n Consider relaxing filters."
            df <- data.frame(
              x = 0,
              y = 0,
              label = empty_plot_label
            )
            patient_timeline_plot <- ggplot2::ggplot(
              data = df,
              ggplot2::aes(
                x = x,
                y = y,
                label = label
              )
            ) +
              ggplot2::geom_label() +
              ggplot2::theme_void()
          } else {
            vistime_data$event <- factor(vistime_data$event, levels = rev(unique(vistime_data$event)))
            vistime_data$group <- factor(vistime_data$group, levels = unique(vistime_data$group))
            patient_timeline_plot <- ggplot2::ggplot(
              vistime_data,
              ggplot2::aes(x = start, y = event, xend = end, yend = event, color = color)
            ) +
              ggplot2::geom_segment(size = 4) +
              ggplot2::facet_grid(group ~ ., scales = "free", space = "free") +
              ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
              labs +
              ggthemes +
              themes
          }
          plot <- patient_timeline_plot
        },
        env = list(
          labs = parsed_ggplot2_args$labs,
          ggthemes = parsed_ggplot2_args$ggtheme,
          themes = parsed_ggplot2_args$theme
        )
      )
    )
  }

  chart_list
}

#' teal Module: Patient Profile Timeline Plot
#'
#' This module produces a patient profile timeline [ggplot2::ggplot()] type plot using ADaM datasets.
#'
#' @inheritParams tm_g_pp_adverse_events
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_patient_timeline
#' @param dataname_adcm (`character`)\cr name of `ADCM` dataset or equivalent.
#' @param dataname_adae (`character`)\cr name of `ADAE` dataset or equivalent.
#' @param aerelday_start ([teal.transform::choices_selected()])\cr object
#'   with all available choices and preselected option for the `ASTDY` variable from `dataname_adae`.
#' @param aerelday_end ([teal.transform::choices_selected()])\cr object
#'   with all available choices and preselected option for the `AENDY` variable from `dataname_adae`.
#' @param dsrelday_start ([teal.transform::choices_selected()])\cr object
#'   with all available choices and preselected option for the `ASTDY` variable from `dataname_adcm`.
#' @param dsrelday_end ([teal.transform::choices_selected()])\cr object
#'   with all available choices and preselected option for the `AENDY` variable from `dataname_adcm`.
#' @param cmdecod ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `CMDECOD` variable from `dataname_adcm`.
#' @param aetime_start ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for the `ASTDTM` variable from `dataname_adae`.
#' @param aetime_end ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `AENDTM` variable from `dataname_adae`.
#' @param dstime_start ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for the `CMASTDTM` variable from `dataname_adcm`.
#' @param dstime_end ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the `CMAENDTM` variable from `dataname_adcm`.
#'
#' @inherit module_arguments return
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_pp_patient_timeline(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied only to `plot` output
#'    )
#' )
#' ```
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.clinical")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' library(nestcolor)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   library(teal.modules.clinical)
#'   library(formatters)
#'   library(dplyr)
#'   ADAE <- tmc_ex_adae
#'   ADSL <- filter(tmc_ex_adsl, USUBJID %in% ADAE$USUBJID)
#'   ADCM <- tmc_ex_adcm %>%
#'     mutate(
#'       CMSTDY = case_when(
#'         CMCAT == "medcl B" ~ 20,
#'         CMCAT == "medcl C" ~ 150,
#'         TRUE ~ 1
#'       ) %>% with_label("Study Day of Start of Medication"),
#'       CMENDY = case_when(
#'         CMCAT == "medcl B" ~ 700,
#'         CMCAT == "medcl C" ~ 1000,
#'         TRUE ~ 500
#'       ) %>% with_label("Study Day of End of Medication"),
#'       CMASTDTM = ASTDTM,
#'       CMAENDTM = AENDTM
#'     )
#' })
#'
#' join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADAE", "ADCM")]
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#' join_keys(data)["ADCM", "ADCM"] <- adcm_keys
#' join_keys(data)["ADAE", "ADCM"] <- c("STUDYID", "USUBJID")
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_pp_patient_timeline(
#'       label = "Patient Timeline",
#'       dataname_adae = "ADAE",
#'       dataname_adcm = "ADCM",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       cmdecod = choices_selected(
#'         choices = variable_choices(data[["ADCM"]], "CMDECOD"),
#'         selected = "CMDECOD",
#'       ),
#'       aeterm = choices_selected(
#'         choices = variable_choices(data[["ADAE"]], "AETERM"),
#'         selected = c("AETERM")
#'       ),
#'       aetime_start = choices_selected(
#'         choices = variable_choices(data[["ADAE"]], "ASTDTM"),
#'         selected = c("ASTDTM")
#'       ),
#'       aetime_end = choices_selected(
#'         choices = variable_choices(data[["ADAE"]], "AENDTM"),
#'         selected = c("AENDTM")
#'       ),
#'       dstime_start = choices_selected(
#'         choices = variable_choices(data[["ADCM"]], "CMASTDTM"),
#'         selected = c("CMASTDTM")
#'       ),
#'       dstime_end = choices_selected(
#'         choices = variable_choices(data[["ADCM"]], "CMAENDTM"),
#'         selected = c("CMAENDTM")
#'       ),
#'       aerelday_start = choices_selected(
#'         choices = variable_choices(data[["ADAE"]], "ASTDY"),
#'         selected = c("ASTDY")
#'       ),
#'       aerelday_end = choices_selected(
#'         choices = variable_choices(data[["ADAE"]], "AENDY"),
#'         selected = c("AENDY")
#'       ),
#'       dsrelday_start = choices_selected(
#'         choices = variable_choices(data[["ADCM"]], "ASTDY"),
#'         selected = c("ASTDY")
#'       ),
#'       dsrelday_end = choices_selected(
#'         choices = variable_choices(data[["ADCM"]], "AENDY"),
#'         selected = c("AENDY")
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_pp_patient_timeline <- function(label,
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
  message("Initializing tm_g_pp_patient_timeline")

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
    ui         = ui_g_patient_timeline,
    ui_args    = args[names(args) %in% names(formals(ui_g_patient_timeline))],
    server     = srv_g_patient_timeline,
    server_args = args[names(args) %in% names(formals(srv_g_patient_timeline))],
    transformators = transformators,
    datanames  = c(dataname_adcm, dataname_adae, parentname)
  )
}

#' @keywords internal
ui_g_patient_timeline <- function(id,
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
srv_g_patient_timeline <- function(id,
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
