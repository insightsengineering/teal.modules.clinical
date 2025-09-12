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
                                     parentname = "ADSL",
                                     patient_col = "USUBJID",
                                     aeterm = NULL,
                                     cmdecod = NULL,
                                     aetime_start = NULL,
                                     aetime_end = NULL,
                                     dstime_start = NULL,
                                     dstime_end = NULL,
                                     aerelday_start = NULL,
                                     aerelday_end = NULL,
                                     dsrelday_start = NULL,
                                     dsrelday_end = NULL,
                                     font_size = c(12L, 12L, 25L),
                                     plot_height = c(700L, 200L, 2000L),
                                     plot_width = NULL,
                                     pre_output = NULL,
                                     post_output = NULL,
                                     ggplot2_args = teal.widgets::ggplot2_args(),
                                     transformators = list(),
                                     decorators = list()) {
  message("Initializing tm_g_pp_patient_timeline")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname_adcm)
  checkmate::assert_string(dataname_adae)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(aeterm, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(cmdecod, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(aetime_start, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(aetime_end, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(dstime_start, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(dstime_end, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(aerelday_start, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(aerelday_end, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(dsrelday_start, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(dsrelday_end, "choices_selected", null.ok = TRUE)
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
    paste(
      "Assertion on `", x, "` and `", y, "` failed:",
      "Both `", x, "` and `", y, "` needs to be provided or both need to be `NULL`."
    )
  }

  if (xor(is.null(aetime_start), is.null(aetime_end))) stop(xor_error_string("aetime_start", "aetime_end"))
  if (xor(is.null(dstime_start), is.null(dstime_end))) stop(xor_error_string("dstime_start", "dstime_end"))
  if (xor(is.null(aerelday_start), is.null(aerelday_end))) stop(xor_error_string("aerelday_start", "aerelday_end"))
  if (xor(is.null(dsrelday_start), is.null(dsrelday_end))) stop(xor_error_string("dsrelday_start", "dsrelday_end"))

  if (is.null(aeterm) && is.null(cmdecod)) {
    stop("At least one of 'aeterm' or 'cmdecod' needs to be provided.")
  }
  if (!is.null(aeterm) && (is.null(aetime_start) || is.null(aerelday_start))) {
    stop("If 'aeterm' is provided, then one of 'aetime_start' and 'aerelday_start' must not be empty.")
  }
  if (!is.null(cmdecod) && (is.null(dstime_start) || is.null(dsrelday_start))) {
    stop("If 'cmdecod' is provided, then one of 'dstime_start' and 'dsrelday_start' must not be empty.")
  }

  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  args <- as.list(environment())
  data_extract_list <- list(
    aeterm = `if`(is.null(aeterm), NULL, cs_to_des_select(aeterm, dataname = dataname_adae)),
    cmdecod = `if`(is.null(cmdecod), NULL, cs_to_des_select(cmdecod, dataname = dataname_adcm)),
    aetime_start = `if`(is.null(aetime_start), NULL, cs_to_des_select(aetime_start, dataname = dataname_adae)),
    aetime_end = `if`(is.null(aetime_end), NULL, cs_to_des_select(aetime_end, dataname = dataname_adae)),
    dstime_start = `if`(is.null(dstime_start), NULL, cs_to_des_select(dstime_start, dataname = dataname_adcm)),
    dstime_end = `if`(is.null(dstime_end), NULL, cs_to_des_select(dstime_end, dataname = dataname_adcm)),
    aerelday_start = `if`(is.null(aerelday_start), NULL, cs_to_des_select(aerelday_start, dataname = dataname_adae)),
    aerelday_end = `if`(is.null(aerelday_end), NULL, cs_to_des_select(aerelday_end, dataname = dataname_adae)),
    dsrelday_start = `if`(is.null(dsrelday_start), NULL, cs_to_des_select(dsrelday_start, dataname = dataname_adcm)),
    dsrelday_end = `if`(is.null(dsrelday_end), NULL, cs_to_des_select(dsrelday_end, dataname = dataname_adcm))
  )

  module(
    label = label,
    ui = ui_g_patient_timeline,
    ui_args = c(data_extract_list, args),
    server = srv_g_patient_timeline,
    server_args = c(
      data_extract_list,
      list(
        dataname_adae = dataname_adae,
        dataname_adcm = dataname_adcm,
        parentname = parentname,
        label = label,
        patient_col = patient_col,
        plot_height = plot_height,
        plot_width = plot_width,
        ggplot2_args = ggplot2_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = c(dataname_adcm, dataname_adae, parentname)
  )
}

#' @keywords internal
ui_g_patient_timeline <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$aeterm,
    ui_args$cmdecod,
    ui_args$aetime_start,
    ui_args$aetime_end,
    ui_args$dstime_start,
    ui_args$dstime_end,
    ui_args$aerelday_start,
    ui_args$aerelday_end,
    ui_args$dsrelday_start,
    ui_args$dsrelday_end
  )

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("patient_timeline_plot")),
    encoding = tags$div(
      ### Reporter
      teal.reporter::add_card_button_ui(ns("add_reporter"), label = "Add Report Card"),
      tags$br(), tags$br(),
      ###
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(
        ui_args[c(
          "aeterm", "cmdecod",
          "aetime_start", "aetime_end", "dstime_start", "dstime_end",
          "aerelday_start", "aerelday_end", "dsrelday_start", "dsrelday_end"
        )]
      ),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("cmdecod"),
        label = "Select Medication standardized term variable:",
        data_extract_spec = ui_args$cmdecod,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aeterm"),
        label = "Select AE reported term variable:",
        data_extract_spec = ui_args$aeterm,
        is_single_dataset = is_single_dataset_value
      ),
      if (!is.null(ui_args$aerelday_start) || !is.null(ui_args$dsrelday_start)) {
        tagList(
          checkboxInput(ns("relday_x_axis"), label = "Use relative days on the x-axis", value = TRUE),
          conditionalPanel(
            paste0("input.relday_x_axis == true"),
            ns = ns,
            if (!is.null(ui_args$aerelday_start)) {
              tagList(
                teal.transform::data_extract_ui(
                  id = ns("aerelday_start"),
                  label = "Select AE relative start date variable:",
                  data_extract_spec = ui_args$aerelday_start,
                  is_single_dataset = is_single_dataset_value
                ),
                teal.transform::data_extract_ui(
                  id = ns("aerelday_end"),
                  label = "Select AE relative end date variable:",
                  data_extract_spec = ui_args$aerelday_end,
                  is_single_dataset = is_single_dataset_value
                )
              )
            },
            if (!is.null(ui_args$dsrelday_start)) {
              tagList(
                teal.transform::data_extract_ui(
                  id = ns("dsrelday_start"),
                  label = "Select Medication relative start date variable:",
                  data_extract_spec = ui_args$dsrelday_start,
                  is_single_dataset = is_single_dataset_value
                ),
                teal.transform::data_extract_ui(
                  id = ns("dsrelday_end"),
                  label = "Select Medication relative end date variable:",
                  data_extract_spec = ui_args$dsrelday_end,
                  is_single_dataset = is_single_dataset_value
                )
              )
            }
          )
        )
      } else {
        shinyjs::hidden(checkboxInput(ns("relday_x_axis"), label = "", value = FALSE))
      },
      conditionalPanel(
        paste0("input.relday_x_axis == false"),
        ns = ns,
        teal.transform::data_extract_ui(
          id = ns("aetime_start"),
          label = "Select ASTDTM variable:",
          data_extract_spec = ui_args$aetime_start,
          is_single_dataset = is_single_dataset_value
        ),
        teal.transform::data_extract_ui(
          id = ns("aetime_end"),
          label = "Select AENDTM variable:",
          data_extract_spec = ui_args$aetime_end,
          is_single_dataset = is_single_dataset_value
        ),
        teal.transform::data_extract_ui(
          id = ns("dstime_start"),
          label = "Select TRTSDTM variable:",
          data_extract_spec = ui_args$dstime_start,
          is_single_dataset = is_single_dataset_value
        ),
        teal.transform::data_extract_ui(
          id = ns("dstime_end"),
          label = "Select TRTEDTM variable:",
          data_extract_spec = ui_args$dstime_end,
          is_single_dataset = is_single_dataset_value
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(ui_args$decorators, "plot")),
      bslib::accordion_panel(
        title = "Plot settings",
        open = TRUE,
        teal.widgets::optionalSliderInputValMinMax(
          ns("font_size"),
          "Font Size",
          ui_args$font_size,
          ticks = FALSE,
          step = 1
        )
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

#' @keywords internal
srv_g_patient_timeline <- function(id,
                                   data,
                                   reporter,
                                   filter_panel_api,
                                   dataname_adae,
                                   dataname_adcm,
                                   parentname,
                                   patient_col,
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
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    patient_id <- reactive(input$patient_id)

    # Init
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

    # Patient timeline tab ----
    check_box <- reactive(input$relday_x_axis)

    check_relative <- function(main_param, return_name) {
      function(value) {
        if (length(selector_list()[[main_param]]()$select) > 0 && length(value) == 0) {
          sprintf("Please add %s", return_name)
        }
      }
    }

    rule_one_parameter <- function(other) {
      function(value) {
        if (length(value) == 0L && length(selector_list()[[other]]()$select) == 0L) {
          "At least one term variable must be selected."
        }
      }
    }

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        dsrelday_start = dsrelday_start, dsrelday_end = dsrelday_end,
        aerelday_start = aerelday_start, aerelday_end = aerelday_end,
        aeterm = aeterm, aetime_start = aetime_start,
        aetime_end = aetime_end, dstime_start = dstime_start, dstime_end = dstime_end, cmdecod = cmdecod
      ),
      datasets = data,
      select_validation_rule = list(
        # aeterm
        aeterm = rule_one_parameter("cmdecod"),
        aerelday_start = check_relative("aeterm", "AE start date."),
        aerelday_end = check_relative("aeterm", "AE end date."),
        aetime_start = check_relative("aeterm", "AE start date."),
        aetime_end = check_relative("aeterm", "AE end date."),
        # cmdecod
        cmdecod = rule_one_parameter("aeterm"),
        dsrelday_start = check_relative("cmdecod", "Medication start date."),
        dsrelday_end = check_relative("cmdecod", "Medication end date."),
        dstime_start = check_relative("cmdecod", "Medication start date."),
        dstime_end = check_relative("cmdecod", "Medication end date.")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select a patient"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list
    )

    anl_q <- reactive({
      data() %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    all_q <- reactive({
      teal::validate_inputs(iv_r())

      aeterm <- input[[extract_input("aeterm", dataname_adae)]]
      aetime_start <- input[[extract_input("aetime_start", dataname_adae)]]
      aetime_end <- input[[extract_input("aetime_end", dataname_adae)]]
      dstime_start <- input[[extract_input("dstime_start", dataname_adcm)]]
      dstime_end <- input[[extract_input("dstime_end", dataname_adcm)]]
      cmdecod <- input[[extract_input("cmdecod", dataname_adcm)]]
      aerelday_start <- input[[extract_input("aerelday_start", dataname_adae)]]
      aerelday_end <- input[[extract_input("aerelday_end", dataname_adae)]]
      dsrelday_start <- input[[extract_input("dsrelday_start", dataname_adcm)]]
      dsrelday_end <- input[[extract_input("dsrelday_end", dataname_adcm)]]
      font_size <- input[["font_size"]]

      ae_chart_vars_null <- any(vapply(list(aeterm, aetime_start, aetime_end), is.null, FUN.VALUE = logical(1)))
      ds_chart_vars_null <- any(vapply(list(cmdecod, dstime_start, dstime_end), is.null, FUN.VALUE = logical(1)))

      p_timeline_data <- anl_q()[["ANL"]]
      # time variables can not be NA
      p_time_data_pat <- p_timeline_data[p_timeline_data[[patient_col]] == patient_id(), ]

      validate(
        need(
          input$relday_x_axis ||
            (
              sum(stats::complete.cases(p_time_data_pat[, c(aetime_start, aetime_end)])) > 0 ||
                sum(stats::complete.cases(p_time_data_pat[, c(dstime_start, dstime_end)])) > 0
            ),
          "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
        )
      )

      aerel_chart_vars_null <- any(vapply(list(aeterm, aerelday_start, aerelday_end), is.null, FUN.VALUE = logical(1)))
      dsrel_chart_vars_null <- any(vapply(list(cmdecod, dsrelday_start, dsrelday_end), is.null, FUN.VALUE = logical(1)))

      # These lines are needed because there is a naming conflict: ADCM and ADAE will be both pass in their ASTDY and
      # AENDY columns to data_merge_module call above.
      aerelday_start_name <- `if`(
        length(aerelday_start),
        anl_inputs()$columns_source$aerelday_start[[1]],
        aerelday_start
      )
      aerelday_end_name <- `if`(
        length(aerelday_end),
        anl_inputs()$columns_source$aerelday_end[[1]],
        aerelday_end
      )
      dsrelday_start_name <- `if`(
        length(dsrelday_start),
        anl_inputs()$columns_source$dsrelday_start[[1]],
        dsrelday_start
      )
      dsrelday_end_name <- `if`(
        length(dsrelday_end),
        anl_inputs()$columns_source$dsrelday_end[[1]],
        dsrelday_end
      )

      validate(
        need(
          !input$relday_x_axis ||
            (
              sum(stats::complete.cases(p_time_data_pat[, c(aerelday_start_name, aerelday_end_name)])) > 0 ||
                sum(stats::complete.cases(p_time_data_pat[, c(dsrelday_start_name, dsrelday_end_name)])) > 0
            ),
          "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
        )
      )

      patient_timeline_calls <- template_patient_timeline(
        dataname = "ANL",
        aeterm = aeterm,
        aetime_start = aetime_start,
        aetime_end = aetime_end,
        dstime_start = dstime_start,
        dstime_end = dstime_end,
        cmdecod = cmdecod,
        aerelday_start = aerelday_start_name,
        aerelday_end = aerelday_end_name,
        dsrelday_start = dsrelday_start_name,
        dsrelday_end = dsrelday_end_name,
        font_size = font_size,
        relative_day = input$relday_x_axis,
        patient_id = patient_id(),
        ggplot2_args = ggplot2_args
      )

      qenv <- teal.code::eval_code(
        anl_q(),
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )

      teal.code::eval_code(object = qenv, as.expression(patient_timeline_calls))
    })

    decorated_all_q <- srv_decorate_teal_data(
      "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr = plot
    )

    plot_r <- reactive(decorated_all_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "patient_timeline_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    # Render R code
    source_code_r <- reactive(teal.code::get_code(req(decorated_all_q())))
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Patient Profile Timeline Plot",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(source_code_r())
        card
      }
      teal.reporter::add_card_button_srv("add_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
