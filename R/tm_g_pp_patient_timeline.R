#' Template: Patient Timeline Tab
#'
#' Creates a patient timeline template call.
#'
#' @inheritParams template_arguments
#' @param aeterm (`character`)\cr name of the reported term for the adverse event variable.
#' @param aetime_start (`character`)\cr name of `datetime` start of adverse event variable.
#' @param aetime_end (`character`)\cr name of `datetime` end of adverse event variable.
#' @param dstime_start (`character`)\cr name of `datetime` first exposure to treatment variable.
#' @param dstime_end (`character`)\cr name of `datetime` last exposure to treatment variable.
#' @param cmdecod (`character`)\cr name of reported standardized name of drug, med, or therapy variable.
#' @param aerelday_start (`character`)\cr name of start study day variable.
#' @param aerelday_end (`character`)\cr name of end study day variable.
#' @param dsrelday_start (`character`)\cr name of start study day variable.
#' @param dsrelday_end (`character`)\cr name of end study day variable.
#' @param relative_day (`logical`)\cr whether to use relative days or absolute dates
#' @param patient_id (`character`)\cr patient ID.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#' @keywords internal
#'
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
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(aeterm) || is.null(aeterm),
    assertthat::is.string(aetime_start) || is.null(aetime_start),
    assertthat::is.string(aetime_end) || is.null(aetime_end),
    assertthat::is.string(dstime_start) || is.null(dstime_start),
    assertthat::is.string(dstime_end) || is.null(dstime_end),
    assertthat::is.string(cmdecod) || is.null(cmdecod),
    assertthat::is.string(aerelday_start) || is.null(aerelday_start),
    assertthat::is.string(dsrelday_start) || is.null(dsrelday_start),
    is.numeric(font_size),
    is.logical(relative_day),
    assertthat::is.string(patient_id)
  )

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
          patient_timeline_plot
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
          patient_timeline_plot
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

#' Teal Module: Patient Profile Timeline Teal Module
#'
#' This teal module produces a patient profile timeline plot using `ADaM` datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param aeterm ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AETERM} column of the `ADAE` dataset.
#' @param dataname_adcm (`character`)\cr name of `ADCM` dataset or equivalent.
#' @param dataname_adae (`character`)\cr name of `ADAE` dataset or equivalent.
#' @param aerelday_start ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{ASTDY} column of the `ADAE` dataset.
#' @param aerelday_end ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr \code{AENDY}
#' column of the `ADAE` dataset.
#' @param dsrelday_start ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr \code{ASTDY}
#' column of the `ADCM` dataset.
#' @param dsrelday_end ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr \code{AENDY}
#' column of the `ADCM` dataset.
#' @param cmdecod ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{cmdecod} column of the `ADCM` dataset.
#' @param aetime_start ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{ASTDTM} column of the `AE` start of the `ADAE` dataset.
#' @param aetime_end ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AENDTM} column of the `AE` end of the `ADAE` dataset.
#' @param dstime_start ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMASTDTM} column of treatment start of the `ADCM` dataset.
#' @param dstime_end ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMAENDTM} column of treatment end of the `ADCM` dataset.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#'
#' @export
#'
#' @examples
#' library(nestcolor)
#'
#' adae <- tmc_ex_adae
#' adsl <- tmc_ex_adsl %>% dplyr::filter(USUBJID %in% adae$USUBJID)
#' adcm <- tmc_ex_adcm %>% dplyr::mutate(
#'   CMSTDY = dplyr::case_when(
#'     CMCAT == "medcl B" ~ 20,
#'     CMCAT == "medcl C" ~ 150,
#'     TRUE ~ 1
#'   ) %>% formatters::with_label("Study Day of Start of Medication"),
#'   CMENDY = dplyr::case_when(
#'     CMCAT == "medcl B" ~ 700,
#'     CMCAT == "medcl C" ~ 1000,
#'     TRUE ~ 500
#'   ) %>% formatters::with_label("Study Day of End of Medication"),
#'   CMASTDTM = ASTDTM,
#'   CMAENDTM = AENDTM
#' )
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAE", adae),
#'     cdisc_dataset("ADCM", adcm, keys = adcm_keys)
#'   ),
#'   modules = modules(
#'     tm_g_pp_patient_timeline(
#'       label = "Patient Timeline",
#'       dataname_adae = "ADAE",
#'       dataname_adcm = "ADCM",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       cmdecod = choices_selected(
#'         choices = variable_choices(adcm, "CMDECOD"),
#'         selected = "CMDECOD",
#'       ),
#'       aeterm = choices_selected(
#'         choices = variable_choices(adae, "AETERM"),
#'         selected = c("AETERM")
#'       ),
#'       aetime_start = choices_selected(
#'         choices = variable_choices(adae, "ASTDTM"),
#'         selected = c("ASTDTM")
#'       ),
#'       aetime_end = choices_selected(
#'         choices = variable_choices(adae, "AENDTM"),
#'         selected = c("AENDTM")
#'       ),
#'       dstime_start = choices_selected(
#'         choices = variable_choices(adcm, "CMASTDTM"),
#'         selected = c("CMASTDTM")
#'       ),
#'       dstime_end = choices_selected(
#'         choices = variable_choices(adcm, "CMAENDTM"),
#'         selected = c("CMAENDTM")
#'       ),
#'       aerelday_start = choices_selected(
#'         choices = variable_choices(adae, "ASTDY"),
#'         selected = c("ASTDY")
#'       ),
#'       aerelday_end = choices_selected(
#'         choices = variable_choices(adae, "AENDY"),
#'         selected = c("AENDY")
#'       ),
#'       dsrelday_start = choices_selected(
#'         choices = variable_choices(adcm, "ASTDY"),
#'         selected = c("ASTDY")
#'       ),
#'       dsrelday_end = choices_selected(
#'         choices = variable_choices(adcm, "AENDY"),
#'         selected = c("AENDY")
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
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
                                     ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_pp_patient_timeline")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname_adcm)
  checkmate::assert_string(dataname_adae)
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
  assertthat::assert_that(!xor(is.null(aetime_start), is.null(aetime_end)))
  assertthat::assert_that(!xor(is.null(dstime_start), is.null(dstime_end)))
  assertthat::assert_that(!xor(is.null(aerelday_start), is.null(aerelday_end)))
  assertthat::assert_that(!xor(is.null(dsrelday_start), is.null(dsrelday_end)))
  assertthat::assert_that(
    (!is.null(aeterm) && (!is.null(aetime_start) || !is.null(aerelday_start))) ||
      (!is.null(cmdecod) && (!is.null(dstime_start) || !is.null(dsrelday_start)))
  )
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
        ggplot2_args = ggplot2_args
      )
    ),
    datanames = "all"
  )
}

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

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("patient_timeline_plot")),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
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
        label = "Select CMDECOD variable:",
        data_extract_spec = ui_args$cmdecod,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aeterm"),
        label = "Select AETERM variable:",
        data_extract_spec = ui_args$aeterm,
        is_single_dataset = is_single_dataset_value
      ),
      if (!is.null(ui_args$aerelday_start) || !is.null(ui_args$dsrelday_start)) {
        shiny::tagList(
          shiny::checkboxInput(ns("relday_x_axis"), label = "Use relative days on the x-axis", value = TRUE),
          shiny::conditionalPanel(
            paste0("input.relday_x_axis == true"),
            ns = ns,
            if (!is.null(ui_args$aerelday_start)) {
              shiny::tagList(
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
              shiny::tagList(
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
        shinyjs::hidden(shiny::checkboxInput(ns("relday_x_axis"), label = "", value = FALSE))
      },
      shiny::conditionalPanel(
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
      teal.widgets::panel_item(
        title = "Plot settings",
        collapsed = TRUE,
        teal.widgets::optionalSliderInputValMinMax(
          ns("font_size"),
          "Font Size",
          ui_args$font_size,
          ticks = FALSE,
          step = 1
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


    # Patient timeline tab ----
    check_box <- shiny::reactive(input$relday_x_axis)

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
          "At least one parameter must be selected."
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

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select a patient"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      join_keys = get_join_keys(data),
      selector_list = selector_list
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    all_q <- shiny::reactive({
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

      shiny::validate(
        shiny::need(
          input$relday_x_axis ||
            (sum(stats::complete.cases(p_time_data_pat[, c(aetime_start, aetime_end)])) > 0 ||
              sum(stats::complete.cases(p_time_data_pat[, c(dstime_start, dstime_end)])) > 0),
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

      shiny::validate(
        shiny::need(
          !input$relday_x_axis ||
            (sum(stats::complete.cases(p_time_data_pat[, c(aerelday_start_name, aerelday_end_name)])) > 0 ||
              sum(stats::complete.cases(p_time_data_pat[, c(dsrelday_start_name, dsrelday_end_name)])) > 0),
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
            ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )

      teal.code::eval_code(object = qenv, as.expression(patient_timeline_calls))
    })

    plot_r <- shiny::reactive(all_q()[["patient_timeline_plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "patient_timeline_plot",
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
        card$set_name("Patient Profile Timeline Plot")
        card$append_text("Patient Profile Timeline Plot", "header2")
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
