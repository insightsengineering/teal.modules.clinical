#' Template: Patient Timeline Tab
#'
#' Creates a patient timeline template call.
#'
#' @inheritParams template_arguments
#' @param aeterm (`character`)\cr name of the reported term for the adverse event variable.
#' @param aetime_start (`character`)\cr name of datetime start of adverse event variable.
#' @param aetime_end (`character`)\cr name of datetime end of adverse event variable.
#' @param dstime_start (`character`)\cr name of datetime first exposure to treatment variable.
#' @param dstime_end (`character`)\cr name of datetime last exposure to treatment variable.
#' @param cmdecod (`character`)\cr name of reported standardized name of drug, med, or therapy variable.
#' @param aerelday_start (`character`)\cr name of start study day variable.
#' @param aerelday_end (`character`)\cr name of end study day variable.
#' @param dsrelday_start (`character`)\cr name of start study day variable.
#' @param dsrelday_end (`character`)\cr name of end study day variable.
#' @param relative_day (`logical`)\cr whether to use relative days or absolute dates
#' @param patient_id (`character`)\cr patient ID.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
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
                                      ggplot2_args = teal.devel::ggplot2_args()) {
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

  y <- list()
  y$chart <- list()

  chart_list <- if (!relative_day) {
    parsed_ggplot2_args <- parse_ggplot2_args(
      resolve_ggplot2_args(
        user_plot = ggplot2_args,
        module_plot = ggplot2_args(
          labs = list(title = paste0("Patient ID: ", patient_id), x = "Absolute Study Dates"),
          theme = list(plot.title = substitute(element_text(hjust = 0, size = font_size_var),
                                               list(font_size_var = font_size)),
                       axis.text = substitute(element_text(size = font_size_var, face = "bold", colour = "black"),
                                              list(font_size_var = font_size)),
                       axis.title = substitute(element_text(size = font_size_var, face = "bold", colour = "black"),
                                               list(font_size_var = font_size)),
                       text = substitute(element_text(size = font_size_var), list(font_size_var = font_size)))
        )
      )
    )

    add_expr(
      list(),
      substitute(
        expr = {
          posixct_origin <- "1970-01-01 00:00.00 UTC"

          med_chart <- NULL
          ae_chart <- NULL

          if (all(vapply(list(cmdecod_var, dstime_start_var, dstime_end_var), Negate(is.null), logical(1)))) {
            med_chart <- dataname %>%
              dplyr::select(dstime_start, dstime_end, cmdecod) %>%
              dplyr::distinct()

            colnames(med_chart) <- c("start", "end", "event")
            med_chart$group <- "Medication"
          }
          if (all(vapply(list(aeterm_var, aetime_start_var, aetime_end_var), Negate(is.null), logical(1)))) {
            ae_chart <- dataname %>%
              dplyr::select(aetime_start, aetime_end, aeterm) %>%
              dplyr::distinct()
            colnames(ae_chart) <- c("start", "end", "event")
            ae_chart$group <- "Adverse Events"
          }

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
            patient_timeline_plot <- ggplot(
              data = df,
              aes(
                x = x,
                y = y,
                label = label
              )
            ) +
              geom_label() +
              theme_void()
          } else {
            patient_timeline_plot <- vistime::gg_vistime(
              vistime_data,
              col.event = "event",
              col.group = "group",
              show_labels = FALSE
            ) +
              ggrepel::geom_text_repel(
                mapping = aes(label = event),
                size = font_size_var / 3.5,
                color = "black",
                direction = "x",
                nudge_x = 0.5,
                segment.size = 0.1
              ) +
              scale_x_datetime(labels = scales::date_format("%b-%Y")) + labs + themes
          }
          patient_timeline_plot
        },
        env = list(
          dataname = as.name(dataname),
          aeterm = if_not_empty(aeterm, as.name(aeterm)),
          aetime_start = if_not_empty(aetime_start, as.name(aetime_start)),
          aetime_end = if_not_empty(aetime_end, as.name(aetime_end)),
          dstime_start = if_not_empty(dstime_start, as.name(dstime_start)),
          dstime_end = if_not_empty(dstime_end, as.name(dstime_end)),
          cmdecod = if_not_empty(cmdecod, as.name(cmdecod)),
          aeterm_var = aeterm,
          aetime_start_var = aetime_start,
          aetime_end_var = aetime_end,
          dstime_start_var = dstime_start,
          dstime_end_var = dstime_end,
          cmdecod_var = cmdecod,
          font_size_var = font_size,
          patient_id = patient_id,
          labs = parsed_ggplot2_args$labs,
          themes = parsed_ggplot2_args$theme
        )
      )
    )
  } else {

    parsed_ggplot2_args <- parse_ggplot2_args(
      resolve_ggplot2_args(
        user_plot = ggplot2_args,
        module_plot = ggplot2_args(
          labs = list(title = paste0("Patient ID: ", patient_id), x = "Relative Study Days", y = ""),
          theme = list(plot.title = substitute(element_text(hjust = 0, size = font_size_var),
                                               list(font_size_var = font_size)),
                       axis.text = substitute(element_text(size = font_size_var, face = "bold", colour = "black"),
                                              list(font_size_var = font_size)),
                       axis.title = substitute(element_text(size = font_size_var, face = "bold", colour = "black"),
                                               list(font_size_var = font_size)),
                       text = substitute(element_text(size = font_size_var), list(font_size_var = font_size)),
                       legend.position = "none")
        )
      ),
      ggtheme = "classic"
    )

    add_expr(
      list(),
      substitute(
        expr = {
          med_chart <- if (length(c(dsrelday_start_var, dsrelday_end_var, cmdecod)) == 3) {
            dataname %>%
              dplyr::select(dsrelday_start_var, dsrelday_end_var, cmdecod) %>%
              dplyr::distinct() %>%
              dplyr::rename(start = dsrelday_start_var, end = dsrelday_end_var, event = cmdecod) %>%
              dplyr::mutate(group = "Medication")
          } else {
            NULL
          }

          ae_chart <- if (length(c(aerelday_start_var, aerelday_end_var, aeterm)) == 3) {
            dataname %>%
              dplyr::select(aerelday_start_var, aerelday_end_var, aeterm) %>%
              dplyr::distinct() %>%
              dplyr::rename(start = aerelday_start_var, end = aerelday_end_var, event = aeterm) %>%
              dplyr::mutate(group = "Adverse Events")
          } else {
            NULL
          }

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
            patient_timeline_plot <- ggplot(
              data = df,
              aes(
                x = x,
                y = y,
                label = label
              )
            ) +
              geom_label() +
              theme_void()
          } else {
            patient_timeline_plot <- ggplot(vistime_data,
                                            aes(x = start, y = event, xend = end, yend = event, color = color)) +
              geom_segment(size = 4) +
              facet_grid(group ~ ., scales = "free", space = "free") +
              scale_x_continuous(breaks = scales::pretty_breaks())  + labs + ggthemes + themes
          }
          patient_timeline_plot
        },
        env = list(
          dataname = as.name(dataname),
          aeterm = aeterm,
          cmdecod = cmdecod,
          aerelday_start_var = aerelday_start,
          aerelday_end_var = aerelday_end,
          dsrelday_start_var = dsrelday_start,
          dsrelday_end_var = dsrelday_end,
          font_size_var = font_size,
          patient_id = patient_id,
          labs = parsed_ggplot2_args$labs,
          ggthemes = parsed_ggplot2_args$ggtheme,
          themes = parsed_ggplot2_args$theme
        )
      )
    )
  }

  y$chart <- bracket_expr(chart_list)
  y
}

#' Teal Module: Patient Profile Timeline Teal Module
#'
#' This teal module produces a patient profile timeline plot using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param aeterm ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AETERM} column of the
#' ADAE dataset.
#' @param dataname_adcm (`character`)\cr name of ADCM dataset or equivalent.
#' @param dataname_adae (`character`)\cr name of ADAE dataset or equivalent.
#' @param aerelday_start ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{ASTDY}
#' column of the ADAE dataset.
#' @param aerelday_end ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AENDY}
#' column of the ADAE dataset.
#' @param dsrelday_start ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{ASTDY}
#' column of the ADCM dataset.
#' @param dsrelday_end ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AENDY}
#' column of the ADCM dataset.
#' @param cmdecod ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{cmdecod} column of the ADCM
#' dataset.
#' @param aetime_start ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{ASTDTM} column of the AE
#' start of the ADAE dataset.
#' @param aetime_end ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{AENDTM} column of the AE
#' end of the ADAE dataset.
#' @param dstime_start ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMASTDTM} column of
#' treatment start of the ADCM dataset.
#' @param dstime_end ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMAENDTM} column of treatment
#' end of the ADCM dataset.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
#' ADCM <- synthetic_cdisc_data("latest")$adcm
#'
#' #' Modify ADCM
#' ADCM$CMINDC <- paste0("Indication_", as.numeric(ADCM$CMDECOD))
#' ADCM$CMDOSE <- 1
#' ADCM$CMDOSU <- "U"
#' ADCM$CMROUTE <- "CMROUTE"
#' ADCM$CMDOSFRQ <- "CMDOSFRQ"
#' ADCM$CMSTDY <- 1
#' ADCM[ADCM$CMCAT == "medcl B", ]$CMSTDY <- 20
#' ADCM[ADCM$CMCAT == "medcl C", ]$CMSTDY <- 150
#' ADCM$CMENDY <- 500
#' ADCM[ADCM$CMCAT == "medcl B", ]$CMENDY <- 700
#' ADCM[ADCM$CMCAT == "medcl C", ]$CMENDY <- 1000
#' ADCM$CMASTDTM <- ADCM$ASTDTM
#' ADCM$CMAENDTM <- ADCM$AENDTM
#' rtables::var_labels(
#'   ADCM[c("CMINDC", "CMDECOD", "CMSTDY", "CMENDY")]
#' ) <- c(
#'   "Indication",
#'   "Reported Name of Drug, Med, or Therapy",
#'   "Study Day of Start of Medication",
#'   "Study Day of End of Medication"
#' )
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADAE", ADAE, code = 'ADAE <- synthetic_cdisc_data("latest")$adae'),
#'     cdisc_dataset("ADCM", ADCM,
#'                   code = 'ADCM <- synthetic_cdisc_data("latest")$adcm
#'       ADCM$CMINDC <- paste0("Indication_", as.numeric(ADCM$CMDECOD))
#'       ADCM$CMDOSE <- 1
#'       ADCM$CMDOSU <- "U"
#'       ADCM$CMROUTE <- "CMROUTE"
#'       ADCM$CMDOSFRQ <- "CMDOSFRQ"
#'       ADCM$CMSTDY <- 1
#'       ADCM[ADCM$CMCAT == "medcl B", ]$CMSTDY <- 20
#'       ADCM[ADCM$CMCAT == "medcl C", ]$CMSTDY <- 150
#'       ADCM$CMENDY <- 500
#'       ADCM[ADCM$CMCAT == "medcl B", ]$CMENDY <- 700
#'       ADCM[ADCM$CMCAT == "medcl C", ]$CMENDY <- 1000
#'       ADCM$CMASTDTM <- ADCM$ASTDTM
#'       ADCM$CMAENDTM <- ADCM$AENDTM
#'       rtables::var_labels(
#'         ADCM[c("CMINDC", "CMDECOD", "CMSTDY", "CMENDY")]) <- c(
#'           "Indication",
#'           "Reported Name of Drug, Med, or Therapy",
#'           "Study Day of Start of Medication",
#'           "Study Day of End of Medication")',
#'                   keys = adcm_keys
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_pp_patient_timeline(
#'       label = "Vitals",
#'       dataname_adae = "ADAE",
#'       dataname_adcm = "ADCM",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       cmdecod = choices_selected(
#'         choices = variable_choices(ADCM, "CMDECOD"),
#'         selected = "CMDECOD",
#'       ),
#'       aeterm = choices_selected(
#'         choices = variable_choices(ADAE, "AETERM"),
#'         selected = c("AETERM")
#'       ),
#'       aetime_start = choices_selected(
#'         choices = variable_choices(ADAE, "ASTDTM"),
#'         selected = c("ASTDTM")
#'       ),
#'       aetime_end = choices_selected(
#'         choices = variable_choices(ADAE, "AENDTM"),
#'         selected = c("AENDTM")
#'       ),
#'       dstime_start = choices_selected(
#'         choices = variable_choices(ADCM, "CMASTDTM"),
#'         selected = c("CMASTDTM")
#'       ),
#'       dstime_end = choices_selected(
#'         choices = variable_choices(ADCM, "CMAENDTM"),
#'         selected = c("CMAENDTM")
#'       ),
#'       aerelday_start = choices_selected(
#'         choices = variable_choices(ADAE, "ASTDY"),
#'         selected = c("ASTDY")
#'       ),
#'       aerelday_end = choices_selected(
#'         choices = variable_choices(ADAE, "AENDY"),
#'         selected = c("AENDY")
#'       ),
#'       dsrelday_start = choices_selected(
#'         choices = variable_choices(ADCM, "ASTDY"),
#'         selected = c("ASTDY")
#'       ),
#'       dsrelday_end = choices_selected(
#'         choices = variable_choices(ADCM, "AENDY"),
#'         selected = c("AENDY")
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
                                     ggplot2_args = teal.devel::ggplot2_args()) {
  logger::log_info("Initializing tm_g_pp_patient_timeline")
  assertthat::assert_that(is_character_single(label))
  assertthat::assert_that(is_character_single(dataname_adcm))
  assertthat::assert_that(is_character_single(dataname_adae))
  assertthat::assert_that(is_character_single(parentname))
  assertthat::assert_that(is_character_single(patient_col))
  assertthat::assert_that(is.null(pre_output) || inherits(pre_output, "shiny.tag"),
              msg = "pre_output should be either null or shiny.tag type of object"
  )
  assertthat::assert_that(is.null(post_output) || inherits(post_output, "shiny.tag"),
              msg = "post_output should be either null or shiny.tag type of object"
  )

  checkmate::assert_numeric(font_size, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(font_size[1], lower = font_size[2], upper = font_size[3], .var.name = "font_size")
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(plot_width[1], lower = plot_width[2], upper = plot_width[3], null.ok = TRUE,
                            .var.name = "plot_width")

  assertthat::assert_that(!xor(is.null(aetime_start), is.null(aetime_end)))
  assertthat::assert_that(!xor(is.null(dstime_start), is.null(dstime_end)))
  assertthat::assert_that(!xor(is.null(aerelday_start), is.null(aerelday_end)))
  assertthat::assert_that(!xor(is.null(dsrelday_start), is.null(dsrelday_end)))
  assertthat::assert_that(
    (!is.null(aeterm) && (!is.null(aetime_start) || !is.null(aerelday_start))) ||
      (!is.null(cmdecod) && (!is.null(dstime_start) || !is.null(dsrelday_start)))
  )

  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  args <- as.list(environment())
  data_extract_list <- list(
    aeterm = if_not_null(aeterm, cs_to_des_select(aeterm, dataname = dataname_adae)),
    cmdecod = if_not_null(cmdecod, cs_to_des_select(cmdecod, dataname = dataname_adcm)),
    aetime_start = if_not_null(aetime_start, cs_to_des_select(aetime_start, dataname = dataname_adae)),
    aetime_end = if_not_null(aetime_end, cs_to_des_select(aetime_end, dataname = dataname_adae)),
    dstime_start = if_not_null(dstime_start, cs_to_des_select(dstime_start, dataname = dataname_adcm)),
    dstime_end = if_not_null(dstime_end, cs_to_des_select(dstime_end, dataname = dataname_adcm)),
    aerelday_start = if_not_null(aerelday_start, cs_to_des_select(aerelday_start, dataname = dataname_adae)),
    aerelday_end = if_not_null(aerelday_end, cs_to_des_select(aerelday_end, dataname = dataname_adae)),
    dsrelday_start = if_not_null(dsrelday_start, cs_to_des_select(dsrelday_start, dataname = dataname_adcm)),
    dsrelday_end = if_not_null(dsrelday_end, cs_to_des_select(dsrelday_end, dataname = dataname_adcm))
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
    filters = "all"
  )
}

ui_g_patient_timeline <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- is_single_dataset(
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
  standard_layout(
    output = plot_with_settings_ui(id = ns("patient_timeline_plot")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(
        ui_args[c(
          "aeterm", "cmdecod",
          "aetime_start", "aetime_end", "dstime_start", "dstime_end",
          "aerelday_start", "aerelday_end", "dsrelday_start", "dsrelday_end")]
      ),
      optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      data_extract_ui(
        id = ns("cmdecod"),
        label = "Select CMDECOD variable:",
        data_extract_spec = ui_args$cmdecod,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("aeterm"),
        label = "Select AETERM variable:",
        data_extract_spec = ui_args$aeterm,
        is_single_dataset = is_single_dataset_value
      ),
      if (!is.null(ui_args$aerelday_start) || !is.null(ui_args$dsrelday_start)) {
        shiny::tagList(
          checkboxInput(ns("relday_x_axis"), label = "Use relative days on the x-axis", value = TRUE),
          shiny::conditionalPanel(
            paste0("input.relday_x_axis == true"),
            ns = ns,
            if (!is.null(ui_args$aerelday_start)) {
              shiny::tagList(
                data_extract_ui(
                  id = ns("aerelday_start"),
                  label = "Select AE relative start date variable:",
                  data_extract_spec = ui_args$aerelday_start,
                  is_single_dataset = is_single_dataset_value
                ),
                data_extract_ui(
                  id = ns("aerelday_end"),
                  label = "Select AE relative end date variable:",
                  data_extract_spec = ui_args$aerelday_end,
                  is_single_dataset = is_single_dataset_value
                )
              )
            },
            if (!is.null(ui_args$dsrelday_start)) {
              shiny::tagList(
                data_extract_ui(
                  id = ns("dsrelday_start"),
                  label = "Select Medication relative start date variable:",
                  data_extract_spec = ui_args$dsrelday_start,
                  is_single_dataset = is_single_dataset_value
                ),
                data_extract_ui(
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
      shiny::conditionalPanel(
        paste0("input.relday_x_axis == false"),
        ns = ns,
        data_extract_ui(
          id = ns("aetime_start"),
          label = "Select ASTDTM variable:",
          data_extract_spec = ui_args$aetime_start,
          is_single_dataset = is_single_dataset_value
        ),
        data_extract_ui(
          id = ns("aetime_end"),
          label = "Select AENDTM variable:",
          data_extract_spec = ui_args$aetime_end,
          is_single_dataset = is_single_dataset_value
        ),
        data_extract_ui(
          id = ns("dstime_start"),
          label = "Select TRTSDTM variable:",
          data_extract_spec = ui_args$dstime_start,
          is_single_dataset = is_single_dataset_value
        ),
        data_extract_ui(
          id = ns("dstime_end"),
          label = "Select TRTEDTM variable:",
          data_extract_spec = ui_args$dstime_end,
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


srv_g_patient_timeline <- function(input,
                                   output,
                                   session,
                                   datasets,
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

  # Patient timeline tab ----
  p_timeline_merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(
      dsrelday_start = dsrelday_start, dsrelday_end = dsrelday_end,
      aerelday_start = aerelday_start, aerelday_end = aerelday_end,
      aeterm = aeterm, aetime_start = aetime_start,
      aetime_end = aetime_end, dstime_start = dstime_start, dstime_end = dstime_end, cmdecod = cmdecod
    )
  )

  patient_timeline_calls <- reactive({
    validate(need(patient_id(), "Please select a patient."))

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
    ds_chart_vars_null <- any(vapply(list(dstime_start, dstime_end), is.null, FUN.VALUE = logical(1)))

    p_timeline_data <- p_timeline_merged_data()$data()
    # time variables can not be NA
    p_time_data_pat <- p_timeline_data[p_timeline_data[[patient_col]] == patient_id(), ]

    validate(
      need(
        input$relday_x_axis ||
          (sum(stats::complete.cases(p_time_data_pat[, c(aetime_start, aetime_end)])) > 0 ||
          sum(stats::complete.cases(p_time_data_pat[, c(dstime_start, dstime_end)])) > 0),
        "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
      ),
      need(
        input$relday_x_axis || (isFALSE(ae_chart_vars_null) || isFALSE(ds_chart_vars_null)),
        "The 3 sections of the plot (Adverse Events, Dosing and Medication) do not have enough input variables.
          Please select the appropriate input variables."
      )
    )

    aerel_chart_vars_null <- any(vapply(list(aeterm, aerelday_start, aerelday_end), is.null, FUN.VALUE = logical(1)))
    dsrel_chart_vars_null <- any(vapply(list(cmdecod, dsrelday_start, dsrelday_end), is.null, FUN.VALUE = logical(1)))

    # These lines are needed because there is a naming conflict: ADCM and ADAE will be both pass in their ASTDY and
    # AENDY columns to data_merge_module call above.
    aerelday_start_name <- if_not_empty(aerelday_start, p_timeline_merged_data()$columns_source$aerelday_start[[1]])
    aerelday_end_name <- if_not_empty(aerelday_end, p_timeline_merged_data()$columns_source$aerelday_end[[1]])
    dsrelday_start_name <- if_not_empty(dsrelday_start, p_timeline_merged_data()$columns_source$dsrelday_start[[1]])
    dsrelday_end_name <- if_not_empty(dsrelday_end, p_timeline_merged_data()$columns_source$dsrelday_end[[1]])

    validate(
      need(
        !input$relday_x_axis ||
          (sum(stats::complete.cases(p_time_data_pat[, c(aerelday_start_name, aerelday_end_name)])) > 0 ||
          sum(stats::complete.cases(p_time_data_pat[, c(dsrelday_start_name, dsrelday_end_name)])) > 0),
        "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
      ),
      need(
        !input$relday_x_axis || (isFALSE(aerel_chart_vars_null) || isFALSE(dsrel_chart_vars_null)),
        "The 3 sections of the plot (Adverse Events, Dosing and Medication) do not have enough input variables.
          Please select the appropriate input variables."
      )
    )

    patient_timeline_stack <- chunks$new()
    time_line_stack_push <- function(...) {
      chunks_push(..., chunks = patient_timeline_stack)
    }

    chunks_push_data_merge(p_timeline_merged_data(), chunks = patient_timeline_stack)

    time_line_stack_push(substitute(
      expr = {
        ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
      }, env = list(
        patient_col = patient_col,
        patient_id = patient_id()
      )
    ))

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

    lapply(patient_timeline_calls, time_line_stack_push)
    chunks_safe_eval(chunks = patient_timeline_stack)
    patient_timeline_stack
  })

  patient_timeline_plot <- reactive({
    chunks_reset()
    chunks_push_chunks(patient_timeline_calls())
    chunks_get_var("patient_timeline_plot")
  })

  callModule(
    plot_with_settings_srv,
    id = "patient_timeline_plot",
    plot_r = patient_timeline_plot,
    height = plot_height,
    width = plot_width
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(
      aeterm, aetime_start, aetime_end, dstime_start, dstime_end, cmdecod
    )),
    modal_title = label
  )
}
