#' Template: Therapy
#'
#' Creates a therapy template call.
#'
#' @inheritParams template_arguments
#' @param atirel (`character`)\cr name of time relation of medication variable.
#' @param cmdecod (`character`)\cr name of standardized medication name variable.
#' @param cmindc (`character`)\cr name of indication variable.
#' @param cmdose (`character`)\cr name of dose per administration variable.
#' @param cmtrt (`character`)\cr name of reported name of drug, med, or therapy variable.
#' @param cmdosu (`character`)\cr name of dose units variable.
#' @param cmroute (`character`)\cr name of route of administration variable.
#' @param cmdosfrq (`character`)\cr name of dosing frequency per interval variable.
#' @param cmstdy (`character`)\cr name of study day of start of medication variable.
#' @param cmendy (`character`)\cr name of study day of end of medication variable.
#' @param patient_id (`character`)\cr patient ID.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.

template_therapy <- function(dataname = "ANL",
                             atirel = "ATIREL",
                             cmdecod = "CMDECOD",
                             cmindc = "CMINDC",
                             cmdose = "CMDOSE",
                             cmtrt = "CMTRT",
                             cmdosu = "CMDOSU",
                             cmroute = "CMROUTE",
                             cmdosfrq = "CMDOSFRQ",
                             cmstdy = "CMSTDY",
                             cmendy = "CMENDY",
                             patient_id,
                             font_size = 12L,
                             ggplot2_args = teal.devel::ggplot2_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(atirel),
    assertthat::is.string(cmdecod),
    assertthat::is.string(cmindc),
    assertthat::is.string(cmdose),
    assertthat::is.string(cmtrt),
    assertthat::is.string(cmdosu),
    assertthat::is.string(cmroute),
    assertthat::is.string(cmdosfrq),
    assertthat::is.string(cmstdy),
    assertthat::is.string(cmendy),
    assertthat::is.string(patient_id),
    is.numeric(font_size)
  )

  y <- list()
  y$table_list <- list()
  y$plot_list <- list()

  table_list <- add_expr(
    list(),
    substitute(expr = {
      cols_to_include <- c(
        cmindc_char,
        cmdecod_char,
        cmdose_char,
        cmtrt_char,
        cmdosu_char,
        cmroute_char,
        cmdosfrq_char,
        cmstdy_char,
        cmendy_char
      )

      dataname[setdiff(cols_to_include, names(dataname))] <- NA

      therapy_table <-
        dataname %>%
        dplyr::filter(atirel %in% c("CONCOMITANT", "PRIOR")) %>% # removed PRIOR_CONCOMITANT
        dplyr::select(dplyr::all_of(cols_to_include)) %>%
        dplyr::filter(!is.na(cmdecod)) %>%
        dplyr::mutate(Dosage = paste(cmdose, cmdosu, cmdosfrq, cmroute)) %>%
        dplyr::select(-cmdose, -cmdosu, -cmdosfrq, -cmroute) %>%
        dplyr::select(cmindc, cmdecod, Dosage, dplyr::everything()) %>%
        dplyr::mutate(CMDECOD = dplyr::case_when(
          nchar(as.character(cmdecod)) > 20 ~ as.character(cmtrt),
          TRUE ~ as.character(cmdecod)
        )) %>%
        dplyr::select(-cmtrt) %>%
        dplyr::arrange(cmindc, cmdecod, cmstdy) %>%
        dplyr::distinct() %>%
        `colnames<-`(c(
          get_labels(dataname)$column_labels[c(cmindc_char, cmdecod_char)], "Dosage",
          get_labels(dataname)$column_labels[c(cmstdy_char, cmendy_char)]
        ))
      therapy_table
    }, env = list(
      dataname = as.name(dataname),
      atirel = as.name(atirel),
      cmdecod = as.name(cmdecod),
      cmindc = as.name(cmindc),
      cmdose = as.name(cmdose),
      cmtrt = as.name(cmtrt),
      cmdosu = as.name(cmdosu),
      cmroute = as.name(cmroute),
      cmdosfrq = as.name(cmdosfrq),
      cmstdy = as.name(cmstdy),
      cmendy = as.name(cmendy),
      cmdecod_char = cmdecod,
      cmindc_char = cmindc,
      cmdose_char = cmdose,
      cmtrt_char = cmtrt,
      cmdosu_char = cmdosu,
      cmroute_char = cmroute,
      cmdosfrq_char = cmdosfrq,
      cmendy_char = cmendy,
      cmstdy_char = cmstdy
    ))
  )

  parsed_ggplot2_args <- teal.devel::parse_ggplot2_args(
    teal.devel::resolve_ggplot2_args(
      user_plot = ggplot2_args,
      module_plot = teal.devel::ggplot2_args(
        labs = list(y = "Medication", title = paste0("Patient ID: ", patient_id)),
        theme = list(
          text = substitute(element_text(size = font), list(font = font_size)),
          axis.text.y = quote(element_blank()),
          axis.ticks.y = quote(element_blank()),
          plot.title = substitute(element_text(size = font), list(font = font_size)),
          legend.position = "none",
          panel.grid.minor = quote(element_line(
            size = 0.5,
            linetype = "dotted",
            colour = "grey"
          )),
          panel.grid.major = quote(element_line(
            size = 0.5,
            linetype = "dotted",
            colour = "grey"
          ))
        )
      )
    ),
    ggtheme = "minimal"
  )

  plot_list <- add_expr(
    list(),
    substitute(expr = {
      dataname[[cmstdy_char]] <- as.numeric(dataname[[cmstdy_char]])
      dataname[[cmendy_char]] <- as.numeric(dataname[[cmendy_char]])
      max_day <- max(dataname[[cmendy_char]], na.rm = TRUE)
      data <- dataname %>%
        dplyr::filter(atirel %in% c("CONCOMITANT", "PRIOR")) %>% # remove PRIOR_CONCOMITANT
        dplyr::select_at(cols_to_include) %>%
        dplyr::filter(!is.na(cmdecod)) %>%
        dplyr::mutate(DOSE = paste(cmdose, cmdosu, cmdosfrq)) %>%
        dplyr::select(-cmdose, -cmdosu, -cmdosfrq) %>%
        dplyr::select(cmindc, cmdecod, DOSE, dplyr::everything()) %>%
        dplyr::arrange(cmindc, cmdecod, cmstdy) %>%
        dplyr::distinct() %>%
        dplyr::mutate(CMSTDY = dplyr::case_when(
          is.na(cmstdy) ~ 1,
          TRUE ~ cmstdy
        )) %>%
        dplyr::mutate(CMENDY = dplyr::case_when(
          is.na(cmendy) ~ max_day,
          TRUE ~ cmendy
        )) %>%
        dplyr::arrange(CMSTDY, dplyr::desc(CMSTDY)) %>%
        dplyr::mutate(CMDECOD = dplyr::case_when(
          nchar(as.character(cmdecod)) > 20 ~ as.character(cmtrt),
          TRUE ~ as.character(cmdecod)
        ))

      therapy_plot <-
        ggplot(data = data, aes(fill = cmindc, color = cmindc, y = CMDECOD, x = CMSTDY)) +
        geom_segment(aes(xend = CMENDY, yend = CMDECOD), size = 2) +
        geom_text(
          data =
            data %>%
              dplyr::select(CMDECOD, cmindc, CMSTDY) %>%
              dplyr::distinct(),
            aes(x = CMSTDY, label = CMDECOD), color = "black",
          hjust = "left",
          vjust = "bottom",
          nudge_y = 0.1,
          size = font_size_var / 3.5
        ) +
        scale_y_discrete(expand = expansion(add = 1.2)) +
        geom_point(color = "black", size = 2, shape = 24, position = position_nudge(y = -0.15)) +
        labs + ggtheme + theme

      print(therapy_plot)
    }, env = c(list(
      dataname = as.name(dataname),
      atirel = as.name(atirel),
      cmdecod = as.name(cmdecod),
      cmindc = as.name(cmindc),
      cmdose = as.name(cmdose),
      cmtrt = as.name(cmtrt),
      cmdosu = as.name(cmdosu),
      cmroute = as.name(cmroute),
      cmdosfrq = as.name(cmdosfrq),
      cmstdy = as.name(cmstdy),
      cmendy = as.name(cmendy),
      cmdecod_char = cmdecod,
      cmindc_char = cmindc,
      cmdose_char = cmdose,
      cmtrt_char = cmtrt,
      cmdosu_char = cmdosu,
      cmroute_char = cmroute,
      cmdosfrq_char = cmdosfrq,
      cmstdy_char = cmstdy,
      cmendy_char = cmendy,
      patient_id = patient_id,
      font_size_var = font_size),
      parsed_ggplot2_args)
    )
  )
  y$table_list <- bracket_expr(table_list)
  y$plot_list <- bracket_expr(plot_list)
  y
}

#' Teal Module: Patient Profile Therapy Teal Module
#'
#' This teal module produces a patient profile therapy plot using ADaM datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param atirel ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{ATIREL} column of the
#' ADCM dataset.
#' @param cmdecod ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMDECOD} column of the
#' ADCM dataset.
#' @param cmdose ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMDOSE} column of the
#' ADCM dataset.
#' @param cmtrt ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMTRT} column of the ADCM dataset.
#' @param cmdosu ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMDOSU} column of the
#' ADCM dataset.
#' @param cmroute ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMROUTE} column of the
#' ADCM dataset.
#' @param cmdosfrq ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMDOSFRQ} column of the
#' ADCM dataset.
#' @param cmstdy ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMSTDY} column of the
#' ADCM dataset.
#' @param cmendy ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMENDY} column of the
#' ADCM dataset.
#' @param cmindc ([teal::choices_selected()] or [teal::data_extract_spec()])\cr \code{CMINDC} column of the
#' ADCM dataset.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADCM <- synthetic_cdisc_data("latest")$adcm
#'
#' #' Modify ADCM
#' ADCM$CMINDC <- paste0("Indication_", as.numeric(ADCM$CMDECOD))
#' ADCM$CMDOSE <- 1
#' ADCM$CMTRT <- ADCM$CMCAT
#' ADCM$CMDOSU <- "U"
#' ADCM$CMROUTE <- "CMROUTE"
#' ADCM$CMDOSFRQ <- "CMDOSFRQ"
#' ADCM$CMASTDTM <- ADCM$ASTDTM
#' ADCM$CMAENDTM <- ADCM$AENDTM
#' rtables::var_labels(
#'   ADCM[c("CMINDC", "CMTRT", "ASTDY", "AENDY")]
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
#'     cdisc_dataset("ADCM", ADCM,
#'                   code = 'ADCM <- synthetic_cdisc_data("latest")$adcm
#'       ADCM$CMINDC <- paste0("Indication_", as.numeric(ADCM$CMDECOD))
#'       ADCM$CMDOSE <- 1
#'       ADCM$CMTRT <- ADCM$CMCAT
#'       ADCM$CMDOSU <- "U"
#'       ADCM$CMROUTE <- "CMROUTE"
#'       ADCM$CMDOSFRQ <- "CMDOSFRQ"
#'       ADCM$CMASTDTM <- ADCM$ASTDTM
#'       ADCM$CMAENDTM <- ADCM$AENDTM
#'       rtables::var_labels(
#'         ADCM[c("CMINDC", "CMTRT", "ASTDY", "AENDY")]) <- c(
#'           "Indication",
#'           "Reported Name of Drug, Med, or Therapy",
#'           "Study Day of Start of Medication",
#'           "Study Day of End of Medication")',
#'                   keys = adcm_keys
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_pp_therapy(
#'       label = "Therapy",
#'       dataname = "ADCM",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       atirel = choices_selected(
#'         choices = variable_choices(ADCM, "ATIREL"),
#'         selected = c("ATIREL")
#'       ),
#'       cmdecod = choices_selected(
#'         choices = variable_choices(ADCM, "CMDECOD"),
#'         selected = "CMDECOD"
#'       ),
#'       cmindc = choices_selected(
#'         choices = variable_choices(ADCM, "CMINDC"),
#'         selected = "CMINDC"
#'       ),
#'       cmdose = choices_selected(
#'         choices = variable_choices(ADCM, "CMDOSE"),
#'         selected = "CMDOSE"
#'       ),
#'       cmtrt = choices_selected(
#'         choices = variable_choices(ADCM, "CMTRT"),
#'         selected = "CMTRT"
#'       ),
#'       cmdosu = choices_selected(
#'         choices = variable_choices(ADCM, "CMDOSU"),
#'         selected = c("CMDOSU")
#'       ),
#'       cmroute = choices_selected(
#'         choices = variable_choices(ADCM, "CMROUTE"),
#'         selected = "CMROUTE"
#'       ),
#'       cmdosfrq = choices_selected(
#'         choices = variable_choices(ADCM, "CMDOSFRQ"),
#'         selected = "CMDOSFRQ"
#'       ),
#'       cmstdy = choices_selected(
#'         choices = variable_choices(ADCM, "ASTDY"),
#'         selected = "ASTDY"
#'       ),
#'       cmendy = choices_selected(
#'         choices = variable_choices(ADCM, "AENDY"),
#'         selected = "AENDY"
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_pp_therapy <- function(label,
                            dataname = "ADCM",
                            parentname = "ADSL",
                            patient_col = "USUBJID",
                            atirel = NULL,
                            cmdecod = NULL,
                            cmindc = NULL,
                            cmdose = NULL,
                            cmtrt = NULL,
                            cmdosu = NULL,
                            cmroute = NULL,
                            cmdosfrq = NULL,
                            cmstdy = NULL,
                            cmendy = NULL,
                            font_size = c(12L, 12L, 25L),
                            plot_height = c(700L, 200L, 2000L),
                            plot_width = NULL,
                            pre_output = NULL,
                            post_output = NULL,
                            ggplot2_args = teal.devel::ggplot2_args()) {
  logger::log_info("Initializing tm_g_pp_therapy")
  assertthat::assert_that(utils.nest::is_character_single(label))
  assertthat::assert_that(utils.nest::is_character_single(dataname))
  assertthat::assert_that(utils.nest::is_character_single(parentname))
  assertthat::assert_that(utils.nest::is_character_single(patient_col))
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

  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  args <- as.list(environment())
  data_extract_list <- list(
    atirel = utils.nest::if_not_null(atirel, cs_to_des_select(atirel, dataname = dataname)),
    cmdecod = utils.nest::if_not_null(cmdecod, cs_to_des_select(cmdecod, dataname = dataname)),
    cmindc = utils.nest::if_not_null(cmindc, cs_to_des_select(cmindc, dataname = dataname)),
    cmdose = utils.nest::if_not_null(cmdose, cs_to_des_select(cmdose, dataname = dataname)),
    cmtrt = utils.nest::if_not_null(cmtrt, cs_to_des_select(cmtrt, dataname = dataname)),
    cmdosu = utils.nest::if_not_null(cmdosu, cs_to_des_select(cmdosu, dataname = dataname)),
    cmdosfrq = utils.nest::if_not_null(cmdosfrq, cs_to_des_select(cmdosfrq, dataname = dataname)),
    cmroute = utils.nest::if_not_null(cmroute, cs_to_des_select(cmroute, dataname = dataname)),
    cmstdy = utils.nest::if_not_null(cmstdy, cs_to_des_select(cmstdy, dataname = dataname)),
    cmendy = utils.nest::if_not_null(cmendy, cs_to_des_select(cmendy, dataname = dataname))
  )

  module(
    label = label,
    ui = ui_g_therapy,
    ui_args = c(data_extract_list, args),
    server = srv_g_therapy,
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
    filters = "all"
  )
}

ui_g_therapy <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.devel::is_single_dataset(
    ui_args$atirel,
    ui_args$cmdecod,
    ui_args$cmindc,
    ui_args$cmdose,
    ui_args$cmtrt,
    ui_args$cmdosu,
    ui_args$cmdosfrq,
    ui_args$cmroute,
    ui_args$cmstdy,
    ui_args$cmendy
  )

  ns <- NS(id)
  teal.devel::standard_layout(
    output = div(
      teal.devel::get_dt_rows(ns("therapy_table"), ns("therapy_table_rows")),
      DT::DTOutput(outputId = ns("therapy_table")),
      teal.devel::plot_with_settings_ui(id = ns("therapy_plot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(ui_args[c(
        "atirel", "cmdecod", "cmindc", "cmdose", "cmtrt",
        "cmdosu", "cmroute", "cmdosfrq", "cmstdy", "cmendy"
      )]),
      optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.devel::data_extract_ui(
        id = ns("cmdecod"),
        label = "Select the medication decoding column:",
        data_extract_spec = ui_args$cmdecod,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("atirel"),
        label = "Select ATIREL variable:",
        data_extract_spec = ui_args$atirel,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cmindc"),
        label = "Select CMINDC variable:",
        data_extract_spec = ui_args$cmindc,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cmdose"),
        label = "Select CMDOSE variable:",
        data_extract_spec = ui_args$cmdose,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cmtrt"),
        label = "Select CMTRT variable:",
        data_extract_spec = ui_args$cmtrt,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cmdosu"),
        label = "Select CMDOSU variable:",
        data_extract_spec = ui_args$cmdosu,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cmroute"),
        label = "Select CMROUTE variable:",
        data_extract_spec = ui_args$cmroute,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cmdosfrq"),
        label = "Select CMDOSFRQ variable:",
        data_extract_spec = ui_args$cmdosfrq,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cmstdy"),
        label = "Select CMSTDY variable:",
        data_extract_spec = ui_args$cmstdy,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cmendy"),
        label = "Select CMENDY variable:",
        data_extract_spec = ui_args$cmendy,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::panel_item(
        title = "Plot settings",
        collapsed = TRUE,
        optionalSliderInputValMinMax(ns("font_size"), "Font Size", ui_args$font_size, ticks = FALSE, step = 1)
      )
    ),
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}


srv_g_therapy <- function(input,
                          output,
                          session,
                          datasets,
                          dataname,
                          parentname,
                          patient_col,
                          atirel,
                          cmdecod,
                          cmindc,
                          cmdose,
                          cmtrt,
                          cmdosu,
                          cmdosfrq,
                          cmroute,
                          cmstdy,
                          cmendy,
                          plot_height,
                          plot_width,
                          label,
                          ggplot2_args) {
  stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

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

  # Therapy tab ----
  therapy_merged_data <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(
      atirel = atirel, cmdecod = cmdecod, cmindc = cmindc,
      cmdose = cmdose, cmtrt = cmtrt, cmdosu = cmdosu,
      cmroute = cmroute, cmdosfrq = cmdosfrq, cmstdy = cmstdy, cmendy = cmendy
    ),
    merge_function = "dplyr::left_join"
  )

  therapy_call <- reactive({
    validate(need(patient_id(), "Please select a patient."))

    teal.devel::validate_has_data(therapy_merged_data()$data(), 1)

    validate(
      need(
        input[[extract_input("atirel", dataname)]],
        "Please select ATIREL variable."
      ),
      need(
        input[[extract_input("cmdecod", dataname)]],
        "Please select Medication decoding variable."
      ),
      need(
        input[[extract_input("cmindc", dataname)]],
        "Please select CMINDC variable."
      ),
      need(
        input[[extract_input("cmdose", dataname)]],
        "Please select CMDOSE variable."
      ),
      need(
        input[[extract_input("cmtrt", dataname)]],
        "Please select CMTRT variable."
      ),
      need(
        input[[extract_input("cmdosu", dataname)]],
        "Please select CMDOSU variable."
      ),
      need(
        input[[extract_input("cmroute", dataname)]],
        "Please select CMROUTE variable."
      ),
      need(
        input[[extract_input("cmdosfrq", dataname)]],
        "Please select CMDOSFRQ variable."
      ),
      need(
        input[[extract_input("cmstdy", dataname)]],
        "Please select CMSTDY variable."
      ),
      need(
        input[[extract_input("cmendy", dataname)]],
        "Please select CMENDY variable."
      ),
      need(
        nrow(therapy_merged_data()$data()[input$patient_id == therapy_merged_data()$data()[patient_col], ]) > 0,
        "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
      )
    )

    therapy_stack <- teal.devel::chunks$new()
    therapy_stack_push <- function(...) {
      teal.devel::chunks_push(..., chunks = therapy_stack)
    }
    teal.devel::chunks_push_data_merge(therapy_merged_data(), chunks = therapy_stack)

    therapy_stack_push(substitute(
      expr = {
        ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
      }, env = list(
        patient_col = patient_col,
        patient_id = patient_id()
      )
    ))

    my_calls <- template_therapy(
      dataname = "ANL",
      atirel = input[[extract_input("atirel", dataname)]],
      cmdecod = input[[extract_input("cmdecod", dataname)]],
      cmtrt = input[[extract_input("cmtrt", dataname)]],
      cmdosu = input[[extract_input("cmdosu", dataname)]],
      cmroute = input[[extract_input("cmroute", dataname)]],
      cmdosfrq = input[[extract_input("cmdosfrq", dataname)]],
      cmstdy = input[[extract_input("cmstdy", dataname)]],
      cmendy = input[[extract_input("cmendy", dataname)]],
      cmindc = input[[extract_input("cmindc", dataname)]],
      cmdose = input[[extract_input("cmdose", dataname)]],
      patient_id = patient_id(),
      font_size = input[["font_size"]],
      ggplot2_args = ggplot2_args
      )

    lapply(my_calls, therapy_stack_push)
    teal.devel::chunks_safe_eval(chunks = therapy_stack)
    therapy_stack
  })

  output$therapy_table <- DT::renderDataTable({
    teal.devel::chunks_reset()
    teal.devel::chunks_push_chunks(therapy_call())
    teal.devel::chunks_get_var("therapy_table")
    },
    options = list(pageLength = input$therapy_table_rows)
  )

  therapy_plot <- reactive({
    teal.devel::chunks_reset()
    teal.devel::chunks_push_chunks(therapy_call())
    teal.devel::chunks_get_var("therapy_plot")
  })

  callModule(
    teal.devel::plot_with_settings_srv,
    id = "therapy_plot",
    plot_r = therapy_plot,
    height = plot_height,
    width = plot_width
  )

  callModule(
    teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(list(
      atirel, cmdecod, cmindc, cmdose, cmtrt, cmdosu, cmdosfrq, cmroute, cmstdy, cmendy
    )),
    modal_title = label
  )
}
