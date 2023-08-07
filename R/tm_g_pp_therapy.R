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
#' @keywords internal
#'
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
                             ggplot2_args = teal.widgets::ggplot2_args()) {
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

  parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
    teal.widgets::resolve_ggplot2_args(
      user_plot = ggplot2_args,
      module_plot = teal.widgets::ggplot2_args(
        labs = list(y = "Medication", title = paste0("Patient ID: ", patient_id)),
        theme = list(
          text = substitute(ggplot2::element_text(size = font), list(font = font_size)),
          axis.text.y = quote(ggplot2::element_blank()),
          axis.ticks.y = quote(ggplot2::element_blank()),
          plot.title = substitute(ggplot2::element_text(size = font), list(font = font_size)),
          legend.position = "none",
          panel.grid.minor = quote(ggplot2::element_line(
            linewidth = 0.5,
            linetype = "dotted",
            colour = "grey"
          )),
          panel.grid.major = quote(ggplot2::element_line(
            linewidth = 0.5,
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
        ggplot2::ggplot(data = data, ggplot2::aes(fill = cmindc, color = cmindc, y = CMDECOD, x = CMSTDY)) +
        ggplot2::geom_segment(ggplot2::aes(xend = CMENDY, yend = CMDECOD), size = 2) +
        ggplot2::geom_text(
          data =
            data %>%
              dplyr::select(CMDECOD, cmindc, CMSTDY) %>%
              dplyr::distinct(),
          ggplot2::aes(x = CMSTDY, label = CMDECOD), color = "black",
          hjust = "left",
          vjust = "bottom",
          nudge_y = 0.1,
          size = font_size_var / 3.5
        ) +
        ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 1.2)) +
        ggplot2::geom_point(color = "black", size = 2, shape = 24, position = ggplot2::position_nudge(y = -0.15)) +
        labs +
        ggtheme +
        theme

      print(therapy_plot)
    }, env = c(
      list(
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
        font_size_var = font_size
      ),
      parsed_ggplot2_args
    ))
  )
  y$table_list <- bracket_expr(table_list)
  y$plot_list <- bracket_expr(plot_list)
  y
}

#' Teal Module: Patient Profile Therapy Teal Module
#'
#' This teal module produces a patient profile therapy plot using `ADaM` datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param atirel ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{ATIREL} column of the `ADCM` dataset.
#' @param cmdecod ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMDECOD} column of the `ADCM` dataset.
#' @param cmdose ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMDOSE} column of the `ADCM` dataset.
#' @param cmtrt ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMTRT} column of the `ADCM` dataset.
#' @param cmdosu ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMDOSU} column of the `ADCM` dataset.
#' @param cmroute ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMROUTE} column of the `ADCM` dataset.
#' @param cmdosfrq ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMDOSFRQ} column of the `ADCM` dataset.
#' @param cmstdy ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMSTDY} column of the `ADCM` dataset.
#' @param cmendy ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMENDY} column of the `ADCM` dataset.
#' @param cmindc ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{CMINDC} column of the `ADCM` dataset.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#'
#' @export
#'
#' @examples
#' library(nestcolor)
#'
#' adcm <- tmc_ex_adcm
#' adsl <- tmc_ex_adsl %>% dplyr::filter(USUBJID %in% adcm$USUBJID)
#' adcm$CMASTDTM <- adcm$ASTDTM
#' adcm$CMAENDTM <- adcm$AENDTM
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADCM", adcm, keys = adcm_keys)
#'   ),
#'   modules = modules(
#'     tm_g_pp_therapy(
#'       label = "Therapy",
#'       dataname = "ADCM",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       atirel = choices_selected(
#'         choices = variable_choices(adcm, "ATIREL"),
#'         selected = c("ATIREL")
#'       ),
#'       cmdecod = choices_selected(
#'         choices = variable_choices(adcm, "CMDECOD"),
#'         selected = "CMDECOD"
#'       ),
#'       cmindc = choices_selected(
#'         choices = variable_choices(adcm, "CMINDC"),
#'         selected = "CMINDC"
#'       ),
#'       cmdose = choices_selected(
#'         choices = variable_choices(adcm, "CMDOSE"),
#'         selected = "CMDOSE"
#'       ),
#'       cmtrt = choices_selected(
#'         choices = variable_choices(adcm, "CMTRT"),
#'         selected = "CMTRT"
#'       ),
#'       cmdosu = choices_selected(
#'         choices = variable_choices(adcm, "CMDOSU"),
#'         selected = c("CMDOSU")
#'       ),
#'       cmroute = choices_selected(
#'         choices = variable_choices(adcm, "CMROUTE"),
#'         selected = "CMROUTE"
#'       ),
#'       cmdosfrq = choices_selected(
#'         choices = variable_choices(adcm, "CMDOSFRQ"),
#'         selected = "CMDOSFRQ"
#'       ),
#'       cmstdy = choices_selected(
#'         choices = variable_choices(adcm, "ASTDY"),
#'         selected = "ASTDY"
#'       ),
#'       cmendy = choices_selected(
#'         choices = variable_choices(adcm, "AENDY"),
#'         selected = "AENDY"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
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
                            ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_pp_therapy")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
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
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  args <- as.list(environment())
  data_extract_list <- list(
    atirel = `if`(is.null(atirel), NULL, cs_to_des_select(atirel, dataname = dataname)),
    cmdecod = `if`(is.null(cmdecod), NULL, cs_to_des_select(cmdecod, dataname = dataname)),
    cmindc = `if`(is.null(cmindc), NULL, cs_to_des_select(cmindc, dataname = dataname)),
    cmdose = `if`(is.null(cmdose), NULL, cs_to_des_select(cmdose, dataname = dataname)),
    cmtrt = `if`(is.null(cmtrt), NULL, cs_to_des_select(cmtrt, dataname = dataname)),
    cmdosu = `if`(is.null(cmdosu), NULL, cs_to_des_select(cmdosu, dataname = dataname)),
    cmdosfrq = `if`(is.null(cmdosfrq), NULL, cs_to_des_select(cmdosfrq, dataname = dataname)),
    cmroute = `if`(is.null(cmroute), NULL, cs_to_des_select(cmroute, dataname = dataname)),
    cmstdy = `if`(is.null(cmstdy), NULL, cs_to_des_select(cmstdy, dataname = dataname)),
    cmendy = `if`(is.null(cmendy), NULL, cs_to_des_select(cmendy, dataname = dataname))
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
    datanames = "all"
  )
}

ui_g_therapy <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
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

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = shiny::div(
      teal.widgets::get_dt_rows(ns("therapy_table"), ns("therapy_table_rows")),
      DT::DTOutput(outputId = ns("therapy_table")),
      teal.widgets::plot_with_settings_ui(id = ns("therapy_plot"))
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(ui_args[c(
        "atirel", "cmdecod", "cmindc", "cmdose", "cmtrt",
        "cmdosu", "cmroute", "cmdosfrq", "cmstdy", "cmendy"
      )]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("cmdecod"),
        label = "Select the medication decoding column:",
        data_extract_spec = ui_args$cmdecod,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("atirel"),
        label = "Select ATIREL variable:",
        data_extract_spec = ui_args$atirel,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmindc"),
        label = "Select CMINDC variable:",
        data_extract_spec = ui_args$cmindc,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmdose"),
        label = "Select CMDOSE variable:",
        data_extract_spec = ui_args$cmdose,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmtrt"),
        label = "Select CMTRT variable:",
        data_extract_spec = ui_args$cmtrt,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmdosu"),
        label = "Select CMDOSU variable:",
        data_extract_spec = ui_args$cmdosu,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmroute"),
        label = "Select CMROUTE variable:",
        data_extract_spec = ui_args$cmroute,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmdosfrq"),
        label = "Select CMDOSFRQ variable:",
        data_extract_spec = ui_args$cmdosfrq,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmstdy"),
        label = "Select CMSTDY variable:",
        data_extract_spec = ui_args$cmstdy,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cmendy"),
        label = "Select CMENDY variable:",
        data_extract_spec = ui_args$cmendy,
        is_single_dataset = is_single_dataset_value
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


srv_g_therapy <- function(id,
                          data,
                          reporter,
                          filter_panel_api,
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
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    patient_id <- shiny::reactive(input$patient_id)

    # Init
    patient_data_base <- shiny::reactive(unique(data[[parentname]]()[[patient_col]]))
    teal.widgets::updateOptionalSelectInput(
      session, "patient_id",
      choices = patient_data_base(), selected = patient_data_base()[1]
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

    # Therapy tab ----
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        atirel = atirel, cmdecod = cmdecod, cmindc = cmindc,
        cmdose = cmdose, cmtrt = cmtrt, cmdosu = cmdosu,
        cmroute = cmroute, cmdosfrq = cmdosfrq, cmstdy = cmstdy, cmendy = cmendy
      ),
      datasets = data,
      select_validation_rule = list(
        atirel = shinyvalidate::sv_required("Please select ATIREL variable."),
        cmdecod = shinyvalidate::sv_required("Please select medication decoding variable."),
        cmindc = shinyvalidate::sv_required("Please select CMINDC variable."),
        cmdose = shinyvalidate::sv_required("Please select CMDOSE variable."),
        cmtrt = shinyvalidate::sv_required("Please select CMTRT variable."),
        cmdosu = shinyvalidate::sv_required("Please select CMDOSU variable."),
        cmroute = shinyvalidate::sv_required("Please select CMROUTE variable."),
        cmdosfrq = shinyvalidate::sv_required("Please select CMDOSFRQ variable."),
        cmstdy = shinyvalidate::sv_required("Please select CMSTDY variable."),
        cmendy = shinyvalidate::sv_required("Please select CMENDY variable.")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required("Please select a patient."))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      join_keys = get_join_keys(data),
      selector_list = selector_list,
      merge_function = "dplyr::left_join"
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })

    merged <- list(anl_input_r = anl_inputs, anl_q = anl_q)

    all_q <- shiny::reactive({
      teal::validate_has_data(merged$anl_q()[["ANL"]], 1)

      teal::validate_inputs(iv_r())

      shiny::validate(
        shiny::need(
          nrow(merged$anl_q()[["ANL"]][input$patient_id == merged$anl_q()[["ANL"]][, patient_col], ]) > 0,
          "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
        )
      )

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

      teal.code::eval_code(
        merged$anl_q(),
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ] # nolint
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      ) %>%
        teal.code::eval_code(as.expression(my_calls))
    })

    output$therapy_table <- DT::renderDataTable(
      expr = {
        teal.code::dev_suppress(all_q()[["therapy_table"]])
      },
      options = list(pageLength = input$therapy_table_rows)
    )

    plot_r <- shiny::reactive({
      shiny::req(iv_r()$is_valid())
      all_q()[["therapy_plot"]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "therapy_plot",
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
        card$set_name("Patient Profile Therapy Plot")
        card$append_text("Patient Profile Therapy Plot", "header2")
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
