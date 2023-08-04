#' Template: Vitals
#'
#' Creates a vitals template.
#' @inheritParams template_arguments
#' @param paramcd (`character`)\cr name of the parameter code variable.
#' @param paramcd_levels (`character`)\cr (`paramcd`)\cr vector with (`#'paramcd`)\cr levels.
#' @param xaxis (`character`)\cr name of time variable used for the x-axis.
#' @param aval (`character`)\cr name of the analysis value variable.
#' @param patient_id (`character`)\cr patient ID.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#'
#' @note
#' The vitals plot supports horizontal lines for 6 `PARAMCD` levels (`SYSBP, DIABP, TEMP, RESP, OXYSAT
#' and PULSE`)\cr when these levels are present in the analyzed dataset and the spelling matches.
#' @keywords internal
#'
template_vitals <- function(dataname = "ANL",
                            paramcd = "PARAMCD",
                            paramcd_levels = c("SYSBP", "DIABP", "PUL", "RESP", "OXYSAT", "WGHT", "TEMP"),
                            xaxis = "ADY",
                            aval = "AVAL",
                            patient_id,
                            font_size = 12L,
                            ggplot2_args = teal.widgets::ggplot2_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(paramcd),
    assertthat::is.string(xaxis),
    assertthat::is.string(aval),
    assertthat::is.string(patient_id),
    is.numeric(font_size)
  )

  # Note: VSDY (study day of vital signs) was replaced with ADY (analysis day)
  y <- list()
  y$plot <- list()

  parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
    teal.widgets::resolve_ggplot2_args(
      user_plot = ggplot2_args,
      module_plot = teal.widgets::ggplot2_args(
        labs = list(title = paste0("Patient ID: ", patient_id)),
        theme = list(
          text = substitute(ggplot2::element_text(size = font), list(font = font_size)),
          axis.text.y = quote(ggplot2::element_blank()),
          axis.ticks.y = quote(ggplot2::element_blank()),
          plot.title = substitute(ggplot2::element_text(size = font), list(font = font_size)),
          legend.position = "top",
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

  vital_plot <- add_expr(
    list(),
    substitute(expr = {
      vitals <-
        dataname %>%
        dplyr::group_by(paramcd, xaxis) %>%
        dplyr::filter(paramcd %in% paramcd_levels_chars) %>%
        dplyr::summarise(AVAL = max(aval, na.rm = TRUE)) %>%
        dplyr::mutate(AVAL = ifelse(is.infinite(AVAL), NA, AVAL))

      max_day <- max(vitals[[xaxis_char]], na.rm = TRUE)
      max_aval <- max(vitals[[aval_char]], na.rm = TRUE)
      max_aval_seq <- seq(0, max_aval, 10)

      full_vita <- levels(dataname[[paramcd_char]])
      provided_vita <- paramcd_levels_chars
      known_vita <- c("SYSBP", "DIABP", "TEMP", "RESP", "OXYSAT", "PULSE")

      paramcd_levels_e <- known_vita[stats::na.omit(match(provided_vita, known_vita))]
      len_paramcd_levels_e <- length(paramcd_levels_e)

      all_colors <- stats::setNames(nestcolor::color_palette(length(full_vita), "stream"), full_vita)
      vars_colors <- all_colors[provided_vita]
      names(vars_colors) <- provided_vita

      base_stats <- stats::setNames(c(140, 90, 38, 20, 94, 100), known_vita)
      paramcd_stats_e <- base_stats[paramcd_levels_e]

      base_labels <- stats::setNames(c("140mmHg", "90mmHg", "38\u00B0 C", "20/min", "94%", "100bpm"), known_vita)
      paramcd_labels_e <- base_labels[paramcd_levels_e]

      base_stats_df <- data.frame(
        x = rep(1, len_paramcd_levels_e),
        y = paramcd_stats_e,
        label = paramcd_labels_e,
        color = paramcd_levels_e
      )

      result_plot <- ggplot2::ggplot(data = vitals, mapping = ggplot2::aes(x = xaxis)) + # replaced VSDY
        ggplot2::geom_line(
          data = vitals,
          mapping = ggplot2::aes(y = aval, color = paramcd),
          size = 1.5,
          alpha = 0.5
        ) +
        ggplot2::scale_color_manual(
          values = vars_colors,
        ) +
        ggplot2::geom_text(
          data = base_stats_df,
          ggplot2::aes(x = x, y = y, label = label, color = color),
          alpha = 1,
          nudge_y = 2.2,
          size = font_size_var / 3.5,
          show.legend = FALSE
        ) +
        ggplot2::geom_hline(
          data = base_stats_df,
          ggplot2::aes(yintercept = y, color = color),
          linetype = 2,
          alpha = 0.5,
          size = 1,
          show.legend = FALSE
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(0, max(vitals[[xaxis_char]], na.rm = TRUE), 50),
          minor_breaks = seq(0, max(vitals[[aval_char]], na.rm = TRUE), 10)
        ) +
        ggplot2::geom_text(
          data = data.frame(
            x = rep(max_day, length(max_aval_seq)),
            y = max_aval_seq,
            l = as.character(max_aval_seq)
          ),
          ggplot2::aes(
            x = x,
            y = y,
            label = l
          ),
          color = "black",
          alpha = 1,
          nudge_y = 2.2,
          size = font_size_var / 3.5
        ) +
        labs +
        ggthemes +
        themes

      print(result_plot)
    }, env = list(
      dataname = as.name(dataname),
      paramcd = as.name(paramcd),
      paramcd_char = paramcd,
      paramcd_levels_chars = paramcd_levels,
      xaxis = as.name(xaxis),
      xaxis_char = xaxis,
      aval = as.name(aval),
      aval_char = aval,
      patient_id = patient_id,
      font_size_var = font_size,
      labs = parsed_ggplot2_args$labs,
      ggthemes = parsed_ggplot2_args$ggtheme,
      themes = parsed_ggplot2_args$theme
    ))
  )

  y$plot <- bracket_expr(vital_plot)
  y
}

#' Teal Module: Patient Profile Vitals Teal Module
#'
#' This teal module produces a patient profile vitals plot using `ADaM` datasets.
#'
#' @inheritParams module_arguments
#' @param patient_col (`character`)\cr patient ID column to be used.
#' @param paramcd ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{PARAMCD} column of the `ADVS` dataset.
#' @param aval ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' \code{AVAL} column of the `ADVS` dataset.
#' @param xaxis ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' time variable to be represented in the vitals plot x-axis.
#' @param font_size (`numeric`)\cr numeric vector of length 3 for current, min and max font size values.
#'
#' @export
#'
#' @examples
#' library(nestcolor)
#'
#' adsl <- tmc_ex_adsl
#' advs <- tmc_ex_advs
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADVS", advs)
#'   ),
#'   modules = modules(
#'     tm_g_pp_vitals(
#'       label = "Vitals",
#'       dataname = "ADVS",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       paramcd = choices_selected(
#'         choices = variable_choices(advs, "PARAMCD"),
#'         selected = "PARAMCD"
#'       ),
#'       xaxis = choices_selected(
#'         choices = variable_choices(advs, "ADY"),
#'         selected = "ADY"
#'       ),
#'       aval = choices_selected(
#'         choices = variable_choices(advs, "AVAL"),
#'         selected = "AVAL"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_pp_vitals <- function(label,
                           dataname = "ADVS",
                           parentname = "ADSL",
                           patient_col = "USUBJID",
                           paramcd = NULL,
                           aval = NULL,
                           xaxis = NULL,
                           font_size = c(12L, 12L, 25L),
                           plot_height = c(700L, 200L, 2000L),
                           plot_width = NULL,
                           pre_output = NULL,
                           post_output = NULL,
                           ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_pp_vitals")
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
  checkmate::assert_multi_class(paramcd, c("choices_selected", "data_extract_spec"), null.ok = TRUE)
  checkmate::assert_multi_class(aval, c("choices_selected", "data_extract_spec"), null.ok = TRUE)
  checkmate::assert_multi_class(xaxis, c("choices_selected", "data_extract_spec"), null.ok = TRUE)

  args <- as.list(environment())
  data_extract_list <- list(
    paramcd = `if`(is.null(paramcd), NULL, cs_to_des_select(paramcd, dataname = dataname)),
    aval = `if`(is.null(aval), NULL, cs_to_des_select(aval, dataname = dataname)),
    xaxis = `if`(is.null(xaxis), NULL, cs_to_des_select(xaxis, dataname = dataname))
  )

  module(
    label = label,
    ui = ui_g_vitals,
    ui_args = c(data_extract_list, args),
    server = srv_g_vitals,
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

ui_g_vitals <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$paramcd,
    ui_args$aval,
    ui_args$xaxis
  )

  ns <- shiny::NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("vitals_plot")),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(ui_args[c("paramcd", "aval", "xaxis")]),
      teal.widgets::optionalSelectInput(
        ns("patient_id"),
        "Select Patient:",
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(`liveSearch` = TRUE)
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select PARAMCD variable:",
        data_extract_spec = ui_args$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::uiOutput(ns("paramcd_levels")),
      teal.transform::data_extract_ui(
        id = ns("xaxis"),
        label = "Select vital plot x-axis:",
        data_extract_spec = ui_args$xaxis,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval"),
        label = "Select AVAL variable:",
        data_extract_spec = ui_args$aval,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::panel_item(
        title = "Plot settings",
        collapsed = TRUE,
        teal.widgets::optionalSliderInputValMinMax(
          ns("font_size"), "Font Size", ui_args$font_size,
          ticks = FALSE, step = 1
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


srv_g_vitals <- function(id,
                         data,
                         reporter,
                         filter_panel_api,
                         dataname,
                         parentname,
                         patient_col,
                         paramcd,
                         aval,
                         xaxis,
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

    # Vitals tab ----

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(paramcd = paramcd, xaxis = xaxis, aval = aval),
      datasets = data,
      select_validation_rule = list(
        paramcd = shinyvalidate::sv_required(
          "Please select PARAMCD variable."
        ),
        xaxis = shinyvalidate::sv_required(
          "Please select Vitals x-axis variable."
        ),
        aval = shinyvalidate::sv_required(
          "Please select AVAL variable."
        )
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("patient_id", shinyvalidate::sv_required(
        "Please select a patient."
      ))
      iv$add_rule("paramcd_levels_vals", shinyvalidate::sv_required(
        "Please select PARAMCD variable levels."
      ))
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

    output$paramcd_levels <- shiny::renderUI({
      paramcd_var <- input[[extract_input("paramcd", dataname)]]

      shiny::req(paramcd_var)
      shiny::req(input$patient_id)

      vitals_dat <- merged$anl_q()[["ANL"]]
      vitals_dat_sub <- vitals_dat[vitals_dat[[patient_col]] == patient_id(), ]
      paramcd_col <- vitals_dat_sub[[paramcd_var]]
      paramcd_col_levels <- unique(paramcd_col)

      cur_selected <- shiny::isolate(input$paramcd_levels_vals)

      selected <- if (length(cur_selected) > 0) {
        cur_selected
      } else {
        paramcd_col_levels
      }

      shiny::tagList(
        shiny::selectInput(
          session$ns("paramcd_levels_vals"),
          "Select PARAMCD variable levels:",
          selected = selected,
          choices = paramcd_col_levels,
          multiple = TRUE
        )
      )
    })

    all_q <- shiny::reactive({
      teal::validate_has_data(merged$anl_q()[["ANL"]], 1)

      teal::validate_inputs(iv_r())

      shiny::validate(
        shiny::need(
          nrow(merged$anl_q()[["ANL"]][input$patient_id == merged$anl_q()[["ANL"]][, patient_col], ]) > 0,
          "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
        )
      )

      my_calls <- template_vitals(
        dataname = "ANL",
        paramcd = input[[extract_input("paramcd", dataname)]],
        paramcd_levels = input[["paramcd_levels_vals"]],
        xaxis = input[[extract_input("xaxis", dataname)]],
        aval = input[[extract_input("aval", dataname)]],
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

    plot_r <- shiny::reactive(all_q()[["result_plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "vitals_plot",
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
        card$set_name("Patient Profile Vitals Plot")
        card$append_text("Patient Profile Vitals Plot", "header2")
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
