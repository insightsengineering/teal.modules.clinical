#' Template: Patient Profile Vitals Plot
#'
#' Creates a valid expression to generate a patient profile vitals [ggplot2::ggplot()] plot using ADaM datasets.
#'
#' @inheritParams template_arguments
#' @param paramcd_levels (`character`)\cr vector of all levels of `paramcd`.
#' @param xaxis (`character`)\cr name of the time variable to put on the x-axis.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_pp_vitals()]
#'
#' @keywords internal
template_vitals <- function(dataname = "ANL",
                            paramcd = "PARAMCD",
                            paramcd_levels = c("SYSBP", "DIABP", "PUL", "RESP", "OXYSAT", "WGHT", "TEMP"),
                            xaxis = "ADY",
                            aval_var = "AVAL",
                            patient_id,
                            font_size = 12L,
                            ggplot2_args = teal.widgets::ggplot2_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(paramcd)
  checkmate::assert_string(xaxis)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(patient_id)
  checkmate::assert_number(font_size)

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
    substitute_names(
      names = list(
        dataname = as.name(dataname),
        paramcd = as.name(paramcd),
        xaxis = as.name(xaxis),
        aval_var = as.name(aval_var)
      ),
      others = list(paramcd_levels = paramcd_levels),
      expr = {
        vitals <-
          dataname %>%
          dplyr::group_by(paramcd, xaxis) %>%
          dplyr::filter(paramcd %in% paramcd_levels) %>%
          dplyr::summarise(aval_var = max(aval_var, na.rm = TRUE)) %>%
          dplyr::mutate(
            aval_var = ifelse(is.infinite(aval_var), NA, aval_var),
            xaxis = as.numeric(xaxis) # difftime fails ggplot2::scale_x_continuous
          )
      }
    )
  )

  vital_plot <- add_expr(
    vital_plot,
    substitute(
      expr = {
        max_day <- max(vitals[[xaxis_char]], na.rm = TRUE)
        max_aval <- max(vitals[[aval_char]], na.rm = TRUE)
        max_aval_seq <- seq(0, max_aval, 10)

        full_vita <- levels(dataname[[paramcd_char]])
        provided_vita <- paramcd_levels
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

        plot <- ggplot2::ggplot(data = vitals, mapping = ggplot2::aes(x = xaxis)) + # replaced VSDY
          ggplot2::geom_line(
            data = vitals,
            mapping = ggplot2::aes(y = aval_var, color = paramcd),
            size = 1.5,
            alpha = 0.5
          ) +
          ggplot2::scale_color_manual(
            values = vars_colors
          ) +
          ggplot2::geom_text(
            data = base_stats_df,
            ggplot2::aes(x = x, y = y, label = label, color = color),
            alpha = 1,
            nudge_y = 2.2,
            linewidth = font_size_var / 3.5,
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
      },
      env = list(
        dataname = as.name(dataname),
        paramcd = as.name(paramcd),
        paramcd_char = paramcd,
        paramcd_levels = paramcd_levels,
        xaxis = as.name(xaxis),
        xaxis_char = xaxis,
        aval_var = as.name(aval_var),
        aval_char = aval_var,
        patient_id = patient_id,
        font_size_var = font_size,
        labs = parsed_ggplot2_args$labs,
        ggthemes = parsed_ggplot2_args$ggtheme,
        themes = parsed_ggplot2_args$theme
      )
    )
  )

  y$plot <- bracket_expr(vital_plot)
  y
}

#' teal Module: Patient Profile Vitals Plot
#'
#' This module produces a patient profile vitals [ggplot2::ggplot()] type plot using ADaM datasets.
#'
#' This plot supports horizontal lines for the following 6 `PARAMCD` levels when they are present in `dataname`:
#' `"SYSBP"`, `"DIABP"`, `"TEMP"`, `"RESP"`, `"OXYSAT"`.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_vitals
#' @inheritParams template_arguments
#' @param xaxis ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for the time variable from `dataname` to be put on the plot x-axis.
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
#' tm_g_pp_vitals(
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
#'   ADSL <- tmc_ex_adsl
#'   ADVS <- tmc_ex_advs
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADVS <- data[["ADVS"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_pp_vitals(
#'       label = "Vitals",
#'       dataname = "ADVS",
#'       parentname = "ADSL",
#'       patient_col = "USUBJID",
#'       plot_height = c(600L, 200L, 2000L),
#'       paramcd = choices_selected(
#'         choices = variable_choices(ADVS, "PARAMCD"),
#'         selected = "PARAMCD"
#'       ),
#'       xaxis = choices_selected(
#'         choices = variable_choices(ADVS, "ADY"),
#'         selected = "ADY"
#'       ),
#'       aval_var = choices_selected(
#'         choices = variable_choices(ADVS, "AVAL"),
#'         selected = "AVAL"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_pp_vitals <- function(label,
                           dataname = "ADVS",
                           parentname = "ADSL",
                           patient_col = "USUBJID",
                           paramcd = NULL,
                           aval = lifecycle::deprecated(),
                           aval_var = NULL,
                           xaxis = NULL,
                           font_size = c(12L, 12L, 25L),
                           plot_height = c(700L, 200L, 2000L),
                           plot_width = NULL,
                           pre_output = NULL,
                           post_output = NULL,
                           ggplot2_args = teal.widgets::ggplot2_args(),
                           transformators = list(),
                           decorators = list()) {
  if (lifecycle::is_present(aval)) {
    lifecycle::deprecate_stop(
      when = "0.8.16",
      what = "tm_g_pp_vitals(aval)",
      with = "tm_g_pp_vitals(aval_var)"
    )
  }

  message("Initializing tm_g_pp_vitals")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(patient_col)
  checkmate::assert_class(paramcd, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(aval_var, "choices_selected", null.ok = TRUE)
  checkmate::assert_class(xaxis, "choices_selected", null.ok = TRUE)
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
  checkmate::assert_multi_class(aval_var, c("choices_selected", "data_extract_spec"), null.ok = TRUE)
  checkmate::assert_multi_class(xaxis, c("choices_selected", "data_extract_spec"), null.ok = TRUE)
  assert_decorators(decorators, "plot")

  args <- as.list(environment())
  data_extract_list <- list(
    paramcd = `if`(is.null(paramcd), NULL, cs_to_des_select(paramcd, dataname = dataname)),
    aval_var = `if`(is.null(aval_var), NULL, cs_to_des_select(aval_var, dataname = dataname)),
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
        ggplot2_args = ggplot2_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_vitals <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$paramcd,
    ui_args$aval_var,
    ui_args$xaxis
  )

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("vitals_plot")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(ui_args[c("paramcd", "aval_var", "xaxis")]),
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
      uiOutput(ns("paramcd_levels")),
      teal.transform::data_extract_ui(
        id = ns("xaxis"),
        label = "Select vital plot x-axis:",
        data_extract_spec = ui_args$xaxis,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Select AVAL variable:",
        data_extract_spec = ui_args$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(ui_args$decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"), "Font Size", ui_args$font_size,
            ticks = FALSE, step = 1
          )
        )
      )
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

#' @keywords internal
srv_g_vitals <- function(id,
                         data,
                         dataname,
                         parentname,
                         patient_col,
                         paramcd,
                         aval_var,
                         xaxis,
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

    # Vitals tab ----

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(paramcd = paramcd, xaxis = xaxis, aval_var = aval_var),
      datasets = data,
      select_validation_rule = list(
        paramcd = shinyvalidate::sv_required(
          "Please select PARAMCD variable."
        ),
        xaxis = shinyvalidate::sv_required(
          "Please select Vitals x-axis variable."
        ),
        aval_var = shinyvalidate::sv_required(
          "Please select AVAL variable."
        )
      )
    )

    iv_r <- reactive({
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
      selector_list = selector_list,
      merge_function = "dplyr::left_join"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj %>% teal.code::eval_code(code = as.expression(anl_inputs()$expr))
    })

    merged <- list(anl_input_r = anl_inputs, anl_q = anl_q)

    output$paramcd_levels <- renderUI({
      paramcd_var <- input[[extract_input("paramcd", dataname)]]

      req(paramcd_var)
      req(input$patient_id)

      vitals_dat <- merged$anl_q()[["ANL"]]
      vitals_dat_sub <- vitals_dat[vitals_dat[[patient_col]] == patient_id(), ]
      paramcd_col <- vitals_dat_sub[[paramcd_var]]
      paramcd_col_levels <- unique(paramcd_col)

      cur_selected <- isolate(input$paramcd_levels_vals)

      selected <- if (length(cur_selected) > 0) {
        cur_selected
      } else {
        paramcd_col_levels
      }

      tagList(
        selectInput(
          session$ns("paramcd_levels_vals"),
          "Select PARAMCD variable levels:",
          selected = selected,
          choices = paramcd_col_levels,
          multiple = TRUE
        )
      )
    })

    all_q <- reactive({
      teal::validate_has_data(merged$anl_q()[["ANL"]], 1)

      teal::validate_inputs(iv_r())

      validate(
        need(
          nrow(merged$anl_q()[["ANL"]][input$patient_id == merged$anl_q()[["ANL"]][, patient_col], ]) > 0,
          "Selected patient is not in dataset (either due to filtering or missing values). Consider relaxing filters."
        )
      )

      my_calls <- template_vitals(
        dataname = "ANL",
        paramcd = input[[extract_input("paramcd", dataname)]],
        paramcd_levels = input[["paramcd_levels_vals"]],
        xaxis = input[[extract_input("xaxis", dataname)]],
        aval_var = input[[extract_input("aval_var", dataname)]],
        patient_id = patient_id(),
        font_size = input[["font_size"]],
        ggplot2_args = ggplot2_args
      )

      obj <- teal.code::eval_code(
        merged$anl_q(),
        substitute(
          expr = {
            ANL <- ANL[ANL[[patient_col]] == patient_id, ]
          }, env = list(
            patient_col = patient_col,
            patient_id = patient_id()
          )
        )
      )
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_all_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "plot"),
      expr = plot
    )
    plot_r <- reactive(decorated_all_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "vitals_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
