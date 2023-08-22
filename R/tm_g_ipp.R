#' Template: Individual Patient Plots
#'
#' @inheritParams template_arguments
#' @param avalu_var (`string`)\cr variable name designating the unit of the analysis variable.
#' @param base_var (`string`)\cr variable name designating the baseline values of analysis variable.
#' @param visit_var (`string`)\cr variable name designating the visit timepoint variable.
#' @param add_baseline_hline (`flag`)\cr adds horizontal line at baseline y-value on plot
#' @param separate_by_obs (`flag`)\cr creates multi panel plots when TRUE
#' @param suppress_legend (`flag`)\cr allow user to suppress legend
#' @param arm_levels (`character`)\cr vector of all arm variable levels.
#' @param avalu_first (`string`)\cr `avalu` value.
#' @param paramcd_first (`string`)\cr `paramcd` value.
#' @param add_avalu (`flag`)\cr allow user to not display value unit in the plot.
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `ggplot2_args` object with `labs` list of following child elements:
#' `title`, `subtitle`, `x`, `y`.
#' No other elements would be taken into account. The argument is merged with option `teal.ggplot2_args` and
#' with default module arguments (hard coded in the module body).
#'
#' For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#' @keywords internal
#'
template_g_ipp <- function(dataname = "ANL",
                           paramcd,
                           arm_var,
                           arm_levels,
                           avalu_first,
                           paramcd_first,
                           aval_var = "AVAL",
                           avalu_var = "AVALU",
                           id_var = "USUBJID",
                           visit_var = "AVISIT",
                           base_var = "BASE",
                           add_baseline_hline = FALSE,
                           separate_by_obs = FALSE,
                           ggplot2_args = teal.widgets::ggplot2_args(),
                           suppress_legend = FALSE,
                           add_avalu = TRUE) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(paramcd),
    assertthat::is.string(arm_var),
    assertthat::is.string(aval_var),
    assertthat::is.string(avalu_var),
    assertthat::is.string(id_var),
    assertthat::is.string(visit_var),
    assertthat::is.string(base_var),
    assertthat::is.flag(add_baseline_hline),
    assertthat::is.flag(separate_by_obs),
    assertthat::is.flag(suppress_legend),
    assertthat::is.flag(add_avalu)
  )

  y <- list()
  # Data preprocessing

  y$data <- substitute(
    expr = anl <- df %>% droplevels(),
    env = list(df = as.name(dataname))
  )

  title <- ifelse(
    add_avalu,
    sprintf("Individual Patient Plot for %s Values (%s) over Time", paramcd_first, avalu_first),
    sprintf("Individual Patient Plot for %s Values over Time", paramcd_first)
  )
  y_axis <- ifelse(
    add_avalu,
    sprintf("%s (%s)", paramcd_first, avalu_first),
    paramcd_first
  )

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = teal.widgets::ggplot2_args(
      labs = list(
        title = title,
        x = "Visit",
        y = y_axis,
        subtitle = paste(arm_levels, collapse = ", ")
      )
    )
  )



  graph_list <- list()
  graph_list <- add_expr(
    graph_list,
    substitute(
      expr = {
        plot <- h_g_ipp(
          df = anl,
          xvar = visit,
          yvar = aval,
          xlab = xlab_val,
          ylab = ylab_val,
          title = title_val,
          subtitle = subtitle_val,
          id_var = id,
          add_baseline_hline = add_baseline_hline,
          yvar_baseline = base
        )
      },
      env = list(
        xlab_val = all_ggplot2_args$labs$x,
        ylab_val = all_ggplot2_args$labs$y,
        title_val = all_ggplot2_args$labs$title,
        subtitle_val = all_ggplot2_args$labs$subtitle,
        paramcd = paramcd,
        visit = visit_var,
        aval = aval_var,
        id = id_var,
        add_baseline_hline = add_baseline_hline,
        base = base_var,
        avalu = avalu_var,
        arm = arm_var
      )
    )
  )

  if (separate_by_obs) {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(id)),
        env = list(id = as.name(id_var))
      )
    )
  }

  if (suppress_legend) {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = {
          plot <- plot + ggplot2::theme(legend.position = "none")
        },
        env = list(id = as.name(id_var))
      )
    )
  }

  graph_list <- add_expr(
    graph_list,
    quote(grid::grid.newpage())
  )

  graph_list <- add_expr(
    graph_list,
    quote(grid::grid.draw(plot))
  )

  y$graph <- bracket_expr(graph_list)

  y
}

#' Teal Module: Individual Patient Plot
#'
#' This teal module produces grid style Individual patient plot(s) that show
#' trends in parameter values over time for each patient using data with
#' `ADaM` structure.
#'
#' @inheritParams template_g_ipp
#' @inheritParams module_arguments
#' @param arm_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all available choices
#'   and preselected option for variable values that can be used as `arm_var`.
#' @param avalu_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all available choices
#'   and preselected option for variable values that can be used as `avalu_var`.
#' @param base_var ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#'   object with all available choices
#'   and preselected option for variable values that can be used as `base_var`.
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `ggplot2_args` object with `labs` list of following child elements:
#' `title`, `subtitle`, `x`, `y`.
#' No other elements would be taken into account. The argument is merged with option `teal.ggplot2_args` and
#' with default module arguments (hard coded in the module body).
#'
#' For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @export
#'
#' @examples
#' library(nestcolor)
#'
#' adsl <- tmc_ex_adsl %>% dplyr::slice(1:20)
#' adlb <- tmc_ex_adlb %>% dplyr::filter(USUBJID %in% adsl$USUBJID)
#'
#' adsl <- df_explicit_na(adsl)
#' adlb <- df_explicit_na(adlb) %>% dplyr::filter(AVISIT != "SCREENING")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADLB", adlb)
#'   ),
#'   modules = modules(
#'     tm_g_ipp(
#'       label = "Individual Patient Plot",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         value_choices(adlb, "ARMCD"),
#'         "ARM A"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(adlb, "PARAMCD"),
#'         "ALT"
#'       ),
#'       aval_var = choices_selected(
#'         variable_choices(adlb, c("AVAL", "CHG")),
#'         "AVAL"
#'       ),
#'       avalu_var = choices_selected(
#'         variable_choices(adlb, c("AVALU")),
#'         "AVALU",
#'         fixed = TRUE
#'       ),
#'       id_var = choices_selected(
#'         variable_choices(adlb, c("USUBJID")),
#'         "USUBJID",
#'         fixed = TRUE
#'       ),
#'       visit_var = choices_selected(
#'         variable_choices(adlb, c("AVISIT")),
#'         "AVISIT"
#'       ),
#'       base_var = choices_selected(
#'         variable_choices(adlb, c("BASE")),
#'         "BASE",
#'         fixed = TRUE
#'       ),
#'       add_baseline_hline = FALSE,
#'       separate_by_obs = FALSE
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_ipp <- function(label,
                     dataname,
                     parentname = ifelse(
                       inherits(arm_var, "data_extract_spec"),
                       teal.transform::datanames_input(arm_var),
                       "ADSL"
                     ),
                     arm_var,
                     paramcd,
                     id_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "USUBJID"),
                       "USUBJID",
                       fixed = TRUE
                     ),
                     visit_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "AVISIT"),
                       "AVISIT",
                       fixed = TRUE
                     ),
                     aval_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "AVAL"),
                       "AVAL",
                       fixed = TRUE
                     ),
                     avalu_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "AVALU"),
                       "AVALU",
                       fixed = TRUE
                     ),
                     base_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, "BASE"),
                       "BASE",
                       fixed = TRUE
                     ),
                     add_baseline_hline = FALSE,
                     separate_by_obs = FALSE,
                     suppress_legend = FALSE,
                     add_avalu = TRUE,
                     plot_height = c(1200L, 400L, 5000L),
                     plot_width = NULL,
                     pre_output = NULL,
                     post_output = NULL,
                     ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_ipp")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_flag(add_baseline_hline)
  checkmate::assert_flag(separate_by_obs)
  checkmate::assert_flag(suppress_legend)
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
    arm_var = cs_to_des_filter(arm_var, dataname = parentname, multiple = TRUE, include_vars = TRUE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    avalu_var = cs_to_des_select(avalu_var, dataname = dataname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    visit_var = cs_to_des_select(visit_var, dataname = dataname),
    base_var = cs_to_des_select(base_var, dataname = dataname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname)
  )

  module(
    label = label,
    server = srv_g_ipp,
    ui = ui_g_ipp,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        label = label,
        parentname = parentname,
        plot_height = plot_height,
        plot_width = plot_width,
        ggplot2_args = ggplot2_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}


ui_g_ipp <- function(id, ...) {
  a <- list(...) # module args
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$aval_var,
    a$avalu_var,
    a$id_var,
    a$visit_var,
    a$paramcd,
    a$base_var
  )

  ns <- shiny::NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(
        a[c("arm_var", "aval_var", "avalu_var", "id_var", "visit_var", "paramcd", "base_var")]
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Arm",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("visit_var"),
        label = "Timepoint Variable",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Parameter values over Time",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("id_var"),
        label = "Patient ID",
        data_extract_spec = a$id_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("avalu_var"),
        label = "Analysis Variable Unit",
        data_extract_spec = a$avalu_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("base_var"),
        label = "Baseline Parameter Values",
        data_extract_spec = a$base_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional plot settings",
          shiny::checkboxInput(
            ns("add_baseline_hline"),
            "Add reference lines at baseline value",
            value = a$add_baseline_hline
          ),
          shiny::checkboxInput(
            ns("separate_by_obs"),
            "Separate plots by ID",
            value = a$separate_by_obs
          ),
          shiny::checkboxInput(
            ns("suppress_legend"),
            "Suppress legend",
            value = a$suppress_legend
          ),
          shiny::checkboxInput(
            ns("add_avalu"),
            "Add unit value in title/y axis",
            value = a$add_avalu
          )
        )
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_ipp <- function(id,
                      data,
                      reporter,
                      filter_panel_api,
                      dataname,
                      parentname,
                      arm_var,
                      paramcd,
                      aval_var,
                      avalu_var,
                      id_var,
                      visit_var,
                      base_var,
                      plot_height,
                      plot_width,
                      label,
                      ggplot2_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    selector_list <- teal.transform::data_extract_multiple_srv(
      datasets = data,
      data_extract = list(
        arm_var = arm_var,
        aval_var = aval_var,
        avalu_var = avalu_var,
        id_var = id_var,
        paramcd = paramcd,
        visit_var = visit_var,
        base_var = base_var
      ),
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("A Parameter values over Time must be selected"),
        avalu_var = shinyvalidate::sv_required("An Analysis Variable Unit must be selected"),
        visit_var = shinyvalidate::sv_required("A Timepoint Variable must be selected"),
        id_var = shinyvalidate::sv_required("A Patient ID must be selected"),
        base_var = shinyvalidate::sv_required("Baseline Parameter Values must be selected")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required(message = "Please select Parameter filter."),
        arm_var = shinyvalidate::sv_required(message = "Please select Arm filter.")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join",
      join_keys = get_join_keys(data)
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      join_keys = get_join_keys(data),
      data_extract = list(arm_var = arm_var, id_var = id_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- shiny::reactive({
      q <- teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data))
      qenv <- teal.code::eval_code(q, as.expression(anl_inputs()$expr))
      teal.code::eval_code(qenv, as.expression(adsl_inputs()$expr))
    })

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())

      qenv <- anl_q()
      adsl_filtered <- qenv[[parentname]]
      anl_filtered <- qenv[[dataname]]

      anl_m <- anl_inputs()
      input_arm_var <- unlist(arm_var$filter)["vars_selected"]
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_avalu_var <- as.vector(anl_m$columns_source$avalu_var)
      input_id_var <- as.vector(anl_m$columns_source$id_var)
      input_visit_var <- as.vector(anl_m$columns_source$visit_var)
      input_base_var <- as.vector(anl_m$columns_source$base_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("STUDYID", input_id_var, input_arm_var),
        anl = anl_filtered,
        anlvars = c(
          "STUDYID",
          input_id_var,
          input_arm_var,
          input_aval_var,
          input_avalu_var,
          input_paramcd,
          input_visit_var,
          input_base_var
        ),
        arm_var = input_arm_var
      )

      do.call(what = "validate_standard_inputs", validate_args)
      NULL
    })

    # The R-code corresponding to the analysis.
    all_q <- shiny::reactive({
      validate_checks()
      qenv <- anl_q()
      anl_m <- anl_inputs()

      ANL <- qenv[["ANL"]] # nolint
      teal::validate_has_data(ANL, 2)

      arm_var <- unlist(arm_var$filter)["vars_selected"]
      avalu_var <- as.vector(anl_m$columns_source$avalu_var)
      paramcd <- unlist(paramcd$filter)["vars_selected"]

      avalu_first <- as.character(ANL[[avalu_var]][1])
      paramcd_first <- as.character(ANL[[paramcd]][1])
      arm_levels <- levels(droplevels(ANL[[arm_var]]))

      my_calls <- template_g_ipp(
        dataname = "ANL",
        aval_var = as.vector(anl_m$columns_source$aval_var),
        avalu_var = avalu_var,
        avalu_first = avalu_first,
        id_var = as.vector(anl_m$columns_source$id_var),
        visit_var = as.vector(anl_m$columns_source$visit_var),
        base_var = as.vector(anl_m$columns_source$base_var),
        add_baseline_hline = input$add_baseline_hline,
        separate_by_obs = input$separate_by_obs,
        suppress_legend = input$suppress_legend,
        paramcd = paramcd,
        paramcd_first = paramcd_first,
        arm_var = arm_var,
        arm_levels = arm_levels,
        ggplot2_args = ggplot2_args,
        add_avalu = input$add_avalu
      )
      teal.code::eval_code(qenv, as.expression(my_calls))
    })

    # Outputs to render.
    plot_r <- shiny::reactive(all_q()[["plot"]])

    # Insert the plot into a plot with settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
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
        card$set_name("Individual Patient Plot")
        card$append_text("Individual Patient Plot", "header2")
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
