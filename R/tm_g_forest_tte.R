#' Template: Survival Forest Plot
#'
#' Creates a valid expression for survival forest plot.
#'
#' @inheritParams template_arguments
#' @inheritParams template_forest_rsp
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `ggplot2_args` object with `labs` list of following child elements:
#' `title`, `caption`.
#' No other elements would be taken into account. The argument is merged with option `teal.ggplot2_args` and
#' with default module arguments (hard coded in the module body).
#'
#' For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @seealso [tm_g_forest_tte()]
#' @keywords internal
#'
template_forest_tte <- function(dataname = "ANL",
                                parentname = "ANL_ADSL",
                                arm_var,
                                ref_arm = NULL,
                                comp_arm = NULL,
                                obj_var_name = "",
                                aval_var = "AVAL",
                                cnsr_var = "CNSR",
                                subgroup_var,
                                strata_var = NULL,
                                conf_level = 0.95,
                                col_symbol_size = NULL,
                                time_unit_var = "AVALU",
                                rel_width_forest = 0.25,
                                font_size = 15,
                                ggplot2_args = teal.widgets::ggplot2_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(arm_var),
    assertthat::is.string(obj_var_name),
    is.character(subgroup_var) || is.null(subgroup_var)
  )
  checkmate::assert_number(rel_width_forest, lower = 0, upper = 1)
  checkmate::assert_number(font_size)

  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  # Data processing.
  data_list <- list()
  anl_list <- list()
  parent_list <- list()

  anl_list <- add_expr(
    anl_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  anl_list <- add_expr(
    anl_list,
    substitute_names(
      expr = {
        dplyr::mutate(arm_var = combine_levels(arm_var, comp_arm)) %>%
          dplyr::mutate(is_event = cnsr_var == 0)
      },
      names = list(arm_var = as.name(arm_var)),
      others = list(
        comp_arm = comp_arm,
        cnsr_var = as.name(cnsr_var)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- anl_list,
      env = list(
        anl_list = pipe_expr(anl_list)
      )
    )
  )

  parent_list <- add_expr(
    parent_list,
    prepare_arm(
      dataname = parentname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  parent_list <- add_expr(
    parent_list,
    substitute_names(
      expr = dplyr::mutate(arm_var = combine_levels(arm_var, comp_arm)),
      names = list(arm_var = as.name(arm_var)),
      others = list(
        ref_arm = ref_arm,
        comp_arm = comp_arm
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parent <- parent_list,
      env = list(
        parent_list = pipe_expr(parent_list)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Tabulate subgroup analysis of response.
  summary_list <- list()

  summary_list <- add_expr(
    summary_list,
    substitute(
      expr = df <- extract_survival_subgroups(
        variables = list(
          tte = aval_var,
          is_event = "is_event",
          arm = arm_var,
          subgroups = subgroup_var,
          strat = strata_var
        ),
        control = control_coxph(conf_level = conf_level),
        data = anl
      ),
      env = list(
        aval_var = aval_var,
        arm_var = arm_var,
        subgroup_var = subgroup_var,
        strata_var = strata_var,
        conf_level = conf_level
      )
    )
  )

  y$summary <- bracket_expr(summary_list)

  # Table output.
  y$table <- substitute(
    expr = {
      result <- rtables::basic_table() %>%
        tabulate_survival_subgroups(
          df,
          vars = c("n_tot", "n_tot_events", "n", "n_events", "median", "hr", "ci"),
          time_unit = as.character(anl$time_unit_var[1])
        )
    },
    env = list(time_unit_var = as.name(time_unit_var))
  )

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = teal.widgets::ggplot2_args(
      labs = list(
        title = paste(
          paste("Forest Plot of Survival Duration for", obj_var_name),
          ifelse(is.null(strata_var), "", paste("Stratified by", paste(strata_var, collapse = " and "))),
          sep = "\n"
        ),
        caption = ""
      )
    )
  )

  plot_list <- list()

  plot_list <- add_expr(
    plot_list,
    substitute(
      expr = {
        f <- g_forest(
          tbl = result,
          col_symbol_size = col_s_size,
          font_size = font_size,
          as_list = TRUE
        )
      },
      env = list(
        col_s_size = col_symbol_size,
        font_size = font_size
      )
    )
  )

  plot_list <- add_expr(
    plot_list,
    substitute(
      expr = {
        p <- cowplot::plot_grid(
          f[["table"]] + ggplot2::labs(title = ggplot2_args_title, subtitle = ggplot2_args_subtitle),
          f[["plot"]] + ggplot2::labs(caption = ggplot2_args_caption),
          align = "h",
          axis = "tblr",
          rel_widths = c(1 - rel_width_forest, rel_width_forest)
        )
      },
      env = list(
        rel_width_forest = rel_width_forest,
        ggplot2_args_title = all_ggplot2_args$labs$title,
        ggplot2_args_subtitle = all_ggplot2_args$labs$subtitle,
        ggplot2_args_caption = all_ggplot2_args$labs$caption
      )
    )
  )

  # Plot output.
  y$plot <- plot_list

  y
}

#' Teal Module: Forest Survival Plot teal Module
#'
#' This teal module produces a grid style Forest plot for time-to-event data
#' with `ADaM` structure
#'
#' @inheritParams module_arguments
#' @inheritParams tm_g_forest_rsp
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `ggplot2_args` object with `labs` list of following child elements:
#' `title`, `caption`.
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
#' ADSL <- tmc_ex_adsl
#' ADTTE <- tmc_ex_adtte
#' ADSL$RACE <- droplevels(ADSL$RACE) %>% formatters::with_label("Race")
#'
#' arm_ref_comp <- list(
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   ),
#'   ARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     ADSL = ADSL,
#'     ADTTE = ADTTE,
#'     code = "
#'       ADSL <- tmc_ex_adsl
#'       ADTTE <- tmc_ex_adtte
#'       ADSL$RACE <- droplevels(ADSL$RACE) %>% formatters::with_label(\"Race\")
#'     "
#'   ),
#'   modules = modules(
#'     tm_g_forest_tte(
#'       label = "Forest Survival",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD")),
#'         "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"),
#'         "OS"
#'       ),
#'       subgroup_var = choices_selected(
#'         variable_choices(ADSL, names(ADSL)),
#'         c("BMRKR2", "SEX")
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("STRATA1", "STRATA2")),
#'         "STRATA2"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_forest_tte <- function(label,
                            dataname,
                            parentname = ifelse(
                              inherits(arm_var, "data_extract_spec"),
                              teal.transform::datanames_input(arm_var),
                              "ADSL"
                            ),
                            arm_var,
                            arm_ref_comp = NULL,
                            subgroup_var,
                            paramcd,
                            strata_var,
                            aval_var = teal.transform::choices_selected(
                              teal.transform::variable_choices(dataname, "AVAL"), "AVAL",
                              fixed = TRUE
                            ),
                            cnsr_var = teal.transform::choices_selected(
                              teal.transform::variable_choices(dataname, "CNSR"), "CNSR",
                              fixed = TRUE
                            ),
                            conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                            time_unit_var = teal.transform::choices_selected(
                              teal.transform::variable_choices(dataname, "AVALU"), "AVALU",
                              fixed = TRUE
                            ),
                            fixed_symbol_size = TRUE,
                            plot_height = c(500L, 200L, 2000L),
                            plot_width = c(1500L, 800L, 3000L),
                            rel_width_forest = c(25L, 0L, 100L),
                            font_size = c(15L, 1L, 30L),
                            pre_output = NULL,
                            post_output = NULL,
                            ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_forest_tte")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_flag(fixed_symbol_size)
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
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    subgroup_var = cs_to_des_select(subgroup_var, dataname = parentname, multiple = TRUE, ordered = TRUE),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    time_unit_var = cs_to_des_select(time_unit_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_g_forest_tte,
    ui = ui_g_forest_tte,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        arm_ref_comp = arm_ref_comp,
        parentname = parentname,
        plot_height = plot_height,
        plot_width = plot_width,
        ggplot2_args = ggplot2_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}


ui_g_forest_tte <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$subgroup_var,
    a$strata_var,
    a$aval_var,
    a$cnsr_var,
    a$time_unit_var
  )

  ns <- shiny::NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "subgroup_var", "strata_var", "aval_var", "cnsr_var")]),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::uiOutput(
        ns("arms_buckets"),
        title = paste(
          "Multiple reference groups are automatically combined into a single group when more than one",
          "value is selected."
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("subgroup_var"),
        label = "Subgroup Variables",
        data_extract_spec = a$subgroup_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional plot settings",
          teal.widgets::optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          shiny::checkboxInput(ns("fixed_symbol_size"), "Fixed symbol size", value = TRUE),
          teal.transform::data_extract_ui(
            id = ns("time_unit_var"),
            label = "Time Unit Variable",
            data_extract_spec = a$time_unit_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("rel_width_forest"),
            "Relative Width of Forest Plot (%)",
            a$rel_width_forest,
            ticks = FALSE, step = 1
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"),
            "Table Font Size",
            a$font_size,
            ticks = FALSE, step = 1
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

srv_g_forest_tte <- function(id,
                             data,
                             reporter,
                             filter_panel_api,
                             dataname,
                             parentname,
                             arm_var,
                             arm_ref_comp,
                             paramcd,
                             subgroup_var,
                             strata_var,
                             aval_var,
                             cnsr_var,
                             time_unit_var,
                             plot_height,
                             plot_width,
                             ggplot2_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  shiny::moduleServer(id, function(input, output, session) {
    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data()[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_g_forest_tte"
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        subgroup_var = subgroup_var,
        strata_var = strata_var,
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        time_unit_var = time_unit_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        cnsr_var = shinyvalidate::sv_required("A censor variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required")
      ),
      filter_validation_rule = list(paramcd = shinyvalidate::sv_required(message = "Please select Endpoint filter."))
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(0, 1, message_fmt = "Confidence level must be between 0 and 1")
      )
      iv$add_validator(iv_arm_ref)
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var, subgroup_var = subgroup_var, strata_var = strata_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- shiny::reactive({
      data() %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      anl_m <- anl_inputs()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
      input_subgroup_var <- as.vector(anl_m$columns_source$subgroup_var)
      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_time_unit_var <- as.vector(anl_m$columns_source$time_unit_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_subgroup_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var, input_time_unit_var),
        arm_var = input_arm_var
      )

      # validate arm levels
      if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }
      validate_args <- append(
        validate_args, list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
      )

      if (length(input_subgroup_var) > 0) {
        shiny::validate(
          shiny::need(
            all(vapply(adsl_filtered[, input_subgroup_var], is.factor, logical(1))),
            "Not all subgroup variables are factors."
          )
        )
      }

      if (length(input_strata_var) > 0) {
        shiny::validate(
          shiny::need(
            all(vapply(adsl_filtered[, input_strata_var], is.factor, logical(1))),
            "Not all stratification variables are factors."
          )
        )
      }

      do.call(what = "validate_standard_inputs", validate_args)

      shiny::validate(shiny::need(
        length(anl[[input_paramcd]]) > 0,
        "Value of the endpoint variable should not be empty."
      ))

      NULL
    })

    # The R-code corresponding to the analysis.
    all_q <- shiny::reactive({
      validate_checks()

      anl_m <- anl_inputs()

      strata_var <- as.vector(anl_m$columns_source$strata_var)
      subgroup_var <- as.vector(anl_m$columns_source$subgroup_var)

      obj_var_name <- get_g_forest_obj_var_name(paramcd, input)

      my_calls <- template_forest_tte(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        obj_var_name = obj_var_name,
        aval_var = as.vector(anl_m$columns_source$aval_var),
        cnsr_var = as.vector(anl_m$columns_source$cnsr_var),
        subgroup_var = if (length(subgroup_var) != 0) subgroup_var else NULL,
        strata_var = if (length(strata_var) != 0) strata_var else NULL,
        conf_level = as.numeric(input$conf_level),
        col_symbol_size = if (!input$fixed_symbol_size) 1,
        time_unit_var = as.vector(anl_m$columns_source$time_unit_var),
        rel_width_forest = input$rel_width_forest / 100,
        font_size = input$font_size,
        ggplot2_args = ggplot2_args
      )
      teal.code::eval_code(anl_q(), as.expression(my_calls))
    })

    # Outputs to render.
    plot_r <- shiny::reactive(all_q()[["p"]])

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
      title = "R Code for the Current Time-to-Event Forest Plot"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Forest Survival Plot",
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
        card$append_src(teal.code::get_code(all_q()))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
