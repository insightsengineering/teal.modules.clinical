#' Template: Survival Forest Plot
#'
#' Creates a valid expression for survival forest plot.
#'
#' @inheritParams template_arguments
#' @inheritParams template_forest_rsp
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.devel::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `labs` arguments such as: `title`, `caption`.
#' `theme` arguments will be not taken into account. The argument is merged with option `teal.ggplot2_args` and
#' with default module arguments (hard coded in the module body).\cr For more details, see the help vignette:\cr
#' `vignette("Custom ggplot2_args arguments module", package = "teal.devel")`.
#'
#'
#' @seealso [tm_g_forest_tte()]
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
                                ggplot2_args = teal.devel::ggplot2_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(arm_var),
    assertthat::is.string(obj_var_name),
    is.character(subgroup_var) || is.null(subgroup_var)
  )

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
    utils.nest::substitute_names(
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
    utils.nest::substitute_names(
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

  all_ggplot2_args <- teal.devel::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = teal.devel::ggplot2_args(
      labs = list(title = paste0("Forest plot of survival duration for ", obj_var_name), caption = "")
    )
  )

  plot_call <- substitute(
    expr = g_forest(
      tbl = result,
      col_symbol_size = col_s_size
    ),
    env = list(col_s_size = col_symbol_size)
  )

  plot_call <- substitute(
    decorate_grob(p, titles = title, footnotes = caption, gp_footnotes = grid::gpar(fontsize = 12)),
    env = list(title = all_ggplot2_args$labs$title, caption = all_ggplot2_args$labs$caption, p = plot_call)
  )

  plot_call <- substitute(
    env = list(plot_call = plot_call),
    expr = {
      p <- plot_call
      grid::grid.newpage()
      grid::grid.draw(p)
    }
  )

  # Plot output.
  y$plot <- plot_call

  y
}

#' Teal Module: Forest Survival Plot teal Module
#'
#' This teal module produces a grid style Forest plot for time-to-event data
#' with ADaM structure
#'
#' @inheritParams module_arguments
#' @inheritParams tm_g_forest_rsp
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.devel::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `labs` arguments such as: `title`, `caption`.
#' `theme` arguments will be not taken into account. The argument is merged with option `teal.ggplot2_args` and
#' with default module arguments (hard coded in the module body).\cr For more details, see the help vignette:\cr
#' `vignette("Custom ggplot2_args arguments module", package = "teal.devel")`.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#'
#' ADSL$RACE <- droplevels(ADSL$RACE)
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
#'     cdisc_dataset(
#'       "ADSL",
#'       ADSL,
#'       code = "ADSL <- synthetic_cdisc_data('latest')$adsl
#'         ADSL$RACE <- droplevels(ADSL$RACE)"
#'     ),
#'     cdisc_dataset("ADTTE", ADTTE, code = 'ADTTE <- synthetic_cdisc_data("latest")$adtte'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
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
#'       ),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_forest_tte <- function(label,
                            dataname,
                            parentname = ifelse(
                              inherits(arm_var, "data_extract_spec"),
                              teal.devel::datanames_input(arm_var),
                              "ADSL"
                            ),
                            arm_var,
                            arm_ref_comp = NULL,
                            subgroup_var,
                            paramcd,
                            strata_var,
                            aval_var = choices_selected(variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE),
                            cnsr_var = choices_selected(variable_choices(dataname, "CNSR"), "CNSR", fixed = TRUE),
                            conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                            time_unit_var = choices_selected(
                              variable_choices(dataname, "AVALU"), "AVALU",
                              fixed = TRUE
                            ),
                            fixed_symbol_size = TRUE,
                            plot_height = c(700L, 200L, 2000L),
                            plot_width = c(980L, 500L, 2000L),
                            pre_output = NULL,
                            post_output = NULL,
                            ggplot2_args = teal.devel::ggplot2_args()) {
  logger::log_info("Initializing tm_g_forest_tte")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  utils.nest::stop_if_not(
    is.choices_selected(conf_level),
    utils.nest::is_logical_single(fixed_symbol_size)
  )

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1], lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_outpput, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    subgroup_var = cs_to_des_select(subgroup_var, dataname = parentname, multiple = TRUE),
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
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}


ui_g_forest_tte <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.devel::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$subgroup_var,
    a$strata_var,
    a$aval_var,
    a$cnsr_var,
    a$time_unit_var
  )

  ns <- NS(id)

  teal.devel::standard_layout(
    output = teal.devel::plot_with_settings_ui(id = ns("myplot")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(a[c("arm_var", "paramcd", "subgroup_var", "strata_var", "aval_var", "cnsr_var")]),
      teal.devel::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        ns("ref_arm"),
        div(
          "Reference Group",
          title = paste(
            "Multiple reference groups are automatically combined into a single group when more than one",
            "value selected."
          ),
          icon("info-circle")
        ),
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      selectInput(
        ns("comp_arm"),
        div(
          "Comparison Group",
          title = paste(
            "Multiple comparison groups are automatically combined into a single group when more than one",
            "value selected."
          ),
          icon("info-circle")
        ),
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      teal.devel::data_extract_ui(
        id = ns("subgroup_var"),
        label = "Subgroup Variables",
        data_extract_spec = a$subgroup_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional plot settings",
          optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          checkboxInput(ns("fixed_symbol_size"), "Fixed symbol size", value = TRUE),
          teal.devel::data_extract_ui(
            id = ns("time_unit_var"),
            label = "Time Unit Variable",
            data_extract_spec = a$time_unit_var,
            is_single_dataset = is_single_dataset_value
          )
        )
      )
    ),
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_forest_tte <- function(input,
                             output,
                             session,
                             datasets,
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
  stopifnot(is_cdisc_data(datasets))
  teal.devel::init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  teal.devel::arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    dataname = parentname,
    arm_ref_comp = arm_ref_comp,
    module = "tm_g_forest_tte"
  )

  anl_selectors <- teal.devel::data_extract_multiple_srv(
    data_extract = list(
      arm_var = arm_var,
      paramcd = paramcd,
      subgroup_var = subgroup_var,
      strata_var = strata_var,
      aval_var = aval_var,
      cnsr_var = cnsr_var,
      time_unit_var = time_unit_var
    ),
    datasets = datasets
  )

  anl_merged <- teal.devel::data_merge_srv(
    selector_list = anl_selectors,
    datasets = datasets,
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var = arm_var, subgroup_var = subgroup_var, strata_var = strata_var),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
    input_subgroup_var <- anl_selectors()$subgroup_var()$select_ordered
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
    validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))

    if (length(input_subgroup_var) > 0) {
      validate(
        need(
          all(vapply(adsl_filtered[, input_subgroup_var], is.factor, logical(1))),
          "Not all subgroup variables are factors."
        )
      )
    }

    if (length(input_strata_var) > 0) {
      validate(
        need(
          all(vapply(adsl_filtered[, input_strata_var], is.factor, logical(1))),
          "Not all stratification variables are factors."
        )
      )
    }

    do.call(what = "validate_standard_inputs", validate_args)

    validate(need(
      input$conf_level >= 0 && input$conf_level <= 1,
      "Please choose a confidence level between 0 and 1"
    ))

    validate(need(
      !utils.nest::is_empty(anl_m$data()[[input_paramcd]]),
      "Value of the endpoint variable should not be empty."
    ))
    validate(need(utils.nest::is_character_single(input_aval_var), "Analysis variable should be a single column."))
    validate(need(utils.nest::is_character_single(input_cnsr_var), "Censor variable should be a single column."))

    NULL
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    validate_checks()

    teal.devel::chunks_reset()
    anl_m <- anl_merged()
    teal.devel::chunks_push_data_merge(anl_m)
    teal.devel::chunks_push_new_line()

    anl_adsl <- adsl_merged()
    teal.devel::chunks_push_data_merge(anl_adsl)
    teal.devel::chunks_push_new_line()

    ANL <- teal.devel::chunks_get_var("ANL") # nolint

    strata_var <- as.vector(anl_m$columns_source$strata_var)
    subgroup_var <- anl_selectors()$subgroup_var()$select_ordered

    obj_var_name <- get_g_forest_obj_var_name(paramcd, input)

    my_calls <- template_forest_tte(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      obj_var_name = obj_var_name,
      aval_var = as.vector(anl_m$columns_source$aval_var),
      cnsr_var = as.vector(anl_m$columns_source$cnsr_var),
      subgroup_var = if (length(subgroup_var) != 0) subgroup_var else NULL,
      strata_var = if (length(strata_var) != 0) strata_var else NULL,
      conf_level = as.numeric(input$conf_level),
      col_symbol_size = if (!input$fixed_symbol_size) 1,
      time_unit_var = as.vector(anl_m$columns_source$time_unit_var),
      ggplot2_args = ggplot2_args
    )
    mapply(expression = my_calls, teal.devel::chunks_push)
  })

  # Outputs to render.
  get_plot <- reactive({
    call_preparation()
    teal.devel::chunks_safe_eval()
    teal.devel::chunks_get_var("p")
  })

  callModule(
    teal.devel::plot_with_settings_srv,
    id = "myplot",
    plot_r = get_plot,
    height = plot_height,
    width = plot_width
  )

  callModule(
    module = teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(
      list(arm_var, paramcd, subgroup_var, strata_var, aval_var, cnsr_var)
    ),
    modal_title = "R Code for the Current Time-to-Event Forest Plot",
    code_header = "Time-to-Event Forest Plot"
  )
}
