#' Teal Module: Kaplan-Meier
#'
#' This teal module produces a grid style Kaplan-Meier plot for data with
#' ADaM structure.
#'
#' @name kaplan_meier
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @inheritParams tern::g_km
#' @inheritParams tm_t_tte
#' @inheritParams shared_params
#' @param facet_var ([choices_selected()])\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used for facet plotting.
#' @param conf_level ([choices_selected()])\cr
#'   object with all available choices and preselected option
#'   for confidence level, each within range of (0, 1).
#'
NULL

#' @describeIn kaplan_meier create the expression corresponding to the analysis.
#' @order 2
#'
template_g_km <- function(anl_name = "ANL",
                          arm_var = "ARM",
                          arm_ref_comp = NULL,
                          comp_arm = NULL,
                          compare_arm = FALSE,
                          combine_comp_arms = FALSE,
                          aval = "AVAL",
                          cnsr = "CNSR",
                          strata_var = NULL,
                          time_points = NULL,
                          facet = "SEX",
                          font_size = 8,
                          conf_level = 0.95,
                          ties = "efron",
                          xlab = "Survival time in Days",
                          pval_method = "log-rank",
                          annot_surv_med = TRUE,
                          annot_coxph = TRUE) {

  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl %>%
        mutate(
          is_event = cnsr == 0
        ),
      env = list(
        anl = as.name(anl_name),
        cnsr = as.name(cnsr)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = filter(arm_var %in% c(arm_ref_comp, comp_arm)),
      env = list(arm_var = as.name(arm_var), arm_ref_comp = arm_ref_comp, comp_arm = comp_arm)
    )
  )
  if (combine_comp_arms) {
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = mutate(
          arm = factor(
            x = case_when(
              arm %in% arm_ref_comp ~ new_arm_ref_comp,
              TRUE ~ new_comp_arm
            ),
            levels = c(new_arm_ref_comp, new_comp_arm)
          )
        ),
        names = list(arm = as.name(arm_var)),
        others = list(
          arm_ref_comp = arm_ref_comp,
          new_arm_ref_comp = paste(arm_ref_comp, collapse = "/"),
          comp_arm = comp_arm,
          new_comp_arm = paste(comp_arm, collapse = "/")
        )
      )
    )
  }

  y$data <- substitute(
    expr = anl <- data_pipe, # nolint
    env = list(anl = as.name(anl_name), data_pipe = pipe_expr(data_list))
  )

  y$variables <- if (!is.null(strata_var) && length(strata_var) != 0) {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm, strat = strata_var),
      env = list(tte = aval, arm = arm_var, strata_var = strata_var)
    )
  } else {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm),
      env = list(tte = aval, arm = arm_var)
    )
  }
  graph_list <- list()

  if (!is.null(facet) && length(facet) != 0) {
    graph_list <- add_expr(
      graph_list,
      quote(grid::grid.newpage())
    )
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = lyt <- grid::grid.layout(nrow = nlevels(df$facet), ncol = 1) %>%
          grid::viewport(layout = .) %>%
          grid::pushViewport(),
        env = list(
          df = as.name(anl_name),
          facet = as.name(facet)
        )
      )
    )

    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = result <- mapply(
          df = split(df, f = df$facet), nrow = seq_along(levels(df$facet)),
          FUN = function(df_i, nrow_i) {
            g_km(
              df = df_i,
              variables = variables,
              font_size = font_size,
              xlab = xlab,
              newpage = FALSE,
              ggtheme = theme_minimal(),
              annot_surv_med = annot_surv_med,
              annot_coxph = annot_coxph,
              control_surv = control_surv_timepoint(conf_level = conf_level),
              control_coxph_pw = control_coxph(conf_level = conf_level, pval_method = pval_method, ties = ties),
              vp = grid::viewport(layout.pos.row = nrow_i, layout.pos.col = 1)
            )
          }
        ),
        env = list(
          df = as.name(anl_name),
          font_size = font_size,
          facet = as.name(facet),
          xlab = xlab,
          conf_level = conf_level,
          pval_method = pval_method,
          annot_surv_med = annot_surv_med,
          annot_coxph = annot_coxph,
          ties = ties
        )
      )
    )
  } else {
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = result <- g_km(
          df = df,
          variables = variables,
          font_size = font_size,
          xlab = xlab,
          newpage = TRUE,
          ggtheme = theme_minimal(),
          control_surv = control_surv_timepoint(conf_level = conf_level),
          control_coxph_pw = control_coxph(conf_level = conf_level, pval_method = pval_method, ties = ties),
          annot_surv_med = annot_surv_med,
          annot_coxph = annot_coxph
        ),
        env = list(
          df = as.name(anl_name),
          font_size = font_size,
          xlab = xlab,
          conf_level = conf_level,
          pval_method = pval_method,
          annot_surv_med = annot_surv_med,
          annot_coxph = annot_coxph,
          ties = ties
        )
      )
    )
  }

  y$graph <- bracket_expr(graph_list)

  y
}


#' @describeIn kaplan_meier teal module for Kaplan-Meier curves.
#' @export
#' @order 1
#' @examples
#'
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' arm_ref_comp = list(
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   ),
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADTTE", ADTTE, code = "ADTTE <- radtte(cached = TRUE)"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_km(
#'       label = "KM PLOT",
#'       dataname = "ADTTE",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")),
#'         "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"),
#'         "OS"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("SEX", "BMRKR2")),
#'         "SEX"
#'       ),
#'       facet_var = choices_selected(
#'         variable_choices(ADSL, c("SEX", "BMRKR2")),
#'         NULL
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_km <- function(label,
                    dataname,
                    parent_name = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                    arm_var,
                    arm_ref_comp = NULL,
                    paramcd,
                    strata_var,
                    facet_var,
                    aval_var = choices_selected(variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE),
                    cnsr_var = choices_selected(variable_choices(dataname, "CNSR"), "CNSR", fixed = TRUE),
                    conf_level = choices_selected(c(0.8, 0.85, 0.90, 0.95, 0.99, 0.995), 0.95),
                    plot_height = c(1200L, 400L, 5000L),
                    plot_width = NULL,
                    pre_output = NULL,
                    post_output = NULL) {

  stopifnot(
    is.cs_or_des(arm_var),
    is.cs_or_des(paramcd),
    is.cs_or_des(facet_var),
    is.cs_or_des(strata_var)
  )

  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  # Convert choices-selected to data_extract_spec
  if (is.choices_selected(arm_var)) {
    arm_var <- cs_to_des_select(arm_var, dataname = parent_name, multiple = FALSE)
  }
  if (is.choices_selected(paramcd)) {
    paramcd <- cs_to_des_filter(paramcd, dataname = dataname, multiple = FALSE)
  }
  if (is.choices_selected(strata_var)) {
    strata_var <- cs_to_des_select(strata_var, dataname = parent_name, multiple = TRUE)
  }
  if (is.choices_selected(facet_var)) {
    facet_var <- cs_to_des_select(facet_var, dataname = parent_name, multiple = TRUE)
  }
  if (is.choices_selected(aval_var)) {
    aval_var <- cs_to_des_select(aval_var, dataname = dataname, multiple = FALSE)
  }
  if (is.choices_selected(cnsr_var)) {
    cnsr_var <- cs_to_des_select(cnsr_var, dataname = dataname, multiple = FALSE)
  }

  args <- as.list(environment())
  data_extract_list <- list(
    arm = arm_var,
    paramcd = paramcd,
    strata = strata_var,
    facet = facet_var,
    aval = aval_var,
    cnsr = cnsr_var
  )

  module(
    label = label,
    server = srv_g_km,
    ui = ui_g_km,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        label = label,
        parent_name = parent_name,
        arm_ref_comp = arm_ref_comp,
        plot_height = plot_height,
        plot_width = plot_width
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}


#' User Interface for KM Module
#' @noRd
#'
ui_g_km <- function(id, ...) {

  a <- list(...)
  is_single_dataset_value <- is_single_dataset(
    a$arm_var, a$param_cd, a$strata_var, a$facet_var,
    a$aval_var, a$cnsr_var
  )

  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      verbatimTextOutput(outputId = ns("text")),
      plot_with_settings_ui(
        id = ns("myplot"), height = a$plot_height, width = a$plot_width
      )
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(
        a[
          c(
            "arm_var", "paramcd", "strata_var", "facet_var",
            "aval_var", "cnsr_var"
          )
          ]
      ),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("cnsr"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("facet"),
        label = "Facet Plots by",
        data_extract_spec = a$facet_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      div(
        class = "arm-comp-box",
        tags$label("Compare Arms"),
        shinyWidgets::switchInput(
          inputId = ns("compare_arms"),
          value = !is.null(a$arm_ref_comp),
          size = "mini"
        ),
        conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          div(
            selectInput(
              ns("ref_arm"),
              "Reference Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            helpText("Multiple reference groups are automatically combined into a single group."),
            selectInput(
              ns("comp_arm"),
              "Comparison Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            ),
            data_extract_input(
              id = ns("strata_var"),
              label = "Stratify by",
              data_extract_spec = a$strata_var,
              is_single_dataset = is_single_dataset_value
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        panel_group(
          panel_item(
            "Comparison settings",
            radioButtons(
              ns("pval_method_coxph"),
              label = HTML(
                paste(
                  "p-value method for ",
                  tags$span(style = "color:darkblue", "Coxph"), # nolint
                  " (Hazard Ratio)",
                  sep = ""
                )
              ),
              choices = c("wald", "log-rank", "likelihood"),
              selected = "log-rank"
            ),
            radioButtons(
              ns("ties_coxph"),
              label = HTML(
                paste(
                  "Ties for ",
                  tags$span(style = "color:darkblue", "Coxph"), # nolint
                  " (Hazard Ratio)",
                  sep = ""
                )
              ),
              choices = c("exact", "breslow", "efron"),
              selected = "exact"
            )
          )
        )
      ),
      panel_group(
        panel_item(
          "Additional plot settings",
          numericInput(
            inputId = ns("font_size"),
            label = "Font size",
            value = 8,
            min = 5,
            max = 15,
            step = 1,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns("show_km_table"),
            label = "Show KM table",
            value = TRUE,
            width = "100%"
          ),
          optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          textInput(ns("xlab"), "X-axis label", "Overall survival in Days")
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


#' Server for KM Module
#' @noRd
#'
srv_g_km <- function(input,
                     output,
                     session,
                     datasets,
                     dataname,
                     parent_name,
                     paramcd,
                     arm,
                     arm_ref_comp,
                     strata,
                     facet,
                     aval,
                     cnsr,
                     label,
                     plot_height,
                     plot_width) {

  init_chunks()

  # Setup arm variable selection, default reference arms and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", # from UI
    id_comp = "comp_arm", # from UI
    id_arm_var = paste0("arm_var-dataset_", parent_name, "_singleextract-select"), # from UI
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte",
    on_off = reactive(input$compare_arms)
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(aval, cnsr, arm, paramcd, strata, facet),
    input_id = c("aval", "cnsr", "arm_var", "paramcd", "strata_var", "facet"),
    merge_function = "dplyr::inner_join"
  )

  validate_checks <- reactive({

    adsl_filtered <- datasets$get_data(parent_name, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata)
    input_facet_var <- as.vector(anl_m$columns_source$facet)
    input_aval_var <- as.vector(anl_m$columns_source$aval)
    input_cnsr_var <- as.vector(anl_m$columns_source$cnsr)

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var, input_facet_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", input_aval_var, input_cnsr_var),
      arm_var = input_arm_var
    )

    # validate arm levels
    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      if (input$compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = input$ref_arm))
      }
    } else {
      if (input$compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
      }
    }

    do.call(what = "validate_standard_inputs", validate_args)

    NULL
  })

  call_preparation <- reactive({
    validate_checks()
    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 10)
    my_calls <- template_g_km(
      anl_name = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      arm_ref_comp = input$ref_arm,
      comp_arm = input$comp_arm,
      compare_arm = input$compare_arms,
      combine_comp_arms = input$combine_comp_arms,
      aval = as.vector(anl_m$columns_source$aval),
      cnsr = as.vector(anl_m$columns_source$cnsr),
      strata_var = as.vector(anl_m$columns_source$strata),
      time_points = NULL,
      facet = as.vector(anl_m$columns_source$facet),
      annot_surv_med = input$show_km_table,
      annot_coxph = input$compare_arms,
      font_size = input$font_size,
      pval_method = input$pval_method_coxph,
      conf_level = as.numeric(input$conf_level),
      ties = input$ties_coxph,
      xlab = input$xlab
    )
    mapply(expression = my_calls, chunks_push)
  })

  km_plot <- reactive({
    call_preparation()
    chunks_safe_eval()
  })

  # Insert the plot into a plot with settings module from teal.devel
  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = km_plot,
    height = plot_height,
    width = plot_width
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(arm, paramcd, strata)),
    modal_title = label
  )

}
