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
#' @inheritParams tern::control_coxreg
#' @param facet_var ([choices_selected()])\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used for facet plotting.
#'
NULL

#' @describeIn kaplan_meier create the expression corresponding to the analysis.
#' @order 2
#'
template_g_km <- function(dataname = "ANL",
                          arm_var = "ARM",
                          ref_arm = NULL,
                          comp_arm = NULL,
                          compare_arm = FALSE,
                          combine_comp_arms = FALSE,
                          aval_var = "AVAL",
                          cnsr_var = "CNSR",
                          strata_var = NULL,
                          time_points = NULL,
                          facet_var = "SEX",
                          font_size = 8,
                          conf_level = 0.95,
                          ties = "efron",
                          xlab = "Survival time in Days",
                          pval_method = "log-rank",
                          annot_surv_med = TRUE,
                          annot_coxph = TRUE) {
  assert_that(
    is.string(dataname),
    is.string(arm_var),
    is.string(aval_var),
    is.string(cnsr_var),
    is.flag(compare_arm),
    is.flag(combine_comp_arms)
  )

  ref_arm_val <- paste(ref_arm, collapse = "/")
  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      compare_arm = compare_arm,
      ref_arm_val = ref_arm_val
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = mutate(
        is_event = cnsr_var == 0
      ),
      env = list(
        anl = as.name(dataname),
        cnsr_var = as.name(cnsr_var)
      )
    )
  )

  if (compare_arm && combine_comp_arms) {
    comp_arm_val <- paste(comp_arm, collapse = "/")
    data_list <- add_expr(
      data_list,
      substitute_names(
        expr = mutate(arm_var = combine_levels(arm_var, levels = comp_arm, new_level = comp_arm_val)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm, comp_arm_val = comp_arm_val)
      )
    )
  }

  y$data <- substitute(
    expr = {
      anl <- data_pipe
    },
    env = list(
      data_pipe = pipe_expr(data_list)
    )
  )

  y$variables <- if (!is.null(strata_var) && length(strata_var) != 0) {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm, strat = strata_var),
      env = list(tte = aval_var, arm = arm_var, strata_var = strata_var)
    )
  } else {
    substitute(
      expr = variables <- list(tte = tte, is_event = "is_event", arm = arm),
      env = list(tte = aval_var, arm = arm_var)
    )
  }
  graph_list <- list()

  if (!is.null(facet_var) && length(facet_var) != 0) {
    graph_list <- add_expr(
      graph_list,
      quote(grid::grid.newpage())
    )
    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = lyt <- grid::grid.layout(nrow = nlevels(df$facet_var), ncol = 1) %>%
          grid::viewport(layout = .) %>%
          grid::pushViewport(),
        env = list(
          df = as.name(dataname),
          facet_var = as.name(facet_var)
        )
      )
    )

    graph_list <- add_expr(
      graph_list,
      substitute(
        expr = result <- mapply(
          df = split(anl, f = anl$facet_var), nrow = seq_along(levels(anl$facet_var)),
          FUN = function(df_i, nrow_i) {
            g_km(
              df = df_i,
              variables = variables,
              font_size = font_size,
              xlab = xlab,
              newpage = FALSE,
              title = paste("KM Plot", quote(facet_var), "=", as.character(unique(df_i$facet_var))),
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
          font_size = font_size,
          facet_var = as.name(facet_var),
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
        expr = {
          result <- g_km(
            df = anl,
            variables = variables,
            font_size = font_size,
            xlab = xlab,
            newpage = TRUE,
            ggtheme = theme_minimal(),
            control_surv = control_surv_timepoint(conf_level = conf_level),
            control_coxph_pw = control_coxph(conf_level = conf_level, pval_method = pval_method, ties = ties),
            annot_surv_med = annot_surv_med,
            annot_coxph = annot_coxph
          )
        print(result)
        },
        env = list(
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
                    parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                    arm_var,
                    arm_ref_comp = NULL,
                    paramcd,
                    strata_var,
                    facet_var,
                    aval_var = choices_selected(variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE),
                    cnsr_var = choices_selected(variable_choices(dataname, "CNSR"), "CNSR", fixed = TRUE),
                    conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                    plot_height = c(1200L, 400L, 5000L),
                    plot_width = NULL,
                    pre_output = NULL,
                    post_output = NULL) {
  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is.choices_selected(conf_level),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
      ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
      )
    )

  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())
  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    facet_var = cs_to_des_select(facet_var, dataname = parentname, multiple = TRUE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_g_km,
    ui = ui_g_km,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        label = label,
        parentname = parentname,
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
    a$arm_var,
    a$paramcd,
    a$strata_var,
    a$facet_var,
    a$aval_var,
    a$cnsr_var
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
      datanames_input(a[c("arm_var", "paramcd", "strata_var", "facet_var", "aval_var", "cnsr_var")]),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("facet_var"),
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
                     parentname,
                     paramcd,
                     arm_var,
                     arm_ref_comp,
                     strata_var,
                     facet_var,
                     aval_var,
                     cnsr_var,
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
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte",
    on_off = reactive(input$compare_arms)
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(aval_var, cnsr_var, arm_var, paramcd, strata_var, facet_var),
    input_id = c("aval_var", "cnsr_var", "arm_var", "paramcd", "strata_var", "facet_var"),
    merge_function = "dplyr::inner_join"
  )

  validate_checks <- reactive({

    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)
    input_facet_var <- as.vector(anl_m$columns_source$facet_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
    input_paramcd <- unlist(paramcd$filter)["vars"]

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var, input_facet_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var),
      arm_var = input_arm_var
    )

    # validate arm levels
    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
    }
    if (input$compare_arms) {
      validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
    }

    do.call(what = "validate_standard_inputs", validate_args)

    validate(need(
      input$conf_level > 0 && input$conf_level < 1,
      "Please choose a confidence level between 0 and 1"
    ))

    validate(need(is_character_single(input_aval_var), "Analysis variable should be a single column."))
    validate(need(is_character_single(input_cnsr_var), "Censor variable should be a single column."))

    NULL
  })

  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 10)

    my_calls <- template_g_km(
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      compare_arm = input$compare_arms,
      combine_comp_arms = input$combine_comp_arms,
      aval_var = as.vector(anl_m$columns_source$aval_var),
      cnsr_var = as.vector(anl_m$columns_source$cnsr_var),
      strata_var = as.vector(anl_m$columns_source$strata_var),
      time_points = NULL,
      facet_var = as.vector(anl_m$columns_source$facet_var),
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
    datanames = get_extract_datanames(
      list(arm_var, paramcd, strata_var, facet_var, aval_var, cnsr_var)
      ),
    modal_title = label
  )
}
