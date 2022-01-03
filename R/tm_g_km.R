#' Template: Kaplan-Meier
#'
#' @inheritParams template_arguments
#' @inheritParams tern::g_km
#' @inheritParams tern::control_coxreg
#' @param facet_var (`character`)\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used for facet plotting.
#'
#' @seealso [tm_g_km()]
template_g_km <- function(dataname = "ANL",
                          arm_var = "ARM",
                          ref_arm = NULL,
                          comp_arm = NULL,
                          compare_arm = FALSE,
                          combine_comp_arms = FALSE,
                          aval_var = "AVAL",
                          cnsr_var = "CNSR",
                          xticks = NULL,
                          strata_var = NULL,
                          time_points = NULL,
                          facet_var = "SEX",
                          font_size = 8,
                          conf_level = 0.95,
                          ties = "efron",
                          xlab = "Survival time",
                          time_unit_var = "AVALU",
                          yval = "Survival",
                          pval_method = "log-rank",
                          annot_surv_med = TRUE,
                          annot_coxph = TRUE,
                          ci_ribbon = FALSE,
                          title = "KM Plot") {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(arm_var),
    assertthat::is.string(aval_var),
    assertthat::is.string(cnsr_var),
    assertthat::is.string(time_unit_var),
    assertthat::is.flag(compare_arm),
    assertthat::is.flag(combine_comp_arms),
    is.null(xticks) | is.numeric(xticks),
    assertthat::is.string(title)
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
      expr = dplyr::mutate(
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
      utils.nest::substitute_names(
        expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm, new_level = comp_arm_val)),
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
        expr = {
          result <- mapply(
            df = split(anl, f = anl$facet_var), nrow = seq_along(levels(anl$facet_var)),
            FUN = function(df_i, nrow_i) {
              if (nrow(df_i) == 0) {
                grid::grid.text(
                  "No data found for a given facet value.",
                  x = 0.5,
                  y = 0.5,
                  vp = grid::viewport(layout.pos.row = nrow_i, layout.pos.col = 1)
                )
              } else {
                g_km(
                  df = df_i,
                  variables = variables,
                  font_size = font_size,
                  xlab = paste0(
                    xlab,
                    " (",
                    gsub(
                      "(^|[[:space:]])([[:alpha:]])",
                      "\\1\\U\\2",
                      tolower(anl$time_unit_var[1]),
                      perl = TRUE
                    ),
                    ")"
                  ),
                  yval = yval,
                  xticks = xticks,
                  newpage = FALSE,
                  title = paste(
                    title, ",", quote(facet_var),
                    "=", as.character(unique(df_i$facet_var))
                  ),
                  ggtheme = theme_minimal(),
                  annot_surv_med = annot_surv_med,
                  annot_coxph = annot_coxph,
                  control_surv = control_surv_timepoint(conf_level = conf_level),
                  control_coxph_pw = control_coxph(conf_level = conf_level, pval_method = pval_method, ties = ties),
                  ci_ribbon = ci_ribbon,
                  vp = grid::viewport(layout.pos.row = nrow_i, layout.pos.col = 1),
                  draw = TRUE
                )
              }
            },
            SIMPLIFY = FALSE
          )
          km_grobs <- tern::stack_grobs(grobs = result)
          km_grobs
        },
        env = list(
          font_size = font_size,
          facet_var = as.name(facet_var),
          xticks = xticks,
          xlab = xlab,
          time_unit_var = as.name(time_unit_var),
          yval = yval,
          conf_level = conf_level,
          pval_method = pval_method,
          annot_surv_med = annot_surv_med,
          annot_coxph = annot_coxph,
          ties = ties,
          ci_ribbon = ci_ribbon,
          title = title
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
            xlab = paste0(
              xlab,
              " (",
              gsub(
                "(^|[[:space:]])([[:alpha:]])",
                "\\1\\U\\2",
                tolower(anl$time_unit_var[1]),
                perl = TRUE
              ),
              ")"
            ),
            yval = yval,
            xticks = xticks,
            newpage = TRUE,
            ggtheme = theme_minimal(),
            control_surv = control_surv_timepoint(conf_level = conf_level),
            control_coxph_pw = control_coxph(conf_level = conf_level, pval_method = pval_method, ties = ties),
            annot_surv_med = annot_surv_med,
            annot_coxph = annot_coxph,
            ci_ribbon = ci_ribbon,
            title = title,
          )
          result
        },
        env = list(
          font_size = font_size,
          xticks = xticks,
          xlab = xlab,
          time_unit_var = as.name(time_unit_var),
          yval = yval,
          conf_level = conf_level,
          pval_method = pval_method,
          annot_surv_med = annot_surv_med,
          annot_coxph = annot_coxph,
          ties = ties,
          ci_ribbon = ci_ribbon,
          title = title
        )
      )
    )
  }

  y$graph <- bracket_expr(graph_list)

  y
}


#' Teal Module: Kaplan-Meier
#'
#' This teal module produces a grid style Kaplan-Meier plot for data with
#' ADaM structure.
#'
#' @inheritParams module_arguments
#' @param facet_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all available choices and preselected option
#'   for variable names that can be used for facet plotting.
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#'
#' arm_ref_comp <- list(
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
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADTTE", ADTTE, code = 'ADTTE <- synthetic_cdisc_data("latest")$adtte'),
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
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_g_km <- function(label,
                    dataname,
                    parentname = ifelse(
                      inherits(arm_var, "data_extract_spec"),
                      teal.devel::datanames_input(arm_var),
                      "ADSL"
                    ),
                    arm_var,
                    arm_ref_comp = NULL,
                    paramcd,
                    strata_var,
                    facet_var,
                    time_unit_var = choices_selected(variable_choices(dataname, "AVALU"), "AVALU", fixed = TRUE),
                    aval_var = choices_selected(variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE),
                    cnsr_var = choices_selected(variable_choices(dataname, "CNSR"), "CNSR", fixed = TRUE),
                    conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                    plot_height = c(1200L, 400L, 5000L),
                    plot_width = NULL,
                    pre_output = NULL,
                    post_output = NULL) {
  logger::log_info("Initializing tm_g_km")

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  utils.nest::stop_if_not(
    is.choices_selected(conf_level),
    list(
      is.null(pre_output) || inherits(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || inherits(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(plot_width[1],
    lower = plot_width[2],
    upper = plot_width[3],
    null.ok = TRUE,
    .var.name = "plot_width"
  )

  args <- as.list(environment())
  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    facet_var = cs_to_des_select(facet_var, dataname = parentname, multiple = FALSE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    time_unit_var = cs_to_des_select(time_unit_var, dataname = dataname)
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
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}


#' User Interface for KM Module
#' @noRd
ui_g_km <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.devel::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$strata_var,
    a$facet_var,
    a$aval_var,
    a$cnsr_var,
    a$time_unit_var
  )

  ns <- NS(id)

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(
      verbatimTextOutput(outputId = ns("text")),
      teal.devel::plot_with_settings_ui(
        id = ns("myplot")
      )
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(a[c("arm_var", "paramcd", "strata_var", "facet_var", "aval_var", "cnsr_var")]),
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
        id = ns("facet_var"),
        label = "Facet Plots by",
        data_extract_spec = a$facet_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      div(
        class = "arm-comp-box",
        tags$label("Compare Treatments"),
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
            teal.devel::data_extract_ui(
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
        teal.devel::panel_group(
          teal.devel::panel_item(
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
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional plot settings",
          textInput(
            inputId = ns("xticks"),
            label = "Specify break intervals for x-axis e.g. 0 ; 500"
          ),
          radioButtons(
            ns("yval"),
            tags$label("Value on y-axis", class = "text-primary"),
            choices = c("Survival probability", "Failure probability"),
            selected = c("Survival probability"),
          ),
          numericInput(
            inputId = ns("font_size"),
            label = "Plot tables font size",
            value = 8,
            min = 5,
            max = 15,
            step = 1,
            width = "100%"
          ),
          checkboxInput(
            inputId = ns("show_ci_ribbon"),
            label = "Show CI ribbon",
            value = FALSE,
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
          textInput(ns("xlab"), "X-axis label", "Time"),
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
                     time_unit_var,
                     plot_height,
                     plot_width) {
  stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

  # Setup arm variable selection, default reference arms and default
  # comparison arms for encoding panel
  teal.devel::arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", # from UI
    id_comp = "comp_arm", # from UI
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    dataname = parentname,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte",
    on_off = reactive(input$compare_arms)
  )

  anl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(
      aval_var = aval_var,
      cnsr_var = cnsr_var,
      arm_var = arm_var,
      paramcd = paramcd,
      strata_var = strata_var,
      facet_var = facet_var,
      time_unit_var = time_unit_var
    ),
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
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]
    input_time_unit_var <- as.vector(anl_m$columns_source$time_unit_var)
    input_xticks <- gsub(";", ",", trimws(input$xticks)) %>%
      strsplit(",") %>%
      unlist() %>%
      as.numeric()

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var, input_facet_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_cnsr_var, input_time_unit_var),
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

    # validate xticks
    if (length(input_xticks) == 0) {
      input_xticks <- NULL
    } else {
      validate(need(all(!is.na(input_xticks)), "Not all values entered were numeric"))
      validate(need(all(input_xticks >= 0), "All break intervals for x-axis must be non-negative"))
      validate(need(any(input_xticks > 0), "At least one break interval for x-axis must be positive"))
    }

    validate(need(
      input$conf_level > 0 && input$conf_level < 1,
      "Please choose a confidence level between 0 and 1"
    ))

    validate(need(utils.nest::is_character_single(input_aval_var), "Analysis variable should be a single column."))
    validate(need(utils.nest::is_character_single(input_cnsr_var), "Censor variable should be a single column."))

    # validate font size
    validate(need(input$font_size >= 5, "Plot tables font size must be greater than or equal to 5."))

    NULL
  })

  call_preparation <- reactive({
    validate_checks()

    teal.devel::chunks_reset()
    anl_m <- anl_merged()
    teal.devel::chunks_push_data_merge(anl_m)
    teal.devel::chunks_push_new_line()

    ANL <- teal.devel::chunks_get_var("ANL") # nolint
    teal.devel::validate_has_data(ANL, 2)

    input_xticks <- gsub(";", ",", trimws(input$xticks)) %>%
      strsplit(",") %>%
      unlist() %>%
      as.numeric()

    if (length(input_xticks) == 0) {
      input_xticks <- NULL
    }

    input_paramcd <- as.character(unique(anl_m$data()[[as.vector(anl_m$columns_source$paramcd)]]))
    title <- paste("KM Plot of", input_paramcd)

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
      time_unit_var = as.vector(anl_m$columns_source$time_unit_var),
      facet_var = as.vector(anl_m$columns_source$facet_var),
      annot_surv_med = input$show_km_table,
      annot_coxph = input$compare_arms,
      xticks = input_xticks,
      font_size = input$font_size,
      pval_method = input$pval_method_coxph,
      conf_level = as.numeric(input$conf_level),
      ties = input$ties_coxph,
      xlab = input$xlab,
      yval = ifelse(input$yval == "Survival probability", "Survival", "Failure"),
      ci_ribbon = input$show_ci_ribbon,
      title = title
    )
    mapply(expression = my_calls, teal.devel::chunks_push)
  })

  km_plot <- reactive({
    call_preparation()
    teal.devel::chunks_safe_eval()
  })


  # Insert the plot into a plot with settings module from teal.devel
  callModule(
    teal.devel::plot_with_settings_srv,
    id = "myplot",
    plot_r = km_plot,
    height = plot_height,
    width = plot_width
  )

  callModule(
    teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(
      list(arm_var, paramcd, strata_var, facet_var, aval_var, cnsr_var, time_unit_var)
    ),
    modal_title = label
  )
}
