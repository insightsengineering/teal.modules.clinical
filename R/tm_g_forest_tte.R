#' Template for Survival Forest Plot
#'
#' Creates a valid expression for survival forest plot.
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @inheritParams tm_t_tte
#' @inheritParams shared_params
#'
template_forest_tte <- function(anl_name = "ANL",
                                parent_name = "ANL_ADSL",
                                arm_var,
                                ref_arm = NULL,
                                comp_arm = NULL,
                                aval_var = "AVAL",
                                cnsr_var = "CNSR",
                                subgroup_var,
                                strata_var = NULL,
                                conf_level = 0.95,
                                col_symbol_size = NULL) {

  assert_that(
    is.string(anl_name),
    is.string(arm_var),
    is.character(subgroup_var)
  )

  y <- list()

  # Data processing.
  data_list <- list()
  anl_list <- list()
  parent_list <- list()

  anl_list <- add_expr(
    anl_list,
    substitute(
      data %>% filter(arm_var %in% arm_vals),
      env = list(
        data = as.name(anl_name),
        arm_var = as.name(arm_var),
        arm_vals = c(ref_arm, comp_arm)
      )
    )
  )

  anl_list <- add_expr(
    anl_list,
    substitute_names(
      mutate(
        arm_var = arm_var %>%
          relevel(ref_arm) %>%
          droplevels() %>%
          combine_levels(comp_arm)
        ) %>%
        mutate(is_event = cnsr_var == 0),
      names = list(
        arm_var = as.name(arm_var)
      ),
      others = list(
        ref_arm = ref_arm,
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
    substitute(
      data %>% filter(arm_var %in% arm_vals),
      env = list(
        data = as.name(parent_name),
        arm_var = as.name(arm_var),
        arm_vals = c(ref_arm, comp_arm)
      )
    )
  )

  parent_list <- add_expr(
    parent_list,
    substitute_names(
      mutate(
        arm_var = arm_var %>%
          relevel(ref_arm) %>%
          droplevels() %>%
          combine_levels(comp_arm)
      ),
      names = list(
        arm_var = as.name(arm_var)
      ),
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
          subgroups = subgroup_var
          ),
        data = anl
      ),
      env = list(
        aval_var = aval_var,
        arm_var = arm_var,
        subgroup_var = subgroup_var
      )
    )
  )

  summary_list <- add_expr(
    summary_list,
    substitute(
      tbl_survtime <- basic_table() %>%
        tabulate_survival_subgroups(
          vars = c("n", "median"),
          control = control_coxph(conf_level = conf_level)
          ) %>%
        build_table(df$survtime),
      env = list(conf_level = conf_level)
    )
  )

  summary_list <- add_expr(
    summary_list,
    substitute(
      tbl_hr <- basic_table() %>%
        tabulate_survival_subgroups(
          vars = c("n_tot", "hr", "ci"),
          control = control_coxph(conf_level = conf_level)
          ) %>%
        build_table(df$hr),
      env = list(conf_level = conf_level)
    )
  )

  y$summary <- bracket_expr(summary_list)

  # Table output.
  y$table <- substitute(
    result <- cbind_rtables(tbl_hr[, 1], tbl_survtime, tbl_hr[, 2:3])
  )

  y$plot <- substitute(
    plot <- g_forest(
      tbl = result,
      col_x = 6,
      col_ci = 7,
      vline = NULL,
      forest_header = NULL,
      xlim = NULL,
      logx = FALSE,
      x_at = NULL,
      width_row_names = NULL,
      width_columns = NULL,
      width_forest = unit(1, "null"),
      col_symbol_size = col_symbol_size,
      draw = TRUE,
      newpage = TRUE
      ),
    env = list(col_symbol_size = col_symbol_size)
  )

  y
}

## Example----
#' Forest Survival Plot teal Module
#'
#' This teal module produces a grid style Forest plot for time-to-event data
#' with ADaM structure
#'
#' @inheritParams argument_convention
#' @inheritParams tm_g_forest_rsp
#' @param parent_name (\code{character}) name of \code{ADSL} dataset used in the analysis.
#' @param aval_var (\code{\link[teal]{choices_selected}} or \code{data_extract_spec}) object with all available choices
#'   and preselected option for analysis variable
#' @param cnsr_var (\code{\link[teal]{choices_selected}} or \code{data_extract_spec}) object with all available choices
#'   and preselected option for censor variable
#'
#' @export
#'
#' @template author_song24
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' ADSL$RACE <- droplevels(ADSL$RACE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL,
#'       code = 'ADSL <- radsl(cached = TRUE)
#'               ADSL$RACE <- droplevels(ADSL$RACE)'),
#'     cdisc_dataset("ADTTE", ADTTE, code = 'ADTTE <- radtte(cached = TRUE)'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_forest_tte(
#'        label = "Forest Survival",
#'        dataname = "ADTTE",
#'        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'        subgroup_var = choices_selected(names(ADSL), c("SEX", "BMRKR2")),
#'        paramcd = choices_selected(value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"),
#'        strata_var = choices_selected(c("STRATA1", "STRATA2"), "STRATA2"),
#'        plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_forest_tte <- function(label,
                            dataname,
                            parent_name = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                            arm_var,
                            subgroup_var,
                            paramcd,
                            strata_var,
                            aval_var = choices_selected(variable_choices(dataname, "AVAL"), "AVAL", fixed = TRUE),
                            cnsr_var = choices_selected(variable_choices(dataname, "CNSR"), "CNSR", fixed = TRUE),
                            conf_level = choices_selected(
                              c(0.8, 0.85, 0.90, 0.95, 0.99, 0.995),
                              0.95,
                              keep_order = TRUE
                              ),
                            fixed_symbol_size = TRUE,
                            plot_height = c(700L, 200L, 2000L),
                            plot_width = c(980L, 500L, 2000L),
                            pre_output = NULL,
                            post_output = NULL) {

  stopifnot(
    is.cs_or_des(arm_var),
    is.cs_or_des(paramcd),
    is.cs_or_des(subgroup_var),
    is.cs_or_des(strata_var),
    is.choices_selected(conf_level),
    is_logical_single(fixed_symbol_size)
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
  if (is.choices_selected(subgroup_var)) {
    subgroup_var <- cs_to_des_select(subgroup_var, dataname = parent_name, multiple = TRUE)
  }
  if (is.choices_selected(strata_var)) {
    strata_var <- cs_to_des_select(strata_var, dataname = parent_name, multiple = TRUE)
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
    subgroup = subgroup_var,
    strata = strata_var,
    aval = aval_var,
    cnsr = cnsr_var
  )

  module(
    label = label,
    server = srv_g_forest_tte,
    ui = ui_g_forest_tte,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parent_name = parent_name,
        plot_height = plot_height,
        plot_width = plot_width
        )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}


ui_g_forest_tte <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(a$arm, a$paramcd, a$subgroup, a$strata, a$aval, a$cnsr)

  ns <- NS(id)
## UI ----
  standard_layout(
    output = plot_with_settings_ui(id = ns("myplot"), height = a$plot_height, width = a$plot_width),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "paramcd", "subgroup", "strata", "aval", "cnsr")]),
      helpText("Analysis data:", code(a$dataname)),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = a$arm,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        ns("ref_arm"),
        div(
          "Reference Arm",
          title = "Cannot select more than one reference arm.",
          icon("info-circle")
        ),
        choices = NULL,
        selected = NULL,
        multiple = TRUE
        ),
      selectInput(
        ns("comp_arm"),
        div(
          "Comparison Arm",
          title = "Multiple arms automatically combined into a single arm if more than one value selected.",
          icon("info-circle")
        ),
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      data_extract_input(
        id = ns("subgroup_var"),
        label = "Subgroup Variables",
        data_extract_spec = a$subgroup,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        panel_item(
          "Additional plot settings",
          optionalSelectInput(
            ns("conf_level"),
            "Level of Confidence",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          checkboxInput(ns("fixed_symbol_size"), "Fixed symbol size", value = TRUE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_forest_tte <- function(input,
                             output,
                             session,
                             datasets,
                             dataname,
                             parent_name,
                             arm,
                             paramcd,
                             subgroup,
                             strata,
                             aval,
                             cnsr,
                             plot_height,
                             plot_width) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = paste0("arm_var-dataset_", parent_name, "_singleextract-select"), # from UI
    datasets = datasets,
    arm_ref_comp = NULL,
    module = "tm_g_forest_tte"
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm, paramcd, subgroup, strata, aval, cnsr),
    input_id = c("arm_var", "paramcd", "subgroup_var", "strata_var", "aval_var", "cnsr_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm, subgroup, strata),
    input_id = c("arm_var", "subgroup_var", "strata_var"),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parent_name, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval)
    input_cnsr_var <- as.vector(anl_m$columns_source$cnsr)
    input_subgroup_var <- as.vector(anl_m$columns_source$subgroup_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_subgroup_var, input_strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", input_aval_var, input_cnsr_var),
      arm_var = input_arm_var
    )

    validate(
      need(
        c(input$ref_arm, input$comp_arm) %in% levels(adsl_filtered[[input_arm_var]]),
        "Arm variable is updating"
      )
    )

    # validate arm levels
    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
    }

    do.call(what = "validate_standard_inputs", validate_args)
    NULL
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 10)

    my_calls <- template_forest_tte(
      anl_name = "ANL",
      parent_name = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      aval_var = as.vector(anl_m$columns_source$aval),
      cnsr_var = as.vector(anl_m$columns_source$cnsr),
      subgroup_var = as.vector(anl_m$columns_source$subgroup_var),
      strata_var = as.vector(anl_m$columns_source$strata_var),
      conf_level = as.numeric(input$conf_level),
      col_symbol_size = if (!input$fixed_symbol_size) 1
      )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  get_plot <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("plot")
  })

  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = get_plot,
    height = plot_height,
    width = plot_width
  )

  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = "R Code for the Current Time-to-Event Forest Plot",
    code_header = "Time-to-Event Forest Plot"
  )
}
