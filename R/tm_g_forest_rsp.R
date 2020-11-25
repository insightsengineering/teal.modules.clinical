#' Forest Response Plot teal module
#'
#' This is teal module produces a grid style Forest plot for response data with ADaM structure
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @inheritParams tm_t_tte
#' @name tm_g_forest_rsp
#'
NULL

#' Template for Response Forest Plot
#'
#' Creates a valid expression for response forest plot.
#'
#' @param responders (`character`)\cr values of `aval_var` that are considered to be responders.
#' @param col_symbol_size (`integer`)\cr column index to be used to determine relative size for
#'  estimator plot symbol. Typically, the symbol size is proportional to the sample size used
#'  to calculate the estimator. If `NULL`, the same symbol size is used for all subgroups.
#'
template_forest_rsp <- function(anl_name = "ANL",
                                parent_name = "ADSL_FILTERED",
                                arm_var,
                                ref_arm = NULL,
                                comp_arm = NULL,
                                aval_var = "AVALC",
                                responders = c("CR", "PR"),
                                subgroup_var,
                                strata_var = NULL,
                                conf_level = 0.95,
                                col_symbol_size = NULL) {

  assert_that(
    is.string(anl_name),
    is.string(parent_name),
    is.string(arm_var),
    is.string(aval_var),
    is.null(subgroup_var) || is.character(subgroup_var),
    is.null(subgroup_var) || is.character(subgroup_var)
  )

  y <- list()

  # Data processing.
  data_list <- list()
  anl_list <- list()
  parent_list <- list()

  anl_list <- add_expr(
    anl_list,
    substitute(
      expr = anl %>%
        mutate(is_rsp = aval_var %in% responders) %>%
        filter(arm_var %in% arm_vals),
      env = list(
        anl = as.name(anl_name),
        arm_var = as.name(arm_var),
        arm_vals = c(ref_arm, comp_arm),
        aval_var = as.name(aval_var),
        responders = responders
      )
    )
  )

  anl_list <- add_expr(
    anl_list,
    substitute_names(
      expr = mutate(
        arm_var = relevel(arm_var, ref = ref_arm) %>%
          droplevels() %>%
          combine_levels(levels = comp_arm)
      ) %>%
        droplevels(),
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
      anl <- anl_list,
      env = list(
        anl = as.name(anl_name),
        anl_list = pipe_expr(anl_list)
      )
    )
  )

  parent_list <- add_expr(
    parent_list,
    substitute(
      parent %>% filter(arm_var %in% arm_vals),
      env = list(
        parent = as.name(parent_name),
        arm_var = as.name(arm_var),
        arm_vals = c(ref_arm, comp_arm)
      )
    )
  )

  parent_list <- add_expr(
    parent_list,
    substitute_names(
      expr = mutate(
        arm_var = relevel(arm_var, ref = ref_arm) %>%
          droplevels() %>%
          combine_levels(levels = comp_arm)
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
      expr = df <- extract_rsp_subgroups(
        variables = list(
          rsp = "is_rsp", arm = arm_var, subgroups = subgroup_var, strata_var = strata_var),
        data = anl,
        conf_level = conf_level
      ),
      env = list(
        anl = as.name(anl_name),
        arm_var = arm_var,
        subgroup_var = subgroup_var,
        strata_var = strata_var,
        conf_level = conf_level
      )
    )
  )

  summary_list <- add_expr(
    summary_list,
    substitute(
      expr = rsp_tab <- basic_table() %>%
        tabulate_rsp_subgroups(vars = c("n", "prop")) %>%
        build_table(df$prop)
    )
  )

  summary_list <- add_expr(
    summary_list,
    substitute(
      expr = or_tab <- basic_table() %>%
        tabulate_rsp_subgroups(vars = c("n_tot", "or", "ci"), conf_level = conf_level) %>%
        build_table(df$or),
      env = list(conf_level = conf_level)
    )
  )

  y$summary <- bracket_expr(summary_list)

  # Table output.
  y$table <- substitute(
    result <- cbind_rtables(or_tab[, 1], rsp_tab, or_tab[, -1])
  )

  # Plot output.
  y$plot <- substitute(
    expr = {
      p <- g_forest(
        tbl = result,
        col_x = 6,
        col_ci = 7,
        vline = 1,
        forest_header = paste0(levels(anl[[arm_var]]), "\nbetter"),
        xlim = c(.1, 10),
        logx = TRUE,
        x_at = c(.1, 1, 10),
        draw = FALSE,
        col_symbol_size = col_symbol_size
      )
      if (!is.null(footnotes(p))) {
        p <- decorate_grob(p, title = "Forest plot", footnotes = footnotes(p),
                           gp_footnotes = gpar(fontsize = 12))
      }

      grid::grid.newpage()
      grid::grid.draw(p)
    },
    env = list(
      anl = as.name(anl_name),
      arm_var = arm_var,
      col_symbol_size = col_symbol_size
    )
  )

  y
}

#' @describeIn tm_g_forest_rsp Teal module for response forest plot.
#' @param fixed_symbol_size (`flag`)\cr When (`TRUE`), the same symbol size is used for plotting each
#' estimate. Otherwise, the symbol size will be proportional to the sample size in each each subgroup.
#'
#' @export
#'
#' @template author_song24
#'
#' @examples
#'
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADRS <- radrs(cached = TRUE)
#'
#' arm_ref_comp = list(
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
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- radsl(cached = TRUE)'),
#'     cdisc_dataset("ADRS", ADRS, code = 'ADRS <- radrs(cached = TRUE)'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_forest_rsp(
#'       label = "Forest Response",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD")),
#'         "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADRS, "PARAMCD", "PARAM"),
#'         "BESRSPI"
#'       ),
#'       subgroup_var = choices_selected(
#'         variable_choices(ADSL, names(ADSL)),
#'         c("BMRKR2", "SEX")
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("STRATA1", "STRATA2")),
#'         "STRATA2"
#'       ),
#'       plot_height = c(600L, 200L, 2000L)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_forest_rsp <- function(label,
                            dataname,
                            parent_name = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                            arm_var,
                            arm_ref_comp = NULL,
                            paramcd,
                            aval_var = choices_selected(variable_choices(dataname, "AVALC"), "AVALC", fixed = TRUE),
                            subgroup_var,
                            strata_var,
                            fixed_symbol_size = TRUE,
                            plot_height = c(700L, 200L, 2000L),
                            plot_width = c(980L, 500L, 2000L),
                            pre_output = NULL,
                            post_output = NULL) {

  stop_if_not(list(is_character_single(label), "Label should be single (i.e. not vector) character type of object"))
  stopifnot(length(dataname) == 1)
  stopifnot(is.cs_or_des(arm_var))
  stopifnot(is.cs_or_des(paramcd))
  stopifnot(is.cs_or_des(aval_var))
  stopifnot(is.cs_or_des(subgroup_var))
  stopifnot(is.cs_or_des(strata_var))
  stopifnot(is_logical_single(fixed_symbol_size))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)
  stop_if_not(list(
    is.null(pre_output) || is(pre_output, "shiny.tag"),
    "pre_output should be either null or shiny.tag type of object"
  ))
  stop_if_not(list(
    is.null(pre_output) || is(pre_output, "shiny.tag"),
    "pre_output should be either null or shiny.tag type of object"
  ))


  # Convert choices-selected to data_extract_spec
  if (is.choices_selected(arm_var)) {
    arm_var <- cs_to_des_select(arm_var, dataname = parent_name, multiple = FALSE)
  }
  if (is.choices_selected(paramcd)) {
    paramcd <- cs_to_des_filter(paramcd, dataname = dataname, multiple = FALSE)
  }
  if (is.choices_selected(aval_var)) {
    aval_var <- cs_to_des_select(aval_var, dataname = dataname, multiple = FALSE)
  }
  if (is.choices_selected(subgroup_var)) {
    subgroup_var <- cs_to_des_select(subgroup_var, dataname = parent_name, multiple = TRUE)
  }
  if (is.choices_selected(strata_var)) {
    strata_var <- cs_to_des_select(strata_var, dataname = parent_name, multiple = TRUE)
  }

  args <- as.list(environment())

  data_extract_list <- list(
    arm = arm_var,
    paramcd = paramcd,
    aval_var = aval_var,
    subgroup_var = subgroup_var,
    strata_var = strata_var
  )

  module(
    label = label,
    ui = ui_g_forest_rsp,
    ui_args = args,
    server = srv_g_forest_rsp,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parent_name = parent_name,
        arm_ref_comp = arm_ref_comp,
        label = label,
        plot_height = plot_height,
        plot_width = plot_width
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @import teal.devel
#'
ui_g_forest_rsp <- function(id, ...) {

  a <- list(...) # module args
  is_single_dataset_value <- is_single_dataset(a$arm_var, a$paramcd, a$subgroup_var, a$strata_var)

  ns <- NS(id)

  standard_layout(
    output = plot_with_settings_ui(id = ns("myplot"), height = a$plot_height, width = a$plot_width),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "paramcd", "aval_var", "subgroup_var", "strata_var")]),
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
      selectInput(
        ns("responders"),
        "Responders",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
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
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      data_extract_input(
        id = ns("subgroup_var"),
        label = "Subgroup Variables",
        data_extract_spec = a$subgroup_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        panel_item(
          "Additional plot settings",
          numericInput(
            inputId = ns("conf_level"),
            label = "Confidence Level",
            value = 0.95,
            min = 0.01,
            max = 0.99,
            step = 0.01,
            width = "100%"
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

srv_g_forest_rsp <- function(input,
                             output,
                             session,
                             datasets,
                             dataname,
                             parent_name,
                             arm_var,
                             arm_ref_comp,
                             paramcd,
                             aval_var,
                             subgroup_var,
                             strata_var,
                             plot_height,
                             plot_width,
                             label) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = paste0("arm_var-dataset_", parent_name, "_singleextract-select"),
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte"
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, subgroup_var, strata_var, paramcd, aval_var),
    input_id = c("arm_var", "subgroup_var", "strata_var", "paramcd", "aval_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, subgroup_var, strata_var),
    input_id = c("arm_var", "subgroup_var", "strata_var"),
    anl_name = "ANL_ADSL"
  )

  # Update UI choices depending on selection of previous options
  observe({
    anl_m <- anl_merged()
    anl <- anl_m$data()
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    rsp_choices <- unique(anl[[input_aval_var]])

    updateSelectInput(
      session, "responders",
      choices = rsp_choices,
      selected = intersect(rsp_choices, c("CR", "PR"))
    )
  })

  # Prepare the analysis environment (filter data, check data, populate envir).
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parent_name, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_subgroup_var <- as.vector(anl_m$columns_source$subgroup_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_subgroup_var, input_strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", input_aval_var),
      arm_var = input_arm_var
    )
    validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))

    do.call(what = "validate_standard_inputs", validate_args)

    if (!is.null(input_subgroup_var)) {
      need(all(vapply(adsl_filtered[, input_subgroup_var], is.factor, logical(1))),
           "Not all subgroup variables are factors.")
    }

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

    my_calls <- template_forest_rsp(
      anl_name = "ANL",
      parent_name = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      aval_var = as.vector(anl_m$columns_source$aval_var),
      responders = input$responders,
      subgroup_var = as.vector(anl_m$columns_source$subgroup_var),
      strata_var = as.vector(anl_m$columns_source$strata_var),
      conf_level = as.numeric(input$conf_level),
      col_symbol_size = if (input$fixed_symbol_size) {
        NULL
      } else {
        1
      }
    )
    mapply(expression = my_calls, chunks_push)

    chunks_safe_eval()
    chunks_get_var("p")
  })

  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = call_preparation,
    height = plot_height,
    width = plot_width
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(arm_var, paramcd, aval_var, subgroup_var, strata_var)),
    modal_title = label
  )
}
