#' Template: Response Forest Plot
#'
#' Creates a valid expression for response forest plot.
#'
#' @inheritParams template_arguments
#' @param obj_var_name (`character`)\cr additional text string append to output title
#' @param responders (`character`)\cr values of `aval_var` that are considered to be responders.
#' @param col_symbol_size (`integer`)\cr column index to be used to determine relative size for
#'  estimator plot symbol. Typically, the symbol size is proportional to the sample size used
#'  to calculate the estimator. If `NULL`, the same symbol size is used for all subgroups.
#' @param strata_var (`character`)\cr
#'   names of the variables for stratified analysis.
#'
#' @seealso [tm_g_forest_rsp()]
#'
#' @importFrom grid grid.newpage grid.draw
#'
template_forest_rsp <- function(dataname = "ANL",
                                parentname = "ADSL_FILTERED",
                                arm_var,
                                ref_arm = NULL,
                                comp_arm = NULL,
                                obj_var_name = "",
                                aval_var = "AVALC",
                                responders = c("CR", "PR"),
                                subgroup_var,
                                strata_var = NULL,
                                conf_level = 0.95,
                                col_symbol_size = NULL) {

  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(aval_var),
    is.string(obj_var_name),
    is.null(subgroup_var) || is.character(subgroup_var)
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
    substitute(
      expr = dplyr::mutate(is_rsp = aval_var %in% responders),
      env = list(
        aval_var = as.name(aval_var),
        responders = responders
      )
    )
  )

  anl_list <- add_expr(
    anl_list,
    substitute_names(
      expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
      names = list(arm_var = as.name(arm_var)),
      others = list(comp_arm = comp_arm)
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- anl_list,
      env = list(
        anl = as.name(dataname),
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
      expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
      names = list(arm_var = as.name(arm_var)),
      others = list(comp_arm = comp_arm)
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
          rsp = "is_rsp", arm = arm_var, subgroups = subgroup_var, strat = strata_var),
        data = anl,
        conf_level = conf_level
      ),
      env = list(
        anl = as.name(dataname),
        arm_var = arm_var,
        subgroup_var = subgroup_var,
        strata_var = strata_var,
        conf_level = conf_level
      )
    )
  )

  y$summary <- bracket_expr(summary_list)

  # Table output.
  y$table <- quote(
    result <- basic_table() %>%
      tabulate_rsp_subgroups(df, vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))
  )

  title <- paste0("Forest plot of best overall response for ", obj_var_name)

  # Plot output.
  y$plot <- substitute(
    expr = {
      p <- g_forest(
        tbl = result,
        col_symbol_size = col_symbol_size
      )
      if (!is.null(footnotes(p))) {
        p <- decorate_grob(p, title = title, footnotes = footnotes(p),
                           gp_footnotes = gpar(fontsize = 12))
      } else {
        p <- decorate_grob(p, title = title, footnotes = "",
                           gp_footnotes = gpar(fontsize = 12))
      }

      grid::grid.newpage()
      grid::grid.draw(p)
    },
    env = list(
      anl = as.name(dataname),
      arm_var = arm_var,
      col_symbol_size = col_symbol_size,
      title = title
    )
  )

  y
}

#' Teal Module: Forest Response Plot teal module
#'
#' This teal module produces a grid style Forest plot for response data with ADaM structure.
#'
#' @inheritParams module_arguments
#' @param fixed_symbol_size (`logical`)\cr When (`TRUE`), the same symbol size is used for plotting each
#' estimate. Otherwise, the symbol size will be proportional to the sample size in each each subgroup.
#'
#' @export
#'
#' @template author_song24
#'
#' @examples
#'
#' library(scda)
#' library(dplyr)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADRS <- synthetic_cdisc_data("latest")$adrs
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
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADRS", ADRS, code = 'ADRS <- synthetic_cdisc_data("latest")$adrs'),
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
#'         "INVET"
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
                            parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                            arm_var,
                            arm_ref_comp = NULL,
                            paramcd,
                            aval_var = choices_selected(variable_choices(dataname, "AVALC"), "AVALC", fixed = TRUE),
                            subgroup_var,
                            strata_var,
                            fixed_symbol_size = TRUE,
                            conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                            plot_height = c(700L, 200L, 2000L),
                            plot_width = c(900L, 200L, 2000L),
                            pre_output = NULL,
                            post_output = NULL) {
  logger::log_info("Initializing tm_g_forest_rsp")
  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is_logical_single(fixed_symbol_size),
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
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    subgroup_var = cs_to_des_select(subgroup_var, dataname = parentname, multiple = TRUE),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE)
  )

  module(
    label = label,
    ui = ui_g_forest_rsp,
    ui_args = c(data_extract_list, args),
    server = srv_g_forest_rsp,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        plot_height = plot_height,
        plot_width = plot_width
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_g_forest_rsp <- function(id, ...) {

  a <- list(...) # module args
  is_single_dataset_value <- is_single_dataset(a$arm_var, a$paramcd, a$subgroup_var, a$strata_var)

  ns <- NS(id)

  standard_layout(
    output = plot_with_settings_ui(id = ns("myplot")),
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
        choices = c("CR", "PR"),
        selected = c("CR", "PR"),
        multiple = TRUE
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        ns("ref_arm"),
        div(
          "Reference Group",
          title = paste("Multiple reference groups are automatically combined into a single group when more than one",
          "value selected."),
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
          title = paste("Multiple comparison groups are automatically combined into a single group when more than one",
          "value selected."),
          icon("info-circle")
        ),
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
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
          optionalSelectInput(
            inputId = ns("conf_level"),
            label = "Confidence Level",
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

srv_g_forest_rsp <- function(input,
                             output,
                             session,
                             datasets,
                             dataname,
                             parentname,
                             arm_var,
                             arm_ref_comp,
                             paramcd,
                             aval_var,
                             subgroup_var,
                             strata_var,
                             plot_height,
                             plot_width,
                             label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    dataname = parentname,
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
  observeEvent(anl_merged(), {
    aval_var <- anl_merged()$columns_source$aval_var
    if (nrow(anl_merged()$data()) == 0) {
      responder_choices <- c("CR", "PR")
      responder_sel <- c("CR", "PR")
    } else {
      responder_choices <- unique(anl_merged()$data()[[aval_var]])
      responder_sel <- intersect(responder_choices, isolate(input$responders))
    }
    updateSelectInput(
      session, "responders",
      choices = responder_choices,
      selected = responder_sel
    )
  })

  subgroup_var_ordered <- get_input_order("subgroup_var", subgroup_var$dataname)

  # Prepare the analysis environment (filter data, check data, populate envir).
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_subgroup_var <- subgroup_var_ordered()
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]

    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_subgroup_var, input_strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var),
      arm_var = input_arm_var
    )
    validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))

    do.call(what = "validate_standard_inputs", validate_args)

    validate_one_row_per_id(anl_m$data(), key = c("USUBJID", "STUDYID", input_paramcd))

    if (length(input_subgroup_var) > 0) {
      validate(
        need(all(vapply(adsl_filtered[, input_subgroup_var], is.factor, logical(1))),
             "Not all subgroup variables are factors.")
      )
    }
    if (length(input_strata_var) > 0) {
      validate(
        need(all(vapply(adsl_filtered[, input_strata_var], is.factor, logical(1))),
             "Not all stratification variables are factors.")
      )
    }

    validate(need(
      input$conf_level >= 0 && input$conf_level <= 1,
      "Please choose a confidence level between 0 and 1"
    ))

    validate(
      need(is_character_single(input_aval_var), "Analysis variable should be a single column."),
      need(input$responders, "`Responders` field is empty."),
      need(input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]],
        "`Select Endpoint` is not selected."
      )
    )

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

    strata_var <- as.vector(anl_m$columns_source$strata_var)
    subgroup_var <-  as.vector(anl_m$columns_source$subgroup_var)

    obj_var_name <- get_g_forest_obj_var_name(paramcd, input)

    my_calls <- template_forest_rsp(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      obj_var_name = obj_var_name,
      aval_var = as.vector(anl_m$columns_source$aval_var),
      responders = input$responders,
      subgroup_var = if (length(subgroup_var_ordered()) != 0) subgroup_var_ordered() else NULL,
      strata_var = if (length(strata_var) != 0) strata_var else NULL,
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
    datanames = get_extract_datanames(
      list(arm_var, paramcd, subgroup_var, strata_var)
    ),
    modal_title = label
  )
}
