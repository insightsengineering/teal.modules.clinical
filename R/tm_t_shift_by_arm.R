#' Template: Shift by Arm
#'
#' @inheritParams template_arguments
#' @param visit (`character`)\cr variable value designating the analysis visit.
#' @param anrind_var (`character`)\cr the variable name for the analysis reference range indicator.
#' @param bnrind_var (`character`)\cr the variable name for the baseline reference range indicator.
#'
#' @examples
#'
#' adsl <- df_explicit_na(scda::synthetic_cdisc_data("latest")$adsl)
#' adeg <- df_explicit_na(scda::synthetic_cdisc_data("latest")$adeg)
#'
#' tbl_code <- template_shift_by_arm("adeg", parentname = "adsl")
#' lapply(tbl_code, eval)
#'
#'
template_shift_by_arm <- function(dataname,
                                  parentname,
                                  arm_var = "ARM",
                                  paramcd = "PARAMCD",
                                  visit = "AVISIT",
                                  anrind_var = "ANRIND",
                                  bnrind_var = "BNRIND",
                                  anrind_levels = c("LOW", "NORMAL", "HIGH", "<Missing>"),
                                  bnrind_levels = c("LOW", "NORMAL", "HIGH", "<Missing>"),
                                  anrind_labels = c("LOW", "NORMAL", "HIGH", "Missing"),
                                  bnrind_labels = c("LOW", "NORMAL", "HIGH", "Missing"),
                                  drop_arm_levels = TRUE) {

  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(paramcd),
    is.string(anrind_var),
    is.string(bnrind_var),
    is.string(visit),
    is.flag(drop_arm_levels)
  )

  y <- list()

  # Start data steps.
  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- df_explicit_na(parentname),
      env = list(parentname = as.name(parentname))
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = dataname$bnrind_var <- factor(
        dataname$bnrind_var,
        levels = bnrind_levels,
        labels = bnrind_labels
      ),
      env = list(
        dataname = as.name(dataname),
        bnrind_var = bnrind_var,
        bnrind_levels = bnrind_levels,
        bnrind_labels = bnrind_labels
      )
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      expr = dataname$anrind_var <- factor(
        dataname$anrind_var,
        levels = anrind_levels,
        labels = anrind_labels
      ),
      env = list(
        dataname = as.name(dataname),
        anrind_var = anrind_var,
        anrind_levels = anrind_levels,
        anrind_labels = anrind_labels
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        mutate(col_label = visit) %>%
        df_explicit_na(),
      env = list(
        df = as.name(dataname),
        visit = visit
      )
    )
  )

  data_list <- add_expr(
    data_list,
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = arm_var,
      drop_arm_levels = drop_arm_levels
    )
  )

  y$data <- bracket_expr(data_list)

  # Start layout steps.
  layout_list <- list()

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = basic_table() %>%
        split_cols_by("col_label") %>% # temprary solution for over arching column
        split_cols_by(anrind_var) %>%
        split_rows_by(
          arm_var,
          split_fun = drop_split_levels,
          label_pos = "topleft",
          split_label = obj_label(dataname$arm_var)) %>%
        add_rowcounts() %>%
        summarize_vars(bnrind_var, denom = "N_row") %>%
        append_varlabels(dataname, bnrind_var, indent = 1L),
      env = list(
        anrind_var = anrind_var,
        arm_var = arm_var,
        bnrind_var = bnrind_var,
        dataname = as.name(dataname)
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- substitute(
    expr = {
      result <- build_table(lyt = lyt, df = anl)
      result
    }
  )

  y

}

#' Teal Module: Shift by Arm
#'
#' @param arm_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#'   object with all available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table. If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param drop_arm_levels (`logical`)\cr drop the unused `arm_var` levels.
#'   When `TRUE`, `arm_var` levels are set to those used in the `dataname` dataset. When `FALSE`,
#'   `arm_var` levels are set to those used in the `parentname` dataset.
#'   If `dataname` dataset and `parentname` dataset are the same (i.e. ADSL), then `drop_arm_levels` will always be
#'   TRUE regardless of the user choice when `tm_t_summary` is called.
#' @param numeric_stats (`character`)\cr
#'   selected statistics for numeric summarize variables to be displayed. Possible values are `n`, `mean_sd`, `mean_ci`,
#'   `median`, `median_ci`, `quantiles` and `range`. By default,  `n`, `mean_sd`, `median`, `range` are selected.
#' @inheritParams module_arguments
#'
#' @export
#' @examples
#'
#' library(dplyr)
#' library(tern)
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adeg <- synthetic_cdisc_data("latest")$adeg
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADEG", adeg, code = 'ADEG <- synthetic_cdisc_data("latest")$adeg'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_shift_by_arm(
#'       label = "Shift by Arm Table",
#'       dataname = "ADEG",
#'       arm_var = choices_selected(variable_choices(adsl, subset = c("ARM", "ARMCD")), selected = "ARM"),
#'       anrind_var = choices_selected(
#'       variable_choices(adeg, subset = "ANRIND"), selected = "ANRIND", fixed = TRUE
#'       ),
#'       bnrind_var = choices_selected(
#'         variable_choices(adeg, subset = "BNRIND"), selected = "BNRIND", fixed = TRUE
#'         ),
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_shift_by_arm <- function(label,
                              dataname,
                              parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                              arm_var,
                              paramcd = choices_selected(
                                value_choices(dataname, "PARAMCD"), selected = "HR"
                              ),
                              visit = choices_selected(
                                value_choices(dataname, "AVISIT"), selected = "POST-BASELINE MINIMUM"
                              ),
                              anrind_var = choices_selected(
                                variable_choices(dataname, subset = "ANRIND"), selected = "ANRIND", fixed = TRUE
                              ),
                              bnrind_var = choices_selected(
                                variable_choices(dataname, subset = "BNRIND"), selected = "BNRIND", fixed = TRUE
                              ),
                              anrind_levels = c("LOW", "NORMAL", "HIGH", "<Missing>"),
                              bnrind_levels = c("LOW", "NORMAL", "HIGH", "<Missing>"),
                              anrind_labels = c("LOW", "NORMAL", "HIGH", "Missing"),
                              bnrind_labels = c("LOW", "NORMAL", "HIGH", "Missing"),
                              drop_arm_levels = TRUE,
                              useNA = c("ifany", "no"), # nolint
                              na_level = "<Missing>",
                              pre_output = NULL,
                              post_output = NULL) {

  stop_if_not(
    is_character_single(dataname),
    is_character_single(parentname),
    useNA %in% c("ifany", "no"), # nolint,
    is_character_single(na_level),
    is_logical_single(drop_arm_levels),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )
  useNA <- match.arg(useNA) # nolint

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    visit = cs_to_des_filter(visit, dataname = dataname),
    anrind_var = cs_to_des_select(anrind_var, dataname = dataname),
    bnrind_var = cs_to_des_select(bnrind_var, dataname = dataname)
    #ontrtfl_var = cs_to_des_select(ontrtfl_var, dataname = dataname, multiple = TRUE),
    #saffl_var = cs_to_des_select(saffl_var, dataname = dataname, multiple = TRUE)
  )

  args <- as.list(environment())

  module(
    label = label,
    server = srv_shift_by_arm,
    ui = ui_shift_by_arm,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        anrind_levels = anrind_levels,
        anrind_labels = anrind_labels,
        bnrind_levels = bnrind_levels,
        bnrind_labels = bnrind_labels
        # na_level = na_level
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )

}

#' @noRd
ui_shift_by_arm <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  is_single_dataset_value <- is_single_dataset(
    a$id_var,
    a$arm_var,
    a$paramcd,
    a$visit,
    a$anrind_var,
    a$bnrind_var
  )

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c(
         "arm_var", "paramcd_var", "paramcd", "anrind_var", "bnrind_var", "visit_var", "visit"
      )]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("anrind_var"),
        label = "Select Analysis Range Indicator Variable",
        data_extract_spec = a$anrind_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("bnrind_var"),
        label = "Select Baseline Reference Range Indicator Variable",
        data_extract_spec = a$bnrind_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("visit"),
        label = "Select Visit",
        data_extract_spec = a$visit,
        is_single_dataset = is_single_dataset_value
      ),

      panel_group(
        panel_item(
          "Additional table settings",
          checkboxInput(ns("drop_zero_levels"), "Drop rows with 0 count", value = a$drop_zero_levels),
          radioButtons(
            ns("useNA"),
            label = "Display NA counts",
            choices = c("ifany", "no"),
            selected = a$useNA
          ),
          if (a$dataname == a$parentname) {
            shinyjs::hidden(
              checkboxInput(
                ns("drop_arm_levels"),
                label = "it's a BUG if you see this",
                value = TRUE
              )
            )
          } else {
            checkboxInput(
              ns("drop_arm_levels"),
              label = sprintf("Drop columns not in filtered %s", a$dataname),
              value = a$drop_arm_levels
            )
          }
        )
      ),
      panel_group(
        # panel_item(
        #   "Additional Variables Info",
        #   data_extract_input(
        #     id = ns("id_var"),
        #     label = "Subject Identifier",
        #     data_extract_spec = a$id_var,
        #     is_single_dataset = is_single_dataset_value
        #   )
        # )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_shift_by_arm <- function(input,
                             output,
                             session,
                             datasets,
                             dataname,
                             parentname,
                             arm_var,
                             paramcd,
                             visit,
                             anrind_var,
                             bnrind_var,
                             anrind_levels,
                             bnrind_levels,
                             anrind_labels,
                             bnrind_labels,
                             label,
                             drop_arm_levels = TRUE) {

  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, paramcd, visit, anrind_var, bnrind_var),
    input_id = c("arm_var", "paramcd", "visit", "anrind_var", "bnrind_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  # validate inputs
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_anrind_var <- as.vector(anl_m$columns_source$input_anrind_var)
    input_bnrind_var <- as.vector(anl_m$columns_source$input_bnrind_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]
    input_visit <- unlist(visit$filter)["vars_selected"]

    validate(
      need(input_arm_var, "Please select a treatment variable"),
      # need(input_anrind_var, "Please select analysis range indicator variable"),
      # need(input_bnrind_var,  "Please select baseline reference range indicator variable"),

      need(length(input_arm_var) <= 2, "Please limit treatment variables within two"),
      if (length(input_arm_var) == 2) {
        need(
          is.factor(adsl_filtered[[input_arm_var[[2]]]]) & all(!adsl_filtered[[input_arm_var[[2]]]] %in% c(
            "", NA
          )),
          "Please check nested treatment variable which needs to be a factor without NA or empty strings."
        )
      }
    )

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_anrind_var, input_bnrind_var),
      arm_var = input_arm_var[[1]]
    )
  })

  # generate r code for the analysis
  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    my_calls <- template_shift_by_arm(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      paramcd = unlist(paramcd$filter)["vars_selected"],
      anrind_var = as.vector(anl_m$columns_source$anrind_var),
      bnrind_var = as.vector(anl_m$columns_source$bnrind_var),
      anrind_levels = anrind_levels,
      anrind_labels = anrind_labels,
      bnrind_levels = bnrind_levels,
      bnrind_labels = bnrind_labels,
      drop_arm_levels = input$drop_arm_levels
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("result")
  })

  callModule(
    table_with_settings_srv,
    id = "table",
    table_r = table
  )

  # Render R code.
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(arm_var, paramcd, visit, anrind_var, bnrind_var)),
    modal_title = "R Code for Shift Table by Arm",
    code_header = label
  )
}
