#' Template: Shift by Arm
#'
#' @inheritParams template_arguments
#' @param event_type (`character`)\cr type of event that is summarized (e.g. adverse event, treatment).
#'   Default is "event".
#' @param sort_criteria (`character`)\cr how to sort the final table. Default option `freq_desc` sorts
#'   by decreasing total number of patients with event. Alternative option `alpha` sorts events
#'   alphabetically.
#'
#'
template_shift_by_arm <- function(dataname,
                                  parentname,
                                  arm_var = "ARM",
                                  paramcd_var = "PARAMCD",
                                  paramcd = "HR", # Heart Rate
                                  visit_var = "AVISIT",
                                  visit = "POST-BASELINE MINIMUM",
                                  anrind_var = "ANRIND",
                                  bnrind_var = "BNRIND",
                                  ontrtfl_var = "ONTRTFL",  # "On Treatment Record Flag"
                                  ontrtfl = "Y",
                                  saffl_var = "SAFFL",
                                  saffl = "Y",
                                  #sort_criteria = c("freq_desc", "alpha"),
                                  drop_arm_levels = TRUE) {

  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(paramcd_var),
    is.string(paramcd),
    is.string(anrind_var),
    is.string(bnrind_var),
    is.string(visit_var),
    is.string(visit),
    is.string(ontrtfl_var),
    is.string(ontrtfl),
    is.string(saffl_var),
    is.string(saffl),
    is.flag(drop_arm_levels)
  )

  y <- list()

  # Start data steps.
  data_list <- add_expr(
    list(),
    substitute(
      expr = anl <- df %>%
        filter(
          paramcd_var == paramcd,
          saffl_var == saffl,
          ontrtfl_var == ontrtfl,
          visit_var == visit
        ) %>%
        df_explicit_na(na_level = ""),
      env = list(
        df = as.name(dataname),
        paramcd_var = as.name(paramcd_var),
        saffl_var = as.name(saffl_var),
        ontrtfl_var = as.name(ontrtfl_var),
        visit_var = as.name(visit_var),
        paramcd = paramcd,
        saffl = saffl,
        ontrtfl = ontrtfl,
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

  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- df_explicit_na(parentname, na_level = ""),
      env = list(parentname = as.name(parentname))
    )
  )

  # term_vars <- c(hlt, llt)
  #
  #  data_list <- add_expr(
  #    data_list,
  #    substitute(
  #      expr = anl <- anl %>%
  #        df_explicit_na(omit_columns = setdiff(names(anl), term_vars)),
  #      env = list(
  #        term_vars = term_vars
  #      )
  #    )
  #  )

  y$data <- bracket_expr(data_list)

  # Start layout steps.
  layout_list <- add_expr(
    list(),
    substitute(
      expr = basic_table() %>%
        split_cols_by(anrind_var) %>%
        split_rows_by(arm_var, split_fun = drop_split_levels, label_pos = "topleft") %>%
        add_rowcounts() %>%
        summarize_vars(bnrind_var, denom = "N_row"),
      append_varlabels(dataname, bnrind_var, indent = 1L),
      env = list(
        anrind_var = as.name(anrind_var),
        arm_var = as.name(arm_var),
        bnrind_var = as.name(bnrind_var),
        dataname = as.name(dataname),
      )
    )
  )

  #y$layout_prep <- quote(split_fun <- drop_split_levels)

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent),
    env = list(parent = as.name(parentname))
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
#' # Preparation of the test case.
#' library(dplyr)
#' library(scda)
#' library(tern)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#'
#' # Include `EOSDY` and `DCSREAS` variables below because they contain missing data.
#' stopifnot(
#'   any(is.na(adsl$EOSDY)),
#'   any(is.na(adsl$DCSREAS))
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     code = 'ADSL <- synthetic_cdisc_data("latest")$adsl',
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_summary(
#'       label = "Demographic Table",
#'       dataname = "ADSL",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       add_total = TRUE,
#'       summarize_vars = choices_selected(
#'         c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE"),
#'         c("SEX", "RACE")
#'       ),
#'       useNA = "ifany"
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
                              parentname = ifelse(
                                is(arm_var, "data_extract_spec"),
                                datanames_input(arm_var),
                                "ADSL"
                              ),
                              arm_var = "ARM",
                              paramcd_var = "PARAMCD",
                              paramcd = "HR", # Heart Rate
                              visit_var = "AVISIT",
                              visit = "POST-BASELINE MINIMUM",
                              anrind_var = "ANRIND",
                              bnrind_var = "BNRIND",
                              ontrtfl_var = "ONTRTFL",  # "On Treatment Record Flag"
                              ontrtfl = "Y",
                              saffl_var = "SAFFL",
                              saffl = "Y",
                              #sort_criteria = c("freq_desc", "alpha"),
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

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname, multiple = TRUE),
    paramcd_var = cs_to_des_select(paramcd_var, dataname = dataname, multiple = TRUE),
    visit_var = cs_to_des_select(visit_var, dataname = dataname, multiple = TRUE),
    anrind_var = cs_to_des_select(anrind_var, dataname = dataname, multiple = TRUE),
    bnrind_var = cs_to_des_select(bnrind_var, dataname = dataname, multiple = TRUE),
    ontrtfl_var = cs_to_des_select(ontrtfl_var, dataname = dataname, multiple = TRUE),
    saffl_var = cs_to_des_select(saffl_var, dataname = dataname, multiple = TRUE)
  )

  module(
    label = label,
    server = srv_summary,
    ui = ui_summary,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        na_level = na_level
      )
    ),
    filters = dataname
  )

}

#' @noRd
ui_shift_by_arm <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  is_single_dataset_value <- is_single_dataset(
    a$id_var,
    a$arm_var,
    a$paramcd_var,
    a$paramcd,
    a$visit_var,
    a$visit,
    a$anrind_var,
    a$bnrind_var,
    a$ontrtfl_var,
    a$ontrtfl,
    a$saffl_var,
    a$saffl
  )

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c(
        "id_var", "arm_var", "paramcd_var", "paramcd", "anrind_var", "bnrind_var",
        "visit_var", "visit", "ontrtfl_var", "ontrtfl", "saffl_var", "saffl"
      )]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("paramcd_var"),
        label = "Select Parameter Variable",
        data_extract_spec = a$paramcd_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("visit_var"),
        label = "Select Visit Variable",
        data_extract_spec = a$visit_var,
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
        id = ns("ontrtfl_var"),
        label = "Select Treatment Record Flag Variable",
        data_extract_spec = a$ontrtfl_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("saffl_var"),
        label = "Select Safety Population Flag Variable",
        data_extract_spec = a$saffl_var,
        is_single_dataset = is_single_dataset_value
      ),
      if_not_null(
        a$paramcd,
        data_extract_input(
          id = ns("paramcd"),
          label = "Select Endpoint",
          data_extract_spec = a$paramcd,
          is_single_dataset = is_single_dataset_value
        )
      ),
      if_not_null(
        a$visit,
        data_extract_input(
          id = ns("visit"),
          label = "Select Visit",
          data_extract_spec = a$visit,
          is_single_dataset = is_single_dataset_value
        )
      ),
      if_not_null(
        a$ontrtfl,
        data_extract_input(
          id = ns("ontrtfl"),
          label = "Select Treatment Record Flag",
          data_extract_spec = a$ontrtfl,
          is_single_dataset = is_single_dataset_value
        )
      ),
      if_not_null(
        a$saffl,
        data_extract_input(
          id = ns("saffl"),
          label = "Select Safety Population Flag",
          data_extract_spec = a$saffl,
          is_single_dataset = is_single_dataset_value
        )
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
        panel_item(
          "Additional Variables Info",
          data_extract_input(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          )
        )
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
                             paramcd_var,
                             paramcd,
                             visit_var,
                             visit,
                             anrind_var,
                             bnrind_var,
                             ontrtfl_var,
                             ontrtfl,
                             saffl_var,
                             saffl,
                             drop_arm_levels = TRUE) {

  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, anrind_var, ),
    input_id = c("arm_var", "paramcd_var", "visit_var", "anrind_var", "bnrind_var", "ontrtfl_var", "saffl_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  arm_var_user_input <- get_input_order("arm_var", arm_var$dataname)
  paramcd_var_user_input <- get_input_order("paramcd_var", paramcd_var$dataname)
  visit_var_user_input <- get_input_order("visit_var", visit_var$dataname)
  anrind_var_user_input <- get_input_order("anrind_var", anrind_var$dataname)
  bnrind_var_user_input <- get_input_order("bnrind_var", bnrind_var$dataname)
  ontrtfl_var_user_input <- get_input_order("ontrtfl_var", ontrtfl_var$dataname)
  saffl_var_var_user_input <- get_input_order("saffl_var", saffl_var$dataname)

  # validate inputs
  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- arm_var_user_input()
    input_paramcd_var <- paramcd_var_user_input()
    input_visit_var <- visit_var_user_input()
    input_anrind_var <- anrind_var_user_input()
    input_bnrind_var <- bnrind_var_user_input()
    input_ontrtfl_var <- ontrtfl_var_user_input()
    input_saffl_var <- saffl_var_var_user_input()

    validate(
      need(input_arm_var, "Please select a treatment variable"),
      need(input_paramcd_var, "Please select parameter variable"),
      need(input_visit_var, "Please select visit variable"),
      need(input_anrind_var, "Please select analysis range indicator variable"),
      need(input_bnrind_var,  "Please select baseline reference range indicator variable"),
      need(input_ontrtfl_var, "Please select treatment record flag variable"),
      need(input_saffl_var, "Please select safety population flag variable"),

      need(length(input_arm_var) <= 2, "Please limit treatment variables within two"),
      if (length(input_arm_var) == 2) {
        need(
          is.factor(adsl_filtered[[input_arm_var[[2]]]]) & all(!adsl_filtered[[input_arm_var[[2]]]] %in% c(
            "", NA
          )),
          "Please check nested treatment variable which needs to be a factor without NA or empty strings."
        )
      },
      need(!is.null(input$numeric_stats), "Please select at least one statistic to display.")
    )

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_summarize_vars),
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

    sum_vars <- summary_user_input()

    my_calls <- template_shift_by_arm(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = arm_var_user_input(),
      paramcd_var = paramcd_var_user_input(),
      paramcd = input$paramcd,
      visit_var = visit_var_user_input(),
      visit = input$visit,
      anrind_var = anrind_var_user_input(),
      bnrind_var = bnrind_var_user_input(),
      ontrtfl_var = ontrtfl_var_user_input(),
      ontrtfl = input$ontrtfl,
      saffl_var = saffl_var_var_user_input(),
      saffl = saffl$saffl,
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
    datanames = get_extract_datanames(list(arm_var, summarize_vars)),
    modal_title = "R Code for the current Summary Table",
    code_header = label
  )
}

