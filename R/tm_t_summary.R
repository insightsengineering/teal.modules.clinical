#' Template: Summary of Variables
#'
#' @param show_labels (`character`)\cr
#'   defines whether the labels for `sum_vars` should display. For details see [rtables::analyze()].
#' @inheritParams template_arguments
#'
#' @seealso [tm_t_summary()]
#'
template_summary <- function(dataname,
                             parentname,
                             arm_var,
                             sum_vars,
                             show_labels = c("default", "visible", "hidden"),
                             add_total = TRUE,
                             var_labels = character(),
                             na.rm = FALSE,  #nolint
                             na_level = "<Missing>",
                             denominator = c("N", "n", "omit"),
                             drop_arm_levels = TRUE) {
  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.character(sum_vars),
    is.flag(add_total),
    is.character(var_labels),
    is.flag(na.rm),
    is.string(na_level),
    is.flag(drop_arm_levels)
  )
  denominator <- match.arg(denominator)
  show_labels <- match.arg(show_labels)

  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        df_explicit_na(omit_columns = setdiff(names(df), c(sum_vars)), na_level = na_level),
      env = list(
        df = as.name(dataname),
        sum_vars = sum_vars,
        na_level = na_level
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

  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    quote(basic_table())
  )
  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = split_cols_by(arm_var, split_fun = add_overall_level(label = "All Patients", first = FALSE)),
        env = list(arm_var = arm_var)
      )
    } else {
      substitute(
        expr = split_cols_by(arm_var),
        env = list(arm_var = arm_var)
      )
    }
  )
  layout_list <- add_expr(
    layout_list,
    quote(add_colcounts())
  )

  env_sum_vars <- list(
    sum_vars = sum_vars,
    sum_var_labels = var_labels[sum_vars],
    show_labels = show_labels,
    na.rm = na.rm,
    na_level = na_level,
    denom = ifelse(denominator == "n", "n", "N_col"),
    stats = c(
      c("n", "mean_sd", "median", "range"),
      ifelse(denominator == "omit", "count", "count_fraction")
    )
  )

  layout_list <- add_expr(
    layout_list,
    if (length(var_labels) > 0) {
      substitute(
        expr = summarize_vars(
          vars = sum_vars,
          var_labels = sum_var_labels,
          show_labels = show_labels,
          na.rm = na.rm,
          na_level = na_level,
          denom = denom,
          .stats = stats
        ),
        env = env_sum_vars
      )
    } else {
      substitute(
        expr = summarize_vars(
          vars = sum_vars,
          show_labels = show_labels,
          na.rm = na.rm,
          na_level = na_level,
          denom = denom,
          .stats = stats
        ),
        env = env_sum_vars
      )
    }
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      result
    },
    env = list(parent = as.name(parentname))
  )

  y
}



#' Teal Module: Summary of Variables
#'
#' @inheritParams module_arguments
#'
#' @export
#' @examples
#' # Preparation of the test case.
#' library(dplyr)
#' library(random.cdisc.data)
#' library(tern)
#'
#' adsl <- radsl(cached = TRUE)
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
#'     code = 'ADSL <- radsl(cached = TRUE)',
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_summary(
#'       label = "Demographic Table",
#'       dataname = "ADSL",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       add_total = TRUE,
#'       summarize_vars = choices_selected(
#'         c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS"),
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
tm_t_summary <- function(label,
                         dataname,
                         parentname = ifelse(
                           is(arm_var, "data_extract_spec"),
                           datanames_input(arm_var),
                           "ADSL"
                         ),
                         arm_var,
                         summarize_vars,
                         add_total = TRUE,
                         useNA = c("ifany", "no"), # nolint
                         na_level = "<Missing>",
                         denominator = c("N", "n", "omit"),
                         drop_arm_levels = TRUE,
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
  denominator <- match.arg(denominator)

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    summarize_vars = cs_to_des_select(summarize_vars, dataname = dataname, multiple = TRUE)
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
ui_summary <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(a$arm_var, a$summarize_vars)

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "summarize_vars")]),
      data_extract_input(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      data_extract_input(
        id = ns("summarize_vars"),
        label = "Summarize Variables",
        data_extract_spec = a$summarize_vars,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        panel_item(
          "Additional table settings",
          radioButtons(
            ns("useNA"),
            label = "Display NA counts",
            choices = c("ifany", "no"),
            selected = a$useNA
          ),
          radioButtons(
            ns("denominator"),
            label = "Denominator choice",
            choices = c("N", "n", "omit"),
            selected = a$denominator
          ),
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
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
srv_summary <- function(input,
                        output,
                        session,
                        datasets,
                        dataname,
                        parentname,
                        arm_var,
                        summarize_vars,
                        add_total,
                        na_level,
                        drop_arm_levels,
                        label) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, summarize_vars),
    input_id = c("arm_var", "summarize_vars"),
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
    input_summarize_vars <- as.vector(anl_m$columns_source$summarize_vars)

    validate(
      need(input_arm_var, "Please select a treatment variable"),
      need(input_summarize_vars, "Please select a summarize variable")
    )

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_summarize_vars),
      arm_var = input_arm_var
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

    sum_vars <- as.vector(anl_m$columns_source$summarize_vars)

    my_calls <- template_summary(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      sum_vars = sum_vars,
      show_labels = "visible",
      add_total = input$add_total,
      var_labels = get_var_labels(datasets, dataname, sum_vars),
      na.rm = ifelse(input$useNA == "ifany", FALSE, TRUE), # nolint
      na_level = na_level,
      denominator = input$denominator,
      drop_arm_levels = input$drop_arm_levels
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
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
