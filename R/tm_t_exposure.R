
#' Template: Exposure Table for Risk management plan
#'
#' @inheritParams template_arguments
#' @param row_by_var (`character`)\cr
#'   variable name used to split the rows.
#' @param col_by_var (`character`)\cr
#'   variable name used to split the columns.
#' @param avalu_var (`character`)\cr
#'   name of the analysis unit variable.
#' @param drop_levels (`flag`)\cr
#'   whether empty rows should be removed from the table.
#' @seealso [tm_t_exposure()]

template_exposure <- function(parentname,
                              dataname,
                              id_var,
                              paramcd,
                              row_by_var,
                              col_by_var,
                              add_total = FALSE,
                              drop_levels = TRUE,
                              na_level = "<Missing>",
                              aval_var,
                              avalu_var,
                              basic_table_args = teal.devel::basic_table_args()) {

  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(row_by_var),
    is.string(col_by_var) || is_empty(col_by_var),
    is.string(paramcd),
    is.string(id_var),
    is.flag(add_total),
    is.string(na_level),
    is.string(aval_var),
    is.string(avalu_var) || is_empty(avalu_var),
    is.flag(drop_levels)
  )

  y <- list()
  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- dataname,
      env = list(
        dataname = as.name(dataname)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      dataname <- df_explicit_na(dataname, na_level = na_level),
      env = list(dataname = as.name("anl"),
                 na_level = na_level
      )
    )
  )
  y$data <- bracket_expr(data_list)

  #layout start
  y$layout_prep <- quote(split_fun <- drop_split_levels)

  parsed_basic_table_args <- parse_basic_table_args(
    resolve_basic_table_args(
      user_table = basic_table_args
    )
  )

  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  if (!is_empty(col_by_var)) {
    if (add_total) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          split_cols_by(col_by_var, split_fun = add_overall_level("Total", first = FALSE)),
          env = list(
            col_by_var = col_by_var
          )
        )
      )
    } else {
      layout_list <- add_expr(
        layout_list,
        substitute(
          split_cols_by(col_by_var),
          env = list(
            col_by_var = col_by_var
          )
        )
      )
    }
  }

  layout_list <- add_expr(
    layout_list,
    quote(add_colcounts())
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_patients_exposure_in_cols(
        var = aval_var, col_split = TRUE,
        .labels = c(
          n_patients = "Patients",
          sum_exposure = paste("Sum of", paramcd, sprintf("(%s)", avalu_var))
          )
        ),
      env = list(
        aval_var = aval_var,
        avalu_var = avalu_var,
        paramcd = paramcd
      )
    )
  )

  split_label <- substitute(
    expr = var_labels(dataname[row_by_var], fill = TRUE),
    env = list(
      dataname = as.name(dataname),
      row_by_var = row_by_var
    )
  )

  if (drop_levels) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by(
          row_by_var, label_pos = "topleft", split_fun = split_fun, split_label = split_label, nested = FALSE
          ),
        env = list(
          row_by_var = row_by_var,
          split_label = split_label
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by(
          row_by_var, label_pos = "topleft", split_label = split_label, nested = FALSE
        ),
        env = list(
          row_by_var = row_by_var,
          split_label = split_label
        )
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_patients_exposure_in_cols(
        var = aval_var,
        col_split = FALSE),
      env = list(
        aval_var = aval_var
      )
    )
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

#' Teal module: Exposure Table for Risk management plan
#'
#' @inheritParams module_arguments
#' @inheritParams template_exposure
#' @param row_by_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#'   object with all available choices and preselected option for
#'   variable names that can be used to split rows.
#' @param col_by_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#'   object with all available choices and preselected option for
#'   variable names that can be used to split columns.
#' @param parcat ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#'   object with all available choices and preselected option for
#'   parameter category values.
#' @param avalu_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#'   object with the analysis unit variable.
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#' library(dplyr)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adex <- synthetic_cdisc_data("latest")$adex
#'
#' set.seed(1, kind = "Mersenne-Twister")
#' labels <- var_labels(adex)
#' adex <- adex %>%
#'  distinct(USUBJID, .keep_all = TRUE) %>%
#'   mutate(PARAMCD = "TDURD",
#'          PARAM = "Overall duration (days)",
#'          AVAL = sample(x = seq(1, 200), size = n(), replace = TRUE),
#'          AVALU = "Days") %>%
#'          bind_rows(adex)
#'  var_labels(adex) <- labels
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADEX", adex,
#'       code = 'set.seed(1, kind = "Mersenne-Twister")
#'       ADEX <- synthetic_cdisc_data("latest")$adex
#'       labels <- var_labels(ADEX)
#'       ADEX <- ADEX %>%
#'        distinct(USUBJID, .keep_all = TRUE) %>%
#'        mutate(PARAMCD = "TDURD",
#'               PARAM = "Overall duration (days)",
#'               AVAL = sample(x = seq(1, 200), size = n(), replace = TRUE),
#'               AVALU = "Days") %>%
#'               bind_rows(ADEX)
#'       var_labels(ADEX) <- labels'  #nolint
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_exposure(
#'       label = "Duration of Exposure Table",
#'       dataname = "ADEX",
#'       paramcd = choices_selected(
#'         choices = value_choices(adex, "PARAMCD", "PARAM"),
#'         selected = "TDURD"
#'       ),
#'       col_by_var = choices_selected(
#'         choices = variable_choices(adex, subset = c("SEX", "ARM")),
#'         selected = "SEX"
#'       ),
#'       row_by_var = choices_selected(
#'         choices = variable_choices(adex, subset = c("RACE", "REGION1", "STRATA1", "SEX")),
#'         selected = "RACE"
#'       ),
#'       parcat = choices_selected(
#'         choices = value_choices(adex, "PARCAT2"),
#'         selected = "Drug A"
#'       ),
#'       add_total = FALSE
#'     )
#'   ),
#'  filter = list(
#'    ADSL = list(SAFFL = "Y")
#'  )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'

tm_t_exposure <- function(label,
                          dataname,
                          parentname = ifelse(
                            is(col_by_var, "data_extract_spec"),
                            datanames_input(col_by_var),
                            "ADSL"
                            ),
                          row_by_var,
                          col_by_var,
                          paramcd = choices_selected(
                            choices = value_choices(dataname, "PARAMCD", "PARAM"),
                            selected = "TDURD"
                          ),
                          id_var = choices_selected(
                            variable_choices(dataname, subset = "USUBJID"),
                            selected = "USUBJID",
                            fixed = TRUE
                          ),
                          parcat,
                          aval_var = choices_selected(
                            variable_choices(dataname, subset = "AVAL"),
                            selected = "AVAL",
                            fixed = TRUE
                          ),
                          avalu_var = choices_selected(
                            variable_choices(dataname, subset = "AVALU"),
                            selected = "AVALU",
                            fixed = TRUE
                          ),
                          add_total,
                          na_level = "<Missing>",
                          pre_output = NULL,
                          post_output = NULL,
                          basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_exposure")
  stop_if_not(
    is.string(dataname),
    is.flag(add_total),
    is.choices_selected(paramcd),
    is.choices_selected(row_by_var),
    is.choices_selected(col_by_var),
    is.choices_selected(id_var),
    is.choices_selected(parcat),
    is.choices_selected(aval_var),
    is.choices_selected(avalu_var),
    is.string(na_level),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  data_extract_list <- list(
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    row_by_var = cs_to_des_select(row_by_var, dataname = dataname),
    col_by_var = cs_to_des_select(col_by_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    parcat = cs_to_des_filter(parcat, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    avalu_var = cs_to_des_select(avalu_var, dataname = dataname)
  )

  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())
  module(
    label = label,
    ui = ui_t_exposure,
    server = srv_t_exposure,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        na_level = na_level,
        basic_table_args = basic_table_args
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}


#' @noRd
ui_t_exposure <- function(id, ...) {

  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- is_single_dataset(
    a$paramcd,
    a$col_by_var,
    a$row_by_var,
    a$id_var,
    a$parcat,
    a$aval_var,
    a$avalu_var
  )

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c(
        "paramcd", "col_by_var", "row_by_var", "id_var", "parcat", "aval_var", "avalu_var"
        )]),
      data_extract_ui(
        id = ns("paramcd"),
        label = "Select the Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("parcat"),
        label = "Select the Parameter Category",
        data_extract_spec = a$parcat,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("col_by_var"),
        label = "Select Column by Variable",
        data_extract_spec = a$col_by_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("row_by_var"),
        label = "Select Row by Variable",
        data_extract_spec = a$row_by_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      panel_group(
        panel_item(
          "Additional Variables Info",
          data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_ui(
            id = ns("aval_var"),
            label = "Analysis Value Variable",
            data_extract_spec = a$aval_var,
            is_single_dataset = is_single_dataset_value
          ),
          data_extract_ui(
            id = ns("avalu_var"),
            label = "Analysis Value Unit Variable",
            data_extract_spec = a$avalu_var,
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
srv_t_exposure <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           parentname,
                           paramcd,
                           id_var,
                           row_by_var,
                           col_by_var,
                           parcat,
                           aval_var,
                           avalu_var,
                           na_level,
                           label,
                           basic_table_args = basic_table_args) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(
      id_var = id_var,
      paramcd = paramcd,
      row_by_var = row_by_var,
      col_by_var = col_by_var,
      parcat = parcat,
      aval_var = aval_var,
      avalu_var = avalu_var
    ),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(col_by_var = col_by_var),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
    anl_m <- anl_merged()
    anl_adsl <- adsl_merged()

    input_paramcd <- unlist(paramcd$filter)["vars_selected"]
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_row_by_var <- as.vector(anl_m$columns_source$row_by_var)
    input_col_by_var <- as.vector(anl_adsl$columns_source$col_by_var)
    input_parcat <- unlist(parcat$filter)["vars_selected"]
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_avalu_var <- as.vector(anl_m$columns_source$avalu_var)

    validate(
      need(input_row_by_var, "Please select a row by variable."),
      need(input_aval_var, "Please select an analysis variable."),
      need(input_avalu_var, "Please select an analysis unit variable."),
      need(
        input[[extract_input("parcat", parcat$filter[[1]]$dataname, filter = TRUE)]],
        "Please select a parameter category value."
        ),
      need(
        input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]],
        "Please select a parameter value."
        ),
      validate_no_intersection(
        input[[extract_input("col_by_var", parentname)]],
        input[[extract_input("row_by_var", dataname)]],
        "Column by and row by variables should not be the same."
      )
    )
    # validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_col_by_var),
      anl = anl_filtered,
      anlvars = c(
        "USUBJID", "STUDYID", input_id_var, input_paramcd,
        input_row_by_var, input_parcat, input_aval_var, input_avalu_var
        ),
      arm_var = NULL,
      need_arm = FALSE
    )
    NULL
  })

  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()
    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    input_avalu_var <- as.character(
      unique(anl_m$data()[[as.vector(anl_m$columns_source$avalu_var)]])
      )
    input_paramcd <- as.character(
      unique(anl_m$data()[[as.vector(anl_m$columns_source$paramcd)]])
      )
    my_calls <- template_exposure(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      id_var <- as.vector(anl_m$columns_source$id_var),
      paramcd <- as.vector(anl_m$columns_source$paramcd),
      row_by_var <- as.vector(anl_m$columns_source$row_by_var),
      col_by_var <- as.vector(anl_m$columns_source$col_by_var),
      add_total = input$add_total,
      drop_levels = TRUE,
      na_level = na_level,
      aval_var <- as.vector(anl_m$columns_source$aval_var),
      avalu_var <- input_avalu_var,
      basic_table_args = basic_table_args
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
    datanames = get_extract_datanames(
      list(id_var, paramcd, row_by_var, col_by_var, parcat, aval_var, avalu_var)
    ),
    modal_title = "R Code for Risk Management Plan Table",
    code_header = label
  )
}
