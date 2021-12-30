#' Template: Grade Summary Table
#'
#' @inheritParams template_arguments
#' @param worst_flag_var (`character`)\cr name of the worst flag variable.
#' @param worst_flag_indicator (`character`)\cr value indicating worst grade.
#' @param anl_toxgrade_var (`character`)\cr name of the variable indicating the analysis toxicity grade.
#' @param base_toxgrade_var (`character`)\cr name of the variable indicating the base toxicity grade.
#' @param code_missing_baseline (`character`)\cr whether missing baseline should be considered
#' as grade 0.
#'
#' @seealso [tm_t_shift_by_grade()]
#'
template_shift_by_grade <- function(parentname,
                                    dataname,
                                    arm_var = "ARM",
                                    id_var = "USUBJID",
                                    visit_var = "AVISIT",
                                    worst_flag_var = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"),
                                    worst_flag_indicator = "Y",
                                    anl_toxgrade_var = "ATOXGR",
                                    base_toxgrade_var = "BTOXGR",
                                    paramcd = "PARAMCD",
                                    drop_arm_levels = TRUE,
                                    add_total = FALSE,
                                    na_level = "<Missing>",
                                    code_missing_baseline = FALSE,
                                    basic_table_args = teal.devel::basic_table_args()) {


  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(id_var),
    assertthat::is.string(visit_var),
    assertthat::is.string(worst_flag_indicator),
    is.character(worst_flag_var),
    assertthat::is.string(anl_toxgrade_var),
    assertthat::is.string(base_toxgrade_var),
    assertthat::is.string(paramcd),
    assertthat::is.flag(drop_arm_levels),
    assertthat::is.flag(add_total),
    assertthat::is.string(na_level)
  )

  worst_flag_var <- match.arg(worst_flag_var)

  y <- list()
  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        dplyr::filter(worst_flag_var == worst_flag_indicator),
      env = list(
        df = as.name(dataname),
        worst_flag_var = as.name(worst_flag_var),
        worst_flag_indicator = worst_flag_indicator
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
      dataname <- df_explicit_na(dataname, na_level = na_level),
      env = list(dataname = as.name("anl"),
                 na_level = na_level
                )
      )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- df_explicit_na(parentname, na_level = na_level),
      env = list(parentname = as.name(parentname),
                 na_level = na_level
                )
      )
  )

  by_visit_fl <- dplyr::if_else(worst_flag_var %in% c("WGRLOVFL", "WGRHIVFL"), TRUE, FALSE)

  data_list <- add_expr(
    data_list,
    substitute(
      by_visit <- by_visit_fl,
      env = list(
        by_visit_fl = by_visit_fl
      )
    )
  )


  # Create new grouping variables ATOXGR_GP, BTOXGR_GP
  if (!code_missing_baseline) {
    if (worst_flag_var %in% c("WGRLOVFL", "WGRLOFL")) {
      data_list <- add_expr(
        data_list,
        substitute(
          dataname <- dplyr::mutate(dataname,
            ATOXGR_GP = factor(dplyr::case_when(
              ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
              ATOXGR == -1 ~ "1",
              ATOXGR == -2 ~ "2",
              ATOXGR == -3 ~ "3",
              ATOXGR == -4 ~ "4",
              ATOXGR == na_level ~ "Missing"
            )),
            BTOXGR_GP = factor(dplyr::case_when(
              BTOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
              BTOXGR == -1 ~ "1",
              BTOXGR == -2 ~ "2",
              BTOXGR == -3 ~ "3",
              BTOXGR == -4 ~ "4",
              BTOXGR == na_level ~ "Missing"
            ))
          ),
          env = list(
            dataname = as.name("anl"),
            ATOXGR = as.name(anl_toxgrade_var),
            BTOXGR = as.name(base_toxgrade_var),
            na_level = na_level
          )
        )
      )
    } else {
      data_list <- add_expr(
        data_list,
        substitute(
          dataname <- dplyr::mutate(dataname,
            ATOXGR_GP = factor(dplyr::case_when(
              ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
              ATOXGR == 1 ~ "1",
              ATOXGR == 2 ~ "2",
              ATOXGR == 3 ~ "3",
              ATOXGR == 4 ~ "4",
              ATOXGR == na_level ~ "Missing"
            )),
            BTOXGR_GP = factor(dplyr::case_when(
              BTOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
              BTOXGR == 1 ~ "1",
              BTOXGR == 2 ~ "2",
              BTOXGR == 3 ~ "3",
              BTOXGR == 4 ~ "4",
              BTOXGR == na_level ~ "Missing"
              ))
          ),
          env = list(
            dataname = as.name("anl"),
            ATOXGR = as.name(anl_toxgrade_var),
            BTOXGR = as.name(base_toxgrade_var),
            na_level = na_level
          )
        )
      )
    }
  } else {
    if (worst_flag_var %in% c("WGRLOVFL", "WGRLOFL")) {
      data_list <- add_expr(
        data_list,
        substitute(
          dataname <- dplyr::mutate(dataname,
            ATOXGR_GP = factor(dplyr::case_when(
              ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
              ATOXGR == -1 ~ "1",
              ATOXGR == -2 ~ "2",
              ATOXGR == -3 ~ "3",
              ATOXGR == -4 ~ "4",
              ATOXGR == na_level ~ "Missing"
            )),
            BTOXGR_GP = factor(dplyr::case_when(
              BTOXGR %in% c(0, 1, 2, 3, 4, na_level) ~ "Not Low",
              BTOXGR == -1 ~ "1",
              BTOXGR == -2 ~ "2",
              BTOXGR == -3 ~ "3",
              BTOXGR == -4 ~ "4"
            ))
          ),
          env = list(
            dataname = as.name("anl"),
            ATOXGR = as.name(anl_toxgrade_var),
            BTOXGR = as.name(base_toxgrade_var),
            na_level = na_level
          )
        )
      )
    } else {
      data_list <- add_expr(
        data_list,
        substitute(
          dataname <- dplyr::mutate(dataname,
            ATOXGR_GP = factor(dplyr::case_when(
              ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
              ATOXGR == 1 ~ "1",
              ATOXGR == 2 ~ "2",
              ATOXGR == 3 ~ "3",
              ATOXGR == 4 ~ "4",
              ATOXGR == na_level ~ "Missing"
            )),
            BTOXGR_GP = factor(dplyr::case_when(
              BTOXGR %in% c(0, -1, -2, -3, -4, na_level) ~ "Not High",
              BTOXGR == 1 ~ "1",
              BTOXGR == 2 ~ "2",
              BTOXGR == 3 ~ "3",
              BTOXGR == 4 ~ "4"
            ))
          ),
          env = list(
            dataname = as.name("anl"),
            ATOXGR = as.name(anl_toxgrade_var),
            BTOXGR = as.name(base_toxgrade_var),
            na_level = na_level
          )
        )
      )
    }
  }

  data_list <- add_expr(
    data_list,
    substitute(
      dataname <- dplyr::mutate(
        dataname,
        ATOXGR_GP = factor(
          ATOXGR_GP,
          levels = c(
            dplyr::if_else(
              worst_flag_var %in% c("WGRLOVFL", "WGRLOFL"), "Not Low", "Not High"), "1", "2", "3", "4", "Missing"
            )
          ),
        BTOXGR_GP = factor(
          BTOXGR_GP,
          levels = c(
            dplyr::if_else(worst_flag_var %in% c("WGRLOVFL", "WGRLOFL"), "Not Low", "Not High"),
            "1",
            "2",
            "3",
            "4",
            "Missing"
          )
        )
      ),
      env = list(
        dataname = as.name("anl"),
        worst_flag_var = worst_flag_var
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      dataname <- var_relabel(
        dataname,
        PARAMCD = var_labels(dataname)[[paramcd]],
        AVISIT = var_labels(dataname)[[visit_var]],
        ATOXGR_GP = dplyr::if_else(by_visit_fl, "Grade at Visit", "Post-baseline Grade"),
        BTOXGR_GP = "Baseline Grade"
      ),
      env = list(
        dataname = as.name("anl"),
        paramcd = paramcd,
        visit_var = visit_var,
        by_visit_fl = by_visit_fl
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
    if (add_total) {
      substitute(
        expr = expr_basic_table_args %>%
          split_cols_by(
            var = arm_var,
            split_fun = add_overall_level("All Patients", first = FALSE)
          ) %>%
          add_colcounts(),
        env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
      )
    } else {
      substitute(
        expr = expr_basic_table_args %>%
          split_cols_by(var = arm_var) %>%
          add_colcounts(),
        env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
      )
    }
  )

  split_label <- substitute(
    expr = var_labels(dataname)[[paramcd]],
    env = list(
      dataname = as.name("anl"),
      paramcd = paramcd
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_rows_by(
        var = paramcd,
        split_fun = split_fun,
        label_pos = "topleft",
        split_label = split_label
      ),
      env = list(
        paramcd = paramcd,
        split_label = split_label
      )
    )
  )

  if (by_visit_fl) {
    split_label <- substitute(
      expr = var_labels(dataname)[[visit_var]],
      env = list(
        dataname = as.name("anl"),
        visit_var = visit_var
      )
    )

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = split_rows_by(
          visit_var,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = split_label
        ),
        env = list(
          visit_var = visit_var,
          split_label = split_label
        )
      )
    )
  }

  if (by_visit_fl) {
    by_var_gp <- "ATOXGR_GP"
  } else {
    by_var_gp <- "BTOXGR_GP"
  }

  split_label <- substitute(
    expr = var_labels(dataname)[[by_var_gp]],
    env = list(
      dataname = as.name("anl"),
      by_var_gp = by_var_gp
    )
  )
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_rows_by(
        var = by_var_gp,
        split_fun = split_fun,
        label_pos = "topleft",
        split_label = split_label
      ),
      env = list(
        by_var_gp = by_var_gp,
        split_label = split_label
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = summarize_num_patients(
        var = id_var,
        .stats = c("unique_count")
      ),
      env = list(
        id_var = id_var
      )
    )
  )

  count_var <- setdiff(c("ATOXGR_GP", "BTOXGR_GP"), by_var_gp)

  if (by_visit_fl) {
    indent <- 3L
  } else {
    indent <- 2L
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = count_occurrences(
        vars = count_var,
        denom = "n",
        drop = TRUE
      ) %>%
        append_varlabels(dataname, count_var, indent = indent),
      env = list(
        count_var = count_var,
        dataname = as.name("anl"),
        indent = indent
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = parent) %>%
        prune_table()
      result
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' Teal Module: Grade Summary Table
#'
#' @inheritParams module_arguments
#' @inheritParams template_shift_by_grade
#' @param visit_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr object with all available
#'   choices and preselected option for variable names that can be used as visit.
#' @param worst_flag_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr object with all available
#'   choices and preselected option for variable names that can be used as worst flag variable.
#' @param worst_flag_indicator ([teal::choices_selected()] or [teal::data_extract_spec])\cr value indicating
#' worst grade.
#' @param anl_toxgrade_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#' variable for analysis toxicity grade.
#' @param base_toxgrade_var ([teal::choices_selected()] or [teal::data_extract_spec])\cr
#' variable for baseline toxicity grade.
#'
#' @export
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adlb <- synthetic_cdisc_data("latest")$adlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADLB", adlb,
#'                   code = 'ADLB <- synthetic_cdisc_data("latest")$adlb'),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_shift_by_grade(
#'       label = "Grade Laboratory Abnormality Table",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(adlb, "PARAMCD", "PARAM"),
#'         selected = "ALT"
#'       ),
#'       worst_flag_var = choices_selected(
#'         choices = variable_choices(adlb, subset = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL")),
#'         selected = c("WGRLOVFL")
#'       ),
#'       worst_flag_indicator = choices_selected(
#'         value_choices(adlb, "WGRLOVFL"), selected = "Y", fixed = TRUE
#'       ),
#'       anl_toxgrade_var = choices_selected(
#'         choices = variable_choices(adlb, subset = c("ATOXGR")),
#'         selected = c("ATOXGR"),
#'         fixed = TRUE
#'       ),
#'       base_toxgrade_var = choices_selected(
#'         choices = variable_choices(adlb, subset = c("BTOXGR")),
#'         selected = c("BTOXGR"),
#'         fixed = TRUE
#'       ),
#'       add_total = FALSE
#'    )
#'  ),
#'   filter = list(
#'     ADSL = list(SAFFL = "Y")
#'   )
#' )
#'
#'  \dontrun{
#' shinyApp(app$ui, app$server)
#' }

tm_t_shift_by_grade <- function(label,
                                dataname,
                                parentname = ifelse(inherits(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                                arm_var,
                                visit_var = choices_selected(
                                  variable_choices(dataname, subset = "AVISIT"), selected = "AVISIT", fixed = TRUE
                                ),
                                paramcd,
                                worst_flag_var = choices_selected(
                                  variable_choices(dataname, subset = c(
                                    "WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"
                                    )
                                  ),
                                  selected = "WGRLOVFL"
                                ),
                                worst_flag_indicator = choices_selected(
                                  value_choices(dataname, "WGRLOVFL"), selected = "Y", fixed = TRUE
                                ),
                                anl_toxgrade_var = choices_selected(
                                  variable_choices(dataname, subset = c("ATOXGR")), selected = c("ATOXGR"), fixed = TRUE
                                ),
                                base_toxgrade_var = choices_selected(
                                  variable_choices(dataname, subset = c("BTOXGR")), selected = c("BTOXGR"), fixed = TRUE
                                ),
                                id_var = choices_selected(
                                  variable_choices(dataname, subset = "USUBJID"), selected = "USUBJID", fixed = TRUE
                                ),
                                add_total = FALSE,
                                drop_arm_levels = TRUE,
                                pre_output = NULL,
                                post_output = NULL,
                                na_level = "<Missing>",
                                code_missing_baseline = FALSE,
                                basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_shift_by_grade")
  stop_if_not(
    assertthat::is.string(dataname),
    is.choices_selected(arm_var),
    is.choices_selected(paramcd),
    is.choices_selected(worst_flag_var),
    is.choices_selected(worst_flag_indicator),
    is.choices_selected(anl_toxgrade_var),
    is.choices_selected(base_toxgrade_var),
    is.choices_selected(id_var),
    assertthat::is.flag(add_total),
    assertthat::is.flag(drop_arm_levels),
    assertthat::is.string(na_level),
    assertthat::is.flag(code_missing_baseline),
    list(
      is.null(pre_output) || inherits(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || inherits(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    visit_var = cs_to_des_select(visit_var, dataname = dataname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE),
    worst_flag_var = cs_to_des_select(worst_flag_var, dataname = dataname),
    anl_toxgrade_var = cs_to_des_select(anl_toxgrade_var, dataname = dataname),
    base_toxgrade_var = cs_to_des_select(base_toxgrade_var, dataname = dataname)
  )

  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_shift_by_grade,
    server = srv_t_shift_by_grade,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        na_level = na_level,
        code_missing_baseline,
        basic_table_args = basic_table_args
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_shift_by_grade <- function(id, ...) {

  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- is_single_dataset(
    a$arm_var,
    a$id_var,
    a$visit_var,
    a$paramcd,
    a$worst_flag_var,
    a$worst_flag_indicator,
    a$anl_toxgrade_var,
    a$base_toxgrade_var
  )

  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(
        a[c("arm_var", "id_var", "visit_var", "paramcd", "worst_flag_var", "anl_toxgrade_var", "base_toxgrade_var")]
        ),
      data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = FALSE),
      data_extract_ui(
        id = ns("paramcd"),
        label = "Select Lab Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("worst_flag_var"),
        label = "Worst flag variable",
        data_extract_spec = a$worst_flag_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("visit_var"),
        label = "Analysis Visit",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("anl_toxgrade_var"),
        label = "Analysis toxicity grade",
        data_extract_spec = a$anl_toxgrade_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("base_toxgrade_var"),
        label = "Baseline toxicity grade",
        data_extract_spec = a$base_toxgrade_var,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        panel_item(
          "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          ),
          checkboxInput(
            ns("code_missing_baseline"),
            label = "Code missing baseline records as grade 0",
            value = a$code_missing_baseline
          )
        )
      ),
      panel_group(
        panel_item(
          "Additional Variables Info",
          data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          optionalSelectInput(
            ns("worst_flag_indicator"),
            label = "Value Indicating Worst Grade",
            choices = a$worst_flag_indicator$choices,
            selected = a$worst_flag_indicator$selected,
            multiple = FALSE,
            fixed = a$worst_flag_indicator$fixed
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
srv_t_shift_by_grade <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 parentname,
                                 arm_var,
                                 visit_var,
                                 paramcd,
                                 worst_flag_var,
                                 anl_toxgrade_var,
                                 base_toxgrade_var,
                                 id_var,
                                 add_total,
                                 drop_arm_levels,
                                 na_level,
                                 input_code_missing_baseline,
                                 label,
                                 basic_table_args) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(
      arm_var = arm_var,
      visit_var = visit_var,
      id_var = id_var,
      paramcd = paramcd,
      worst_flag_var = worst_flag_var,
      anl_toxgrade_var = anl_toxgrade_var,
      base_toxgrade_var = base_toxgrade_var
    ),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var = arm_var),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_visit_var <- as.vector(anl_m$columns_source$visit_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]
    input_paramcd_var <- anl_m$data()[[as.vector(anl_m$columns_source$paramcd)]]
    input_worst_flag_var <- as.vector(anl_m$columns_source$worst_flag_var)
    input_worst_flag <- anl_m$data()[[as.vector(anl_m$columns_source$worst_flag_var)]]
    input_anl_toxgrade_var <- as.vector(anl_m$columns_source$anl_toxgrade_var)
    input_base_toxgrade_var <- as.vector(anl_m$columns_source$base_toxgrade_var)

    validate(
      need(input_worst_flag_var, "Please select the worst flag variable."),
      need(input_paramcd_var, "Please select Laboratory parameter."),
      need(input_id_var, "Please select a subject identifier."),
      need(input$worst_flag_indicator, "Please select the value indicating worst grade."),
      need(
        any(input_worst_flag == input$worst_flag_indicator),
        "There's no positive flag, please select another flag parameter."
      )
    )

    # validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c(
        "USUBJID", "STUDYID", input_visit_var, input_paramcd, input_worst_flag_var,
        input_anl_toxgrade_var, input_base_toxgrade_var
        ),
      arm_var = input_arm_var
    )

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

    my_calls <- template_shift_by_grade(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      visit_var = as.vector(anl_m$columns_source$visit_var),
      id_var = as.vector(anl_m$columns_source$id_var),
      worst_flag_var = as.vector(anl_m$columns_source$worst_flag_var),
      worst_flag_indicator = input$worst_flag_indicator,
      anl_toxgrade_var = as.vector(anl_m$columns_source$anl_toxgrade_var),
      base_toxgrade_var = as.vector(anl_m$columns_source$base_toxgrade_var),
      paramcd = unlist(paramcd$filter)["vars_selected"],
      drop_arm_levels = input$drop_arm_levels,
      add_total = input$add_total,
      na_level = na_level,
      code_missing_baseline = input$code_missing_baseline,
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
      list(arm_var, id_var, paramcd, worst_flag_var,
           anl_toxgrade_var, base_toxgrade_var)
    ),
    modal_title = "R Code for Grade Laboratory Abnormalities",
    code_header = label
  )
}
