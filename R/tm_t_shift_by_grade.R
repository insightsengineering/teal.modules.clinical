#' Template: Grade Summary Table
#'
#' Creates a valid expression to generate a grade summary table.
#'
#' @inheritParams template_arguments
#' @param anl_toxgrade_var (`character`)\cr name of the variable indicating the analysis toxicity grade.
#' @param base_toxgrade_var (`character`)\cr name of the variable indicating the baseline toxicity grade.
#' @param code_missing_baseline (`logical`)\cr whether missing baseline grades should be counted as grade 0.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_shift_by_grade()]
#'
#' @keywords internal
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
                                    total_label = default_total_label(),
                                    na_level = tern::default_na_str(),
                                    code_missing_baseline = FALSE,
                                    basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(id_var)
  checkmate::assert_string(visit_var)
  checkmate::assert_string(worst_flag_indicator)
  checkmate::assert_character(worst_flag_var)
  checkmate::assert_string(anl_toxgrade_var)
  checkmate::assert_string(base_toxgrade_var)
  checkmate::assert_string(paramcd)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)

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
      expr = dataname <- tern::df_explicit_na(dataname, na_level = na_str),
      env = list(
        dataname = as.name("anl"),
        na_str = na_level
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- tern::df_explicit_na(parentname, na_level = na_str),
      env = list(
        parentname = as.name(parentname),
        na_str = na_level
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
              worst_flag_var %in% c("WGRLOVFL", "WGRLOFL"), "Not Low", "Not High"
            ), "1", "2", "3", "4", "Missing"
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
      expr = {
        column_labels <- list(
          PARAMCD = teal.data::col_labels(dataname, fill = FALSE)[[paramcd]],
          AVISIT = teal.data::col_labels(dataname, fill = FALSE)[[visit_var]],
          ATOXGR_GP = dplyr::if_else(by_visit_fl, "Grade at Visit", "Post-baseline Grade"),
          BTOXGR_GP = "Baseline Grade"
        )
        teal.data::col_labels(dataname)[names(column_labels)] <- as.character(column_labels)
        dataname
      },
      env = list(
        dataname = as.name("anl"),
        paramcd = paramcd,
        visit_var = visit_var,
        by_visit_fl = by_visit_fl
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # layout start
  y$layout_prep <- quote(split_fun <- rtables::drop_split_levels)

  basic_table_args$title <- "Grade Summary Table"
  basic_table_args$subtitles <- paste("Worst Flag Variable:", worst_flag_var)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(show_colcounts = TRUE)
    )
  )

  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(
            var = arm_var,
            split_fun = add_overall_level(total_label, first = FALSE)
          ),
        env = list(
          arm_var = arm_var,
          total_label = total_label,
          expr_basic_table_args = parsed_basic_table_args
        )
      )
    } else {
      substitute(
        expr = expr_basic_table_args %>%
          rtables::split_cols_by(var = arm_var),
        env = list(arm_var = arm_var, expr_basic_table_args = parsed_basic_table_args)
      )
    }
  )

  split_label <- substitute(
    expr = teal.data::col_labels(dataname, fill = FALSE)[[paramcd]],
    env = list(
      dataname = as.name("anl"),
      paramcd = paramcd
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_rows_by(
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
      expr = teal.data::col_labels(dataname, fill = FALSE)[[visit_var]],
      env = list(
        dataname = as.name("anl"),
        visit_var = visit_var
      )
    )

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::split_rows_by(
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
    expr = teal.data::col_labels(dataname, fill = FALSE)[[by_var_gp]],
    env = list(
      dataname = as.name("anl"),
      by_var_gp = by_var_gp
    )
  )
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_rows_by(
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
      expr = tern::summarize_num_patients(
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
      expr = tern::count_occurrences(
        vars = count_var,
        denom = "n",
        drop = TRUE,
        .indent_mods = 4L
      ) %>%
        tern::append_varlabels(dataname, count_var, indent = indent),
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
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent) %>%
        rtables::prune_table()
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' teal Module: Grade Summary Table
#'
#' This module produces a summary table of worst grades per subject by visit and parameter.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_shift_by_grade
#' @param anl_toxgrade_var ([teal.transform::choices_selected()])\cr
#'   variable for analysis toxicity grade.
#' @param base_toxgrade_var ([teal.transform::choices_selected()])\cr
#'   variable for baseline toxicity grade.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`TableTree` - output of `rtables::build_table()`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_shift_by_grade(
#'    ..., # arguments for module
#'    decorators = list(
#'      table = teal_transform_module(...) # applied only to `table` output
#'    )
#' )
#' ```
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.clinical")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADLB <- tmc_ex_adlb
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_shift_by_grade(
#'       label = "Grade Laboratory Abnormality Table",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADSL, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(ADLB, "PARAMCD", "PARAM"),
#'         selected = "ALT"
#'       ),
#'       worst_flag_var = choices_selected(
#'         choices = variable_choices(ADLB, subset = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL")),
#'         selected = c("WGRLOVFL")
#'       ),
#'       worst_flag_indicator = choices_selected(
#'         value_choices(ADLB, "WGRLOVFL"),
#'         selected = "Y", fixed = TRUE
#'       ),
#'       anl_toxgrade_var = choices_selected(
#'         choices = variable_choices(ADLB, subset = c("ATOXGR")),
#'         selected = c("ATOXGR"),
#'         fixed = TRUE
#'       ),
#'       base_toxgrade_var = choices_selected(
#'         choices = variable_choices(ADLB, subset = c("BTOXGR")),
#'         selected = c("BTOXGR"),
#'         fixed = TRUE
#'       ),
#'       add_total = FALSE
#'     )
#'   ),
#'   filter = teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y"))
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_shift_by_grade <- function(label,
                                dataname,
                                parentname = ifelse(
                                  inherits(arm_var, "data_extract_spec"),
                                  teal.transform::datanames_input(arm_var),
                                  "ADSL"
                                ),
                                arm_var,
                                visit_var = teal.transform::choices_selected(
                                  teal.transform::variable_choices(dataname, subset = "AVISIT"),
                                  selected = "AVISIT", fixed = TRUE
                                ),
                                paramcd,
                                worst_flag_var = teal.transform::choices_selected(
                                  teal.transform::variable_choices(dataname, subset = c(
                                    "WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"
                                  )),
                                  selected = "WGRLOVFL"
                                ),
                                worst_flag_indicator = teal.transform::choices_selected(
                                  teal.transform::value_choices(dataname, "WGRLOVFL"),
                                  selected = "Y", fixed = TRUE
                                ),
                                anl_toxgrade_var = teal.transform::choices_selected(
                                  teal.transform::variable_choices(dataname, subset = c("ATOXGR")),
                                  selected = c("ATOXGR"), fixed = TRUE
                                ),
                                base_toxgrade_var = teal.transform::choices_selected(
                                  teal.transform::variable_choices(dataname, subset = c("BTOXGR")),
                                  selected = c("BTOXGR"), fixed = TRUE
                                ),
                                id_var = teal.transform::choices_selected(
                                  teal.transform::variable_choices(dataname, subset = "USUBJID"),
                                  selected = "USUBJID", fixed = TRUE
                                ),
                                add_total = FALSE,
                                total_label = default_total_label(),
                                drop_arm_levels = TRUE,
                                pre_output = NULL,
                                post_output = NULL,
                                na_level = tern::default_na_str(),
                                code_missing_baseline = FALSE,
                                basic_table_args = teal.widgets::basic_table_args(),
                                transformators = list(),
                                decorators = list()) {
  message("Initializing tm_t_shift_by_grade")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(visit_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(worst_flag_var, "choices_selected")
  checkmate::assert_class(worst_flag_indicator, "choices_selected")
  checkmate::assert_class(anl_toxgrade_var, "choices_selected")
  checkmate::assert_class(base_toxgrade_var, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_flag(code_missing_baseline)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    visit_var = cs_to_des_select(visit_var, dataname = dataname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE),
    worst_flag_var = cs_to_des_select(worst_flag_var, dataname = dataname),
    anl_toxgrade_var = cs_to_des_select(anl_toxgrade_var, dataname = dataname),
    base_toxgrade_var = cs_to_des_select(base_toxgrade_var, dataname = dataname)
  )

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
        total_label = total_label,
        na_level = na_level,
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_t_shift_by_grade <- function(id, ...) {
  ns <- NS(id)
  a <- list(...) # module args

  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$id_var,
    a$visit_var,
    a$paramcd,
    a$worst_flag_var,
    a$worst_flag_indicator,
    a$anl_toxgrade_var,
    a$base_toxgrade_var
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(
        a[c("arm_var", "id_var", "visit_var", "paramcd", "worst_flag_var", "anl_toxgrade_var", "base_toxgrade_var")]
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = FALSE),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Lab Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("worst_flag_var"),
        label = "Worst flag variable",
        data_extract_spec = a$worst_flag_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("visit_var"),
        label = "Analysis Visit",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("anl_toxgrade_var"),
        label = "Analysis toxicity grade",
        data_extract_spec = a$anl_toxgrade_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("base_toxgrade_var"),
        label = "Baseline toxicity grade",
        data_extract_spec = a$base_toxgrade_var,
        is_single_dataset = is_single_dataset_value
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
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
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          teal.transform::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.widgets::optionalSelectInput(
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
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_shift_by_grade <- function(id,
                                 data,
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
                                 total_label,
                                 drop_arm_levels,
                                 na_level,
                                 label,
                                 basic_table_args,
                                 decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        visit_var = visit_var,
        id_var = id_var,
        paramcd = paramcd,
        worst_flag_var = worst_flag_var,
        anl_toxgrade_var = anl_toxgrade_var,
        base_toxgrade_var = base_toxgrade_var
      ),
      datasets = data,
      select_validation_rule = list(
        base_toxgrade_var = shinyvalidate::sv_required("A baseline toxicity grade is required"),
        anl_toxgrade_var = shinyvalidate::sv_required("An analysis toxicity grade is required"),
        visit_var = shinyvalidate::sv_required("An analysis visit is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required"),
        worst_flag_var = shinyvalidate::sv_required("A worst treatment flag is required"),
        id_var = shinyvalidate::sv_required("A subject identifier is required.")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("A laboratory parameter is required")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("worst_flag_indicator", shinyvalidate::sv_required("Please select the value indicating worst grade."))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Grade Summary Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    validate_checks <- reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_id_var <- names(merged$anl_input_r()$columns_source$id_var)
      input_visit_var <- names(merged$anl_input_r()$columns_source$visit_var)
      input_paramcd_var <- names(merged$anl_input_r()$columns_source$paramcd)
      input_paramcd <- unlist(merged$anl_input_r()$filter_info$paramcd[[1]]$selected)
      input_worst_flag_var <- names(merged$anl_input_r()$columns_source$worst_flag_var)
      input_anl_toxgrade_var <- names(merged$anl_input_r()$columns_source$anl_toxgrade_var)
      input_base_toxgrade_var <- names(merged$anl_input_r()$columns_source$base_toxgrade_var)

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_visit_var, input_paramcd_var, input_worst_flag_var,
          input_anl_toxgrade_var, input_base_toxgrade_var
        ),
        arm_var = input_arm_var
      )

      # Additional validation for tm_t_shift_by_grade specific issues
      # Check if worst flag filtering will result in non-empty dataset
      if (input_worst_flag_var %in% names(anl_filtered) && length(input$worst_flag_indicator) > 0) {
        available_worst_flag_values <- unique(anl_filtered[[input_worst_flag_var]])
        teal::validate_in(
          input$worst_flag_indicator,
          available_worst_flag_values,
          paste0(
            "The selected worst flag indicator '", input$worst_flag_indicator,
            "' is not found in the filtered data. Available values: ",
            paste(available_worst_flag_values, collapse = ", ")
          )
        )

        # Check if filtering by worst flag will result in any records
        worst_flag_filtered_data <- anl_filtered[anl_filtered[[input_worst_flag_var]] == input$worst_flag_indicator, ]
        validate(
          shiny::need(
            nrow(worst_flag_filtered_data) > 0,
            paste0(
              "No records found with worst flag indicator '", input$worst_flag_indicator,
              "' in variable '", input_worst_flag_var, "'. Please check your data or filters."
            )
          )
        )
      }

      # Check if selected PARAMCD values exist in the filtered dataset
      if (length(input_paramcd) > 0 && input_paramcd_var %in% names(anl_filtered)) {
        available_paramcd_values <- unique(anl_filtered[[input_paramcd_var]])
        teal::validate_in(
          input_paramcd,
          available_paramcd_values,
          paste0(
            "The following selected lab parameters are not available in the filtered dataset: ",
            paste(setdiff(input_paramcd, available_paramcd_values), collapse = ", "),
            ". Available parameters: ",
            paste(available_paramcd_values, collapse = ", ")
          )
        )

        # Check if records exist for the selected PARAMCD values
        paramcd_filtered_data <- anl_filtered[anl_filtered[[input_paramcd_var]] %in% input_paramcd, ]
        validate(
          shiny::need(
            nrow(paramcd_filtered_data) > 0,
            paste0(
              "No records found for the selected lab parameters: ",
              paste(input_paramcd, collapse = ", "),
              ". Please check your data or filters."
            )
          )
        )
      }
    })

    # Generate r code for the analysis.
    all_q <- reactive({
      validate_checks()

      my_calls <- template_shift_by_grade(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = names(merged$anl_input_r()$columns_source$arm_var),
        visit_var = names(merged$anl_input_r()$columns_source$visit_var),
        id_var = names(merged$anl_input_r()$columns_source$id_var),
        worst_flag_var = names(merged$anl_input_r()$columns_source$worst_flag_var),
        worst_flag_indicator = input$worst_flag_indicator,
        anl_toxgrade_var = names(merged$anl_input_r()$columns_source$anl_toxgrade_var),
        base_toxgrade_var = names(merged$anl_input_r()$columns_source$base_toxgrade_var),
        paramcd = unlist(paramcd$filter)["vars_selected"],
        drop_arm_levels = input$drop_arm_levels,
        add_total = input$add_total,
        total_label = total_label,
        na_level = na_level,
        code_missing_baseline = input$code_missing_baseline,
        basic_table_args = basic_table_args
      )

      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Decoration of table output.
    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    # Outputs to render.
    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )


    decorated_table_q
  })
}
