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
#' @param arm_var ([teal.picks::variables()])\cr
#'   variable for treatment arm.
#' @param visit_var ([teal.picks::variables()])\cr
#'   variable for analysis visit.
#' @param paramcd ([teal.picks::variables()])\cr
#'   variable for lab parameter code. The `values()` element is added internally to allow
#'   users to filter the parameter values interactively.
#' @param worst_flag_var ([teal.picks::variables()])\cr
#'   variable for worst grade flag.
#' @param worst_flag_indicator ([teal.transform::choices_selected()])\cr
#'   value(s) indicating worst grade records.
#' @param anl_toxgrade_var ([teal.picks::variables()])\cr
#'   variable for analysis toxicity grade.
#' @param base_toxgrade_var ([teal.picks::variables()])\cr
#'   variable for baseline toxicity grade.
#' @param id_var ([teal.picks::variables()])\cr
#'   variable for subject identifier.
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
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_shift_by_grade(
#'       label = "Grade Laboratory Abnormality Table",
#'       dataname = "ADLB",
#'       arm_var = variables(choices = c("ARM", "ARMCD")),
#'       paramcd = variables(choices = "PARAMCD"),
#'       worst_flag_var = variables(
#'         choices = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"),
#'         selected = "WGRLOVFL"
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
                                parentname = "ADSL",
                                arm_var = variables(choices = c("ARM", "ARMCD")),
                                visit_var = variables(choices = "AVISIT"),
                                paramcd = variables(choices = "PARAMCD"),
                                worst_flag_var = variables(
                                  choices = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"),
                                  selected = "WGRLOVFL"
                                ),
                                worst_flag_indicator = teal.transform::choices_selected(
                                  c("Y", "N"),
                                  selected = "Y", fixed = TRUE
                                ),
                                anl_toxgrade_var = variables(choices = "ATOXGR"),
                                base_toxgrade_var = variables(choices = "BTOXGR"),
                                id_var = variables(choices = "USUBJID"),
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
  arm_var <- teal.picks::as.picks(arm_var, quiet = FALSE)
  visit_var <- teal.picks::as.picks(visit_var, quiet = FALSE)
  paramcd <- teal.picks::as.picks(paramcd, quiet = FALSE)
  worst_flag_var <- teal.picks::as.picks(worst_flag_var, quiet = FALSE)
  anl_toxgrade_var <- teal.picks::as.picks(anl_toxgrade_var, quiet = FALSE)
  base_toxgrade_var <- teal.picks::as.picks(base_toxgrade_var, quiet = FALSE)
  id_var <- teal.picks::as.picks(id_var, quiet = FALSE)
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(visit_var, "variables")
  checkmate::assert_class(paramcd, "variables")
  checkmate::assert_class(worst_flag_var, "variables")
  checkmate::assert_class(worst_flag_indicator, "choices_selected")
  checkmate::assert_class(anl_toxgrade_var, "variables")
  checkmate::assert_class(base_toxgrade_var, "variables")
  checkmate::assert_class(id_var, "variables")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_flag(code_missing_baseline)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  arm_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), arm_var)
  visit_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), visit_var)
  paramcd <- teal.picks::picks(teal.picks::datasets(dataname, dataname), paramcd, values())
  worst_flag_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), worst_flag_var)
  anl_toxgrade_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), anl_toxgrade_var)
  base_toxgrade_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), base_toxgrade_var)
  id_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), id_var)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_shift_by_grade,
    server = srv_t_shift_by_grade,
    ui_args = args[names(args) %in% names(formals(ui_t_shift_by_grade))],
    server_args = args[names(args) %in% names(formals(srv_t_shift_by_grade))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_t_shift_by_grade <- function(id,
                                arm_var,
                                visit_var,
                                paramcd,
                                worst_flag_var,
                                worst_flag_indicator,
                                anl_toxgrade_var,
                                base_toxgrade_var,
                                id_var,
                                add_total,
                                drop_arm_levels,
                                code_missing_baseline,
                                pre_output,
                                post_output,
                                decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Treatment Variable"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = add_total),
      tags$div(
        tags$label("Select Lab Parameter"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Worst flag variable"),
        teal.picks::picks_ui(ns("worst_flag_var"), worst_flag_var)
      ),
      tags$div(
        tags$label("Analysis Visit"),
        teal.picks::picks_ui(ns("visit_var"), visit_var)
      ),
      tags$div(
        tags$label("Analysis toxicity grade"),
        teal.picks::picks_ui(ns("anl_toxgrade_var"), anl_toxgrade_var)
      ),
      tags$div(
        tags$label("Baseline toxicity grade"),
        teal.picks::picks_ui(ns("base_toxgrade_var"), base_toxgrade_var)
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = drop_arm_levels
          ),
          checkboxInput(
            ns("code_missing_baseline"),
            label = "Code missing baseline records as grade 0",
            value = code_missing_baseline
          )
        )
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          teal.picks::picks_ui(ns("id_var"), id_var),
          teal.widgets::optionalSelectInput(
            ns("worst_flag_indicator"),
            label = "Value Indicating Worst Grade",
            choices = worst_flag_indicator$choices,
            selected = worst_flag_indicator$selected,
            multiple = FALSE,
            fixed = worst_flag_indicator$fixed
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
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
                                 worst_flag_indicator,
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

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        arm_var = arm_var,
        visit_var = visit_var,
        paramcd = paramcd,
        worst_flag_var = worst_flag_var,
        anl_toxgrade_var = anl_toxgrade_var,
        base_toxgrade_var = base_toxgrade_var,
        id_var = id_var
      ),
      data = data
    )

    anl_selectors <- selectors
    adsl_selectors <- selectors["arm_var"]

    data_with_card <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj
    })
    merged_anl <- merge_srv("merge_anl", data = data_with_card, selectors = anl_selectors, output_name = "ANL")
    merged_adsl_anl <- merge_srv(
      "merge_adsl_anl",
      data = merged_anl$data,
      selectors = adsl_selectors,
      output_name = "ANL_ADSL"
    )
    anl_q <- merged_adsl_anl$data

    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- anl_selectors$arm_var()$variables$selected
      input_id_var <- anl_selectors$id_var()$variables$selected
      input_visit_var <- anl_selectors$visit_var()$variables$selected
      input_paramcd_var <- anl_selectors$paramcd()$variables$selected
      input_worst_flag_var <- anl_selectors$worst_flag_var()$variables$selected
      input_anl_toxgrade_var <- anl_selectors$anl_toxgrade_var()$variables$selected
      input_base_toxgrade_var <- anl_selectors$base_toxgrade_var()$variables$selected

      shiny::validate(shiny::need(
        length(input_worst_flag_var) >= 1L,
        "Please select a worst flag variable."
      ))

      wf_vec <- as.vector(input_worst_flag_var)
      wf_col <- wf_vec[[1]]
      if (nzchar(wf_col) && wf_col %in% names(anl_filtered)) {
        shiny::validate(shiny::need(
          any(as.character(anl_filtered[[wf_col]]) == input$worst_flag_indicator, na.rm = TRUE),
          sprintf(
            "No observations with %s equal to the selected worst flag indicator (%s).",
            wf_col,
            input$worst_flag_indicator
          )
        ))
      }

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
    })

    # Generate r code for the analysis.
    all_q <- reactive({
      validate_checks()

      my_calls <- template_shift_by_grade(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = anl_selectors$arm_var()$variables$selected,
        visit_var = anl_selectors$visit_var()$variables$selected,
        id_var = anl_selectors$id_var()$variables$selected,
        worst_flag_var = anl_selectors$worst_flag_var()$variables$selected,
        worst_flag_indicator = input$worst_flag_indicator,
        anl_toxgrade_var = anl_selectors$anl_toxgrade_var()$variables$selected,
        base_toxgrade_var = anl_selectors$base_toxgrade_var()$variables$selected,
        paramcd = anl_selectors$paramcd()$variables$selected,
        drop_arm_levels = input$drop_arm_levels,
        add_total = input$add_total,
        total_label = total_label,
        na_level = na_level,
        code_missing_baseline = input$code_missing_baseline,
        basic_table_args = basic_table_args
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Decoration of table output.
    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
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
