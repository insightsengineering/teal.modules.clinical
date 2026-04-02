#' Template: Abnormality Summary Table
#'
#' Creates a valid expression to generate a table to summarize abnormality.
#'
#' @inheritParams template_arguments
#' @param exclude_base_abn (`logical`)\cr whether to exclude patients who had abnormal values at baseline.
#' @param grade (`character`)\cr name of the variable used to
#'   specify the abnormality grade. Variable must be factor.
#' @param abnormal (`named list`)\cr indicating abnormality direction and grades.
#' @param baseline_var (`character`)\cr
#'   name of the variable specifying baseline abnormality grade.
#' @param treatment_flag (`character`) vector of one or more treatments flags
#' @param na_level (`character`)\cr the NA level in the input dataset, defaults to `"<Missing>"`.
#' @param tbl_title (`character`)\cr Title with label of variables from by bars
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_abnormality()]
#' @keywords internal
#'
template_abnormality <- function(parentname,
                                 dataname,
                                 arm_var,
                                 id_var = "USUBJID",
                                 by_vars,
                                 abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
                                 grade = "ANRIND",
                                 baseline_var = "BNRIND",
                                 treatment_flag_var = "ONTRTFL",
                                 treatment_flag = "Y",
                                 add_total = FALSE,
                                 total_label = default_total_label(),
                                 exclude_base_abn = FALSE,
                                 drop_arm_levels = TRUE,
                                 na_level = tern::default_na_str(),
                                 basic_table_args = teal.widgets::basic_table_args(),
                                 tbl_title) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(id_var)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::check_character(by_vars)
  checkmate::check_list(abnormal)
  checkmate::assert_string(grade)
  checkmate::assert_string(baseline_var)
  checkmate::assert_string(treatment_flag_var)
  checkmate::assert_character(treatment_flag, null.ok = TRUE)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(exclude_base_abn)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_string(tbl_title)

  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    if (length(treatment_flag) == 0) {
      substitute(
        expr = anl <- df %>%
          dplyr::filter(!is.na(grade) & grade != na_level),
        env = list(
          df = as.name(dataname),
          grade = as.name(grade),
          na_level = na_level
        )
      )
    } else {
      substitute(
        expr = anl <- df %>%
          dplyr::filter(treatment_flag_var %in% treatment_flag & !is.na(grade) & grade != na_level),
        env = list(
          df = as.name(dataname),
          grade = as.name(grade),
          treatment_flag_var = as.name(treatment_flag_var),
          treatment_flag = treatment_flag,
          na_level = na_level
        )
      )
    }
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
      dataname <- tern::df_explicit_na(dataname, na_level = na_level),
      env = list(dataname = as.name("anl"), na_level = na_level)
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- tern::df_explicit_na(parentname, na_level = na_level),
      env = list(parentname = as.name(parentname), na_level = na_level)
    )
  )

  y$data <- bracket_expr(data_list)

  # layout start
  prep_list <- list()
  prep_list <- add_expr(
    prep_list,
    substitute(
      # Define the map for layout using helper function h_map_for_count_abnormal
      map <- tern::h_map_for_count_abnormal(
        df = dataname,
        variables = list(anl = grade, split_rows = by_vars),
        abnormal = abnormal,
        method = "default",
        na_str = na_level
      ),
      env = list(dataname = as.name("anl"), by_vars = by_vars, grade = grade, abnormal = abnormal, na_level = na_level)
    )
  )

  y$layout_prep <- bracket_expr(prep_list)

  # Prepare footer message
  footer_message <- if (length(treatment_flag) == 0) {
    c(
      "Variables without observed abnormalities are excluded.",
      "Showing all data since no treatment filter is selected."
    )
  } else {
    "Variables without observed abnormalities are excluded."
  }

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        show_colcounts = TRUE,
        title = tbl_title,
        main_footer = footer_message
      )
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
            split_fun = rtables::add_overall_level(total_label, first = FALSE)
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

  for (by_var in by_vars) {
    split_label <- substitute(
      expr = teal.data::col_labels(dataname, fill = FALSE)[[by_var]],
      env = list(
        dataname = as.name(dataname),
        by_var = by_var
      )
    )
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::split_rows_by(
          by_var,
          split_label = split_label,
          label_pos = "topleft",
          split_fun = rtables::trim_levels_to_map(map = map)
        ),
        env = list(
          by_var = by_var,
          split_label = split_label,
          map = as.name("map")
        )
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = tern::count_abnormal(
        var = grade,
        abnormal = abnormal,
        variables = list(id = id_var, baseline = baseline_var),
        .indent_mods = 4L,
        exclude_base_abn = exclude_base_abn
      ) %>%
        tern::append_varlabels(dataname, grade, indent = indent_space),
      env = list(
        grade = grade,
        abnormal = abnormal,
        id_var = id_var,
        baseline_var = baseline_var,
        exclude_base_abn = exclude_base_abn,
        dataname = as.name(dataname),
        by_vars = by_vars,
        indent_space = length(by_vars)
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

#' teal Module: Abnormality Summary Table
#'
#' This module produces a table to summarize abnormality.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_abnormality
#' @param arm_var ([teal.picks::variables()])\cr object with all available choices and preselected
#'   option for the treatment variable.
#' @param by_vars ([teal.picks::variables()])\cr object with all available choices and preselected
#'   option(s) for row-by variables (`multiple = TRUE`, `ordered = TRUE` recommended).
#' @param grade ([teal.picks::variables()])\cr object with all available choices and preselected
#'   option for the abnormality grade variable. Variable must be factor.
#' @param abnormal (`named list`)\cr defined by user to indicate what abnormalities are to be displayed.
#' @param id_var ([teal.picks::variables()])\cr subject identifier variable.
#' @param baseline_var ([teal.picks::variables()])\cr variable for baseline abnormality grade.
#' @param treatment_flag_var ([teal.picks::variables()])\cr on-treatment flag variable.
#' @param na_level (`character`)\cr the NA level in the input dataset, default to `"<Missing>"`.
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
#' tm_t_abnormality(
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
#' @note Patients with the same abnormality at baseline as on the treatment visit can be
#'   excluded in accordance with GDSR specifications by using `exclude_base_abn`.
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#'
#' data <- teal_data()
#' data <- within(data, {
#'   library(teal.modules.clinical)
#'   library(formatters)
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl
#'   ADLB <- tmc_ex_adlb %>%
#'     mutate(
#'       ONTRTFL = case_when(
#'         AVISIT %in% c("SCREENING", "BASELINE") ~ "",
#'         TRUE ~ "Y"
#'       ) %>% with_label("On Treatment Record Flag")
#'     )
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADLB <- data[["ADLB"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_abnormality(
#'       label = "Abnormality Table",
#'       dataname = "ADLB",
#'       arm_var = variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
#'       add_total = FALSE,
#'       by_vars = variables(
#'         choices = c("LBCAT", "PARAM", "AVISIT"),
#'         selected = c("LBCAT", "PARAM"),
#'         multiple = TRUE,
#'         ordered = TRUE
#'       ),
#'       baseline_var = variables(choices = "BNRIND"),
#'       grade = variables(choices = "ANRIND"),
#'       abnormal = list(low = "LOW", high = "HIGH"),
#'       exclude_base_abn = FALSE
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_abnormality <- function(label,
                             dataname,
                             parentname = "ADSL",
                             arm_var = variables(choices = c("ARM", "ARMCD")),
                             by_vars = variables(
                               choices = c("LBCAT", "PARAM", "AVISIT"),
                               selected = c("LBCAT", "PARAM"),
                               multiple = TRUE,
                               ordered = TRUE
                             ),
                             grade = variables(choices = "ANRIND"),
                             abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
                             id_var = variables(choices = "USUBJID"),
                             baseline_var = variables(choices = "BNRIND"),
                             treatment_flag_var = variables(choices = "ONTRTFL"),
                             treatment_flag = teal.transform::choices_selected("Y"),
                             add_total = TRUE,
                             total_label = default_total_label(),
                             exclude_base_abn = FALSE,
                             drop_arm_levels = TRUE,
                             pre_output = NULL,
                             post_output = NULL,
                             na_level = tern::default_na_str(),
                             basic_table_args = teal.widgets::basic_table_args(),
                             transformators = list(),
                             decorators = list()) {
  message("Initializing tm_t_abnormality")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(na_level)
  checkmate::assert_list(abnormal, types = "character", len = 2)
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(by_vars, "variables")
  checkmate::assert_class(grade, "variables")
  checkmate::assert_class(id_var, "variables")
  checkmate::assert_class(baseline_var, "variables")
  checkmate::assert_class(treatment_flag_var, "variables")
  checkmate::assert_class(treatment_flag, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_flag(exclude_base_abn)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  arm_var <- picks(datasets(parentname, parentname), arm_var)
  by_vars <- picks(datasets(dataname, dataname), by_vars)
  grade <- picks(datasets(dataname, dataname), grade)
  id_var <- picks(datasets(dataname, dataname), id_var)
  baseline_var <- picks(datasets(dataname, dataname), baseline_var)
  treatment_flag_var <- picks(datasets(dataname, dataname), treatment_flag_var)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_abnormality,
    server = srv_t_abnormality,
    ui_args = args[names(args) %in% names(formals(ui_t_abnormality))],
    server_args = args[names(args) %in% names(formals(srv_t_abnormality))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}

#' @keywords internal
ui_t_abnormality <- function(id,
                             arm_var,
                             by_vars,
                             grade,
                             id_var,
                             baseline_var,
                             treatment_flag_var,
                             add_total,
                             exclude_base_abn,
                             drop_arm_levels,
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
        picks_ui(ns("arm_var"), arm_var)
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = add_total),
      tags$div(
        tags$label("Row By Variable"),
        picks_ui(ns("by_vars"), by_vars)
      ),
      tags$div(
        tags$label("Grade Variable"),
        picks_ui(ns("grade"), grade)
      ),
      checkboxInput(
        ns("exclude_base_abn"),
        "Exclude subjects whose baseline grade is the same as abnormal grade",
        value = exclude_base_abn
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = drop_arm_levels
          )
        )
      ),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          tags$div(
            tags$label("Subject Identifier"),
            picks_ui(ns("id_var"), id_var)
          ),
          tags$div(
            tags$label("Baseline Grade Variable"),
            picks_ui(ns("baseline_var"), baseline_var)
          ),
          tags$div(
            tags$label("On Treatment Flag Variable"),
            picks_ui(ns("treatment_flag_var"), treatment_flag_var)
          ),
          teal.widgets::optionalSelectInput(
            ns("treatment_flag"),
            label = "Value Indicating On Treatment",
            multiple = TRUE,
            fixed_on_single = FALSE
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_t_abnormality <- function(id,
                              data,
                              dataname,
                              parentname,
                              abnormal,
                              arm_var,
                              id_var,
                              by_vars,
                              grade,
                              baseline_var,
                              treatment_flag_var,
                              treatment_flag,
                              add_total,
                              total_label,
                              drop_arm_levels,
                              label,
                              na_level,
                              basic_table_args,
                              decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- picks_srv(
      id = "",
      picks = list(
        arm_var = arm_var,
        id_var = id_var,
        by_vars = by_vars,
        grade = grade,
        baseline_var = baseline_var,
        treatment_flag_var = treatment_flag_var
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
    merged_anl <- merge_srv(
      "merge_anl", data = data_with_card, selectors = anl_selectors, output_name = "ANL"
    )
    merged_adsl_anl <- merge_srv(
      "merge_adsl_anl", data = merged_anl$data, selectors = adsl_selectors, output_name = "ANL_ADSL"
    )
    anl_q <- merged_adsl_anl$data

    isolate({
      resolved <- teal.transform::resolve_delayed(treatment_flag, as.list(data()))
      teal.widgets::updateOptionalSelectInput(
        session = session,
        inputId = "treatment_flag",
        choices = resolved$choices,
        selected = resolved$selected
      )
    })

    validate_checks <- reactive({
      input_arm <- anl_selectors$arm_var()$variables$selected
      validate(
        need(length(input_arm) == 1L, "Please select a treatment variable.")
      )
      validate(
        need(
          length(anl_selectors$by_vars()$variables$selected) >= 1L,
          "Please select a Row By Variable."
        ),
        need(
          length(anl_selectors$id_var()$variables$selected) >= 1L,
          "Please select a subject identifier."
        ),
        need(
          length(anl_selectors$grade()$variables$selected) >= 1L,
          "Please select a grade variable."
        ),
        need(
          length(anl_selectors$baseline_var()$variables$selected) >= 1L,
          "Please select a baseline grade variable."
        ),
        need(
          length(anl_selectors$treatment_flag_var()$variables$selected) >= 1L,
          "Please select indicator value for on treatment records."
        )
      )

      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- as.vector(anl_selectors$arm_var()$variables$selected)
      input_id_var <- as.vector(anl_selectors$id_var()$variables$selected)
      input_by_vars <- as.vector(anl_selectors$by_vars()$variables$selected)
      input_grade <- as.vector(anl_selectors$grade()$variables$selected)
      input_baseline_var <- as.vector(anl_selectors$baseline_var()$variables$selected)
      input_treatment_flag_var <- as.vector(anl_selectors$treatment_flag_var()$variables$selected)

      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_id_var, input_by_vars, input_grade),
        arm_var = input_arm_var
      )
    })

    all_q <- reactive({
      validate_checks()

      by_vars_names <- anl_selectors$by_vars()$variables$selected
      by_vars_labels <- as.character(sapply(by_vars_names, function(name) {
        attr(anl_q()[["ANL"]][[name]], "label")
      }))

      tbl_title <- ifelse(
        length(by_vars_labels) == 1,
        paste("Laboratory Abnormality summary by", by_vars_labels),
        paste(paste("Laboratory Abnormality summary by", paste(by_vars_labels, collapse = ", ")))
      )

      my_calls <- template_abnormality(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = as.vector(anl_selectors$arm_var()$variables$selected),
        by_vars = anl_selectors$by_vars()$variables$selected,
        id_var = as.vector(anl_selectors$id_var()$variables$selected),
        abnormal = abnormal,
        grade = as.vector(anl_selectors$grade()$variables$selected),
        baseline_var = as.vector(anl_selectors$baseline_var()$variables$selected),
        treatment_flag_var = as.vector(anl_selectors$treatment_flag_var()$variables$selected),
        treatment_flag = input$treatment_flag,
        add_total = input$add_total,
        total_label = total_label,
        exclude_base_abn = input$exclude_base_abn,
        drop_arm_levels = input$drop_arm_levels,
        na_level = na_level,
        basic_table_args = basic_table_args,
        tbl_title = tbl_title
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

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
