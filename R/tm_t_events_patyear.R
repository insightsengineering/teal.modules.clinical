#' Template: Event Rates Adjusted for Patient-Years
#'
#' Creates a valid expression to generate a table of event rates adjusted for patient-years.
#'
#' @inheritParams template_arguments
#' @param events_var (`character`)\cr name of the variable for number of observed events.
#' @param label_paramcd (`character`)\cr `paramcd` variable text to use in the table title.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_events_patyear()]
#'
#' @keywords internal
template_events_patyear <- function(dataname,
                                    parentname,
                                    arm_var,
                                    events_var,
                                    label_paramcd,
                                    aval_var = "AVAL",
                                    add_total = TRUE,
                                    total_label = default_total_label(),
                                    na_level = tern::default_na_str(),
                                    control = tern::control_incidence_rate(),
                                    drop_arm_levels = TRUE,
                                    basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_character(arm_var, min.len = 1, max.len = 2)

  # initialize
  y <- list()
  # data
  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df,
      env = list(df = as.name(dataname))
    )
  )
  data_list <- add_expr(
    data_list,
    prepare_arm_levels(
      dataname = "anl",
      parentname = parentname,
      arm_var = arm_var[[1]],
      drop_arm_levels = drop_arm_levels
    )
  )
  if (length(arm_var) == 2) {
    data_list <- add_expr(
      data_list,
      prepare_arm_levels(
        dataname = "anl",
        parentname = parentname,
        arm_var = arm_var[[2]],
        drop_arm_levels = drop_arm_levels
      )
    )
  }

  data_list <- add_expr(
    data_list,
    substitute(
      expr = dataname <- tern::df_explicit_na(dataname, na_level = na_str),
      env = list(dataname = as.name("anl"), na_str = na_level)
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      expr = parentname <- tern::df_explicit_na(parentname, na_level = na_str),
      env = list(parentname = as.name(parentname), na_str = na_level)
    )
  )

  y$data <- bracket_expr(data_list)

  # layout
  layout_list <- list()

  basic_title <- tools::toTitleCase(paste("Event Rates Adjusted for Patient-Years by", label_paramcd))
  basic_footer <- paste(
    "CI Method:",
    if (control$conf_type == "normal") {
      "Normal (rate)"
    } else if (control$conf_type == "normal_log") {
      "Normal (log rate)"
    } else if (control$conf_type == "exact") {
      "Exact"
    } else {
      "Byar's method"
    }
  )

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        show_colcounts = TRUE,
        title = basic_title,
        main_footer = basic_footer
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = expr_basic_table_args %>%
        rtables::split_cols_by(var = arm_var),
      env = list(arm_var = arm_var[[1]], expr_basic_table_args = parsed_basic_table_args)
    )
  )

  if (length(arm_var) == 2) {
    layout_list <- add_expr(
      layout_list,
      if (drop_arm_levels) {
        substitute(
          expr = rtables::split_cols_by(nested_col, split_fun = rtables::drop_split_levels),
          env = list(nested_col = arm_var[[2]])
        )
      } else {
        substitute(
          expr = rtables::split_cols_by(nested_col),
          env = list(nested_col = arm_var[[2]])
        )
      }
    )
  }

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_overall_col(label = total_label),
        env = list(total_label = total_label)
      )
    )
  }
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = tern::estimate_incidence_rate(
        vars = aval_var,
        n_events = events_var,
        control = tern::control_incidence_rate(
          conf_level = conf_level,
          conf_type = conf_type,
          input_time_unit = input_time_unit,
          num_pt_year = num_pt_year
        )
      ),
      env = list(
        aval_var = aval_var,
        events_var = events_var,
        conf_level = control$conf_level,
        conf_type = control$conf_type,
        input_time_unit = control$input_time_unit,
        num_pt_year = control$num_pt_year
      )
    )
  )
  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # table
  y$table <- substitute(
    expr = {
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
    },
    env = list(parent = as.name(parentname))
  )

  y
}

#' teal Module: Event Rates Adjusted for Patient-Years
#'
#' This module produces a table of event rates adjusted for patient-years.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_events_patyear
#' @param arm_var ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table.
#'   If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param events_var ([teal.transform::choices_selected()])\cr object with
#'   all available choices and preselected option for the variable with all event counts.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`ElementaryTable` as created from `rtables::build_table`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_events_patyear(
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
#'
#' @examples
#' library(dplyr)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADAETTE <- tmc_ex_adaette %>%
#'     filter(PARAMCD %in% c("AETTE1", "AETTE2", "AETTE3")) %>%
#'     mutate(is_event = CNSR == 0) %>%
#'     mutate(n_events = as.integer(is_event))
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADAETTE <- data[["ADAETTE"]]
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ examples[[1]] }}
#' {{ next_example }}
#'
#' @examples
#' # 1. Basic Example
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_events_patyear(
#'       label = "AE Rate Adjusted for Patient-Years At Risk Table",
#'       dataname = "ADAETTE",
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADSL, c("ARM", "ARMCD")),
#'         selected = "ARMCD"
#'       ),
#'       add_total = TRUE,
#'       events_var = choices_selected(
#'         choices = variable_choices(ADAETTE, "n_events"),
#'         selected = "n_events",
#'         fixed = TRUE
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
#'         selected = "AETTE1"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ examples[[1]] }}
#' {{ next_example }}
#'
#' @examples
#' # 2. Example with table split on 2 arm_var variables
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_events_patyear(
#'       label = "AE Rate Adjusted for Patient-Years At Risk Table",
#'       dataname = "ADAETTE",
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADSL, c("ARM", "ARMCD", "SEX")),
#'         selected = c("ARM", "SEX")
#'       ),
#'       add_total = TRUE,
#'       events_var = choices_selected(
#'         choices = variable_choices(ADAETTE, "n_events"),
#'         selected = "n_events",
#'         fixed = TRUE
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
#'         selected = "AETTE1"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_events_patyear <- function(label,
                                dataname,
                                parentname = ifelse(
                                  inherits(arm_var, "data_extract_spec"),
                                  teal.transform::datanames_input(arm_var),
                                  "ADSL"
                                ),
                                arm_var,
                                events_var,
                                paramcd,
                                aval_var = teal.transform::choices_selected(
                                  teal.transform::variable_choices(dataname, "AVAL"), "AVAL",
                                  fixed = TRUE
                                ),
                                avalu_var = teal.transform::choices_selected(
                                  teal.transform::variable_choices(dataname, "AVALU"), "AVALU",
                                  fixed = TRUE
                                ),
                                add_total = TRUE,
                                total_label = default_total_label(),
                                na_level = tern::default_na_str(),
                                conf_level = teal.transform::choices_selected(
                                  c(0.95, 0.9, 0.8), 0.95,
                                  keep_order = TRUE
                                ),
                                drop_arm_levels = TRUE,
                                pre_output = NULL,
                                post_output = NULL,
                                basic_table_args = teal.widgets::basic_table_args(),
                                transformators = list(),
                                decorators = list()) {
  message("Initializing tm_t_events_patyear")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(events_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(avalu_var, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname, multiple = TRUE, ordered = TRUE),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    avalu_var = cs_to_des_select(avalu_var, dataname = dataname),
    events_var = cs_to_des_select(events_var, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_events_patyear,
    ui_args = c(data_extract_list, args),
    server = srv_events_patyear,
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
ui_events_patyear <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var, a$paramcd, a$aval_var, a$avalu_var, a$events_var
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("patyear_table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "aval_var", "avalu_var", "events_var")]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select an Event Type Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("events_var"),
        label = "Event Variable",
        data_extract_spec = a$events_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("avalu_var"),
        label = "Analysis Unit Variable",
        data_extract_spec = a$avalu_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      ),
      teal.widgets::optionalSelectInput(
        ns("conf_method"),
        "CI Method",
        choices = c("Normal (rate)", "Normal (log rate)", "Exact", "Byar's method"),
        selected = "Normal (rate)",
        multiple = FALSE,
        fixed = FALSE
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          ),
          teal.widgets::optionalSelectInput(
            ns("num_pt_year"),
            "Time Unit for AE Rate (in Patient-Years)",
            choices = c(0.1, 1, 10, 100, 1000),
            selected = 100,
            multiple = FALSE,
            fixed = FALSE
          ),
          selectInput(
            ns("input_time_unit"),
            "Analysis Unit",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          )
        )
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_events_patyear <- function(id,
                               data,
                               dataname,
                               parentname,
                               arm_var,
                               paramcd,
                               aval_var,
                               avalu_var,
                               events_var,
                               add_total,
                               total_label,
                               na_level,
                               drop_arm_levels,
                               label,
                               basic_table_args,
                               decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    observeEvent(anl_q(), {
      data_anl <- merged$anl_q()[["ANL"]]
      aval_unit_var <- merged$anl_input_r()$columns_source$avalu_var
      if (length(aval_unit_var) > 0) {
        choices <- stats::na.omit(unique(data_anl[[aval_unit_var]]))
        choices <- gsub("s$", "", tolower(choices))

        updateSelectInput(
          session,
          "input_time_unit",
          choices = choices,
          selected = choices[1]
        )
      }
    })

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        aval_var = aval_var,
        avalu_var = avalu_var,
        events_var = events_var
      ),
      datasets = data,
      select_validation_rule = list(
        arm_var = ~ if (length(.) != 1 && length(.) != 2) "Please select exactly 1 or 2 treatment variables",
        aval_var = shinyvalidate::sv_required("Analysis Variable is required"),
        events_var = shinyvalidate::sv_required("Events Variable is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("A Event Type Parameter is required")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(
          0, 1,
          inclusive = c(FALSE, FALSE),
          message_fmt = "Confidence level must be between 0 and 1"
        )
      )
      iv$add_rule("conf_method", shinyvalidate::sv_required("A CI method is required"))
      iv$add_rule("num_pt_year", shinyvalidate::sv_required("Time Unit for AE Rate is required"))
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

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_aval_var <- as.vector(merged$anl_input_r()$columns_source$aval_var)
      input_avalu_var <- as.vector(merged$anl_input_r()$columns_source$avalu_var)
      input_events_var <- as.vector(merged$anl_input_r()$columns_source$events_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_events_var, input_aval_var, input_avalu_var),
        arm_var = input_arm_var[[1]]
      )

      validate(
        need(
          !any(is.na(merged$anl_q()[["ANL"]][[input_events_var]])),
          "`Event Variable` for selected parameter includes NA values."
        )
      )
      NULL
    })

    # The R-code corresponding to the analysis.
    table_q <- reactive({
      validate_checks()

      ANL <- merged$anl_q()[["ANL"]]
      label_paramcd <- get_paramcd_label(ANL, paramcd)

      my_calls <- template_events_patyear(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(merged$anl_input_r()$columns_source$arm_var),
        aval_var = as.vector(merged$anl_input_r()$columns_source$aval_var),
        events_var = as.vector(merged$anl_input_r()$columns_source$events_var),
        label_paramcd = label_paramcd,
        add_total = input$add_total,
        total_label = total_label,
        na_level = na_level,
        control = tern::control_incidence_rate(
          conf_level = as.numeric(input$conf_level),
          conf_type = if (input$conf_method == "Normal (rate)") {
            "normal"
          } else if (input$conf_method == "Normal (log rate)") {
            "normal_log"
          } else if (input$conf_method == "Exact") {
            "exact"
          } else {
            "byar"
          },
          input_time_unit = if (input$input_time_unit %in% c("day", "week", "month", "year")) {
            input$input_time_unit
          } else {
            "year"
          },
          num_pt_year = as.numeric(input$num_pt_year)
        ),
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = basic_table_args
      )
      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })


    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = table_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    # Outputs to render.
    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "patyear_table",
      table_r = table_r
    )

    decorated_table_q
  })
}
