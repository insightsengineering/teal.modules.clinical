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
#' @param arm_var ([teal.picks::variables()])\cr object with all
#'   available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table.
#'   If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param events_var ([teal.picks::variables()])\cr object with
#'   all available choices and preselected option for the variable with all event counts.
#' @param paramcd ([teal.picks::variables()])\cr variable used to filter the analysis dataset
#'   (typically `PARAMCD`). The `values()` element is added internally to allow users to pick
#'   parameter value(s) interactively.
#' @param aval_var ([teal.picks::variables()])\cr analysis variable (typically `AVAL`).
#' @param avalu_var ([teal.picks::variables()])\cr analysis unit variable (typically `AVALU`).
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
#'       arm_var = variables(choices = c("ARM", "ARMCD")),
#'       add_total = TRUE,
#'       events_var = variables(choices = "n_events"),
#'       paramcd = variables(choices = "PARAMCD"),
#'       aval_var = variables(choices = "AVAL"),
#'       avalu_var = variables(choices = "AVALU")
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
#'       arm_var = variables(
#'         choices = c("ARM", "ARMCD", "SEX"),
#'         selected = c("ARM", "SEX")
#'       ),
#'       add_total = TRUE,
#'       events_var = variables(choices = "n_events"),
#'       paramcd = variables(choices = "PARAMCD"),
#'       aval_var = variables(choices = "AVAL"),
#'       avalu_var = variables(choices = "AVALU")
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
                                parentname = "ADSL",
                                arm_var = variables(choices = c("ARM", "ARMCD")),
                                events_var = variables(choices = "n_events"),
                                paramcd = variables(choices = "PARAMCD"),
                                aval_var = variables(choices = "AVAL"),
                                avalu_var = variables(choices = "AVALU"),
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
  checkmate::assert_class(arm_var, "variables")
  checkmate::assert_class(events_var, "variables")
  checkmate::assert_class(paramcd, "variables")
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(avalu_var, "variables")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  arm_var <- teal.picks::picks(teal.picks::datasets(parentname, parentname), arm_var)
  paramcd <- teal.picks::picks(teal.picks::datasets(dataname, dataname), paramcd, values())
  events_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), events_var)
  aval_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), aval_var)
  avalu_var <- teal.picks::picks(teal.picks::datasets(dataname, dataname), avalu_var)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_events_patyear,
    server = srv_events_patyear,
    ui_args = args[names(args) %in% names(formals(ui_events_patyear))],
    server_args = args[names(args) %in% names(formals(srv_events_patyear))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}

#' @keywords internal
ui_events_patyear <- function(id,
                              arm_var,
                              paramcd,
                              aval_var,
                              avalu_var,
                              events_var,
                              add_total,
                              conf_level,
                              drop_arm_levels,
                              pre_output,
                              post_output,
                              decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("patyear_table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Treatment Variable"),
        picks_ui(ns("arm_var"), arm_var)
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = add_total),
      tags$div(
        tags$label("Select an Event Type Parameter"),
        picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Analysis Variable"),
        picks_ui(ns("aval_var"), aval_var)
      ),
      tags$div(
        tags$label("Event Variable"),
        picks_ui(ns("events_var"), events_var)
      ),
      tags$div(
        tags$label("Analysis Unit Variable"),
        picks_ui(ns("avalu_var"), avalu_var)
      ),
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        conf_level$choices,
        conf_level$selected,
        multiple = FALSE,
        fixed = conf_level$fixed
      ),
      teal.widgets::optionalSelectInput(
        ns("conf_method"),
        "CI Method",
        choices = c("Normal (rate)", "Normal (log rate)", "Exact", "Byar's method"),
        selected = "Normal (rate)",
        multiple = FALSE,
        fixed = FALSE
      ),
      teal::ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = drop_arm_levels
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
    pre_output = pre_output,
    post_output = post_output
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
                               label,
                               total_label,
                               na_level,
                               basic_table_args,
                               decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- picks_srv(
      id = "",
      picks = list(
        arm_var = arm_var,
        paramcd = paramcd,
        aval_var = aval_var,
        avalu_var = avalu_var,
        events_var = events_var
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

    observeEvent(anl_q(), {
      data_anl <- anl_q()[["ANL"]]
      aval_unit_var <- anl_selectors$avalu_var()$variables$selected
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

    validate_checks <- reactive({
      input_arm <- anl_selectors$arm_var()$variables$selected
      validate(
        need(length(input_arm) %in% 1:2, "Please select exactly 1 or 2 treatment variables")
      )

      paramcd_pick <- anl_selectors$paramcd()
      paramcd_vals <- if (is.null(paramcd_pick$values)) {
        character(0)
      } else {
        paramcd_pick$values$selected
      }
      validate(
        need(
          length(paramcd_vals) >= 1L,
          "A Event Type Parameter is required"
        )
      )

      validate(
        need(
          length(anl_selectors$aval_var()$variables$selected) >= 1L,
          "Analysis Variable is required"
        ),
        need(
          length(anl_selectors$events_var()$variables$selected) >= 1L,
          "Events Variable is required"
        )
      )

      validate_input(
        "conf_level",
        condition = function(x) {
          !is.null(x) && nzchar(x) && {
            xv <- suppressWarnings(as.numeric(x))
            !is.na(xv) && xv > 0 && xv < 1
          }
        },
        message = "Confidence level must be between 0 and 1"
      )
      validate_input(
        "conf_method",
        condition = function(x) !is.null(x) && nzchar(x),
        message = "A CI method is required"
      )
      validate_input(
        "num_pt_year",
        condition = function(x) !is.null(x) && nzchar(x),
        message = "Time Unit for AE Rate is required"
      )

      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- as.vector(anl_selectors$arm_var()$variables$selected)
      input_aval_var <- as.vector(anl_selectors$aval_var()$variables$selected)
      input_avalu_var <- as.vector(anl_selectors$avalu_var()$variables$selected)
      input_events_var <- as.vector(anl_selectors$events_var()$variables$selected)
      input_paramcd_var <- as.vector(anl_selectors$paramcd()$variables$selected)

      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_paramcd_var,
          input_events_var, input_aval_var, input_avalu_var
        ),
        arm_var = input_arm_var[[1]]
      )

      validate(
        need(
          !any(is.na(anl_q()[["ANL"]][[input_events_var]])),
          "`Event Variable` for selected parameter includes NA values."
        )
      )
      NULL
    })

    # The R-code corresponding to the analysis.
    table_q <- reactive({
      validate_checks()

      ANL <- anl_q()[["ANL"]]
      pcd <- anl_selectors$paramcd()
      pcd_vals <- if (is.null(pcd$values)) character(0) else pcd$values$selected
      label_paramcd <- paramcd_title_from_anl(
        ANL,
        paramcd_name = pcd$variables$selected[[1]],
        paramcd_vals = pcd_vals
      )

      my_calls <- template_events_patyear(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_selectors$arm_var()$variables$selected),
        aval_var = as.vector(anl_selectors$aval_var()$variables$selected),
        events_var = as.vector(anl_selectors$events_var()$variables$selected),
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
      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })


    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = table_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
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
