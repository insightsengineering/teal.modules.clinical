#' Template: ANCOVA Summary
#'
#' Creates a valid expression to generate an analysis of variance summary table.
#'
#' @inheritParams template_arguments
#' @param paramcd_levels (`character`)\cr
#'   variable levels for the studied parameter.
#' @param paramcd_var (`character`)\cr
#'   variable name for the studied parameter.
#' @param visit_levels (`character`)\cr
#'   variable levels for studied visits.
#' @param label_aval (`character`)\cr
#'   label of value variable used for title rendering.
#' @param label_paramcd (`character`)\cr
#'   variable label used for title rendering.
#' @param interact_var (`character`)\cr name of the variable that should have interactions with arm. If the
#'   interaction is not needed, the default option is `NULL`.
#' @param interact_y (`character`)\cr a selected item from the `interact_var` column which will be used to select the
#'   specific ANCOVA results. If the interaction is not needed, the default option is `FALSE`.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_ancova()]
#'
#' @keywords internal
template_ancova <- function(dataname = "ANL",
                            parentname = "ADSL",
                            arm_var,
                            ref_arm = NULL,
                            comp_arm = NULL,
                            combine_comp_arms = FALSE,
                            aval_var,
                            label_aval = NULL,
                            cov_var,
                            include_interact = FALSE,
                            interact_var = NULL,
                            interact_y = FALSE,
                            paramcd_levels = "",
                            paramcd_var = "PARAMCD",
                            label_paramcd = NULL,
                            visit_levels = "",
                            visit_var = "AVISIT",
                            conf_level = 0.95,
                            basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(label_aval, null.ok = TRUE)
  checkmate::assert_flag(combine_comp_arms)
  checkmate::assert_string(aval_var)
  checkmate::assert_character(cov_var)
  checkmate::assert_flag(include_interact)
  if (!isFALSE(interact_y)) checkmate::assert_character(interact_y)
  checkmate::assert_string(interact_var, null.ok = TRUE)

  y <- list()

  if (include_interact && !any(interact_y == "") && !is.null(interact_var)) {
    cov_var <- c(cov_var, paste0(arm_var, "*", interact_var))
  }

  if (length(cov_var) == 0) {
    cov_var <- NULL
  }

  # Data processing.
  data_list <- list()
  anl_list <- list()
  parent_list <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  anl_list <- add_expr(
    anl_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val,
      drop = FALSE
    )
  )
  anl_list <- add_expr(anl_list, quote(droplevels()))

  parent_list <- add_expr(
    parent_list,
    prepare_arm(
      dataname = parentname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val,
      drop = FALSE
    )
  )
  parent_list <- add_expr(parent_list, quote(droplevels()))

  if (combine_comp_arms) {
    anl_list <- add_expr(
      anl_list,
      substitute_names(
        expr = dplyr::mutate(arm_var = tern::combine_levels(arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
    parent_list <- add_expr(
      parent_list,
      substitute_names(
        expr = dplyr::mutate(arm_var = tern::combine_levels(arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
  }

  anl_list <- add_expr(anl_list, quote(tern::df_explicit_na(na_level = tern::default_na_str())))
  parent_list <- add_expr(parent_list, quote(tern::df_explicit_na(na_level = tern::default_na_str())))

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- anl_list,
      env = list(
        anl = as.name(dataname),
        anl_list = pipe_expr(anl_list)
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parent <- parent_list,
      env = list(
        parent = as.name(parentname),
        parent_list = pipe_expr(parent_list)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Build layout.
  visits_title <- if (length(visit_levels) > 1) {
    paste(
      paste(utils::head(visit_levels, -1), collapse = ", "),
      "and", utils::tail(visit_levels, 1)
    )
  } else if (length(visit_levels) == 1) {
    visit_levels
  } else {
    ""
  }

  table_title <- if (length(label_paramcd) > 1) {
    paste(
      "Summary of Analysis of Variance for", paste(label_paramcd, collapse = " and "),
      "at", visits_title, "for", label_aval
    )
  } else if (length(label_paramcd == 1)) {
    paste("Summary of Analysis of Variance for", label_paramcd, "at", visits_title, "for", label_aval)
  } else {
    ""
  }

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(show_colcounts = TRUE, title = table_title)
    )
  )

  y$layout_prep <- quote(split_fun <- rtables::drop_split_levels)
  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_cols_by(var = arm_var, ref_group = ref_group) %>%
        rtables::split_rows_by(
          visit_var,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::col_labels(dataname[visit_var], fill = TRUE)
        ),
      env = list(
        arm_var = arm_var,
        ref_group = paste(ref_arm, collapse = "/"),
        visit_var = visit_var,
        dataname = as.name(dataname)
      )
    )
  )

  if (length(paramcd_levels) > 1) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::split_rows_by(
          paramcd_var,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::col_labels(dataname[paramcd_var], fill = TRUE)
        ),
        env = list(
          paramcd_var = paramcd_var,
          dataname = as.name(dataname)
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::append_topleft(paste0("  ", paramcd_levels)),
        env = list(
          paramcd_levels = paramcd_levels
        )
      )
    )
  }

  if (!include_interact) {
    if (length(paramcd_levels) > 1) {
      if (length(cov_var) == 0) {
        ls_lbls <- c(lsmean = "Unadjusted Mean", lsmean_diff = "Difference in Unadjusted Means")
        var_lbls <- "Unadjusted mean"
      } else {
        ls_lbls <- NULL
        var_lbls <- "Adjusted mean"
      }
      layout_list <- add_expr(
        layout_list,
        substitute(
          tern::summarize_ancova(
            vars = aval_var,
            variables = list(arm = arm_var, covariates = cov_var),
            conf_level = conf_level,
            var_labels = var_labels,
            show_labels = "hidden",
            .labels = ls_labels
          ),
          env = list(
            aval_var = aval_var,
            arm_var = arm_var,
            cov_var = cov_var,
            conf_level = conf_level,
            var_labels = var_lbls,
            ls_labels = ls_lbls
          )
        )
      )
    } else {
      # Only one entry in `paramcd_levels` here.
      layout_list <- add_expr(
        layout_list,
        substitute(
          tern::summarize_ancova(
            vars = aval_var,
            variables = list(arm = arm_var, covariates = NULL),
            conf_level = conf_level,
            var_labels = "Unadjusted comparison",
            .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
            table_names = "unadjusted_comparison"
          ),
          env = list(
            aval_var = aval_var,
            arm_var = arm_var,
            conf_level = conf_level
          )
        )
      )
      if (length(cov_var) > 0) {
        layout_list <- add_expr(
          layout_list,
          substitute(
            tern::summarize_ancova(
              vars = aval_var,
              variables = list(arm = arm_var, covariates = cov_var),
              conf_level = conf_level,
              var_labels = paste0(
                "Adjusted comparison (", paste(cov_var, collapse = " + "), ")"
              ),
              table_names = "adjusted_comparison"
            ),
            env = list(
              aval_var = aval_var,
              arm_var = arm_var,
              cov_var = cov_var,
              conf_level = conf_level
            )
          )
        )
      }
    }
  } else {
    cts_interact <- all(interact_y == FALSE)
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::append_topleft(paste0("    Interaction Variable: ", interact_var)),
        env = list(
          interact_var = interact_var
        )
      )
    )
    for (int_y in interact_y) {
      if (length(paramcd_levels) > 1) {
        layout_list <- add_expr(
          layout_list,
          substitute(
            tern::summarize_ancova(
              vars = aval_var,
              variables = list(arm = arm_var, covariates = cov_var),
              conf_level = conf_level,
              var_labels = paste("Interaction Level:", interact_y),
              show_labels = if (cts_interact) "hidden" else "visible",
              interaction_y = interact_y,
              interaction_item = interact_var
            ),
            env = list(
              aval_var = aval_var,
              arm_var = arm_var,
              cov_var = cov_var,
              conf_level = conf_level,
              interact_y = int_y,
              interact_var = interact_var,
              cts_interact = cts_interact
            )
          )
        )
      } else {
        # Only one entry in `paramcd_levels` here.
        if (int_y == interact_y[1]) {
          layout_list <- add_expr(
            layout_list,
            substitute(
              tern::summarize_ancova(
                vars = aval_var,
                variables = list(arm = arm_var, covariates = NULL),
                conf_level = conf_level,
                var_labels = "Unadjusted comparison",
                .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
                table_names = "unadjusted_comparison"
              ),
              env = list(
                aval_var = aval_var,
                arm_var = arm_var,
                cov_var = cov_var,
                conf_level = conf_level
              )
            )
          )
        }
        if (length(cov_var) > 0) {
          layout_list <- add_expr(
            layout_list,
            substitute(
              tern::summarize_ancova(
                vars = aval_var,
                variables = list(arm = arm_var, covariates = cov_var),
                conf_level = conf_level,
                var_labels = if (cts_interact) {
                  paste0("Adjusted comparison (", paste(cov_var, collapse = " + "), ")")
                } else {
                  paste0(
                    "Adjusted comparison (", paste(cov_var, collapse = " + "),
                    "), Interaction Level: ", interact_y
                  )
                },
                table_names = "adjusted_comparison",
                interaction_y = interact_y,
                interaction_item = interact_var
              ),
              env = list(
                aval_var = aval_var,
                arm_var = arm_var,
                cov_var = cov_var,
                conf_level = conf_level,
                interact_y = int_y,
                interact_var = interact_var,
                cts_interact = cts_interact
              )
            )
          )
        }
      }
    }
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Build table.
  y$table <- substitute(
    expr = {
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
    },
    env = list(
      anl = as.name(dataname),
      parent = as.name(parentname)
    )
  )

  y
}

#' teal Module: ANCOVA Summary
#'
#' This module produces a table to summarize analysis of variance, consistent with the TLG Catalog
#' template for `AOVT01` available [here](
#' https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/aovt01.html) when multiple
#' endpoints are selected.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_ancova
#'
#' @inherit module_arguments return
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
#' tm_t_ancova(
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
#' @details
#' When a single endpoint is selected, both unadjusted and adjusted comparison are provided. This modules
#' expects that the analysis data has the following variables:
#'
#' * `AVISIT`: variable used to filter for analysis visits.
#' * `PARAMCD`: variable used to filter for endpoints, after filtering for `paramcd` and `avisit`, one
#'   observation per patient is expected for the analysis to be meaningful.
#'
#' @inherit module_arguments return seealso
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
#'   ADQS <- tmc_ex_adqs
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADQS <- data[["ADQS"]]
#'
#' arm_ref_comp <- list(
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   ),
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_ancova(
#'       label = "ANCOVA Table",
#'       dataname = "ADQS",
#'       avisit = picks(
#'         variables("AVISIT", "AVISIT"),
#'         values(selected = "WEEK 1 DAY 8", multiple = TRUE),
#'         check_dataset = FALSE
#'       ),
#'       arm_var = variables(c("ARM", "ACTARMCD", "ARMCD"), selected = "ARMCD"),
#'       arm_ref_comp = arm_ref_comp,
#'       aval_var = variables(c("CHG", "AVAL"), selected = "CHG", multiple = FALSE),
#'       cov_var = variables(c("BASE", "STRATA1", "SEX"), selected = "STRATA1"),
#'       paramcd = picks(
#'         variables("PARAMCD", "PARAMCD"),
#'         values(selected = "FKSI-FWB", multiple = TRUE),
#'         check_dataset = FALSE
#'       ),
#'       interact_var = variables(c("BASE", "STRATA1", "SEX"), selected = "STRATA1", multiple = FALSE)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_ancova <- function(label,
                        dataname,
                        parentname = "ADSL",
                        arm_var,
                        arm_ref_comp = NULL,
                        aval_var,
                        cov_var,
                        include_interact = FALSE,
                        interact_var = NULL,
                        interact_y = FALSE,
                        avisit,
                        paramcd,
                        conf_level = teal.picks::values(c(0.95, 0.9, 0.8), 0.95),
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.widgets::basic_table_args(),
                        transformators = list(),
                        decorators = list()) {
  message("Initializing tm_t_ancova")

  arm_var <- migrate_choices_selected_to_variables(arm_var, multiple = FALSE)
  aval_var <- migrate_choices_selected_to_variables(aval_var, multiple = FALSE)
  cov_var <- migrate_choices_selected_to_variables(cov_var, null.ok = TRUE)
  avisit <- migrate_value_choices_to_picks(avisit, multiple = TRUE)
  paramcd <- migrate_value_choices_to_picks(paramcd, multiple = TRUE)
  conf_level <- migrate_choices_selected_to_values(conf_level)

  if (is.null(interact_var)) {
    interact_var <- cov_var
    interact_var$selected <- NULL
  } else {
    interact_var <- migrate_choices_selected_to_variables(interact_var, multiple = FALSE, null.ok = TRUE)
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  arm_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), arm_var)
  aval_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), aval_var)
  cov_var <- if (!is.null(cov_var)) create_picks_helper(teal.picks::datasets(dataname, dataname), cov_var)
  avisit <- create_picks_helper(teal.picks::datasets(dataname, dataname), avisit)
  paramcd <- create_picks_helper(teal.picks::datasets(dataname, dataname), paramcd)
  interact_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), interact_var)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_ancova,
    ui_args = args[names(args) %in% names(formals(ui_ancova))],
    server = srv_ancova,
    server_args = args[names(args) %in% names(formals(srv_ancova))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_ancova <- function(id,
                      arm_var,
                      aval_var,
                      cov_var,
                      avisit,
                      paramcd,
                      interact_var,
                      conf_level,
                      decorators,
                      pre_output,
                      post_output) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Analysis Visit"),
        teal.picks::picks_ui(ns("avisit"), avisit)
      ),
      tags$div(
        tags$label("Select Endpoint"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Analysis Variable"),
        teal.picks::picks_ui(ns("aval_var"), aval_var)
      ),
      tags$div(
        tags$label("Select Treatment Variable"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      ),
      uiOutput(
        ns("arms_buckets"),
        title = paste(
          "Multiple reference groups are automatically combined into a single group",
          "when more than one value is selected."
        )
      ),
      uiOutput(ns("helptext_ui")),
      checkboxInput(
        ns("combine_comp_arms"),
        "Combine all comparison groups?",
        value = FALSE
      ),
      tags$div(
        tags$label("Covariates"),
        teal.picks::picks_ui(ns("cov_var"), cov_var)
      ),
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = HTML(paste("Confidence Level")),
        conf_level$choices,
        conf_level$selected,
        multiple = FALSE,
        fixed = teal.picks::is_pick_fixed(conf_level)
      ),
      tags$div(
        bslib::input_switch(
          id = ns("include_interact"),
          label = "Include Interaction Term",
          value = FALSE
        ),
        conditionalPanel(
          condition = paste0("input['", ns("include_interact"), "']"),
          tags$div(
            tags$div(
              tags$label("Select Interaction Variable"),
              teal.picks::picks_ui(ns("interact_var"), interact_var)
            ),
            teal.widgets::optionalSelectInput(
              ns("interact_y"),
              label = "Select Interaction y",
              choices = "",
              selected = "",
              multiple = TRUE,
              fixed = FALSE
            )
          )
        ),
        teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table"))
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_ancova <- function(id,
                       data,
                       dataname,
                       parentname,
                       arm_var,
                       arm_ref_comp,
                       aval_var,
                       cov_var,
                       include_interact,
                       interact_var,
                       paramcd,
                       avisit,
                       label,
                       basic_table_args,
                       decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    selectors <- teal.picks::picks_srv(
      picks = list(
        arm_var = arm_var,
        aval_var = aval_var,
        cov_var = cov_var,
        avisit = avisit,
        paramcd = paramcd,
        interact_var = interact_var
      ),
      data = data
    )

    arm_var_r <- reactive(selectors$arm_var()$variables$selected)

    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel.
    iv_arco <- arm_ref_comp_observer_picks(
      session,
      input,
      output,
      id_arm_var = "arm_var-variables-selected",
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_ancova",
      arm_var_r = arm_var_r
    )

    validated_q <- reactive({
      obj <- req(data())
      obj <- teal.code::eval_code(obj, "library(dplyr)")
      validate_input(
        inputId = "arm_var-variables-selected",
        condition = !is.null(selectors$arm_var()$variables$selected),
        message = "Arm variable cannot be empty."
      )
      validate_input(
        inputId = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message = "Analysis variable cannot be empty."
      )
      validate_input(
        inputId = "avisit-values-selected",
        condition = !is.null(selectors$avisit()$values$selected),
        message = "`Analysis Visit` field cannot be empty."
      )
      validate_input(
        inputId = "paramcd-values-selected",
        condition = !is.null(selectors$paramcd()$values$selected),
        message = "`Select Endpoint` is not selected."
      )
      validate_input(
        inputId = "conf_level",
        condition = !is.null(input$conf_level),
        message = "Please choose a confidence level."
      )
      validate_input(
        inputId = "conf_level",
        condition = as.numeric(input$conf_level) > 0 && as.numeric(input$conf_level) < 1,
        message = "Confidence level must be between 0 and 1."
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's output(s)")
      )
      obj |>
        within(
          tern::set_default_na_str(default_na_str),
          default_na_str = getOption("tern_default_na_str", default = "<Missing>")
        )
    })

    anl_inputs <- teal.picks::merge_srv(
      id = "merge",
      data = validated_q,
      selectors = selectors,
      output_name = "ANL"
    )

    adsl_inputs <- teal.picks::merge_srv(
      id = "merge_adsl",
      data = anl_inputs$data,
      selectors = selectors["arm_var"],
      output_name = "ANL_ADSL"
    )

    anl_q <- reactive(adsl_inputs$data())

    output$helptext_ui <- renderUI({
      if (length(selectors$arm_var()$variables$selected) != 0) {
        helpText("Multiple reference groups are automatically combined into a single group.")
      }
    })

    # Event handler:
    # Update interact_y choices to all levels of selected interact_var
    observeEvent(
      {
        input$include_interact
        selectors$interact_var()$variables$selected
      },
      {
        interact_var_sel <- selectors$interact_var()$variables$selected
        if (isTRUE(input$include_interact) && length(interact_var_sel) > 0) {
          interact_choices <- sort(as.vector(unique(anl_q()[[dataname]][[interact_var_sel]])))
          if (all(is.numeric(interact_choices))) {
            shinyjs::hide("interact_y")
          } else {
            interact_select <- if (!all(input$interact_y %in% interact_choices)) {
              interact_choices[1]
            } else {
              input$interact_y
            }
            shinyjs::show("interact_y")
            teal.widgets::updateOptionalSelectInput(
              session,
              "interact_y",
              selected = interact_select,
              choices = interact_choices
            )
          }
        }
      }
    )

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- anl_inputs$variables()$arm_var
      input_aval_var <- anl_inputs$variables()$aval_var
      input_cov_var <- anl_inputs$variables()$cov_var
      input_interact_var <- anl_inputs$variables()$interact_var
      input_avisit <- selectors$avisit()$variables$selected
      input_paramcd <- selectors$paramcd()$variables$selected

      # Validate inputs.
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_paramcd, input_avisit, input_aval_var, input_cov_var, input_interact_var
        ),
        arm_var = input_arm_var
      )
      validate_args <- append(
        validate_args,
        list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
      )
      do.call(what = "validate_standard_inputs", validate_args)

      # Other validations.
      validate(shiny::need(
        length(unique(adsl_filtered[[input_arm_var]])) > 1,
        "ANCOVA table needs at least 2 arm groups to make comparisons."
      ))
      # check that there is at least one record with no missing data
      validate(shiny::need(
        !all(is.na(anl_q()[["ANL"]][[input_aval_var]])),
        "ANCOVA table cannot be calculated as all values are missing."
      ))
      # check that for each visit there is at least one record with no missing data
      all_NA_dataset <- anl_q()[["ANL"]] %>% # nolint: object_name.
        dplyr::group_by(dplyr::across(dplyr::all_of(c(input_avisit, input_arm_var)))) %>%
        dplyr::summarize(all_NA = all(is.na(.data[[input_aval_var]])))
      validate(shiny::need(
        !any(all_NA_dataset$all_NA),
        "ANCOVA table cannot be calculated as all values are missing for one visit for (at least) one arm."
      ))

      if (input$include_interact) {
        if (!is.null(input_interact_var) && length(input_interact_var) > 0) {
          validate(shiny::need(
            !input_interact_var %in% c(input_avisit, input_paramcd) &&
              length(as.vector(unique(anl_filtered[[input_interact_var]]))) > 1,
            paste(
              "Interaction variable cannot be a filter variable and must have more than one level.",
              "Please select a different interaction variable."
            )
          ))
          if (!all(is.numeric(as.vector(unique(anl_filtered[[input_interact_var]]))))) {
            validate(shiny::need(
              !is.null(input$interact_y),
              paste(
                "Interaction y must be selected when a discrete variable is chosen for interact variable.",
                "Please select an interaction y, change the interaction variable, or turn off interactions."
              )
            ))
          }
        }
      }

      if (length(input_cov_var >= 1L)) {
        input_cov_var_dataset <- anl_filtered[input_cov_var]
        validate(
          need(
            all(vapply(input_cov_var_dataset, function(col) length(unique(col)) > 1L, logical(1))),
            "Selected covariates should have more than one level for showing the adjusted analysis."
          )
        )
      }
    })

    # The R-code corresponding to the analysis.
    table_q <- reactive({
      validate_checks()
      ANL <- anl_q()[["ANL"]]

      label_paramcd <- selectors$paramcd()$values$selected
      input_aval <- anl_inputs$variables()$aval_var
      label_aval <- if (length(input_aval) != 0) attributes(ANL[[input_aval]])$label else NULL
      paramcd_levels <- unique(ANL[[selectors$paramcd()$variables$selected]])
      visit_levels <- unique(ANL[[selectors$avisit()$variables$selected]])

      interact_var_sel <- anl_inputs$variables()$interact_var
      if (length(interact_var_sel) > 0) {
        if (is.numeric(ANL[[interact_var_sel]])) {
          interact_y <- FALSE
        } else if (!all(input$interact_y %in% levels(ANL[[interact_var_sel]]))) {
          interact_y <- levels(ANL[[interact_var_sel]])[1]
        } else {
          interact_y <- input$interact_y
        }
      } else {
        interact_var_sel <- NULL
        if (length(input$interact_y) == 0 || all(input$interact_y == "")) {
          interact_y <- FALSE
        }
      }

      my_calls <- template_ancova(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = anl_inputs$variables()$arm_var,
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        combine_comp_arms = input$combine_comp_arms,
        aval_var = anl_inputs$variables()$aval_var,
        label_aval = label_aval,
        cov_var = selectors$cov_var()$variables$selected %||% character(0),
        include_interact = input$include_interact,
        interact_var = interact_var_sel,
        interact_y = interact_y,
        paramcd_levels = paramcd_levels,
        paramcd_var = selectors$paramcd()$variables$selected,
        label_paramcd = label_paramcd,
        visit_levels = visit_levels,
        visit_var = selectors$avisit()$variables$selected,
        conf_level = as.numeric(input$conf_level),
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

    # Output to render.
    table_r <- reactive({
      decorated_table_q()[["table"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
