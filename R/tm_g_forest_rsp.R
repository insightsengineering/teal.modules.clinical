#' Template: Response Forest Plot
#'
#' Creates a valid expression to generate a response forest plot.
#'
#' @inheritParams tern::g_forest
#' @inheritParams template_arguments
#' @param stats (`character`)\cr the names of statistics to be reported among:
#'   * `n`: Total number of observations per group.
#'   * `n_rsp`: Number of responders per group.
#'   * `prop`: Proportion of responders.
#'   * `n_tot`: Total number of observations.
#'   * `or`: Odds ratio.
#'   * `ci` : Confidence interval of odds ratio.
#'   * `pval`: p-value of the effect.
#'   Note, the statistics `n_tot`, `or`, and `ci` are required.
#' @param riskdiff (`list`)\cr if a risk (proportion) difference column should be added, a list of settings to apply
#'   within the column. See [tern::control_riskdiff()] for details. If `NULL`, no risk difference column will be added.
#' @param obj_var_name (`character`)\cr additional text to append to the table title.
#' @param responders (`character`)\cr values of `aval_var` that are considered to be responders.
#' @param col_symbol_size (`integer` or `NULL`)\cr column index to be used to determine relative size for
#'  estimator plot symbol. Typically, the symbol size is proportional to the sample size used
#'  to calculate the estimator. If `NULL`, the same symbol size is used for all subgroups.
#' @param strata_var (`character`)\cr names of the variables for stratified analysis.
#' @param ggplot2_args (`ggplot2_args`) optional\cr
#'   object created by [teal.widgets::ggplot2_args()] with settings for the module plot. For this
#'   module, this argument will only accept `ggplot2_args` object with `labs` list of following child
#'   elements: `title`, `caption`. No other elements would be taken into account. The argument is
#'   merged with option `teal.ggplot2_args` and with default module arguments (hard coded in the module body).
#'
#'   For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_g_forest_rsp()]
#'
#' @keywords internal
template_forest_rsp <- function(dataname = "ANL",
                                parentname = "ADSL",
                                arm_var,
                                ref_arm = NULL,
                                comp_arm = NULL,
                                obj_var_name = "",
                                aval_var = "AVALC",
                                responders = c("CR", "PR"),
                                subgroup_var,
                                strata_var = NULL,
                                stats = c("n_tot", "n", "n_rsp", "prop", "or", "ci"),
                                riskdiff = NULL,
                                conf_level = 0.95,
                                col_symbol_size = NULL,
                                font_size = 15,
                                ggplot2_args = teal.widgets::ggplot2_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(obj_var_name)
  checkmate::assert_character(subgroup_var, null.ok = TRUE)
  checkmate::assert_character(stats, min.len = 3)
  checkmate::assert_true(all(c("n_tot", "or", "ci") %in% stats))
  checkmate::assert_list(riskdiff, null.ok = TRUE)
  checkmate::assert_number(font_size)

  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  # Data processing.
  data_list <- list()
  anl_list <- list()
  parent_list <- list()

  anl_list <- add_expr(
    anl_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  anl_list <- add_expr(
    anl_list,
    substitute(
      expr = dplyr::mutate(is_rsp = aval_var %in% responders),
      env = list(
        aval_var = as.name(aval_var),
        responders = responders
      )
    )
  )

  anl_list <- add_expr(
    anl_list,
    substitute_names(
      expr = dplyr::mutate(arm_var = tern::combine_levels(arm_var, levels = comp_arm)),
      names = list(arm_var = as.name(arm_var)),
      others = list(comp_arm = comp_arm)
    )
  )

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

  parent_list <- add_expr(
    parent_list,
    prepare_arm(
      dataname = parentname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
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

  data_list <- add_expr(
    data_list,
    substitute(
      parent <- parent_list,
      env = list(
        parent_list = pipe_expr(parent_list)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Tabulate subgroup analysis of response.
  summary_list <- list()

  summary_list <- add_expr(
    summary_list,
    substitute(
      expr = df <- tern::extract_rsp_subgroups(
        variables = list(
          rsp = "is_rsp", arm = arm_var, subgroups = subgroup_var, strata = strata_var
        ),
        data = anl,
        conf_level = conf_level
      ),
      env = list(
        anl = as.name(dataname),
        arm_var = arm_var,
        subgroup_var = subgroup_var,
        strata_var = strata_var,
        conf_level = conf_level
      )
    )
  )

  y$summary <- bracket_expr(summary_list)

  # Table output.
  y$table <- substitute(
    expr = result <- rtables::basic_table() %>%
      tern::tabulate_rsp_subgroups(df, vars = stats, riskdiff = riskdiff),
    env = list(stats = stats, riskdiff = riskdiff)
  )

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = teal.widgets::ggplot2_args(
      labs = list(
        title = paste("Forest Plot of Best Overall Response for", obj_var_name),
        caption = ""
      )
    )
  )

  plot_list <- list()

  plot_list <- add_expr(
    plot_list,
    substitute(
      expr = {
        f <- tern::g_forest(
          tbl = result,
          col_symbol_size = col_s_size,
          font_size = font_size,
          as_list = TRUE
        )
      },
      env = list(
        col_s_size = col_symbol_size,
        font_size = font_size
      )
    )
  )

  plot_list <- add_expr(
    plot_list,
    substitute(
      expr = {
        table <- f[["table"]] + ggplot2::labs(title = ggplot2_args_title)
        plot <- f[["plot"]] + ggplot2::labs(caption = ggplot2_args_caption)
      },
      env = list(
        ggplot2_args_title = all_ggplot2_args$labs$title,
        ggplot2_args_caption = all_ggplot2_args$labs$caption
      )
    )
  )

  # Plot output.
  y$plot <- plot_list

  y
}

#' teal Module: Forest Response Plot
#'
#' This module produces a grid-style forest plot for response data with ADaM structure.
#'
#' @inheritParams tern::g_forest
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_forest_rsp
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_forest_rsp(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied only to `plot` output
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
#' library(nestcolor)
#'
#' data <- teal_data()
#' data <- within(data, {
#'   library(teal.modules.clinical)
#'   library(formatters)
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl
#'   ADRS <- tmc_ex_adrs %>%
#'     mutate(AVALC = d_onco_rsp_label(AVALC) %>%
#'       with_label("Character Result/Finding")) %>%
#'     filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADRS <- data[["ADRS"]]
#'
#' arm_ref_comp <- list(
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   ),
#'   ARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_forest_rsp(
#'       label = "Forest Response",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD")),
#'         "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADRS, "PARAMCD", "PARAM"),
#'         "INVET"
#'       ),
#'       subgroup_var = choices_selected(
#'         variable_choices(ADSL, names(ADSL)),
#'         c("BMRKR2", "SEX")
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("STRATA1", "STRATA2")),
#'         "STRATA2"
#'       ),
#'       plot_height = c(600L, 200L, 2000L),
#'       default_responses = list(
#'         BESRSPI = list(
#'           rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
#'           levels = c(
#'             "Complete Response (CR)", "Partial Response (PR)", "Stable Disease (SD)",
#'             "Progressive Disease (PD)", "Not Evaluable (NE)"
#'           )
#'         ),
#'         INVET = list(
#'           rsp = c("Complete Response (CR)", "Partial Response (PR)"),
#'           levels = c(
#'             "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
#'             "Progressive Disease (PD)", "Stable Disease (SD)"
#'           )
#'         ),
#'         OVRINV = list(
#'           rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
#'           levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_forest_rsp <- function(label,
                            dataname,
                            parentname = "ADSL",
                            arm_var = teal.picks::variables(
                              choices = c("ARM", "ARMCD"),
                              selected = "ARM",
                              multiple = FALSE
                            ),
                            arm_ref_comp = NULL,
                            paramcd_var = teal.picks::variables("PARAMCD"),
                            paramcd_value = teal.picks::values(multiple = FALSE),
                            aval_var = teal.picks::variables("AVALC", fixed = TRUE),
                            subgroup_var = teal.picks::variables(selected = NULL, multiple = TRUE),
                            strata_var = teal.picks::variables(selected = NULL, multiple = TRUE),
                            stats = c("n_tot", "n", "n_rsp", "prop", "or", "ci"),
                            riskdiff = NULL,
                            fixed_symbol_size = TRUE,
                            conf_level = teal.picks::values(
                              c("0.95", "0.9", "0.8"),
                              selected = "0.95",
                              keep_order = TRUE
                            ),
                            default_responses = c(
                              "CR", "PR", "Y",
                              "Complete Response (CR)", "Partial Response (PR)"
                            ),
                            plot_height = c(500L, 200L, 2000L),
                            plot_width = c(1500L, 800L, 3000L),
                            rel_width_forest = c(25L, 0L, 100L),
                            font_size = c(15L, 1L, 30L),
                            pre_output = NULL,
                            post_output = NULL,
                            ggplot2_args = teal.widgets::ggplot2_args(),
                            transformators = list(),
                            decorators = list()) {
  message("Initializing tm_g_forest_rsp")

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_flag(fixed_symbol_size)
  checkmate::assert_class(arm_var, "variables", null.ok = TRUE)
  checkmate::assert_class(paramcd_var, "variables")
  checkmate::assert_class(paramcd_value, "values")
  checkmate::assert_class(aval_var, "variables")
  checkmate::assert_class(subgroup_var, "variables")
  checkmate::assert_class(strata_var, "variables")
  checkmate::assert_class(conf_level, "values")
  checkmate::assert_character(stats, min.len = 3)
  checkmate::assert_true(all(c("n_tot", "or", "ci") %in% stats))
  checkmate::assert_list(riskdiff, null.ok = TRUE)
  checkmate::assert_multi_class(default_responses, c("list", "character", "numeric"), null.ok = TRUE)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")
  assert_decorators(decorators, "plot")

  # Build picks from specs -------------------------------------------------------

  # arm_var: treatment variable from parentname (ADSL)
  arm_var <- if (!is.null(arm_var)) {
    teal.picks::picks(teal.picks::datasets(parentname), arm_var)
  } else {
    NULL
  }

  # paramcd: filter picks on dataname by PARAMCD value
  paramcd <- teal.picks::picks(
    teal.picks::datasets(dataname),
    variables = paramcd_var,
    values    = paramcd_value
  )

  # aval_var: analysis variable from dataname
  aval_var <- teal.picks::picks(teal.picks::datasets(dataname), aval_var)

  # subgroup_var: subgroup variables from parentname
  subgroup_var <- teal.picks::picks(teal.picks::datasets(parentname), subgroup_var)

  # strata_var: stratification variables from parentname
  strata_var <- teal.picks::picks(teal.picks::datasets(parentname), strata_var)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_forest_rsp,
    ui = ui_g_forest_rsp,
    ui_args = args[names(args) %in% names(formals(ui_g_forest_rsp))],
    server_args = args[names(args) %in% names(formals(srv_g_forest_rsp))],
    transformators = transformators,
    datanames = c(dataname, parentname)
  )
}

#' @keywords internal
ui_g_forest_rsp <- function(id,
                            arm_var,
                            paramcd,
                            aval_var,
                            subgroup_var,
                            strata_var,
                            conf_level,
                            fixed_symbol_size,
                            rel_width_forest,
                            font_size,
                            pre_output,
                            post_output,
                            decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Endpoint:"),
        teal.picks::picks_ui(ns("paramcd"), paramcd)
      ),
      tags$div(
        tags$label("Analysis Variable:"),
        teal.picks::picks_ui(ns("aval_var"), aval_var)
      ),
      selectInput(
        ns("responders"),
        "Responders",
        choices  = c("CR", "PR"),
        selected = c("CR", "PR"),
        multiple = TRUE
      ),
      if (!is.null(arm_var)) {
        tags$div(
          tags$div(
            tags$label("Select Treatment Variable:"),
            teal.picks::picks_ui(ns("arm_var"), arm_var)
          ),
          uiOutput(
            ns("arms_buckets"),
            title = paste(
              "Multiple reference groups are automatically combined into a single group",
              "when more than one value is selected."
            )
          )
        )
      },
      tags$div(
        tags$label("Subgroup Variables:"),
        teal.picks::picks_ui(ns("subgroup_var"), subgroup_var)
      ),
      tags$div(
        tags$label("Stratify by:"),
        teal.picks::picks_ui(ns("strata_var"), strata_var)
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional plot settings",
          teal.widgets::optionalSelectInput(
            inputId  = ns("conf_level"),
            label    = "Confidence Level",
            choices  = conf_level$choices,
            selected = conf_level$selected,
            multiple = FALSE,
            fixed    = conf_level$fixed %||% FALSE
          ),
          checkboxInput(ns("fixed_symbol_size"), "Fixed symbol size", value = fixed_symbol_size),
          teal.widgets::optionalSliderInputValMinMax(
            ns("rel_width_forest"),
            "Relative Width of Forest Plot (%)",
            rel_width_forest,
            ticks = FALSE, step = 1
          ),
          teal.widgets::optionalSliderInputValMinMax(
            ns("font_size"),
            "Table Font Size",
            font_size,
            ticks = FALSE, step = 1
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_g_forest_rsp <- function(id,
                             data,
                             dataname,
                             parentname,
                             arm_var,
                             arm_ref_comp,
                             paramcd,
                             aval_var,
                             subgroup_var,
                             strata_var,
                             stats,
                             riskdiff,
                             plot_height,
                             plot_width,
                             label,
                             default_responses,
                             ggplot2_args,
                             decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")

    # Setup arm ref/comp buckets UI and validator
    if (!is.null(arm_var)) {
      iv_arm_ref <- arm_ref_comp_observer(
        session,
        input,
        output,
        id_arm_var = "arm_var-variables-selected",
        data = reactive(data()[[parentname]]),
        arm_ref_comp = arm_ref_comp,
        module = "tm_g_forest_rsp"
      )
    }

    picks_list <- Filter(Negate(is.null), list(
      arm_var = arm_var,
      paramcd = paramcd,
      aval_var = aval_var,
      subgroup_var = subgroup_var,
      strata_var = strata_var
    ))

    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data  = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal:::validate_input(
        inputId   = "paramcd-values-selected",
        condition = !is.null(selectors$paramcd()$values$selected),
        message   = "Please select an endpoint (PARAMCD)."
      )
      teal:::validate_input(
        inputId   = "aval_var-variables-selected",
        condition = !is.null(selectors$aval_var()$variables$selected),
        message   = "An analysis variable is required."
      )
      if (!is.null(arm_var)) {
        teal:::validate_input(
          inputId   = "arm_var-variables-selected",
          condition = !is.null(selectors$arm_var()$variables$selected),
          message   = "A treatment variable is required."
        )
      }
      teal:::validate_input(
        inputId   = "conf_level",
        condition = !is.null(input$conf_level),
        message   = "Please choose a confidence level."
      )
      teal:::validate_input(
        inputId = "conf_level",
        condition = {
          cv <- suppressWarnings(as.numeric(input$conf_level))
          !is.na(cv) && cv > 0 && cv < 1
        },
        message = "Confidence level must be between 0 and 1."
      )
      teal:::validate_input(
        inputId   = "responders",
        condition = !is.null(input$responders) && length(input$responders) > 0,
        message   = "`Responders` field is empty."
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Forest Response Plot"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      obj
    })

    # Merge ADRS selectors: PARAMCD filter + aval_var → ANL
    anl_inputs <- teal.picks::merge_srv(
      "anl_inputs",
      data = validated_q,
      selectors = selectors[c("paramcd", "aval_var")],
      join_fun = "dplyr::inner_join",
      output_name = "ANL"
    )

    # Merge ADSL selectors: arm_var + subgroup_var + strata_var → ANL_ADSL
    adsl_selector_names <- intersect(
      c("arm_var", "subgroup_var", "strata_var"),
      names(selectors)
    )
    adsl_inputs <- teal.picks::merge_srv(
      "adsl_inputs",
      data = validated_q,
      selectors = selectors[adsl_selector_names],
      output_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      c(anl_inputs$data(), adsl_inputs$data())
    })

    # Update responders when PARAMCD or aval_var changes
    observeEvent(
      eventExpr = list(
        selectors$paramcd()$values$selected,
        selectors$aval_var()$variables$selected
      ),
      handlerExpr = {
        req(anl_q())
        anl <- anl_q()[["ANL"]]
        aval_var <- selectors$aval_var()$variables$selected
        paramcd_level <- selectors$paramcd()$values$selected

        if (length(paramcd_level) == 0 || length(aval_var) == 0) {
          return(NULL)
        }

        sel_param <- if (is.list(default_responses)) {
          default_responses[[paramcd_level]]
        } else {
          default_responses
        }

        common_rsp <- if (is.list(sel_param)) sel_param$rsp else sel_param

        responder_choices <- if (nrow(anl) == 0 || length(aval_var) == 0) {
          character(0)
        } else {
          if ("levels" %in% names(sel_param)) {
            if (length(intersect(unique(anl[[aval_var]]), sel_param$levels)) > 1) {
              sel_param$levels
            } else {
              union(anl[[aval_var]], sel_param$levels)
            }
          } else {
            unique(anl[[aval_var]])
          }
        }

        updateSelectInput(
          session, "responders",
          choices = responder_choices,
          selected = intersect(responder_choices, common_rsp)
        )
      }
    )

    validate_checks <- reactive({
      req(anl_q())
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      input_arm_var <- selectors$arm_var()$variables$selected
      input_aval_var <- selectors$aval_var()$variables$selected
      input_subgroup_var <- selectors$subgroup_var()$variables$selected
      input_strata_var <- selectors$strata_var()$variables$selected
      input_paramcd <- selectors$paramcd()$variables$selected

      # validate inputs
      validate_args <- list(
        adsl     = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_subgroup_var, input_strata_var),
        anl      = anl_filtered,
        anlvars  = c("USUBJID", "STUDYID", input_paramcd, input_aval_var),
        arm_var  = input_arm_var,
        ref_arm  = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp)
      )
      do.call(what = "validate_standard_inputs", validate_args)

      teal::validate_one_row_per_id(anl, key = c("USUBJID", "STUDYID", input_paramcd))

      if (length(input_subgroup_var) > 0) {
        validate(
          need(
            all(vapply(adsl_filtered[, input_subgroup_var, drop = FALSE], is.factor, logical(1))),
            "Not all subgroup variables are factors."
          )
        )
      }
      if (length(input_strata_var) > 0) {
        validate(
          need(
            all(vapply(adsl_filtered[, input_strata_var, drop = FALSE], is.factor, logical(1))),
            "Not all stratification variables are factors."
          )
        )
      }

      teal::validate_has_data(anl, min_nrow = 1)
      NULL
    })

    all_q <- reactive({
      validate_checks()

      input_arm_var <- selectors$arm_var()$variables$selected
      input_aval_var <- selectors$aval_var()$variables$selected
      input_subgroup_var <- selectors$subgroup_var()$variables$selected
      input_strata_var <- selectors$strata_var()$variables$selected

      # obj_var_name: the label for the selected PARAMCD level
      # In picks, we read it directly from the selector reactive
      paramcd_level <- selectors$paramcd()$values$selected
      obj_var_name <- paramcd_level %||% ""

      my_calls <- template_forest_rsp(
        dataname         = "ANL",
        parentname       = "ANL_ADSL",
        arm_var          = input_arm_var,
        ref_arm          = unlist(input$buckets$Ref),
        comp_arm         = unlist(input$buckets$Comp),
        obj_var_name     = obj_var_name,
        aval_var         = input_aval_var,
        responders       = input$responders,
        subgroup_var     = if (length(input_subgroup_var) != 0) input_subgroup_var else NULL,
        strata_var       = if (length(input_strata_var) != 0) input_strata_var else NULL,
        stats            = stats,
        riskdiff         = riskdiff,
        conf_level       = as.numeric(input$conf_level),
        col_symbol_size  = `if`(input$fixed_symbol_size, NULL, 1),
        font_size        = input$font_size,
        ggplot2_args     = ggplot2_args
      )

      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table and Plot")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_all_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "plot"),
      expr = reactive({
        substitute(
          cowplot::plot_grid(
            table,
            plot,
            align = "h",
            axis = "tblr",
            rel_widths = c(1 - input_rel_width_forest / 100, input_rel_width_forest / 100)
          ),
          env = list(input_rel_width_forest = input$rel_width_forest)
        )
      })
    )

    plot_r <- reactive({
      cowplot::plot_grid(
        decorated_all_q()[["table"]],
        decorated_all_q()[["plot"]],
        align = "h",
        axis = "tblr",
        rel_widths = c(1 - input$rel_width_forest / 100, input$rel_width_forest / 100)
      )
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id     = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width  = plot_width
    )

    set_chunk_dims(pws, decorated_all_q)
  })
}
