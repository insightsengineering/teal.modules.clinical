#' Template: Mixed Model Repeated Measurements (MMRM) Analysis
#'
#' Creates a valid expression to generate analysis tables and plots for Mixed Model Repeated Measurements.
#'
#' @inheritParams template_arguments
#' @param method (`string`)\cr a string specifying the adjustment method.
#' @param cor_struct (`string`)\cr a string specifying the correlation structure, defaults to
#'   `"unstructured"`. See [tern.mmrm::build_formula()] for more options.
#' @param weights_emmeans argument from [emmeans::emmeans()], "proportional" by default.
#' @param parallel (`flag`)\cr flag that controls whether optimizer search can use available free cores on the
#'   machine (not default).
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_a_mmrm()]
#'
#' @keywords internal
template_fit_mmrm <- function(parentname,
                              dataname,
                              aval_var,
                              arm_var,
                              ref_arm,
                              comp_arm = NULL,
                              combine_comp_arms = FALSE,
                              id_var,
                              visit_var,
                              cov_var,
                              conf_level = 0.95,
                              method = "Satterthwaite",
                              cor_struct = "unstructured",
                              weights_emmeans = "proportional",
                              parallel = FALSE) {
  # Data
  y <- list()
  data_list <- list()
  parent_list <- list()

  if (!is.null(arm_var)) {
    ref_arm_val <- paste(ref_arm, collapse = "/")

    data_list <- add_expr(
      data_list,
      prepare_arm(
        dataname = dataname,
        arm_var = arm_var,
        ref_arm = ref_arm,
        comp_arm = comp_arm,
        ref_arm_val = ref_arm_val
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

    if (combine_comp_arms) {
      data_list <- add_expr(
        data_list,
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
  } else {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = dataname,
        env = list(
          dataname = as.name(dataname)
        )
      )
    )

    parent_list <- add_expr(
      parent_list,
      substitute(
        expr = parentname,
        env = list(
          parentname = as.name(parentname)
        )
      )
    )
  }
  data_list <- add_expr(data_list, quote(tern::df_explicit_na(na_level = tern::default_na_str())))
  parent_list <- add_expr(parent_list, quote(tern::df_explicit_na(na_level = tern::default_na_str())))

  y$data <- substitute(
    expr = {
      anl <- data_pipe
      parentname <- parent_pipe
    },
    env = list(
      data_pipe = pipe_expr(data_list),
      parentname = as.name(parentname),
      parent_pipe = pipe_expr(parent_list)
    )
  )

  vars <- substitute(
    expr = list(
      response = aval_var,
      covariates = cov_var,
      id = id_var,
      arm = arm_var,
      visit = visit_var
    ),
    env = list(
      aval_var = aval_var,
      cov_var = cov_var,
      id_var = id_var,
      arm_var = arm_var,
      visit_var = visit_var
    )
  )
  y$fit <- substitute(
    expr = fit <- tern.mmrm::fit_mmrm(
      vars = vars,
      data = anl,
      conf_level = conf_level,
      method = method,
      cor_struct = cor_struct,
      weights_emmeans = weights_emmeans,
      parallel = parallel
    ),
    env = list(
      vars = vars,
      conf_level = conf_level,
      method = method,
      cor_struct = cor_struct,
      weights_emmeans = weights_emmeans,
      parallel = parallel
    )
  )

  y
}

#' @describeIn template_fit_mmrm Creates valid expressions to generate MMRM LS means, covariance matrix,
#'   fixed effects, and diagnostic tables.
#'
#' @inheritParams template_arguments
#' @param fit_name (`string`)\cr name of fitted MMRM object.
#' @param show_relative (`string`)\cr should the "reduction" (`control - treatment`, default) or the "increase"
#'   (`treatment - control`) be shown for the relative change from baseline.
#' @param table_type (`string`)\cr type of table to output.
#'
#' @keywords internal
template_mmrm_tables <- function(parentname,
                                 dataname,
                                 fit_name,
                                 arm_var,
                                 ref_arm,
                                 visit_var,
                                 paramcd,
                                 show_relative = c("increase", "reduction", "none"),
                                 table_type = "t_mmrm_cov",
                                 total_label = default_total_label(),
                                 basic_table_args = teal.widgets::basic_table_args()) {
  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  all_basic_table_args <- teal.widgets::resolve_basic_table_args(
    basic_table_args,
    module_table = teal.widgets::basic_table_args(show_colcounts = !is.null(arm_var))
  )

  # Build layout.
  layout_list <- list()
  layout_list <- layout_list %>%
    add_expr(substitute(
      expr = expr_basic_table_args,
      env = list(expr_basic_table_args = teal.widgets::parse_basic_table_args(all_basic_table_args))
    ))

  if (!is.null(arm_var)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::split_cols_by(var = arm_var, ref_group = ref_arm),
        env = list(arm_var = arm_var, ref_arm = ref_arm_val)
      )
    )
    show_relative <- match.arg(show_relative)

    if (show_relative == "none") {
      layout_list <- add_expr(
        layout_list,
        substitute(
          expr = rtables::split_rows_by(visit_var) %>%
            tern::append_varlabels(dataname, visit_var) %>%
            tern.mmrm::summarize_lsmeans(
              .stats = c(
                "n",
                "adj_mean_se",
                "adj_mean_ci",
                "diff_mean_se",
                "diff_mean_ci",
                "p_value"
              )
            ) %>%
            rtables::append_topleft(paste0("  ", paramcd)),
          env = list(
            dataname = as.name(dataname),
            visit_var = visit_var,
            paramcd = paramcd
          )
        )
      )
    } else {
      layout_list <- add_expr(
        layout_list,
        substitute(
          expr = rtables::split_rows_by(visit_var) %>%
            tern::append_varlabels(dataname, visit_var) %>%
            tern.mmrm::summarize_lsmeans(show_relative = show_relative) %>%
            rtables::append_topleft(paste0("  ", paramcd)),
          env = list(
            dataname = as.name(dataname),
            visit_var = visit_var,
            paramcd = paramcd,
            show_relative = show_relative
          )
        )
      )
    }
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_overall_col(total_label) %>%
          rtables::split_rows_by(visit_var) %>%
          tern.mmrm::summarize_lsmeans(arms = FALSE) %>%
          rtables::append_topleft(paste0("  ", paramcd)),
        env = list(
          total_label = total_label,
          visit_var = visit_var,
          paramcd = paramcd
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  switch(table_type,
    t_mmrm_lsmeans = {
      y$lsmeans_table <- substitute(
        expr = {
          lsmeans_table <- rtables::build_table(
            lyt = lyt,
            df = tern::df_explicit_na(broom::tidy(fit_mmrm), na_level = tern::default_na_str()),
            alt_counts_df = parentname
          )
        },
        env = list(
          parentname = as.name(parentname),
          fit_mmrm = as.name(fit_name)
        )
      )
    },
    t_mmrm_cov = {
      y$cov_matrix <- substitute(
        expr = {
          covariance_table <- tern.mmrm::as.rtable(fit_mmrm, type = "cov")
          rtables::subtitles(covariance_table) <- st
        },
        env = list(
          fit_mmrm = as.name(fit_name),
          st = basic_table_args$subtitles
        )
      )
    },
    t_mmrm_fixed = {
      y$fixed_effects <- substitute(
        expr = {
          fixed_effects_table <- tern.mmrm::as.rtable(fit_mmrm, type = "fixed")
          rtables::subtitles(fixed_effects_table) <- st
        },
        env = list(
          fit_mmrm = as.name(fit_name),
          st = basic_table_args$subtitles
        )
      )
    },
    t_mmrm_diagnostic = {
      y$diagnostic_table <- substitute(
        expr = {
          diagnostic_table <- tern.mmrm::as.rtable(fit_mmrm, type = "diagnostic")
          rtables::subtitles(diagnostic_table) <- st
        },
        env = list(
          fit_mmrm = as.name(fit_name),
          st = basic_table_args$subtitles
        )
      )
    }
  )
  y
}

#' @describeIn template_fit_mmrm Creates valid expressions to generate MMRM LS means and
#'   diagnostic plots.
#'
#' @inheritParams template_arguments
#' @param lsmeans_plot (named `list`)\cr a `list` of controls for LS means plot.
#'   See more [tern.mmrm::g_mmrm_lsmeans()].
#' @param diagnostic_plot (named `list`)\cr a `list` of controls for diagnostic_plot.
#'   See more [tern.mmrm::g_mmrm_diagnostic()].
#'
#' @keywords internal
template_mmrm_plots <- function(fit_name,
                                lsmeans_plot = list(
                                  select = c("estimates", "contrasts"),
                                  width = 0.6,
                                  show_pval = FALSE
                                ),
                                diagnostic_plot = list(
                                  type = "fit-residual",
                                  z_threshold = NULL
                                ),
                                ggplot2_args = teal.widgets::ggplot2_args()) {
  y <- list()

  if (!is.null(lsmeans_plot)) {
    parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
      teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["lsmeans"]],
        user_default = ggplot2_args[["default"]]
      )
    )

    plot_call <- substitute(
      expr =
        tern.mmrm::g_mmrm_lsmeans(
          fit_mmrm,
          select = select,
          width = width,
          show_pval = show_pval,
          titles = if (is.null(fit_mmrm$vars$arm)) {
            c(
              estimates = paste("Adjusted mean of", fit_mmrm$labels$response, " at visits"),
              contrasts = " "
            )
          } else {
            c(
              estimates = paste(
                "Adjusted mean of",
                fit_mmrm$labels$response,
                "by treatment at visits"
              ),
              contrasts = paste0(
                "Differences of ",
                fit_mmrm$labels$response,
                " adjusted means vs. control ('",
                fit_mmrm$ref_level,
                "')"
              )
            )
          }
        ),
      env = list(
        fit_mmrm = as.name(fit_name),
        select = lsmeans_plot$select,
        width = lsmeans_plot$width,
        show_pval = lsmeans_plot$show_pval
      )
    )

    y$lsmeans_plot <- substitute(
      expr = {
        lsmeans_plot <- plot_call
        lsmeans_plot
      },
      env = list(
        plot_call = Reduce(function(x, y) call("+", x, y), c(plot_call, parsed_ggplot2_args))
      )
    )
  }

  if (!is.null(diagnostic_plot)) {
    parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
      teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["diagnostic"]],
        user_default = ggplot2_args[["default"]]
      )
    )

    plot_call <- substitute(
      expr =
        tern.mmrm::g_mmrm_diagnostic(
          fit_mmrm,
          type = type,
          z_threshold = z_threshold
        ),
      env = list(
        fit_mmrm = as.name(fit_name),
        type = diagnostic_plot$type,
        z_threshold = diagnostic_plot$z_threshold
      )
    )

    y$diagnostic_plot <- substitute(
      expr = {
        diagnostic_plot <- plot_call
        diagnostic_plot
      },
      env = list(
        plot_call = Reduce(function(x, y) call("+", x, y), c(plot_call, parsed_ggplot2_args))
      )
    )
  }

  y
}

#' teal Module: Mixed Model Repeated Measurements (MMRM) Analysis
#'
#' This module produces analysis tables and plots for Mixed Model Repeated Measurements.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_mmrm_tables
#' @inheritParams template_mmrm_plots
#' @param method ([teal.transform::choices_selected()])\cr object with
#'   all available choices and pre-selected option for the adjustment method.
#' @param ggplot2_args (`ggplot2_args`) optional\cr object created by [`teal.widgets::ggplot2_args()`]
#'   with settings for all the plots or named list of `ggplot2_args` objects for plot-specific settings.
#'   List names should match the following: `c("default", "lsmeans", "diagnostic")`. The argument is merged
#'   with option `teal.ggplot2_args` and with default module arguments (hard coded in the module body).
#'   For more details, see the help vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @note
#' The ordering of the input data sets can lead to slightly different numerical results or
#' different convergence behavior. This is a known observation with the used package
#' `lme4`. However, once convergence is achieved, the results are reliable up to
#' numerical precision.
#'
#' @inherit module_arguments return seealso
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `lsmeans_plot` (`ggplot`)
#' - `diagnostic_plot` (`ggplot`)
#' - `lsmeans_table` (`TableTree`- output from `rtables::build_table`)
#' - `covariance_table` (`ElementaryTable`- output from `rtables::build_table`)
#' - `fixed_effects_table` (`ElementaryTable`- output from `rtables::build_table`)
#' - `diagnostic_table` (`ElementaryTable`- output from `rtables::build_table`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_a_mrmm(
#'    ..., # arguments for module
#'    decorators = list(
#'      lsmeans_plot = teal_transform_module(...), # applied only to `lsmeans_plot` output
#'      diagnostic_plot = teal_transform_module(...), # applied only to `diagnostic_plot` output
#'      lsmeans_table = teal_transform_module(...), # applied only to `lsmeans_table` output
#'      covariance_table = teal_transform_module(...), # applied only to `covariance_table` output
#'      fixed_effects_table = teal_transform_module(...), # applied only to `fixed_effects_table` output
#'      diagnostic_table = teal_transform_module(...) # applied only to `diagnostic_table` output
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
#'
#' arm_ref_comp <- list(
#'   ARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
#'
#' data <- teal_data()
#' data <- within(data, {
#'   library(teal.modules.clinical)
#'   library(dplyr)
#'   ADSL <- tmc_ex_adsl
#'   ADQS <- tmc_ex_adqs %>%
#'     filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
#'     filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")) %>%
#'     mutate(
#'       AVISIT = as.factor(AVISIT),
#'       AVISITN = rank(AVISITN) %>%
#'         as.factor() %>%
#'         as.numeric() %>%
#'         as.factor() # making consecutive numeric factor
#'     )
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_a_mmrm(
#'       label = "MMRM",
#'       dataname = "ADQS",
#'       aval_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
#'       id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       visit_var = choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         choices = value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
#'         selected = "FKSI-FWB"
#'       ),
#'       cov_var = choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_a_mmrm <- function(label,
                      dataname,
                      parentname = ifelse(
                        inherits(arm_var, "data_extract_spec"),
                        teal.transform::datanames_input(arm_var),
                        "ADSL"
                      ),
                      aval_var,
                      id_var,
                      arm_var,
                      visit_var,
                      cov_var,
                      arm_ref_comp = NULL,
                      paramcd,
                      method = teal.transform::choices_selected(
                        c("Satterthwaite", "Kenward-Roger", "Kenward-Roger-Linear"),
                        "Satterthwaite",
                        keep_order = TRUE
                      ),
                      conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                      plot_height = c(700L, 200L, 2000L),
                      plot_width = NULL,
                      total_label = default_total_label(),
                      pre_output = NULL,
                      post_output = NULL,
                      basic_table_args = teal.widgets::basic_table_args(),
                      ggplot2_args = teal.widgets::ggplot2_args(),
                      transformators = list(),
                      decorators = list()) {
  message("Initializing tm_a_mmrm")
  cov_var <- teal.transform::add_no_selected_choices(cov_var, multiple = TRUE)
  checkmate::assert_string(label)
  checkmate::assert_string(total_label)
  checkmate::assert_string(dataname)
  checkmate::assert_class(aval_var, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(visit_var, "choices_selected")
  checkmate::assert_class(cov_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(method, "choices_selected")
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)
  plot_choices <- c("lsmeans", "diagnostic")
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  assert_decorators(
    decorators,
    c(
      "lsmeans_table",
      "lsmeans_plot",
      "covariance_table",
      "fixed_effects_table",
      "diagnostic_table",
      "diagnostic_plot"
    )
  )

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    visit_var = cs_to_des_select(visit_var, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE),
    split_covariates = cs_to_des_select(split_choices(cov_var), dataname = dataname, multiple = TRUE),
    aval_var = cs_to_des_select(aval_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_mmrm,
    ui = ui_mmrm,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        total_label = total_label,
        plot_height = plot_height,
        plot_width = plot_width,
        basic_table_args = basic_table_args,
        ggplot2_args = ggplot2_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_mmrm <- function(id, ...) {
  a <- list(...) # module args
  ns <- NS(id)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$id_var,
    a$visit_var,
    a$cov_var,
    a$aval_var
  )

  tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        textOutput(ns("null_input_msg")),
        tags$h3(textOutput(ns("mmrm_title"))),
        teal.widgets::table_with_settings_ui(ns("mmrm_table")),
        teal.widgets::plot_with_settings_ui(id = ns("mmrm_plot"))
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"), tags$br(),
        teal.transform::datanames_input(a[c("arm_var", "paramcd", "id_var", "visit_var", "cov_var", "aval_var")]),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Model Settings",
            teal.transform::data_extract_ui(
              id = ns("aval_var"),
              label = "Analysis Variable",
              data_extract_spec = a$aval_var,
              is_single_dataset = is_single_dataset_value
            ),
            teal.transform::data_extract_ui(
              id = ns("paramcd"),
              label = "Select Endpoint",
              data_extract_spec = a$paramcd,
              is_single_dataset = is_single_dataset_value
            ),
            teal.transform::data_extract_ui(
              id = ns("visit_var"),
              label = "Visit Variable",
              data_extract_spec = a$visit_var,
              is_single_dataset = is_single_dataset_value
            ),
            teal.transform::data_extract_ui(
              id = ns("cov_var"),
              label = "Covariates",
              data_extract_spec = a$cov_var,
              is_single_dataset = is_single_dataset_value
            ),
            shinyjs::hidden(
              teal.transform::data_extract_ui(
                id = ns("split_covariates"),
                label = "Split Covariates",
                data_extract_spec = a$split_covariates,
                is_single_dataset = is_single_dataset_value
              )
            ),
            teal.transform::data_extract_ui(
              id = ns("arm_var"),
              label = "Select Treatment Variable",
              data_extract_spec = a$arm_var,
              is_single_dataset = is_single_dataset_value
            ),
            shinyjs::hidden(uiOutput(ns("arms_buckets"))),
            shinyjs::hidden(
              helpText(
                id = ns("help_text"), "Multiple reference groups are automatically combined into a single group."
              )
            ),
            shinyjs::hidden(
              checkboxInput(
                ns("combine_comp_arms"),
                "Combine all comparison groups?",
                value = FALSE
              )
            ),
            teal.transform::data_extract_ui(
              id = ns("id_var"),
              label = "Subject Identifier",
              data_extract_spec = a$id_var,
              is_single_dataset = is_single_dataset_value
            ),
            selectInput(
              ns("weights_emmeans"),
              "Weights for LS means",
              choices = c("proportional", "equal"),
              selected = "proportional",
              multiple = FALSE
            ),
            selectInput(
              ns("cor_struct"),
              "Correlation Structure",
              choices = eval(formals(tern.mmrm::build_formula)$cor_struct),
              multiple = FALSE
            ),
            teal.widgets::optionalSelectInput(
              ns("method"),
              "Adjustment Method",
              a$method$choices,
              a$method$selected,
              multiple = FALSE,
              fixed = a$method$fixed
            ),
            teal.widgets::optionalSelectInput(
              ns("conf_level"),
              "Confidence Level",
              a$conf_level$choices,
              a$conf_level$selected,
              multiple = FALSE,
              fixed = a$conf_level$fixed
            ),
            checkboxInput(
              ns("parallel"),
              "Parallel Computing",
              value = TRUE
            )
          )
        ),
        actionButton(
          ns("button_start"),
          "Fit Model",
          icon = icon("calculator"),
          width = "100%",
          class = "btn action-button",
          style = "background-color: orange; color: black; margin-bottom: 1rem;"
        ),
        radioButtons(
          ns("output_function"),
          "Output Type",
          choices = c(
            "LS means table" = "t_mmrm_lsmeans",
            "LS means plots" = "g_mmrm_lsmeans",
            "Covariance estimate" = "t_mmrm_cov",
            "Fixed effects" = "t_mmrm_fixed",
            "Fit statistics" = "t_mmrm_diagnostic",
            "Diagnostic plots" = "g_mmrm_diagnostic"
          ),
          selected = "t_mmrm_lsmeans"
        ),
        # Decorators ---
        conditionalPanel(
          condition = sprintf("input['%s'] == '%s'", ns("output_function"), "t_mmrm_lsmeans"),
          ui_decorate_teal_data(ns("d_lsmeans_table"), select_decorators(a$decorators, "lsmeans_table"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == '%s'", ns("output_function"), "g_mmrm_lsmeans"),
          ui_decorate_teal_data(ns("d_lsmeans_plot"), select_decorators(a$decorators, "lsmeans_plot"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == '%s'", ns("output_function"), "t_mmrm_cov"),
          ui_decorate_teal_data(ns("d_covariance_table"), select_decorators(a$decorators, "covariance_table"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == '%s'", ns("output_function"), "t_mmrm_fixed"),
          ui_decorate_teal_data(ns("d_fixed_effects_table"), select_decorators(a$decorators, "fixed_effects_table"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == '%s'", ns("output_function"), "t_mmrm_diagnostic"),
          ui_decorate_teal_data(ns("d_diagnostic_table"), select_decorators(a$decorators, "diagnostic_table"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == '%s'", ns("output_function"), "g_mmrm_diagnostic"),
          ui_decorate_teal_data(ns("d_diagnostic_plot"), select_decorators(a$decorators, "diagnostic_plot"))
        ),
        # End of Decorators ---
        conditionalPanel(
          condition = paste0(
            "input['", ns("output_function"), "'] == 't_mmrm_lsmeans'", " || ",
            "input['", ns("output_function"), "'] == 'g_mmrm_lsmeans'", " || ",
            "input['", ns("output_function"), "'] == 'g_mmrm_diagnostic'"
          ),
          bslib::accordion(
            open = TRUE,
            bslib::accordion_panel(
              title = "Output Settings",

              # Additional option for LS means table.
              selectInput(
                ns("t_mmrm_lsmeans_show_relative"),
                "Show Relative Change",
                choices = c("reduction", "increase", "none"),
                selected = "reduction",
                multiple = FALSE
              ),
              checkboxGroupInput(
                ns("g_mmrm_lsmeans_select"),
                "LS means plots",
                choices = c(
                  "Estimates" = "estimates",
                  "Contrasts" = "contrasts"
                ),
                selected = c("estimates", "contrasts"),
                inline = TRUE
              ),
              sliderInput(
                ns("g_mmrm_lsmeans_width"),
                "CI bar width",
                min = 0.1,
                max = 1,
                value = 0.6
              ),
              checkboxInput(
                ns("g_mmrm_lsmeans_contrasts_show_pval"),
                "Show contrasts p-values",
                value = FALSE
              ),
              # Additional options for diagnostic plots.
              radioButtons(
                ns("g_mmrm_diagnostic_type"),
                "Diagnostic plot type",
                choices = c(
                  "Fitted vs. Residuals" = "fit-residual",
                  "Normal Q-Q Plot of Residuals" = "q-q-residual"
                ),
                selected = NULL
              ),
              sliderInput(
                ns("g_mmrm_diagnostic_z_threshold"),
                "Label observations above this threshold",
                min = 0.1,
                max = 10,
                value = 3
              )
            )
          )
        )
      ),
      forms = tagList(
        teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_mmrm <- function(id,
                     data,
                     dataname,
                     parentname,
                     arm_var,
                     paramcd,
                     id_var,
                     visit_var,
                     cov_var,
                     split_covariates,
                     aval_var,
                     arm_ref_comp,
                     label,
                     total_label,
                     plot_height,
                     plot_width,
                     basic_table_args,
                     ggplot2_args,
                     decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    # Reactive responsible for sending a disable/enable signal
    # to show R code and debug info buttons
    disable_r_code <- reactiveVal(FALSE)

    observeEvent(input[[extract_input("cov_var", dataname)]], {
      # update covariates as actual variables
      split_interactions_values <- split_interactions(input[[extract_input("cov_var", dataname)]])
      arm_var_value <- input[[extract_input("arm_var", parentname)]]
      arm_in_cov <- length(intersect(split_interactions_values, arm_var_value)) >= 1L
      if (arm_in_cov) {
        split_covariates_selected <- setdiff(split_interactions_values, arm_var_value)
      } else {
        split_covariates_selected <- split_interactions_values
      }
      teal.widgets::updateOptionalSelectInput(
        session,
        inputId = extract_input("split_covariates", dataname),
        selected = split_covariates_selected
      )
    })

    arm_ref_comp_iv <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname), # From UI.
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_mmrm"
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        id_var = id_var,
        visit_var = visit_var,
        split_covariates = split_covariates,
        cov_var = cov_var, # only needed for validation see selector_list_without_cov reactive
        aval_var = aval_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("'Analysis Variable' field is not selected"),
        visit_var = shinyvalidate::sv_required("'Visit Variable' field is not selected"),
        arm_var = shinyvalidate::sv_required("'Treatment Variable' field is not selected"),
        id_var = shinyvalidate::sv_required("'Subject Identifier' field is not selected"),
        # validation on cov_var
        cov_var = function(value) {
          if (length(selector_list()$visit_var()$select) == 0) {
            return(NULL)
          }
          if ("BASE:AVISIT" %in% value && selector_list()$visit_var()$select == "AVISITN") {
            paste(
              "'BASE:AVISIT' is not a valid covariate when 'AVISITN' is selected as visit variable.",
              "Please deselect 'BASE:AVISIT' as a covariate or change visit variable to 'AVISIT'."
            )
          } else if ("BASE:AVISITN" %in% value && selector_list()$visit_var()$select == "AVISIT") {
            paste(
              "'BASE:AVISITN' is not a valid covariate when 'AVISIT' is selected as visit variable.",
              "Please deselect 'BASE:AVISITN' as a covariate or change visit variable to 'AVISITN'."
            )
          }
        }
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("'Select Endpoint' field is not selected")
      )
    )

    # selector_list includes cov_var as it is needed for validation rules
    # but not needed to merge so it is removed here
    selector_list_without_cov <- reactive({
      selector_list()[names(selector_list()) != "cov_var"]
    })

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(arm_ref_comp_iv)
      iv$add_rule("conf_level", shinyvalidate::sv_required("'Confidence Level' field is not selected"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(
          0, 1,
          message_fmt = "Confidence level must be between 0 and 1"
        )
      )
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list_without_cov,
      merge_function = "dplyr::inner_join"
    )

    adsl_merge_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    # Set tern default for missing values for reproducibility (on .onLoad for the examples)
    data_with_tern_options_r <- reactive({
      within(data(),
        {
          tern::set_default_na_str(default_na_str)
        },
        default_na_str = getOption("tern_default_na_str", default = "<Missing>")
      )
    })

    anl_q <- reactive({
      obj <- data_with_tern_options_r()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Mixed Model Repeated Measurements (MMRM) Analysis"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(code = as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(code = as.expression(adsl_merge_inputs()$expr))
    })

    # Initially hide the output title because there is no output yet.
    shinyjs::hide("mmrm_title")

    # reactiveVal used to send a signal to plot_with_settings module to hide the UI
    show_plot_rv <- reactiveVal(FALSE)

    # this will store the current/last state of inputs and data that generated a model-fit
    # its purpose is to allow any input change to be checked whether it resulted in an out of sync state
    state <- reactiveValues(input = NULL, button_start = 0)

    # Note:
    # input$parallel does not get us out of sync (it just takes longer to get to same result)
    sync_inputs <- c(
      extract_input("aval_var", dataname),
      extract_input("paramcd", dataname, filter = TRUE),
      extract_input("arm_var", parentname),
      "Ref",
      "Comp",
      "combine_comp_arms",
      extract_input("visit_var", dataname),
      extract_input("cov_var", dataname),
      extract_input("id_var", dataname),
      "weights_emmeans",
      "cor_struct",
      "method",
      "conf_level"
    )

    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel.

    observeEvent(adsl_merge_inputs()$columns_source$arm_var, {
      arm_var <- as.vector(adsl_merge_inputs()$columns_source$arm_var)
      if (length(arm_var) == 0) {
        shinyjs::hide("arms_buckets")
        shinyjs::hide("help_text")
        shinyjs::hide("combine_comp_arms")
      } else {
        shinyjs::show("arms_buckets")
        shinyjs::show("help_text")
        shinyjs::show("combine_comp_arms")
      }
    })

    # Event handler:
    # Show either the plot or the table output.
    observeEvent(input$output_function, {
      output_function <- input$output_function
      if (isTRUE(grepl("^t_", output_function))) {
        show_plot_rv(FALSE)
        shinyjs::show("mmrm_table")
      } else if (isTRUE(grepl("^g_", output_function))) {
        shinyjs::hide("mmrm_table")
        show_plot_rv(TRUE)
      } else {
        stop("unknown output type")
      }
    })

    # Event handler:
    # Show or hide LS means table option.
    observeEvent(input$output_function, {
      output_function <- input$output_function
      if (isTRUE(output_function == "t_mmrm_lsmeans")) {
        shinyjs::show("t_mmrm_lsmeans_show_relative")
      } else {
        shinyjs::hide("t_mmrm_lsmeans_show_relative")
      }
    })

    # Event handler:
    # Show or hide the LS means plot options.
    observeEvent(list(input$output_function, input$g_mmrm_lsmeans_select), {
      output_function <- input$output_function
      g_mmrm_lsmeans_select <- input$g_mmrm_lsmeans_select
      if (isTRUE(output_function == "g_mmrm_lsmeans")) {
        shinyjs::show("g_mmrm_lsmeans_select")
        shinyjs::show("g_mmrm_lsmeans_width")
        if (isTRUE("contrasts" %in% g_mmrm_lsmeans_select)) {
          shinyjs::show("g_mmrm_lsmeans_contrasts_show_pval")
        } else {
          shinyjs::hide("g_mmrm_lsmeans_contrasts_show_pval")
        }
      } else {
        shinyjs::hide("g_mmrm_lsmeans_select")
        shinyjs::hide("g_mmrm_lsmeans_width")
        shinyjs::hide("g_mmrm_lsmeans_contrasts_show_pval")
      }
    })

    # Event handler:
    # Show or hide the diagnostic plot type option.
    observeEvent(list(input$output_function, input$g_mmrm_diagnostic_type), {
      output_function <- input$output_function
      g_mmrm_diagnostic_type <- input$g_mmrm_diagnostic_type
      if (isTRUE(output_function == "g_mmrm_diagnostic")) {
        shinyjs::show("g_mmrm_diagnostic_type")
        if (isTRUE(g_mmrm_diagnostic_type == "q-q-residual")) {
          shinyjs::show("g_mmrm_diagnostic_z_threshold")
        } else {
          shinyjs::hide("g_mmrm_diagnostic_z_threshold")
        }
      } else {
        shinyjs::hide("g_mmrm_diagnostic_type")
        shinyjs::hide("g_mmrm_diagnostic_z_threshold")
      }
    })

    # Event handler:
    # When the "Fit Model" button is clicked, hide initial message, show title, disable model fit and enable
    # show R code buttons.
    shinyjs::onclick("button_start", {
      state$input <- mmrm_inputs_reactive()
      shinyjs::hide("null_input_msg")
      shinyjs::disable("button_start")
      success <- try(mmrm_fit(), silent = TRUE)
      if (!inherits(success, "try-error")) {
        shinyjs::show("mmrm_title")
        disable_r_code(FALSE)
      } else {
        shinyjs::hide("mmrm_title")
        # show R code and debug info buttons will have already been hidden by disable_r_code
      }
    })

    # all the inputs and data that can be out of sync with the fitted model
    mmrm_inputs_reactive <- reactive({
      shinyjs::disable("button_start")
      disable_r_code(TRUE)
      teal::validate_inputs(iv_r())
      encoding_inputs <- lapply(
        sync_inputs,
        function(x) {
          if (x %in% c("Ref", "Comp")) {
            unlist(input$buckets[[x]])
          } else {
            input[[x]]
          }
        }
      )
      names(encoding_inputs) <- sync_inputs

      adsl_filtered <- anl_q()[["ADSL"]]
      anl_filtered <- anl_q()[[dataname]]

      teal::validate_has_data(adsl_filtered, min_nrow = 1)
      teal::validate_has_data(anl_filtered, min_nrow = 1)
      validate_checks()
      c(list(adsl_filtered = adsl_filtered, anl_filtered = anl_filtered), encoding_inputs)
    })

    output$null_input_msg <- renderText({
      mmrm_inputs_reactive()
      paste(
        "Please first specify 'Model Settings' and press 'Fit Model'.",
        "Afterwards choose 'Output Type' and optional 'Output Settings'.",
        "If changes to the 'Model Settings' or dataset (by filtering) are made,",
        "then the 'Fit Model' button must be pressed again to update the MMRM model.",
        "Note that the 'Show R Code' button can only be clicked if the model fit is up to date."
      )
    })

    # compares the mmrm_inputs_reactive values with the values stored in 'state'
    state_has_changed <- reactive({
      req(state$input)
      displayed_state <- mmrm_inputs_reactive()
      equal_ADSL <- all.equal(state$input$adsl_filtered, displayed_state$adsl_filtered) # nolint: object_name.
      equal_dataname <- all.equal(state$input$anl_filtered, displayed_state$anl_filtered)
      true_means_change <- vapply(
        sync_inputs,
        FUN = function(x) {
          if (is.null(state$input[[x]])) {
            if (is.null(displayed_state[[x]])) {
              return(FALSE)
            } else {
              return(TRUE)
            }
          } else if (is.null(displayed_state[[x]])) {
            return(TRUE)
          }
          if (length(state$input[[x]]) != length(displayed_state[[x]])) {
            return(TRUE)
          }
          any(sort(state$input[[x]]) != sort(displayed_state[[x]]))
        },
        FUN.VALUE = logical(1)
      )

      # all.equal function either returns TRUE or a character scalar to describe where there is inequality
      any(c(is.character(equal_ADSL), is.character(equal_dataname), true_means_change))
    })

    # Event handler:
    # These trigger when we are out of sync and then enable the start button and
    # disable the show R code button and show warning message
    observeEvent(mmrm_inputs_reactive(), {
      shinyjs::enable("button_start")
      disable_r_code(TRUE)
      if (!state_has_changed()) {
        disable_r_code(FALSE)
        shinyjs::disable("button_start")
      }
    })

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl_data <- anl_q()[["ANL"]]

      anl_m_inputs <- anl_inputs()
      if (!is.null(input[[extract_input("arm_var", parentname)]])) {
        input_arm_var <- as.vector(anl_m_inputs$columns_source$arm_var)
      } else {
        input_arm_var <- NULL
      }
      input_visit_var <- as.vector(anl_m_inputs$columns_source$visit_var)

      input_aval_var <- as.vector(anl_m_inputs$columns_source$aval_var)
      input_id_var <- as.vector(anl_m_inputs$columns_source$id_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # Split the existing covariate strings in their variable parts, to allow "A*B" and "A:B" notations.
      input_cov_var <- as.vector(anl_m_inputs$columns_source$split_covariates)
      covariate_parts <- split_interactions(input_cov_var)

      all_x_vars <- c(input_arm_var, input_visit_var, covariate_parts)

      all_x_vars_in_adsl <- intersect(
        all_x_vars,
        colnames(adsl_filtered)
      )
      all_x_vars_in_anl <- setdiff(
        all_x_vars,
        all_x_vars_in_adsl
      )

      adslvars <- unique(c("USUBJID", "STUDYID", input_arm_var, input_id_var, all_x_vars_in_adsl))
      anlvars <- unique(c("USUBJID", "STUDYID", input_paramcd, input_aval_var, input_visit_var, all_x_vars_in_anl))

      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = adslvars,
        anl = anl_filtered,
        anlvars = anlvars,
        arm_var = input_arm_var,
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        min_nrow = 10,
        need_arm = FALSE
      )

      Map(
        function(visit_df, visit_name) {
          dup <- any(duplicated(visit_df[[input_id_var]]))
          validate(need(!dup, paste("Duplicated subject ID found at", visit_name)))
        },
        split(anl_data, anl_data[[input_visit_var]]),
        levels(anl_data[[input_visit_var]])
      )
    })

    # Connector:
    # Fit the MMRM, once the user clicks on the start button.
    mmrm_fit <- eventReactive(input$button_start, {
      qenv <- anl_q()
      anl_m_inputs <- anl_inputs()

      my_calls <- template_fit_mmrm(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        aval_var = as.vector(anl_m_inputs$columns_source$aval_var),
        arm_var = input[[extract_input("arm_var", parentname)]],
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        combine_comp_arms = input$combine_comp_arms,
        id_var = as.vector(anl_m_inputs$columns_source$id_var),
        visit_var = as.vector(anl_m_inputs$columns_source$visit_var),
        cov_var = input[[extract_input("cov_var", dataname)]],
        conf_level = as.numeric(input$conf_level),
        method = as.character(input$method),
        cor_struct = input$cor_struct,
        weights_emmeans = input$weights_emmeans,
        parallel = input$parallel
      )
      teal.code::eval_code(qenv, as.expression(unlist(my_calls)))
    })

    output$mmrm_title <- renderText({
      new_inputs <- try(state_has_changed(), silent = TRUE)
      # No message needed here because it will be displayed by either plots or tables output
      validate(need(!inherits(new_inputs, "try-error") && !new_inputs, character(0)))

      # Input on output type.
      output_function <- input$output_function
      g_mmrm_diagnostic_type <- input$g_mmrm_diagnostic_type
      g_mmrm_lsmeans_select <- input$g_mmrm_lsmeans_select

      output_title <- switch(output_function,
        "t_mmrm_cov" = "Residual Covariance Matrix Estimate",
        "t_mmrm_diagnostic" = "Model Fit Statistics",
        "t_mmrm_fixed" = "Fixed Effects Estimates",
        "t_mmrm_lsmeans" = "LS Means and Contrasts Estimates",
        "g_mmrm_diagnostic" = switch(g_mmrm_diagnostic_type,
          "fit-residual" = "Marginal Fitted Values vs. Residuals",
          "q-q-residual" = "Q-Q Normal Plot for Standardized Residuals"
        ),
        "g_mmrm_lsmeans" = if (setequal(g_mmrm_lsmeans_select, c("estimates", "contrasts"))) {
          "LS Means Estimates and Contrasts"
        } else if (identical(g_mmrm_lsmeans_select, "estimates")) {
          "LS Means Estimates"
        } else {
          "LS Means Contrasts"
        }
      )
      output_title
    })

    table_q <- reactive({
      validate(
        need(
          !state_has_changed(),
          "Inputs changed and no longer reflect the fitted model. Press `Fit Model` button again to re-fit model."
        )
      )
      # Input on output type.
      output_function <- input$output_function

      # If the output is not a table, stop here.
      if (!isTRUE(grepl("^t_", output_function))) {
        return(NULL)
      }
      # Get the fit stack while evaluating the fit code at the same time.
      qenv <- mmrm_fit()
      fit <- qenv[["fit"]]

      anl_m_inputs <- anl_inputs()

      ANL <- qenv[["ANL"]]
      ANL_ADSL <- qenv[["ANL_ADSL"]]
      paramcd <- unique(ANL[[unlist(paramcd$filter)["vars_selected"]]])

      basic_table_args$subtitles <- paste0(
        "Analysis Variable: ", anl_m_inputs$columns_source$aval_var,
        ",  Endpoint: ", anl_m_inputs$filter_info$paramcd[[1]]$selected[[1]],
        ifelse(is.null(fit$vars$covariates), "", paste(",  Covariates:", paste(fit$vars$covariates, collapse = ", ")))
      )
      basic_table_args$main_footer <- c(
        paste("Weights for LS Means:", input$weights_emmeans),
        paste("Correlation Structure:", input$cor_struct),
        paste("Adjustment Method:", input$method)
      )

      mmrm_table <- template_mmrm_tables(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        fit_name = "fit",
        arm_var = input[[extract_input("arm_var", parentname)]],
        ref_arm = unlist(input$buckets$Ref),
        visit_var = as.vector(anl_m_inputs$columns_source$visit_var),
        paramcd = paramcd,
        show_relative = input$t_mmrm_lsmeans_show_relative,
        table_type = output_function,
        total_label = total_label,
        basic_table_args = basic_table_args
      )

      obj <- qenv
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
      teal.code::eval_code(obj, as.expression(mmrm_table))
    })

    # Endpoint:
    # Plot outputs.
    plot_q <- reactive({
      validate(
        need(
          !state_has_changed(),
          "Inputs changed and no longer reflect the fitted model. Press `Fit Model` button again to re-fit model."
        )
      )
      # Input on output type.
      output_function <- input$output_function

      # Stop here if the output is not a plot.
      if (!isTRUE(grepl("^g_", output_function))) {
        return(NULL)
      }

      qenv <- mmrm_fit()
      fit <- qenv[["fit"]]

      ggplot2_args[["lsmeans"]] <- teal.widgets::ggplot2_args(
        labs <- list(
          subtitle = paste0(
            "Endpoint: ", fit$fit$data$PARAMCD[1],
            ifelse(is.null(fit$vars$covariates), "",
              paste(",  Covariates:", paste(fit$vars$covariates, collapse = ", "))
            )
          ),
          caption = paste(
            paste("Weights for LS Means:", input$weights_emmeans),
            paste("Correlation Structure:", input$cor_struct),
            paste("Adjustment Method:", input$method),
            sep = "\n"
          )
        )
      )

      ggplot2_args[["default"]] <- teal.widgets::ggplot2_args(
        labs <- list(
          subtitle = paste0(
            "Analysis Variable: ", fit$vars$response,
            ",  Endpoint: ", fit$fit$data$PARAMCD[1]
          )
        )
      )

      lsmeans_args <- if (output_function == "g_mmrm_lsmeans") {
        list(
          select = input$g_mmrm_lsmeans_select,
          width = input$g_mmrm_lsmeans_width,
          show_pval = input$g_mmrm_lsmeans_contrasts_show_pval
        )
      }

      diagnostic_args <- if (output_function == "g_mmrm_diagnostic") {
        list(
          type = input$g_mmrm_diagnostic_type,
          z_threshold = input$g_mmrm_diagnostic_z_threshold
        )
      }

      mmrm_plot_expr <- template_mmrm_plots(
        fit_name = "fit",
        lsmeans_plot = lsmeans_args,
        diagnostic_plot = diagnostic_args,
        ggplot2_args = ggplot2_args
      )
      obj <- qenv
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Plot")
      teal.code::eval_code(obj, as.expression(mmrm_plot_expr))
    })

    decorated_tables_q <- lapply(
      stats::setNames(
        nm = c("lsmeans_table", "diagnostic_table", "fixed_effects_table", "covariance_table")
      ),
      function(output_function) {
        srv_decorate_teal_data(
          id = sprintf("d_%s", output_function),
          data = table_q,
          decorators = select_decorators(decorators, output_function),
          expr = reactive(bquote(.(as.name(output_function)))),
          expr_is_reactive = TRUE
        )
      }
    )

    decorated_objs_q <- c(
      decorated_tables_q,
      lapply(
        stats::setNames(nm = c("lsmeans_plot", "diagnostic_plot")),
        function(output_function) {
          srv_decorate_teal_data(
            id = sprintf("d_%s", output_function),
            data = plot_q,
            decorators = select_decorators(decorators, output_function),
            expr = reactive(bquote(.(as.name(output_function)))),
            expr_is_reactive = TRUE
          )
        }
      )
    )

    obj_ix_r <- reactive({
      switch(input$output_function,
        t_mmrm_lsmeans = "lsmeans_table",
        t_mmrm_diagnostic = "diagnostic_table",
        t_mmrm_fixed = "fixed_effects_table",
        t_mmrm_cov = "covariance_table",
        g_mmrm_lsmeans = "lsmeans_plot",
        g_mmrm_diagnostic = "diagnostic_plot"
      )
    })

    plot_r <- reactive({
      if (is.null(plot_q())) {
        NULL
      } else {
        decorated_objs_q[[obj_ix_r()]]()[[obj_ix_r()]]
      }
    })

    table_r <- reactive({
      if (is.null(table_q())) {
        NULL
      } else {
        decorated_objs_q[[obj_ix_r()]]()[[obj_ix_r()]]
      }
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "mmrm_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      show_hide_signal = reactive(show_plot_rv())
    )

    teal.widgets::table_with_settings_srv(
      id = "mmrm_table",
      table_r = table_r,
      show_hide_signal = reactive(!show_plot_rv())
    )

    # Show R code once button is pressed.
    source_code_r <- reactive(
      teal.code::get_code(req(decorated_objs_q[[obj_ix_r()]]()))
    )
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      disabled = disable_r_code,
      title = label
    )

    set_chunk_dims(pws, reactive({
      decorated_objs_q[[obj_ix_r()]]()
    }))
  })
}
