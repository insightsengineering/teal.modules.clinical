#' Template: Mixed Model Repeated Measurements (MMRM) analysis
#'
#' @inheritParams template_arguments
#' @param cor_struct a string specifying the correlation structure, defaults to
#'   `"unstructured"`. See the details.
#' @param weights_emmeans argument from [emmeans::emmeans()], "proportional" by default.
#' @param optimizer a string specifying the optimization algorithm which should be used. By default, "automatic"
#'   will (if necessary) try all possible optimization algorithms and choose the best result. If another algorithm
#'   is chosen and does not give a valid result, an error will occur.
#' @param parallel flag that controls whether "automatic" optimizer search can use available free cores on the
#'   machine (not default).
#'
#' @seealso [tm_a_mmrm()]
#'
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
                              cor_struct = "unstructured",
                              weights_emmeans = "proportional",
                              optimizer = "automatic",
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
        utils.nest::substitute_names(
          expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
          names = list(arm_var = as.name(arm_var)),
          others = list(comp_arm = comp_arm)
        )
      )
      parent_list <- add_expr(
        parent_list,
        utils.nest::substitute_names(
          expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
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
  data_list <- add_expr(data_list, quote(df_explicit_na(na_level = "")))
  parent_list <- add_expr(parent_list, quote(df_explicit_na(na_level = "")))

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
    expr = fit <- fit_mmrm(
      vars = vars,
      data = anl,
      conf_level = conf_level,
      cor_struct = cor_struct,
      weights_emmeans = weights_emmeans,
      optimizer = optimizer,
      parallel = parallel
    ),
    env = list(
      vars = vars,
      conf_level = conf_level,
      cor_struct = cor_struct,
      weights_emmeans = weights_emmeans,
      optimizer = optimizer,
      parallel = parallel
    )
  )

  y
}

#' @describeIn template_fit_mmrm
#'
#' @inheritParams template_arguments
#' @param fit_name name of fitted MMRM object
#' @param show_relative should the "reduction" (`control - treatment`, default) or the "increase"
#'   (`treatment - control`) be shown for the relative change from baseline
#' @param table_type (`character`)\cr
#'   type of table to output.
template_mmrm_tables <- function(parentname,
                                 dataname,
                                 fit_name,
                                 arm_var,
                                 ref_arm,
                                 visit_var,
                                 paramcd,
                                 show_relative = c("increase", "reduction", "none"),
                                 table_type = "t_mmrm_cov",
                                 basic_table_args = teal.devel::basic_table_args()) {
  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  all_basic_table_args <- teal.devel::resolve_basic_table_args(basic_table_args)

  # Build layout.
  layout_list <- list()
  layout_list <- layout_list %>%
    add_expr(substitute(
      expr = expr_basic_table_args,
      env = list(expr_basic_table_args = teal.devel::parse_basic_table_args(all_basic_table_args))
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
          expr = rtables::add_colcounts() %>%
            rtables::split_rows_by(visit_var) %>%
            summarize_lsmeans(
              .stats = c(
                "n",
                "adj_mean_se",
                "adj_mean_ci",
                "diff_mean_se",
                "diff_mean_ci",
                "p_value"
              )
            ),
          env = list(visit_var = visit_var)
        )
      )
    } else {
      layout_list <- add_expr(
        layout_list,
        substitute(
          expr = rtables::add_colcounts() %>%
            rtables::split_rows_by(visit_var) %>%
            append_varlabels(dataname, visit_var) %>%
            summarize_lsmeans(show_relative = show_relative) %>%
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
        expr =
          rtables::split_rows_by(visit_var) %>%
            summarize_lsmeans(arms = FALSE) %>%
            rtables::append_topleft(paste0("  ", paramcd)),
        env = list(
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
            df = df_explicit_na(broom::tidy(fit_mmrm), na_level = ""),
            alt_counts_df = parentname
          )
          lsmeans_table
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
          cov_matrix <- as.rtable(fit_mmrm, type = "cov")
          cov_matrix
        },
        env = list(
          fit_mmrm = as.name(fit_name)
        )
      )
    },
    t_mmrm_fixed = {
      y$fixed_effects <- substitute(
        expr = {
          fixed_effects <- as.rtable(fit_mmrm, type = "fixed")
          fixed_effects
        },
        env = list(
          fit_mmrm = as.name(fit_name)
        )
      )
    },
    t_mmrm_diagnostic = {
      y$diagnostic_table <- substitute(
        expr = {
          diagnostic_table <- as.rtable(fit_mmrm, type = "diagnostic")
          diagnostic_table
        },
        env = list(
          fit_mmrm = as.name(fit_name)
        )
      )
    }
  )
  y
}

#' @describeIn template_fit_mmrm
#'
#' @inheritParams template_arguments
#' @param lsmeans_plot a `list` of controls for LS means plot. See more [tern::g_mmrm_lsmeans()]
#' @param diagnostic_plot a `list` of controls for diagnostic_plot. See more [tern::g_mmrm_diagnostic()]
#'
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
                                ggplot2_args = teal.devel::ggplot2_args()) {
  y <- list()



  if (!is.null(lsmeans_plot)) {
    parsed_ggplot2_args <- teal.devel::parse_ggplot2_args(
      teal.devel::resolve_ggplot2_args(
        user_plot = ggplot2_args[["lsmeans"]],
        user_default = ggplot2_args[["default"]]
      )
    )

    plot_call <- substitute(
      expr =
        g_mmrm_lsmeans(
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
        plot_call = utils.nest::calls_combine_by("+", c(plot_call, parsed_ggplot2_args))
      )
    )
  }

  if (!is.null(diagnostic_plot)) {
    parsed_ggplot2_args <- teal.devel::parse_ggplot2_args(
      teal.devel::resolve_ggplot2_args(
        user_plot = ggplot2_args[["diagnostic"]],
        user_default = ggplot2_args[["default"]]
      )
    )

    plot_call <- substitute(
      expr =
        g_mmrm_diagnostic(
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
        plot_call = utils.nest::calls_combine_by("+", c(plot_call, parsed_ggplot2_args))
      )
    )
  }

  y
}

#' Teal Module: Teal module for Mixed Model Repeated Measurements (MMRM) analysis
#'
#' @inheritParams module_arguments
#' @param ggplot2_args optional, (`ggplot2_args`) \cr
#' object created by [`teal.devel::ggplot2_args()`] with settings for all the plots or named list of `ggplot2_args`
#' objects for plot-specific settings. List names should match the following:\cr `
#' c("default", "lsmeans", "diagnostic")`.
#' The argument is merged with option `teal.ggplot2_args` and with default module arguments
#' (hard coded in the module body).\cr For more details, see the help vignette:\cr
#' `vignette("Custom ggplot2 arguments module", package = "teal.devel")`.
#'
#' @export
#'
#' @note
#' The ordering of the input data sets can lead to slightly different numerical results or
#' different convergence behavior. This is a known observation with the used package
#' `lme4`. However, once convergence is achieved, the results are reliable up to
#' numerical precision.
#'
#' @examples
#'
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADQS <- synthetic_cdisc_data("latest")$adqs %>%
#'   dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
#'   dplyr::filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")) %>%
#'   dplyr::mutate(
#'     AVISIT = as.factor(AVISIT),
#'     AVISITN = rank(AVISITN) %>%
#'       as.factor() %>%
#'       as.numeric() %>%
#'       as.factor() # making consecutive numeric factor
#'   )
#'
#' arm_ref_comp <- list(
#'   ARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data('latest')$adsl"),
#'     cdisc_dataset("ADQS", ADQS,
#'       code = 'ADQS <- synthetic_cdisc_data("latest")$adqs %>%
#'               dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
#'               dplyr::filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")) %>%
#'               dplyr::mutate(
#'                 AVISIT = as.factor(AVISIT),
#'                 AVISITN = rank(AVISITN) %>%
#'                   as.factor() %>%
#'                   as.numeric() %>%
#'                   as.factor() # making consecutive numeric factor
#'               )'
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_a_mmrm(
#'       label = "MMRM",
#'       dataname = "ADQS",
#'       aval_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
#'       id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       visit_var = choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         choices = value_choices(ADQS, "PARAMCD", "PARAM"),
#'         selected = "FKSI-FWB"
#'       ),
#'       cov_var = choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_a_mmrm <- function(label,
                      dataname,
                      parentname = ifelse(
                        inherits(arm_var, "data_extract_spec"),
                        teal.devel::datanames_input(arm_var),
                        "ADSL"
                      ),
                      aval_var,
                      id_var,
                      arm_var,
                      visit_var,
                      cov_var,
                      arm_ref_comp = NULL,
                      paramcd,
                      conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                      plot_height = c(700L, 200L, 2000L),
                      plot_width = NULL,
                      pre_output = NULL,
                      post_output = NULL,
                      basic_table_args = teal.devel::basic_table_args(),
                      ggplot2_args = teal.devel::ggplot2_args()) {
  logger::log_info("Initializing tm_a_mmrm")
  cov_var <- add_no_selected_choices(cov_var, multiple = TRUE)
  utils.nest::stop_if_not(
    utils.nest::is_character_single(dataname),
    is.choices_selected(conf_level),
    list(
      is.null(pre_output) || inherits(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || inherits(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE,
    .var.name = "plot_width"
  )

  checkmate::assert_class(basic_table_args, "basic_table_args")
  plot_choices <- c("lsmeans", "diagnostic")
  checkmate::assert(
    checkmate::check_class(ggplot2_args, "ggplot2_args"),
    checkmate::assert(
      combine = "or",
      checkmate::check_list(ggplot2_args, types = "ggplot2_args"),
      checkmate::check_subset(names(ggplot2_args), c("default", plot_choices))
    )
  )
  # Important step, so we could easily consume it later
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

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
        plot_height = plot_height,
        plot_width = plot_width,
        basic_table_args = basic_table_args,
        ggplot2_args = ggplot2_args
      )
    ),
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_mmrm <- function(id, ...) {
  a <- list(...) # module args
  ns <- NS(id)
  is_single_dataset_value <- teal.devel::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$id_var,
    a$visit_var,
    a$cov_var,
    a$aval_var
  )

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(
      textOutput(ns("null_input_msg")),
      h3(textOutput(ns("mmrm_title"))),
      teal.devel::table_with_settings_ui(ns("mmrm_table")),
      teal.devel::plot_with_settings_ui(id = ns("mmrm_plot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(a[c("arm_var", "paramcd", "id_var", "visit_var", "cov_var", "aval_var")]),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Model Settings",
          teal.devel::data_extract_ui(
            id = ns("aval_var"),
            label = "Analysis Variable",
            data_extract_spec = a$aval_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.devel::data_extract_ui(
            id = ns("paramcd"),
            label = "Select Endpoint",
            data_extract_spec = a$paramcd,
            is_single_dataset = is_single_dataset_value
          ),
          teal.devel::data_extract_ui(
            id = ns("visit_var"),
            label = "Visit Variable",
            data_extract_spec = a$visit_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.devel::data_extract_ui(
            id = ns("cov_var"),
            label = "Covariates",
            data_extract_spec = a$cov_var,
            is_single_dataset = is_single_dataset_value
          ),
          shinyjs::hidden(
            teal.devel::data_extract_ui(
              id = ns("split_covariates"),
              label = "Split Covariates",
              data_extract_spec = a$split_covariates,
              is_single_dataset = is_single_dataset_value
            )
          ),
          teal.devel::data_extract_ui(
            id = ns("arm_var"),
            label = "Select Treatment Variable",
            data_extract_spec = a$arm_var,
            is_single_dataset = is_single_dataset_value
          ),
          shinyjs::hidden(selectInput(
            ns("ref_arm"),
            "Reference Group",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          )),
          shinyjs::hidden(
            helpText(id = ns("help_text"), "Multiple reference groups are automatically combined into a single group.")
          ),
          shinyjs::hidden(
            selectInput(
              ns("comp_arm"),
              "Comparison Group",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            )
          ),
          shinyjs::hidden(
            checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            )
          ),
          teal.devel::data_extract_ui(
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
            choices = c("unstructured", "random-quadratic", "random-slope", "compound-symmetry"),
            selected = "unstructured",
            multiple = FALSE
          ),
          optionalSelectInput(
            ns("conf_level"),
            "Confidence Level",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          selectInput(
            ns("optimizer"),
            "Optimization Algorithm",
            choices = c(
              "automatic",
              "nloptwrap_neldermead",
              "nloptwrap_bobyqa",
              "nlminbwrap",
              "bobyqa",
              "neldermead",
              "nmkbw",
              "optimx_lbfgsb"
            ),
            selected = NULL,
            multiple = FALSE
          ),
          # Additional option for "automatic" optimizer.
          checkboxInput(
            ns("parallel"),
            "Parallel Computing",
            value = TRUE
          ),
          # Show here which automatic optimizer was used in the end.
          textOutput(ns("optimizer_selected")),
          collapsed = FALSE # Start with having this panel opened.
        )
      ),
      tags$style(".btn.disabled { color: grey; background-color: white; }"),
      actionButton(
        ns("button_start"),
        "Fit Model",
        icon = icon("calculator"),
        width = "100%",
        class = "btn action-button",
        style = "color: black; background-color: orange;"
      ),
      br(),
      br(),
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
      conditionalPanel(
        condition = paste0(
          "input['", ns("output_function"), "'] == 't_mmrm_lsmeans'", " || ",
          "input['", ns("output_function"), "'] == 'g_mmrm_lsmeans'", " || ",
          "input['", ns("output_function"), "'] == 'g_mmrm_diagnostic'"
        ),
        teal.devel::panel_group(
          teal.devel::panel_item(
            "Output Settings",
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
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_mmrm <- function(input,
                     output,
                     session,
                     datasets,
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
                     plot_height,
                     plot_width,
                     basic_table_args,
                     ggplot2_args) {
  stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

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
    updateOptionalSelectInput(
      session,
      inputId = extract_input("split_covariates", dataname),
      selected = split_covariates_selected
    )
  })

  anl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(
      arm_var = arm_var,
      paramcd = paramcd,
      id_var = id_var,
      visit_var = visit_var,
      split_covariates = split_covariates,
      aval_var = aval_var
    ),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var = arm_var),
    anl_name = "ANL_ADSL"
  )

  # Initially hide the output title because there is no output yet.
  shinyjs::hide("mmrm_title")

  # reactiveVal used to send a signal to plot_with_settings module to hide the UI
  show_plot_rv <- reactiveVal(FALSE)

  # this will store the current/last state of inputs and data that generatd a model-fit
  # its purpose is so that any input change can be checked whether it resulted in an out of sync state
  state <- reactiveValues(input = NULL, button_start = 0, optimizer = NULL)

  # Note:
  # input$parallel does not get us out of sync (it just takes longer to get to same result)
  sync_inputs <- c(
    extract_input("aval_var", dataname),
    extract_input("paramcd", dataname, filter = TRUE),
    extract_input("arm_var", parentname),
    "ref_arm",
    "comp_arm",
    "combine_comp_arms",
    extract_input("visit_var", dataname),
    extract_input("cov_var", dataname),
    extract_input("id_var", dataname),
    "weights_emmeans",
    "cor_struct",
    "conf_level",
    "optimizer"
  )

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel.

  observeEvent(adsl_merged()$columns_source$arm_var, {
    arm_var <- as.vector(adsl_merged()$columns_source$arm_var)
    if (utils.nest::is_empty(arm_var)) {
      shinyjs::hide("ref_arm")
      shinyjs::hide("comp_arm")
      shinyjs::hide("help_text")
      shinyjs::hide("combine_comp_arms")
    } else {
      shinyjs::show("ref_arm")
      shinyjs::show("comp_arm")
      shinyjs::show("help_text")
      shinyjs::show("combine_comp_arms")
    }
  })

  teal.devel::arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parentname), # From UI.
    datasets = datasets,
    dataname = parentname,
    arm_ref_comp = arm_ref_comp,
    module = "tm_mmrm"
  )

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
  # Show or hide parallel computing option (and result).
  observeEvent(input$optimizer, {
    optimizer <- input$optimizer
    if (isTRUE(optimizer == "automatic")) {
      shinyjs::show("parallel")
    } else {
      shinyjs::hide("parallel")
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

    encoding_inputs <- lapply(sync_inputs, function(x) input[[x]])
    names(encoding_inputs) <- sync_inputs

    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    validate(
      need(encoding_inputs[[extract_input("aval_var", dataname)]], "`Analysis Variable` field is not selected"),
      need(
        encoding_inputs[[extract_input("paramcd", dataname, filter = TRUE)]],
        "`Select Endpoint` field is not selected"
      ),
      need(encoding_inputs[[extract_input("visit_var", dataname)]], "`Visit Variable` field is not selected"),
      need(encoding_inputs[[extract_input("id_var", dataname)]], "`Subject Identifier` field is not selected"),
      need(encoding_inputs[["conf_level"]], "`Confidence Level` field is not selected"),
      need(nrow(adsl_filtered) > 1 && nrow(anl_filtered) > 1, "Filtered data has zero rows")
    )
    teal.devel::validate_no_intersection(
      encoding_inputs[["comp_arm"]],
      encoding_inputs[["ref_arm"]],
      "`Reference Group` and `Comparison Group` cannot have common values"
    )
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
    equal_ADSL <- dplyr::all_equal(state$input$adsl_filtered, displayed_state$adsl_filtered) # nolint
    equal_dataname <- dplyr::all_equal(state$input$anl_filtered, displayed_state$anl_filtered)
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

    # all_equal function either returns TRUE or a character scalar to describe where there is inequality
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
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    if (!is.null(input[[extract_input("arm_var", parentname)]])) {
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    } else {
      input_arm_var <- NULL
    }
    input_visit_var <- as.vector(anl_m$columns_source$visit_var)

    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]

    # Split the existing covariate strings in their variable parts, to allow "A*B" and "A:B" notations.
    input_cov_var <- as.vector(anl_m$columns_source$split_covariates)
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

    teal.devel::validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = adslvars,
      anl = anl_filtered,
      anlvars = anlvars,
      arm_var = input_arm_var,
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      min_nrow = 10,
      need_arm = FALSE
    )


    anl_data <- anl_m$data()

    Map(
      function(visit_df, visit_name) {
        dup <- any(duplicated(visit_df[[input_id_var]]))
        validate(need(!dup, paste("Duplicated subject ID found at", visit_name)))
      },
      split(anl_data, anl_data[[input_visit_var]]),
      levels(anl_data[[input_visit_var]])
    )

    validate(need(
      input$conf_level >= 0 && input$conf_level <= 1,
      "Please choose a confidence level between 0 and 1"
    ))
  })

  # Connector:
  # Fit the MMRM, once the user clicks on the start button.
  mmrm_fit <- eventReactive(input$button_start, {
    # Create a private stack for this function only.
    fit_stack <- teal.devel::chunks$new()
    fit_stack_push <- function(...) {
      teal.devel::chunks_push(..., chunks = fit_stack)
    }

    teal.devel::chunks_reset(chunks = fit_stack)
    anl_m <- anl_merged()
    teal.devel::chunks_push_data_merge(anl_m, chunks = fit_stack)
    teal.devel::chunks_push_new_line(chunks = fit_stack)

    anl_adsl <- adsl_merged()
    teal.devel::chunks_push_data_merge(anl_adsl, chunks = fit_stack)
    teal.devel::chunks_push_new_line(chunks = fit_stack)

    my_calls <- template_fit_mmrm(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      aval_var = as.vector(anl_m$columns_source$aval_var),
      arm_var = input[[extract_input("arm_var", parentname)]],
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      combine_comp_arms = input$combine_comp_arms,
      id_var = as.vector(anl_m$columns_source$id_var),
      visit_var = as.vector(anl_m$columns_source$visit_var),
      cov_var = input[[extract_input("cov_var", dataname)]],
      conf_level = as.numeric(input$conf_level),
      cor_struct = input$cor_struct,
      weights_emmeans = input$weights_emmeans,
      optimizer = input$optimizer,
      parallel = input$parallel
    )
    mapply(expression = my_calls, fit_stack_push)
    teal.devel::chunks_safe_eval(chunks = fit_stack)
    fit_stack
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
      "t_mmrm_cov" = "Residual covariance matrix estimate",
      "t_mmrm_diagnostic" = "Model fit statistics",
      "t_mmrm_fixed" = "Fixed effects estimates",
      "t_mmrm_lsmeans" = "LS means and contrasts estimates",
      "g_mmrm_diagnostic" = switch(g_mmrm_diagnostic_type,
        "fit-residual" = "Marginal fitted values vs. residuals",
        "q-q-residual" = "Q-Q normal plot for standardized residuals"
      ),
      "g_mmrm_lsmeans" = if (setequal(g_mmrm_lsmeans_select, c("estimates", "contrasts"))) {
        "LS means estimates and contrasts"
      } else if (identical(g_mmrm_lsmeans_select, "estimates")) {
        "LS means estimates"
      } else {
        "LS means contrasts"
      }
    )
    output_title
  })

  mmrm_table <- reactive({
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
    # Reset global chunks. Needs to be done here so nothing yet in environment.
    teal.devel::chunks_reset()
    # Get the fit stack while evaluating the fit code at the same time.
    fit_stack <- mmrm_fit()
    fit <- teal.devel::chunks_get_var("fit", chunks = fit_stack)
    # Start new private stack for the table code.
    table_stack <- teal.devel::chunks$new()

    table_stack_push <- function(...) {
      teal.devel::chunks_push(..., chunks = table_stack)
    }

    anl_m <- anl_merged()

    ANL <- teal.devel::chunks_get_var("ANL", chunks = fit_stack) # nolint
    ANL_ADSL <- teal.devel::chunks_get_var("ANL_ADSL", chunks = fit_stack) # nolint
    paramcd <- unique(ANL[[unlist(paramcd$filter)["vars_selected"]]])

    mmrm_table <- function(table_type) {
      res <- template_mmrm_tables(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        fit_name = "fit",
        arm_var = input[[extract_input("arm_var", parentname)]],
        ref_arm = input$ref_arm,
        visit_var = as.vector(anl_m$columns_source$visit_var),
        paramcd = paramcd,
        show_relative = input$t_mmrm_lsmeans_show_relative,
        table_type = table_type,
        basic_table_args = basic_table_args
      )

      mapply(expression = res, table_stack_push)
      teal.devel::chunks_push_chunks(table_stack)
      teal.devel::chunks_safe_eval()
    }
    teal.devel::chunks_push_chunks(fit_stack)
    mmrm_table(output_function)

    # Depending on the table function type, produce different code
    switch(output_function,
      t_mmrm_lsmeans = teal.devel::chunks_get_var("lsmeans_table"),
      t_mmrm_diagnostic = teal.devel::chunks_get_var("diagnostic_table"),
      t_mmrm_fixed = teal.devel::chunks_get_var("fixed_effects"),
      t_mmrm_cov = teal.devel::chunks_get_var("cov_matrix")
    )
  })

  # Endpoint:
  # Plot outputs.
  mmrm_plot_reactive <- reactive({
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
    teal.devel::chunks_reset()
    fit_stack <- mmrm_fit()
    fit <- teal.devel::chunks_get_var("fit", fit_stack)

    # Start new private stack for the plot code.
    plot_stack <- teal.devel::chunks$new()
    plot_stack_push <- function(...) {
      teal.devel::chunks_push(..., chunks = plot_stack)
    }

    lsmeans_args <- list(
      select = input$g_mmrm_lsmeans_select,
      width = input$g_mmrm_lsmeans_width,
      show_pval = input$g_mmrm_lsmeans_contrasts_show_pval
    )
    diagnostic_args <- list(
      type = input$g_mmrm_diagnostic_type,
      z_threshold = input$g_mmrm_diagnostic_z_threshold
    )

    mmrm_plot <- function(lsmeans_plot = lsmeans_args,
                          diagnostic_plot = diagnostic_args) {
      res <- template_mmrm_plots(
        "fit",
        lsmeans_plot = lsmeans_plot,
        diagnostic_plot = diagnostic_plot,
        ggplot2_args = ggplot2_args
      )
      mapply(expression = res, plot_stack_push)
      teal.devel::chunks_push_chunks(plot_stack)
      teal.devel::chunks_safe_eval()
    }
    teal.devel::chunks_push_chunks(fit_stack)
    # Depending on the plot function type, produce different code.
    switch(output_function,
      g_mmrm_lsmeans = {
        mmrm_plot(diagnostic_plot = NULL)
        teal.devel::chunks_get_var("lsmeans_plot")
      },
      g_mmrm_diagnostic = {
        mmrm_plot(lsmeans_plot = NULL)
        teal.devel::chunks_get_var("diagnostic_plot")
      }
    )
  })

  callModule(
    teal.devel::plot_with_settings_srv,
    id = "mmrm_plot",
    plot_r = mmrm_plot_reactive,
    height = plot_height,
    width = plot_width,
    show_hide_signal = reactive(show_plot_rv())
  )

  callModule(
    teal.devel::table_with_settings_srv,
    id = "mmrm_table",
    table_r = mmrm_table,
    show_hide_signal = reactive(!show_plot_rv())
  )

  # Endpoint:
  # Optimizer that was selected.
  output$optimizer_selected <- renderText({
    # First reassign reactive sources:
    fit_stack <- try(mmrm_fit(), silent = TRUE)
    result <- if (!inherits(fit_stack, "try-error")) {
      fit <- teal.devel::chunks_get_var("fit", chunks = fit_stack)
      if (input$optimizer == "automatic") {
        selected <- attr(fit$fit, "optimizer")
        paste("Optimizer used:", selected)
      }
    }
    currnt_state <- !state_has_changed()
    what_to_return <- if (input$button_start > isolate(state$button_start)) {
      state$button_start <- input$button_start
      state$optimizer <- result
      result
    } else if (currnt_state) {
      isolate(state$optimizer)
    } else {
      NULL
    }

    return(what_to_return)
  })

  # Show R code once button is pressed.
  callModule(
    module = teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(list(arm_var, paramcd, id_var, visit_var, cov_var, aval_var)),
    modal_title = "R Code for the Current MMRM Analysis",
    code_header = label,
    disable_buttons = disable_r_code
  )
}
