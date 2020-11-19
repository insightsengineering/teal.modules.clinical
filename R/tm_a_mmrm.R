#' Teal module for Mixed Model Repeated Measurements (MMRM) analysis.
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hidden
#' @importFrom stats complete.cases
#' @importFrom broom tidy
#' @note
#' The ordering of the input data sets can lead to slightly different numerical results or
#' different convergence behavior. This is a known observation with the used package
#' \code{lme4}. However, once convergence is achieved, the results are reliable up to
#' numerical precision.
#'
#' @name MMRM
#'
NULL

#' @describeIn MMRM Creates a fit object for MMRM analysis.
#' @inheritParams argument_convention
#' @param comp_arm comparison arms
#' @param cor_struct a string specifying the correlation structure, defaults to
#'   \code{"unstructured"}. See the details.
#' @param weights_emmeans argument from \code{\link[emmeans]{emmeans}}, "proportional" by default.
#' @param optimizer a string specifying the optimization algorithm which should be used. By default, "automatic"
#'   will (if necessary) try all possible optimization algorithms and choose the best result. If another algorithm
#'   is chosen and does not give a valid result, an error will occur.
#' @param parallel flag that controls whether "automatic" optimizer search can use available free cores on the
#'   machine (not default).
#'
template_fit_mmrm <- function(parentname,
                              dataname,
                              paramcd,
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
                              parallel = FALSE

) {
  # Data
  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        filter(PARAMCD %in% paramcd) %>%
        droplevels(),
      env = list(
        df = as.name(dataname),
        paramcd = paramcd
      )
    )
  )



  data_list <- add_expr(
    data_list,
    substitute(
      expr = df <- filter(df, arm_var %in% c(ref_arm, comp_arm)),
      env = list(
        df = as.name(parentname),
        arm_var = as.name(arm_var),
        ref_arm = ref_arm,
        comp_arm = comp_arm
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- filter(anl, arm_var %in% c(ref_arm, comp_arm)),
      env = list(
        arm_var = as.name(arm_var),
        ref_arm = ref_arm,
        comp_arm = comp_arm
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = df$arm_var <- droplevels(relevel(df$arm_var, ref_arm)),
      env = list(
        df = as.name(parentname),
        arm_var = arm_var,
        ref_arm = ref_arm
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl$arm_var <- droplevels(relevel(anl$arm_var, ref_arm)),
      env = list(
        arm_var = arm_var,
        ref_arm = ref_arm
      )
    )
  )

  if (combine_comp_arms) {
    data_list <- add_expr(
      data_list,
      substitute(
        expr = df$arm_var <- combine_levels(
          x = df$arm_var,
          levels = comp_arm
        ),
        env = list(
          df = as.name(parentname),
          arm_var = arm_var,
          ref_arm = ref_arm,
          comp_arm = comp_arm
        )
      )
    )
    data_list <- add_expr(
      data_list,
      substitute(
        expr = anl$arm_var <- combine_levels(
          x = anl$arm_var,
          levels = comp_arm
        ),
        env = list(
          arm_var = arm_var,
          ref_arm = ref_arm,
          comp_arm = comp_arm
        )
      )
    )

  }

  y$data <- bracket_expr(data_list)

  y$col_counts <- substitute(
    expr = col_counts <- table(parentname$arm_var),
    env = list(parentname = as.name(parentname), arm_var = arm_var)
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

#' @describeIn MMRM Create MMRM tables from a fitted MMRM object
#'
#' @inheritParams argument_convention
#' @param fit_name name of fitted MMRM object
#' @param colcounts_name name of column counts for MMRM LS means table
#' @param show_relative should the "reduction" (`control - treatment`, default) or the "increase"
#'   (`treatment - control`) be shown for the relative change from baseline
#'
template_mmrm_tables <- function(fit_name,
                                 colcounts_name,
                                 arm_var,
                                 ref_arm,
                                 visit_var,
                                 show_relative = c("increase", "reduction", "none")

) {

  y <- list()
  # Build layout.
  layout_list <- list()

  layout_list <- layout_list %>%
    add_expr(quote(basic_table()))

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(var = arm_var, ref_group = ref_arm),
      env = list(
        arm_var = arm_var,
        ref_arm = ref_arm
      )
    )
  )

  show_relative <- match.arg(show_relative)

  if (show_relative == "none") {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = add_colcounts() %>%
          split_rows_by(visit_var) %>%
          summarize_lsmeans(
            .stats = c(
              "n", "adj_mean_se", "adj_mean_ci",
              "diff_mean_se", "diff_mean_ci", "p_value"
            )
          ),
        env = list(
          visit_var = visit_var
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = add_colcounts() %>%
          split_rows_by(visit_var) %>%
          summarize_lsmeans(show_relative = show_relative),
        env = list(
          visit_var = visit_var,
          show_relative = show_relative
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$lsmeans_table <- substitute(
    expr = lsmeans_table <- build_table(lyt = lyt, df = broom::tidy(fit_mmrm), col_counts = col_counts),
    env = list(
      col_counts = as.name(colcounts_name),
      fit_mmrm = as.name(fit_name)
    )
  )

  y$cov_matrix <- substitute(
    expr = cov_matrix <- as.rtable(fit_mmrm, type = "cov"),
    env = list(
      fit_mmrm = as.name(fit_name)
    )
  )

  y$fixed_effects <- substitute(
    expr = fixed_effects <- as.rtable(fit_mmrm, type = "fixed"),
    env = list(
      fit_mmrm = as.name(fit_name)
    )
  )

  y$diagnostic_table <- substitute(
    expr = diagnostic_table <- as.rtable(fit_mmrm, type = "diagnostic"),
    env = list(
      fit_mmrm = as.name(fit_name)
    )
  )

  y

}


#' @describeIn MMRM Create MMRM plots from a fitted MMRM object
#' @inheritParams template_mmrm_tables
#' @param lsmeans_plot a `list` of controls for LS means plot. See more [tern::g_mmrm_lsmeans]
#' @param diagnostic_plot a `list` of controls for diagnostic_plot. See more [tern::g_mmrm_diagnostic]
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
                                )

) {
  y <- list()

  y$lsmeans_plot <- substitute(
    expr = lsmeans_plot <- g_mmrm_lsmeans(
      fit_mmrm,
      select = select,
      width = width,
      show_pval = show_pval
    ),
    env = list(
      fit_mmrm = as.name(fit_name),
      select = lsmeans_plot$select,
      width = lsmeans_plot$width,
      show_pval = lsmeans_plot$show_pval
    )
  )

  y$diagnostic_plot <- substitute(
    expr = diagnostic_plot <- g_mmrm_diagnostic(
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
  y

}

#' @describeIn MMRM teal module for MMRM.
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADQS <- radqs(cached = TRUE) %>%
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
#'     ref = "ARM A",
#'     comp = c("ARM B", "ARM C")
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADQS", ADQS,
#'       code = 'ADQS <- radqs(cached = TRUE) %>%
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
#'       dataname = 'ADQS',
#'       aval_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
#'       id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARMCD"),
#'       visit_var = choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         choices = value_choices(ADQS, "PARAMCD", "PARAM"),
#'         selected = "FKSI-FWB"
#'       ),
#'       cov_var = choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL),
#'       conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_a_mmrm <- function(label,
                      dataname,
                      aval_var,
                      id_var,
                      arm_var,
                      visit_var,
                      cov_var,
                      arm_ref_comp = NULL,
                      paramcd,
                      conf_level,
                      plot_height = c(700L, 200L, 2000L),
                      plot_width = NULL,
                      pre_output = NULL,
                      post_output = NULL
) {
    cov_var <- add_no_selected_choices(cov_var, multiple = TRUE)
    stopifnot(length(dataname) == 1)
    stopifnot(is.choices_selected(aval_var))
    stopifnot(is.choices_selected(id_var))
    stopifnot(is.choices_selected(arm_var))
    stopifnot(is.choices_selected(visit_var))
    stopifnot(is.choices_selected(cov_var))
    stopifnot(is.choices_selected(paramcd))
    stopifnot(is.choices_selected(conf_level))
    check_slider_input(plot_height, allow_null = FALSE)
    check_slider_input(plot_width)

    args <- as.list(environment())

    module(
      label = label,
      server = srv_mmrm,
      ui = ui_mmrm,
      ui_args = args,
      server_args = list(
        dataname = dataname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        plot_height = plot_height,
        plot_width = plot_width
      ),
      filters = dataname
    )
}

#' @noRd
ui_mmrm <- function(id, ...) {

  a <- list(...) # module args

  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      p(
        id = ns("initial_message"),
        paste(
          "Please first specify 'Model Settings' and press 'Fit Model'.",
          "Afterwards choose 'Output Type' and optional 'Output Settings'.",
          "If changes to the 'Model Settings' or dataset (by filtering) are made,",
          "then the 'Fit Model' button must be pressed again to update the MMRM model.",
          "Note that the 'Show R Code' button can only be clicked if the model fit is up to date."
        )
      ),
      hidden(p(
        id = ns("outdated_warning"),
        "Inputs have changed and no longer reflect the fitted model. Press `Fit Model` button again to re-fit model."
      )),
      h3(textOutput(ns("mmrm_title"))),
      uiOutput(ns("mmrm_table")),
      plot_with_settings_ui(id = ns("mmrm_plot"), height = a$plot_height, width = a$plot_width)
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      panel_group(
        panel_item(
          "Model Settings",
          helpText("Analysis data:", code(a$dataname)),
          optionalSelectInput(
            ns("aval_var"),
            "Select Response",
            a$aval_var$choices,
            a$aval_var$selected,
            multiple = FALSE,
            fixed = a$aval_var$fixed
          ),
          optionalSelectInput(
            ns("paramcd"),
            "Select Parameter",
            a$paramcd$choices,
            a$paramcd$selected,
            multiple = FALSE,
            fixed = a$paramcd$fixed
          ),
          optionalSelectInput(
            ns("arm_var"),
            "Arm Variable",
            a$arm_var$choices,
            a$arm_var$selected,
            multiple = FALSE,
            fixed = a$arm_var$fixed
          ),
          optionalSelectInput(
            ns("visit_var"),
            "Visit Variable",
            a$visit_var$choices,
            a$visit_var$selected,
            multiple = FALSE,
            fixed = a$visit_var$fixed
          ),
          optionalSelectInput(
            ns("cov_var"),
            "Covariate Variables",
            a$cov_var$choices,
            a$cov_var$selected,
            multiple = TRUE,
            fixed = a$cov_var$fixed
          ),
          selectInput(
            ns("ref_arm"),
            "Reference Group",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          helpText("Multiple reference groups are automatically combined into a single group."),
          selectInput(
            ns("comp_arm"),
            "Comparison Group",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          checkboxInput(
            ns("combine_comp_arms"),
            "Combine all comparison groups?",
            value = FALSE
          ),
          optionalSelectInput(
            ns("id_var"),
            "Subject Identifier",
            a$id_var$choices,
            a$id_var$selected,
            multiple = FALSE,
            fixed = a$id_var$fixed
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
          collapsed = FALSE  # Start with having this panel opened.
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
      panel_group(
        panel_item(
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
    ),
    forms =  get_rcode_ui(ns("rcode")),
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
                     arm_ref_comp,
                     label,
                     plot_height,
                     plot_width) {
  init_chunks()
  # Initially hide the output title because there is no output yet.
  shinyjs::hide("mmrm_title")

  #reactiveVal used to send a signal to plot_with_settings module to hide the UI
  show_plot_rv <- reactiveVal(FALSE)

  # applicable is set to TRUE only after a `Fit Button` press leads to a successful computation from the inputs
  # it will store the current/last state of inputs and data that generatd a model-fit
  # its purpose is so that any input change can be checked whether it resulted in an out of sync state
  state <- reactiveValues(applicable = FALSE)

  # Note:
  # input$parallel does not get us out of sync (it just takes longer to get to same result)
  sync_inputs <- c(
    "aval_var", "paramcd", "arm_var", "ref_arm", "comp_arm",
    "combine_comp_arms", "visit_var", "cov_var",
    "id_var", "weights_emmeans", "cor_struct", "conf_level",
    "optimizer")

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel.
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",  # From UI.
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_mmrm"
  )

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel.
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",  # From UI.
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_a_mmrm"
  )

  # Event handler:
  # Show either the plot or the table output.
  observeEvent(input$output_function, {
    output_function <- input$output_function
    if (isTRUE(grepl("^t_", output_function))) {
      show_plot_rv(FALSE)
      shinyjs::show("mmrm_table")
    } else if (isTRUE(grepl("^g_", output_function)))  {
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
      shinyjs::show("optimizer_selected")
    } else {
      shinyjs::hide("parallel")
      shinyjs::hide("optimizer_selected")
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
    shinyjs::hide("initial_message")
    shinyjs::show("mmrm_title")
    shinyjs::disable("button_start")
    shinyjs::enable("show_rcode")
    shinyjs::hide("outdated_warning")
  })

  # all the inputs and data that can be out of sync with the fitted model
  mmrm_inputs_reactive <- reactive({
    encoding_inputs <- lapply(sync_inputs, function(x) input[[x]])
    names(encoding_inputs) <- sync_inputs
    c(list(
      adsl_filtered = datasets$get_data("ADSL", filtered = TRUE),
      anl_filtered = datasets$get_data(dataname, filtered = TRUE)),
      encoding_inputs)
  })

  # compares the mmrm_inputs_reactive values with the values stored in 'state'
  state_has_changed <- reactive({
    displayed_state <- mmrm_inputs_reactive()
    equal_ADSL <- all_equal(state$input$adsl_filtered, displayed_state$adsl_filtered) # nolint
    equal_dataname <- all_equal(state$input$anl_filtered, displayed_state$anl_filtered)
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
      FUN.VALUE = logical(1))

    # all_equal function either returns TRUE or a character scalar to describe where there is inequality
    any(c(is.character(equal_ADSL), is.character(equal_dataname),  true_means_change))
  })

  # Event handler:
  # These trigger when we are out of sync and then enable the start button and
  # disable the show R code button and show warning message
  observeEvent(mmrm_inputs_reactive(), {
    shinyjs::enable("button_start")
    shinyjs::disable("show_rcode")
    if (state$applicable) {
      if (state_has_changed()) {
        shinyjs::show("outdated_warning")
      } else {
        shinyjs::hide("outdated_warning")
        shinyjs::enable("show_rcode")
        shinyjs::disable("button_start")
      }
    }
  })

  # Prepare the analysis environment (filter data, check data, populate envir).
  prepared_env <- reactive({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
    validate_has_data(adsl_filtered, 1)
    validate_has_data(anl_filtered, 1)
    # Split the existing covariate strings in their variable parts, to allow "A*B" and "A:B" notations.
    cov_var <- input$cov_var
    cov_var <- no_selected_as_NULL(cov_var)
    covariate_parts <- if_not_null(
      cov_var,
      unique(unlist(strsplit(cov_var, split = "\\*|:")))
    )
    aval_var <- input$aval_var
    arm_var <- input$arm_var
    visit_var <- input$visit_var
    id_var <- input$id_var
    all_x_vars <- c(arm_var, visit_var, covariate_parts)
    all_x_vars_in_adsl <- intersect(
      all_x_vars,
      colnames(adsl_filtered)
    )
    all_x_vars_in_anl <- setdiff(
      all_x_vars,
      all_x_vars_in_adsl
    )

    adslvars <- unique(c("USUBJID", "STUDYID", arm_var, id_var, all_x_vars_in_adsl))
    anlvars <- unique(c("USUBJID", "STUDYID", "PARAMCD", aval_var, visit_var, all_x_vars_in_anl))
    # validate inputs
    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = adslvars,
      anl = anl_filtered,
      anlvars = anlvars,
      arm_var = input$arm_var
    )

    validate_args <- append(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
    do.call(what = "validate_standard_inputs", validate_args)

    paramcd <- input$paramcd
    anl_endpoint <- merge(
      anl_filtered[anl_filtered$PARAMCD == paramcd, anlvars, drop = FALSE],
      adsl_filtered[, adslvars, drop = FALSE],
      all.x = FALSE,
      all.y = FALSE,
      by = c("USUBJID", "STUDYID")
    )
    validate(need(nrow(anl_endpoint) > 5, "need at least 5 data points"))
    Map(function(visit_df, visit_name) {

      dup <- any(duplicated(visit_df[[id_var]]))

      validate(need(!dup, paste("Duplicated subject ID found at", visit_name)))

    }, split(anl_endpoint, anl_endpoint[[visit_var]]),
    levels(anl_endpoint[[visit_var]]))

    validate(need(
      all(complete.cases(anl_endpoint)),
      paste(c("Missing values found in formula vars",
              anl_endpoint[!complete.cases(anl_endpoint), ]))
    ))
    # Send data where the analysis lives.
    e <- new.env()
    e[[paste0(dataname, "_FILTERED")]] <- anl_filtered # nolint
    e$ADSL_FILTERED <- adsl_filtered # nolint
    e
  })


  # Connector:
  # Fit the MMRM, once the user clicks on the start button.
  mmrm_fit <- eventReactive(input$button_start, {
    # set to FALSE because a button press does not always indicate a successful computation
    state$applicable <- FALSE
    # Create a private stack for this function only.
    fit_stack <- chunks$new()
    fit_stack_push <- function(...) {
      chunks_push(..., chunks = fit_stack)
    }
    fit_stack_get_var <- function(...) {
      chunks_get_var(..., chunks = fit_stack)
    }
    chunks_reset(envir = prepared_env(), chunks = fit_stack)
    my_calls <- template_fit_mmrm(
      parentname = "ADSL_FILTERED",
      dataname = paste0(dataname, "_FILTERED"),
      paramcd = input$paramcd,
      aval_var = input$aval_var,
      arm_var = input$arm_var,
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      combine_comp_arms = input$combine_comp_arms,
      id_var = input$id_var,
      visit_var = input$visit_var,
      cov_var = if_not_null(
        no_selected_as_NULL(input$cov_var),
        unique(unlist(strsplit(no_selected_as_NULL(input$cov_var), split = "\\*|:")))
      ),
      conf_level = as.numeric(input$conf_level),
      cor_struct = input$cor_struct,
      weights_emmeans = input$weights_emmeans,
      optimizer = input$optimizer,
      parallel = input$parallel
    )
    state$applicable <- TRUE
    state$input <- mmrm_inputs_reactive()
    mapply(expression = my_calls, fit_stack_push)
    chunks_safe_eval(chunks = fit_stack)
    fit_stack
  })

    output$mmrm_title <- renderText({

      # Input on output type.
      output_function <- input$output_function
      g_mmrm_diagnostic_type <- input$g_mmrm_diagnostic_type
      g_mmrm_lsmeans_select <- input$g_mmrm_lsmeans_select

      output_title <- switch(
        output_function,
        "t_mmrm_cov" = "Residual covariance matrix estimate",
        "t_mmrm_diagnostic" = "Model fit statistics",
        "t_mmrm_fixed" = "Fixed effects estimates",
        "t_mmrm_lsmeans" = "LS means and contrasts estimates",
        "g_mmrm_diagnostic" = switch(
          g_mmrm_diagnostic_type,
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

    output$mmrm_table <- renderUI({
      # Input on output type.
      output_function <- input$output_function

      # If the output is not a table, stop here.
      if (!isTRUE(grepl("^t_", output_function))) return(NULL)
      # Reset global chunks. Needs to be done here so nothing yet in environment.
      chunks_reset()
      # Get the fit stack while evaluating the fit code at the same time.
      fit_stack <- mmrm_fit()
      fit <- chunks_get_var("fit", chunks = fit_stack)
      col_counts <- chunks_get_var("col_counts", chunks = fit_stack) # nolint
      # Start new private stack for the table code.
      table_stack <- chunks$new()

      table_stack_push <- function(...) {
        chunks_push(..., chunks = table_stack)
      }

      mmrm_table <- template_mmrm_tables(
        "fit",
        "col_counts",
        arm_var = input$arm_var,
        ref_arm = input$ref_arm,
        visit_var = input$visit_var,
        show_relative = input$t_mmrm_lsmeans_show_relative
      )
      mapply(expression = mmrm_table, table_stack_push)
      chunks_push_chunks(fit_stack)
      chunks_push_chunks(table_stack)
      chunks_safe_eval()

      # Depending on the table function type, produce different code.
      if (output_function == "t_mmrm_lsmeans") {
        as_html(chunks_get_var("lsmeans_table"))
      } else if (output_function == "t_mmrm_diagnostic") {
        as_html(chunks_get_var("diagnostic_table"))
      } else if (output_function == "t_mmrm_fixed") {
        as_html(chunks_get_var("fixed_effects"))
      } else if (output_function == "t_mmrm_cov") {
        as_html(chunks_get_var("cov_matrix"))
      }
    })

    # Endpoint:
    # Plot outputs.
    mmrm_plot_reactive <- reactive({

      # Input on output type.
      output_function <- input$output_function

      # Stop here if the output is not a plot.
      if (!isTRUE(grepl("^g_", output_function))) return(NULL)
      chunks_reset()
      fit_stack <- mmrm_fit()
      fit <- chunks_get_var("fit", fit_stack)

      # Start new private stack for the plot code.
      plot_stack <- chunks$new()
      plot_stack_push <- function(...) {
        chunks_push(..., chunks = plot_stack)
      }

      mmrm_plot <- template_mmrm_plots(
        "fit",
        lsmeans_plot = list(
          select = input$g_mmrm_lsmeans_select,
          width = input$g_mmrm_lsmeans_width,
          show_pval = input$g_mmrm_lsmeans_contrasts_show_pval
        ),
        diagnostic_plot = list(
          type = input$g_mmrm_diagnostic_type,
          z_threshold = input$g_mmrm_diagnostic_z_threshold
        )
      )
      mapply(expression = mmrm_plot, plot_stack_push)
      chunks_push_chunks(fit_stack)
      chunks_push_chunks(plot_stack)
      chunks_safe_eval()
      # Depending on the plot function type, produce different code.
      if (output_function == "g_mmrm_lsmeans") {
        chunks_get_var("lsmeans_plot")

      } else if (output_function == "g_mmrm_diagnostic") {
        chunks_get_var("diagnostic_plot")
      }
    })

    callModule(
      plot_with_settings_srv,
      id = "mmrm_plot",
      plot_r = mmrm_plot_reactive,
      height = plot_height,
      width = plot_width,
      show_hide_signal = reactive(show_plot_rv())
    )


    # Endpoint:
    # Optimizer that was selected.
    output$optimizer_selected <- renderText({
      # First reassign reactive sources:

      fit_stack <- mmrm_fit()
      fit <- chunks_get_var("fit", chunks = fit_stack)

      # Inputs.
      optimizer <- input$optimizer

      result <- if (!inherits(fit, "try-error") && optimizer == "automatic") {
        selected <- attr(fit$fit, "optimizer")
        paste("Optimizer used:", selected)
      } else {
        NULL
      }
      return(result)
    })
      # Handler:
      # Show R code once button is pressed.
    callModule(
      module = get_rcode_srv,
      id = "rcode",
      datasets = datasets,
      datanames = dataname,
      modal_title = "R Code for the Current MMRM Analysis",
      code_header = label
    )
}
