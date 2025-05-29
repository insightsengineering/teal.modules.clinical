ui_counts <-  function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::selectInput(ns("dataset"),
                       "Select dataset",
                       choices = c("ADSL", "ADTTE"),
                       selected = "ADTTE"
    ),
    shiny::selectInput(
      ns("vars"),
      "Analysis Variable",
      choices = c("ARM", "ARMCD")
    ),
    shiny::selectInput(
      ns("covariates"),
      "Covariates",
      choices = c("var1", "var2")
    ),
    shiny::selectInput(
      ns("strata"),
      "Stratify",
      choices = c("SEX", "SITE", "AGE")
    ),

    tags$div(
      class = "arm-comp-box",
      tags$label("Compare Treatments"),
      shinyWidgets::switchInput(
        inputId = ns("compare_arms"),
        value = FALSE,
        size = "mini"
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        tags$div(
          uiOutput(ns("arms_buckets")),
          uiOutput(ns("helptext_ui")),
          checkboxInput(
            ns("combine_comp_arms"),
            "Combine all comparison groups?",
            value = FALSE
          ),
          shiny::selectInput(
            ns("arm"),
            "Group variable",
            choices = c("SITE", "SEX")
          ),
          shiny::selectInput(
            ns("group_ref"),
            "Ref",
            choices = c("A", "B", "C")
          ),
          shiny::selectInput(
            ns("group_ref"),
            "Comp",
            choices = c("A", "B", "C"),
            selected = c("B", "C"),
            multiple = TRUE
          )
        )
      )
    ),
    shiny::selectInput(
      ns("distribution"),
      "Distribution",
      choices = c("quasipoisson", "negbin", "poisson")
    ),
    shiny::selectInput(
      ns("rate_mean_method"),
      "Rate method",
      choices = c("emmeans", "ppmeans")
    ),
    bslib::accordion_panel(
      "Additional table settings",
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = "Confidence Level",
        choices = c(0.8, 0.9, 0.95, 0.99),
        selected = 0.95,
        multiple = FALSE,
        fixed = FALSE
      ),
    ),
    tags$h2("Output of the code"),
    tags$a("note: UI is not used yet to modify the server part of the module"),
    verbatimTextOutput(ns("out"))
  )
}


srv_counts <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    count <- reactiveVal(0)
    # output$out <- renderText({count()})
    dataset <- input$datasets
    table <- reactive({
        within(data(), {
          library("dplyr")
          library("tern")
          anl <- ADTTE %>% dplyr::filter(PARAMCD == "TNE")
          anl$AVAL_f <- as.factor(anl$AVAL)
          anl <- df_explicit_na(anl)

          lyt <- basic_table(show_colcounts = TRUE) %>%
            split_cols_by("ARMCD", ref_group = "ARM A", split_fun = ref_group_position("first")) %>%
            summarize_glm_count(
              vars = "AVAL",
              variables = list(arm = "ARMCD", covariates = c("BMRKR1","BMRKR2")),
              conf_level = 0.9,
              distribution = "negbin",
              rate_mean_method = "emmeans",
              var_labels = "Adjusted (NB) exacerbation rate (per year)",
              table_names = "adj-nb",
              .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
              .labels = c(
                rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
                rate_ratio_ci = "Rate Ratio CI", pval = "p-value"
              ))
          result <- build_table(
            lyt = lyt,
            df = anl
          )
          result
        })
    })
    output$out <- shiny::renderText({get_code(table())})
    count
  })
}


create_counts_module <- function(label = "Counts Module") {
  teal::module(
    label = label,
    ui = ui_counts,
    server = srv_counts,
    datanames = "all"
  )
}

data <- within(teal_data(), {
  library("tern")
  ADSL <- tern_ex_adsl
  ADTTE <- tern_ex_adtte
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

# Initialize the teal app
app <- init(
  data = data,
  modules = modules(create_counts_module())
)

# Run the app
if (interactive()) {
  shiny::shinyApp(ui = app$ui, server = app$server)
}

#' Template: Counts
#'
#' Creates a valid expression to generate a counts analysis.
#'
#' @inheritParams template_arguments
#' @param control (`list`)\cr list of settings for the analysis. See [control_counts()] for details.
#' @param event_desc_var (`character`)\cr name of the variable with events description.
#' @param paramcd (`character`)\cr endpoint parameter value to use in the table title.
#'
#' @inherit template_arguments return
#'
#' @seealso [control_counts()], [tm_t_counts()]
#'
#' @keywords internal
template_counts <- function(dataname = "ANL",
                            parentname = "ADSL",
                            arm_var = "ARM",
                            paramcd,
                            ref_arm = NULL,
                            comp_arm = NULL,
                            compare_arm = FALSE,
                            combine_comp_arms = FALSE,
                            aval_var = "AVAL",
                            cnsr_var = "CNSR",
                            strata_var = NULL,
                            add_total = FALSE,
                            total_label = default_total_label(),
                            na_level = default_na_str(),
                            basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(aval_var)
  checkmate::assert_string(cnsr_var)
  checkmate::assert_string(time_unit_var)
  checkmate::assert_string(event_desc_var)
  checkmate::assert_flag(compare_arm)
  checkmate::assert_flag(combine_comp_arms)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)

  ref_arm_val <- paste(ref_arm, collapse = "/")
  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      compare_arm = compare_arm,
      ref_arm_val = ref_arm_val
    )
  )


  data_list <- add_expr(
    data_list,
    substitute(
      expr = df_explicit_na(na_level = na_str),
      env = list(na_str = na_level)
    )
  )

  y$data <- substitute(
    expr = {
      anl <- data_pipe
      parentname <- arm_preparation %>% df_explicit_na(na_level = na_str)
    },
    env = list(
      data_pipe = pipe_expr(data_list),
      parentname = as.name(parentname),
      arm_preparation = prepare_arm(
        dataname = parentname,
        arm_var = arm_var,
        ref_arm = ref_arm,
        comp_arm = comp_arm,
        compare_arm = compare_arm,
        ref_arm_val = ref_arm_val
      ),
      na_str = na_level
    )
  )

  if (compare_arm && combine_comp_arms) {
    y$combine_comp_arms <- substitute(
      expr = groups <- combine_groups(fct = df[[group]], ref = ref_arm_val),
      env = list(
        df = as.name(parentname),
        group = arm_var,
        ref_arm_val = ref_arm_val
      )
    )
  }
  layout_list <- list()

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(
        show_colcounts = TRUE,
        title = paste("Count/Frequency Table for", paramcd),
        # main_footer = if (compare_arm) {
        #   c(
        #     paste("p-value method for Coxph (Hazard Ratio):", control$coxph$pval_method),
        #     paste("Ties for Coxph (Hazard Ratio):", control$coxph$ties),
        #     paste("Confidence Level Type for Survfit:", control$surv_time$conf_type)
        #   )
        # } else {
        #   paste("Confidence Level Type for Survfit:", control$surv_time$conf_type)
        # }
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  if (!compare_arm && !combine_comp_arms && add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::split_cols_by(
          var = arm_var,
          split_fun = add_overall_level(total_label, first = FALSE)
        ),
        env = list(
          arm_var = arm_var,
          total_label = total_label
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      split_col_expr(
        compare = compare_arm,
        combine = combine_comp_arms,
        arm_var = arm_var,
        ref = ref_arm_val
      )
    )
  }

  # layout_list <- add_expr(
  #   layout_list,
  #   substitute(
  #     expr = analyze_vars(
  #       "is_event",
  #       .stats = "count_fraction",
  #       .labels = c(count_fraction = "Patients with event (%)"),
  #       na_str = na_str
  #     ) %>%
  #       rtables::split_rows_by(
  #         "EVNT1",
  #         split_label = "Earliest contributing event",
  #         split_fun = keep_split_levels("Patients with event (%)"),
  #         label_pos = "visible",
  #         child_labels = "hidden",
  #         indent_mod = 1L,
  #       ) %>%
  #       rtables::split_rows_by(event_desc_var, split_fun = drop_split_levels) %>%
  #       rtables::summarize_row_groups(format = "xx", na_str = na_str) %>%
  #       analyze_vars(
  #         "is_not_event",
  #         .stats = "count_fraction",
  #         .labels = c(count_fraction = "Patients without event (%)"),
  #         nested = FALSE,
  #         show_labels = "hidden",
  #         na_str = na_str
  #       ),
  #     env = list(
  #       event_desc_var = event_desc_var,
  #       na_str = na_level
  #     )
  #   )
  # )

  # layout_list <- add_expr(
  #   layout_list,
  #   substitute(
  #     expr = surv_time(
  #       vars = aval_var,
  #       var_labels = paste0("Time to Event (", as.character(anl$time_unit_var[1]), ")"),
  #       is_event = "is_event",
  #       control = list(
  #         conf_level = conf_level,
  #         conf_type = conf_type,
  #         quantiles = quantiles
  #       ),
  #       na_str = na_str,
  #       table_names = "time_to_event"
  #     ),
  #     env = c(
  #       aval_var = aval_var,
  #       control$surv_time,
  #       time_unit_var = as.name(time_unit_var),
  #       na_str = na_level
  #     )
  #   )
  # )

  if (compare_arm) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = coxph_pairwise(
          vars = aval_var,
          is_event = "is_event",
          var_labels = c("Unstratified Analysis"),
          control = list(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          ),
          na_str = na_str,
          table_names = "unstratified"
        ),
        env = c(
          aval_var = aval_var,
          control$coxph,
          na_str = na_level
        )
      )
    )
  }

  if (compare_arm && !is.null(strata_var)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = coxph_pairwise(
          vars = aval_var,
          is_event = "is_event",
          var_labels = paste0("Stratified By: ", paste(strata_var, collapse = ", ")),
          strata = strata_var,
          control = control_coxph(
            pval_method = pval_method,
            ties = ties,
            conf_level = conf_level
          ),
          na_str = na_str,
          table_names = "stratified"
        ),
        env = list(
          aval_var = aval_var,
          strata_var = strata_var,
          pval_method = control$coxph$pval_method,
          ties = control$coxph$ties,
          conf_level = control$coxph$conf_level,
          na_str = na_level
        )
      )
    )
  }

  if (!is.null(time_points)) {
    method <- ifelse(compare_arm, "both", "surv")
    indents <- if (compare_arm) {
      c(
        "pt_at_risk" = 0L, "event_free_rate" = 0L, "rate_ci" = 0L,
        "rate_diff" = 1L, "rate_diff_ci" = 1L, "ztest_pval" = 1L
      )
    } else {
      NULL
    }
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = surv_timepoint(
          vars = aval_var,
          var_labels = as.character(anl$time_unit_var[1]),
          is_event = "is_event",
          time_point = time_points,
          method = method,
          control = control_surv_timepoint(
            conf_level = conf_level,
            conf_type = conf_type
          ),
          .indent_mods = indents,
          na_str = na_str
        ),
        env = list(
          aval_var = aval_var,
          time_points = time_points,
          method = method,
          indents = indents,
          time_unit_var = as.name(time_unit_var),
          conf_level = control$surv_timepoint$conf_level,
          conf_type = control$surv_timepoint$conf_type,
          na_str = na_level
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      table <- rtables::build_table(lyt = lyt, df = anl)
    })

  y
}

#' teal Module: Time-To-Event Table
#'
#' This module produces a time-to-event analysis summary table, consistent with the TLG Catalog
#' template for `TTET01` available [here](
#' https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/ttet01.html).
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_counts
#' @param conf_level_coxph ([teal.transform::choices_selected()])\cr object with all available choices and
#'   pre-selected option for confidence level, each within range of (0, 1).
#' @param conf_level_survfit ([teal.transform::choices_selected()])\cr object with all available choices and
#'   pre-selected option for confidence level, each within range of (0, 1).
#' @param event_desc_var (`character` or [teal.transform::data_extract_spec()])\cr variable name with the
#'   event description information, optional.
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
#' tm_t_counts(
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
#' @details
#' * The core functionality of this module is based on [tern::coxph_pairwise()], [tern::surv_timepoint()],
#'   and [tern::surv_time()] from the `tern` package.
#' * The arm and stratification variables are taken from the `parentname` data.
#' * The following variables are used in the module:
#'
#'   * `AVAL`: time to event
#'   * `CNSR`: 1 if record in `AVAL` is censored, 0 otherwise
#'   * `PARAMCD`: variable used to filter for endpoint (e.g. OS). After
#'     filtering for `PARAMCD` one observation per patient is expected
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
#'   ADTTE <- tmc_ex_adtte
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADTTE <- data[["ADTTE"]]
#'
#' arm_ref_comp <- list(
#'   ACTARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   ),
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   )
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_counts(
#'       label = "Count Table",
#'       dataname = "ADSL",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")),
#'         "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADTTE, "PARAMCD", "PARAM"),
#'         "OS"
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("SEX", "BMRKR2")),
#'         "SEX"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_counts <- function(label,
                     dataname,
                     parentname = ifelse(
                       inherits(arm_var, "data_extract_spec"),
                       teal.transform::datanames_input(arm_var),
                       "ADSL"
                     ),
                     arm_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     strata_var,
                     add_total = FALSE,
                     total_label = default_total_label(),
                     na_level = default_na_str(),
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.widgets::basic_table_args(),
                     transformators = list(),
                     decorators = list()) {
  message("Initializing tm_t_counts")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(paramcd, "choices_selected")
  checkmate::assert_class(strata_var, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cnsr_var = cs_to_des_select(cnsr_var, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE),
    event_desc_var = cs_to_des_select(event_desc_var, dataname = dataname),
    time_unit_var = cs_to_des_select(time_unit_var, dataname = dataname)
  )

  module(
    label = label,
    server = srv_t_counts,
    ui = ui_t_counts,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
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
ui_t_counts <- function(id, ...) {
  a <- list(...) # module args
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$vars,
    a$variables,
    a$distribution,
  )

  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(
        a[c("arm_var", "paramcd", "aval_var", "cnsr_var", "strata_var", "event_desc_var")]
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
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
        id = ns("cnsr_var"),
        label = "Censor Variable",
        data_extract_spec = a$cnsr_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      tags$div(
        class = "arm-comp-box",
        tags$label("Compare Treatments"),
        shinyWidgets::switchInput(
          inputId = ns("compare_arms"),
          value = !is.null(a$arm_ref_comp),
          size = "mini"
        ),
        conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          tags$div(
            uiOutput(ns("arms_buckets")),
            uiOutput(ns("helptext_ui")),
            checkboxInput(
              ns("combine_comp_arms"),
              "Combine all comparison groups?",
              value = FALSE
            ),
            teal.transform::data_extract_ui(
              id = ns("strata_var"),
              label = "Stratify by",
              data_extract_spec = a$strata_var,
              is_single_dataset = is_single_dataset_value
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("!input['", ns("compare_arms"), "']"),
        checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total)
      ),
      teal.widgets::optionalSelectInput(ns("time_points"),
                                        "Time Points",
                                        a$time_points$choices,
                                        a$time_points$selected,
                                        multiple = TRUE,
                                        fixed = a$time_points$fixed
      ),
      teal.transform::data_extract_ui(
        id = ns("event_desc_var"),
        label = "Event Description Variable",
        data_extract_spec = a$event_desc_var,
        is_single_dataset = is_single_dataset_value
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        bslib::accordion_panel(
          "Comparison settings",
          radioButtons(
            ns("pval_method_coxph"),
            label = HTML(
              paste(
                "p-value method for ",
                tags$span(class = "text-primary", "Coxph"),
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            choices = c("wald", "log-rank", "likelihood"),
            selected = "log-rank"
          ),
          radioButtons(
            ns("ties_coxph"),
            label = HTML(
              paste(
                "Ties for ",
                tags$span(class = "text-primary", "Coxph"),
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            choices = c("exact", "breslow", "efron"),
            selected = "exact"
          ),
          teal.widgets::optionalSelectInput(
            inputId = ns("conf_level_coxph"),
            label = HTML(
              paste(
                "Confidence Level for ",
                tags$span(class = "text-primary", "Coxph"),
                " (Hazard Ratio)",
                sep = ""
              )
            ),
            a$conf_level_coxph$choices,
            a$conf_level_coxph$selected,
            multiple = FALSE,
            fixed = a$conf_level_coxph$fixed
          )
        )
      ),
      bslib::accordion_panel(
        "Additional table settings",
        open = TRUE,
        teal.widgets::optionalSelectInput(
          inputId = ns("conf_level_survfit"),
          label = HTML(
            paste(
              "Confidence Level for ",
              tags$span(class = "text-primary", "Survfit"),
              " (KM Median Estimate & Event Free Rate)",
              sep = ""
            )
          ),
          a$conf_level_survfit$choices,
          a$conf_level_survfit$selected,
          multiple = FALSE,
          fixed = a$conf_level_survfit$fixed
        ),
        radioButtons(
          ns("conf_type_survfit"),
          "Confidence Level Type for Survfit",
          choices = c("plain", "log", "log-log"),
          selected = "plain"
        ),
        sliderInput(
          inputId = ns("probs_survfit"),
          label = "KM Estimate Percentiles",
          min = 0.01,
          max = 0.99,
          value = c(0.25, 0.75),
          width = "100%"
        ),
        teal.transform::data_extract_ui(
          id = ns("time_unit_var"),
          label = "Time Unit Variable",
          data_extract_spec = a$time_unit_var,
          is_single_dataset = is_single_dataset_value
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_counts <- function(id,
                      data,
                      filter_panel_api,
                      reporter,
                      arm_var,
                      paramcd,
                      aval_var,
                      cnsr_var,
                      strata_var,
                      event_desc_var,
                      dataname,
                      parentname,
                      arm_ref_comp,
                      time_unit_var,
                      add_total,
                      total_label,
                      label,
                      na_level,
                      basic_table_args,
                      decorators) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = reactive(data()[[parentname]]),
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_counts",
      on_off = reactive(input$compare_arms)
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        aval_var = aval_var,
        cnsr_var = cnsr_var,
        strata_var = strata_var,
        event_desc_var = event_desc_var,
        time_unit_var = time_unit_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        cnsr_var = shinyvalidate::sv_required("A censor variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required"),
        event_desc_var = shinyvalidate::sv_required("An event description variable is required"),
        time_unit_var = shinyvalidate::sv_required("A Time unit variable is required")
      ),
      filter_validation_rule = list(
        paramcd = shinyvalidate::sv_required("An endpoint is required")
      )
    )

    output$helptext_ui <- renderUI({
      req(selector_list()$arm_var()$select)
      helpText("Multiple reference groups are automatically combined into a single group.")
    })

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()

      if (isTRUE(input$compare_arms)) {
        iv$add_validator(iv_arm_ref)
      }

      iv$add_rule("conf_level_coxph", shinyvalidate::sv_required("Please choose a hazard ratio confidence level"))
      iv$add_rule(
        "conf_level_coxph", shinyvalidate::sv_between(
          0, 1,
          message_fmt = "Hazard ratio confidence level must between 0 and 1"
        )
      )
      iv$add_rule("conf_level_survfit", shinyvalidate::sv_required("Please choose a KM confidence level"))
      iv$add_rule(
        "conf_level_survfit", shinyvalidate::sv_between(
          0, 1,
          message_fmt = "KM confidence level must between 0 and 1"
        )
      )
      iv$add_rule(
        "probs_survfit",
        ~ if (!is.null(.) && .[1] == .[2]) "KM Estimate Percentiles cannot have a range of size 0"
      )
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_merge_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_merge_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      join_keys = teal.data::join_keys(data),
      data_extract = list(arm_var = arm_var, strata_var = strata_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      data() %>%
        teal.code::eval_code(as.expression(anl_merge_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_merge_inputs()$expr))
    })

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- reactive({
      teal::validate_inputs(iv_r())
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]
      anl <- anl_q()[["ANL"]]

      anl_m <- anl_merge_inputs()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_cnsr_var <- as.vector(anl_m$columns_source$cnsr_var)
      input_event_desc <- as.vector(anl_m$columns_source$event_desc_var)
      input_time_unit_var <- as.vector(anl_m$columns_source$time_unit_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_paramcd, input_aval_var,
          input_cnsr_var, input_event_desc, input_time_unit_var
        ),
        arm_var = input_arm_var
      )

      # validate arm levels
      if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
        validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      }
      if (isTRUE(input$compare_arms)) {
        validate_args <- append(
          validate_args,
          list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
        )
      }

      do.call(what = "validate_standard_inputs", validate_args)

      # check that there is at least one record with no missing data
      validate(shiny::need(
        !all(is.na(anl[[input_aval_var]])),
        "ANCOVA table cannot be calculated as all values are missing."
      ))

      NULL
    })

    # The R-code corresponding to the analysis.

    all_q <- reactive({
      validate_checks()

      anl_m <- anl_merge_inputs()

      strata_var <- as.vector(anl_m$columns_source$strata_var)

      my_calls <- template_counts(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        paramcd = unlist(anl_m$filter_info$paramcd)["selected"],
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        compare_arm = input$compare_arms,
        combine_comp_arms = input$combine_comp_arms && input$compare_arms,
        aval_var = as.vector(anl_m$columns_source$aval_var),
        cnsr_var = as.vector(anl_m$columns_source$cnsr_var),
        strata_var = if (length(strata_var) != 0) strata_var else NULL,
        add_total = input$add_total,
        total_label = total_label,
        na_level = na_level,
        basic_table_args = basic_table_args
      )

      anl_q() %>% teal.code::eval_code(as.expression(unlist(my_calls)))
    })

    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(id = "table", table_r = table_r)

    # Render R code
    source_code_r <- reactive(teal.code::get_code(req(decorated_table_q())))
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Count/Frequency Table",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(source_code_r())
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
