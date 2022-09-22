#' Template: ANCOVA summary
#'
#' Creates a valid expression for analysis of variance summary table.
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
#'
#' @seealso [tm_t_ancova()]
#' @keywords internal
#'
template_ancova <- function(dataname = "ANL",
                            parentname = "ADSL",
                            arm_var,
                            ref_arm = NULL,
                            comp_arm = NULL,
                            combine_comp_arms = FALSE,
                            aval_var,
                            label_aval = NULL,
                            cov_var,
                            paramcd_levels = "",
                            paramcd_var = "PARAMCD",
                            label_paramcd = NULL,
                            visit_levels = "",
                            visit_var = "AVISIT",
                            conf_level = 0.95,
                            basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(label_aval) || is.null(label_aval),
    assertthat::is.flag(combine_comp_arms),
    assertthat::is.string(aval_var),
    is.character(cov_var)
  )

  y <- list()

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
        expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
    parent_list <- add_expr(
      parent_list,
      substitute_names(
        expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
        names = list(arm_var = as.name(arm_var)),
        others = list(comp_arm = comp_arm)
      )
    )
  }

  anl_list <- add_expr(anl_list, quote(df_explicit_na(na_level = "")))
  parent_list <- add_expr(parent_list, quote(df_explicit_na(na_level = "")))

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
      module_table = teal.widgets::basic_table_args(title = table_title)
    )
  )

  y$layout_prep <- quote(split_fun <- drop_split_levels)
  layout_list <- list()
  layout_list <- add_expr(
    layout_list,
    parsed_basic_table_args
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_cols_by(var = arm_var, ref_group = ref_group) %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          visit_var,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = formatters::var_labels(dataname[visit_var], fill = TRUE)
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
    if (length(cov_var) == 0) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          rtables::split_rows_by(
            paramcd_var,
            split_fun = split_fun,
            label_pos = "topleft",
            split_label = formatters::var_labels(dataname[paramcd_var], fill = TRUE)
          ) %>%
            summarize_ancova(
              vars = aval_var,
              variables = list(arm = arm_var, covariates = cov_var),
              conf_level = conf_level,
              var_labels = "Unadjusted mean",
              show_labels = "hidden",
              .labels = c(lsmean = "Unadjusted Mean", lsmean_diff = "Difference in Unadjusted Means")
            ),
          env = list(
            paramcd_var = paramcd_var,
            aval_var = aval_var,
            arm_var = arm_var,
            cov_var = cov_var,
            conf_level = conf_level,
            dataname = as.name(dataname)
          )
        )
      )
    } else {
      layout_list <- add_expr(
        layout_list,
        substitute(
          rtables::split_rows_by(
            paramcd_var,
            split_fun = split_fun,
            label_pos = "topleft",
            split_label = formatters::var_labels(dataname[paramcd_var], fill = TRUE)
          ) %>%
            summarize_ancova(
              vars = aval_var,
              variables = list(arm = arm_var, covariates = cov_var),
              conf_level = conf_level,
              var_labels = "Adjusted mean",
              show_labels = "hidden"
            ),
          env = list(
            paramcd_var = paramcd_var,
            aval_var = aval_var,
            arm_var = arm_var,
            cov_var = cov_var,
            conf_level = conf_level,
            dataname = as.name(dataname)
          )
        )
      )
    }
  } else {

    # Only one entry in `paramcd_levels` here.
    layout_list <- add_expr(
      layout_list,
      substitute(
        rtables::append_topleft(paste0("  ", paramcd_levels)) %>%
          summarize_ancova(
            vars = aval_var,
            variables = list(arm = arm_var, covariates = NULL),
            conf_level = conf_level,
            var_labels = "Unadjusted comparison",
            .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
            table_names = "unadjusted_comparison"
          ),
        env = list(
          paramcd_levels = paramcd_levels,
          aval_var = aval_var,
          arm_var = arm_var,
          conf_level = conf_level,
          dataname = as.name(dataname)
        )
      )
    )

    if (length(cov_var) > 0) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          summarize_ancova(
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
            conf_level = conf_level,
            dataname = as.name(dataname)
          )
        )
      )
    }
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Build table.
  y$table <- substitute(
    expr = {
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
      result
    },
    env = list(
      anl = as.name(dataname),
      parent = as.name(parentname)
    )
  )

  y
}

#' Teal Module: ANCOVA Teal Module
#'
#' @inheritParams module_arguments
#'
#' @details This module produces an analysis of variance summary table that is
#' similar to STREAM template `aovt01` when multiple endpoints are selected.
#' When a single endpoint is selected, both unadjusted and adjusted comparison
#' would be provided. This modules expects that the analysis data has the
#' following variables:
#'
#' \tabular{ll}{
#'  `AVISIT` \tab variable used to filter for analysis visits.\cr
#'  `PARAMCD` \tab variable used to filter for endpoints, after filtering for
#'  `paramcd` and `avisit`, one observation per patient is expected for the analysis
#'  to be meaningful.
#' }
#'
#' @export
#'
#' @examples
#'
#' # Preparation of the test case.
#' library(dplyr)
#' library(scda)
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adqs <- synthetic_cdisc_data("latest")$adqs
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
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADQS", adqs),
#'     code =
#'       '
#'       adsl <- synthetic_cdisc_data("latest")$adsl
#'       adqs <- synthetic_cdisc_data("latest")$adqs
#'       '
#'   ),
#'   modules = modules(
#'     tm_t_ancova(
#'       label = "ANCOVA table",
#'       dataname = "ADQS",
#'       avisit = choices_selected(
#'         choices = value_choices(adqs, "AVISIT"),
#'         selected = "WEEK 1 DAY 8"
#'       ),
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, c("ARM", "ACTARMCD", "ARMCD")),
#'         selected = "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       aval_var = choices_selected(
#'         choices = variable_choices(adqs, c("CHG", "AVAL")),
#'         selected = "CHG"
#'       ),
#'       cov_var = choices_selected(
#'         choices = variable_choices(adqs, c("BASE", "STRATA1", "SEX")),
#'         selected = "STRATA1"
#'       ),
#'       paramcd = choices_selected(
#'         choices = value_choices(adqs, "PARAMCD", "PARAM"),
#'         selected = "FKSI-FWB"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_ancova <- function(label,
                        dataname,
                        parentname = ifelse(
                          inherits(arm_var, "data_extract_spec"),
                          teal.transform::datanames_input(arm_var),
                          "ADSL"
                        ),
                        arm_var,
                        arm_ref_comp = NULL,
                        aval_var,
                        cov_var,
                        avisit,
                        paramcd,
                        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_ancova")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- c(as.list(environment()))

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    cov_var = cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE),
    avisit = cs_to_des_filter(avisit, dataname = dataname, multiple = TRUE, include_vars = TRUE),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname, multiple = TRUE)
  )

  module(
    label = label,
    ui = ui_ancova,
    ui_args = c(data_extract_list, args),
    server = srv_ancova,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_ancova <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var, a$aval_var, a$cov_var, a$avisit, a$paramcd
  )

  ns <- shiny::NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "aval_var", "cov_var", "avisit", "paramcd")]),
      teal.transform::data_extract_ui(
        id = ns("avisit"),
        label = "Analysis Visit",
        data_extract_spec = a$avisit,
        is_single_dataset = is_single_dataset_value
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
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::uiOutput(
        ns("arms_buckets"),
        title = paste(
          "Multiple reference groups are automatically combined into a single group when more than one",
          "value is selected."
        )
      ),
      shiny::helpText("Multiple reference groups are automatically combined into a single group."),
      shiny::checkboxInput(
        ns("combine_comp_arms"),
        "Combine all comparison groups?",
        value = FALSE
      ),
      teal.transform::data_extract_ui(
        id = ns("cov_var"),
        label = "Covariates",
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::optionalSelectInput(
        inputId = ns("conf_level"),
        label = shiny::HTML(paste("Confidence Level")),
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      )
    ),
    forms = teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_ancova <- function(id,
                       data,
                       reporter,
                       filter_panel_api,
                       dataname,
                       parentname,
                       arm_var,
                       arm_ref_comp,
                       aval_var,
                       cov_var,
                       paramcd,
                       avisit,
                       label,
                       basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel.
    arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_ancova"
    )

    anl_merged_input <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(
        arm_var = arm_var,
        aval_var = aval_var,
        cov_var = cov_var,
        avisit = avisit,
        paramcd = paramcd
      ),
      merge_function = "dplyr::inner_join",
      join_keys = get_join_keys(data)
    )

    adsl_merged_input <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL",
      join_keys = get_join_keys(data)
    )

    anl_merged_q <- reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code(data)) %>%
        teal.code::eval_code(as.expression(anl_merged_input()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_merged_input()$expr))
    })

    merged <- list(
      anl_input_r = anl_merged_input,
      adsl_input_r = adsl_merged_input,
      anl_q_r = anl_merged_q
    )

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- shiny::reactive({
      adsl_filtered <- data[[parentname]]()
      anl_filtered <- data[[dataname]]()

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_aval_var <- as.vector(merged$anl_input_r()$columns_source$aval_var)
      input_cov_var <- as.vector(merged$anl_input_r()$columns_source$cov_var)
      input_avisit <- unlist(avisit$filter)["vars_selected"]
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # Validate inputs.
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_avisit, input_aval_var, input_cov_var),
        arm_var = input_arm_var
      )
      validate_args <- append(
        validate_args,
        list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
      )
      do.call(what = "validate_standard_inputs", validate_args)

      # Other validations.
      shiny::validate(shiny::need(
        length(input_aval_var) > 0,
        "Analysis variable cannot be empty."
      ))
      shiny::validate(shiny::need(
        length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) > 1,
        "ANCOVA table needs at least 2 arm groups to make comparisons."
      ))
      # check that there is at least one record with no missing data
      shiny::validate(shiny::need(
        !all(is.na(merged$anl_q_r()[["ANL"]][[input_aval_var]])),
        "ANCOVA table cannot be calculated as all values are missing."
      ))
      # check that for each visit there is at least one record with no missing data
      all_NA_dataset <- merged$anl_q_r()[["ANL"]] %>% # nolint
        dplyr::group_by(dplyr::across(dplyr::all_of(c(input_avisit, input_arm_var)))) %>%
        dplyr::summarize(all_NA = all(is.na(.data[[input_aval_var]])))
      shiny::validate(shiny::need(
        !any(all_NA_dataset$all_NA),
        "ANCOVA table cannot be calculated as all values are missing for one visit for (at least) one arm."
      ))
      shiny::validate(shiny::need(
        input$conf_level >= 0 && input$conf_level <= 1,
        "Please choose a confidence level between 0 and 1"
      ))

      shiny::validate(shiny::need(
        input[[extract_input("avisit", avisit$filter[[1]]$dataname, filter = TRUE)]],
        "`Analysis Visit` field cannot be empty"
      ))

      shiny::validate(shiny::need(
        input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]],
        "`Select Endpoint` is not selected."
      ))

      if (length(input_cov_var >= 1L)) {
        input_cov_var_dataset <- anl_filtered[input_cov_var]
        shiny::validate(
          shiny::need(
            all(vapply(input_cov_var_dataset, function(col) length(unique(col)) > 1L, logical(1))),
            "Selected covariates should have more than one level for showing the adjusted analysis."
          )
        )
      }
    })

    # The R-code corresponding to the analysis.
    output_table <- shiny::reactive({
      validate_checks()
      ANL <- merged$anl_q_r()[["ANL"]] # nolint

      label_paramcd <- get_paramcd_label(ANL, paramcd)
      input_aval <- as.vector(merged$anl_input_r()$columns_source$aval_var)
      label_aval <- if (length(input_aval) != 0) attributes(ANL[[input_aval]])$label else NULL
      paramcd_levels <- unique(ANL[[unlist(paramcd$filter)["vars_selected"]]])
      visit_levels <- unique(ANL[[unlist(avisit$filter)["vars_selected"]]])

      my_calls <- template_ancova(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = as.vector(merged$anl_input_r()$columns_source$arm_var),
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        combine_comp_arms = input$combine_comp_arms,
        aval_var = as.vector(merged$anl_input_r()$columns_source$aval_var),
        label_aval = label_aval,
        cov_var = as.vector(merged$anl_input_r()$columns_source$cov_var),
        paramcd_levels = paramcd_levels,
        paramcd_var = unlist(paramcd$filter)["vars_selected"],
        label_paramcd = label_paramcd,
        visit_levels = visit_levels,
        visit_var = unlist(avisit$filter)["vars_selected"],
        conf_level = as.numeric(input$conf_level),
        basic_table_args = basic_table_args
      )
      teal.code::eval_code(merged$anl_q_r(), as.expression(my_calls))
    })

    # Output to render.
    table_r <- shiny::reactive({
      output_table()[["result"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    # Render R code.
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(output_table())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("ANCOVA")
        card$append_text("ANCOVA", "header2")
        card$append_text("Analysis of Covariance", "header3")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(output_table()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
