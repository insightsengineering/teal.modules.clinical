#' Template: Events by Term
#'
#' @inheritParams template_arguments
#' @param label_hlt (`string`)\cr label of the `hlt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param label_llt (`string`)\cr label of the `llt` variable from `dataname`. The label will be extracted from the
#' module.
#' @param event_type (`character`)\cr type of event that is summarized (e.g. adverse event, treatment).
#'   Default is "event".
#' @param sort_criteria (`character`)\cr how to sort the final table. Default option `freq_desc` sorts
#'   by decreasing total number of patients with event. Alternative option `alpha` sorts events
#'   alphabetically.
#'
#' @seealso [tm_t_events()]
#' @keywords internal
#'
template_events <- function(dataname,
                            parentname,
                            arm_var,
                            hlt,
                            llt,
                            label_hlt = NULL,
                            label_llt = NULL,
                            add_total = TRUE,
                            total_label = "All Patients",
                            event_type = "event",
                            sort_criteria = c("freq_desc", "alpha"),
                            prune_freq = 0,
                            prune_diff = 0,
                            drop_arm_levels = TRUE,
                            basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    is.character(arm_var) && length(arm_var) %in% c(1, 2),
    assertthat::is.string(hlt) || is.null(hlt),
    assertthat::is.string(llt) || is.null(llt),
    assertthat::is.string(label_hlt) || is.null(label_hlt),
    assertthat::is.string(label_llt) || is.null(label_llt),
    is.character(c(llt, hlt)),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label),
    assertthat::is.string(event_type),
    assertthat::is.flag(drop_arm_levels)
  )
  checkmate::assert_scalar(prune_freq)
  checkmate::assert_scalar(prune_diff)

  sort_criteria <- match.arg(sort_criteria)

  y <- list()

  # Start data steps.
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
      parentname <- df_explicit_na(parentname, na_level = ""),
      env = list(parentname = as.name(parentname))
    )
  )

  if (sort_criteria == "alpha") {
    if (!is.null(hlt)) {
      data_list <- add_expr(
        data_list,
        substitute(
          expr = anl <- anl %>% dplyr::mutate(`:=`(hlt, as.character(hlt))),
          env = list(hlt = as.name(hlt))
        )
      )
    }

    if (!is.null(llt)) {
      data_list <- add_expr(
        data_list,
        substitute(
          expr = anl <- anl %>% dplyr::mutate(`:=`(llt, as.character(llt))),
          env = list(llt = as.name(llt))
        )
      )
    }
  }

  term_vars <- c(hlt, llt)

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- anl %>%
        df_explicit_na(omit_columns = setdiff(names(anl), term_vars)),
      env = list(
        term_vars = term_vars
      )
    )
  )
  y$data <- bracket_expr(data_list)

  # Start layout steps.
  layout_list <- list()

  basic_title <- if (is.null(hlt) && !is.null(llt)) {
    paste0("Event Summary by Term : ", label_llt)
  } else if (!is.null(hlt) && is.null(llt)) {
    paste0("Event Summary by Term : ", label_hlt)
  } else if (!is.null(hlt) && !is.null(llt)) {
    paste0("Event Summary by Term : ", label_hlt, " and ", label_llt)
  } else {
    "Event Summary by Term"
  }

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(title = basic_title)
    )
  )

  layout_list <- add_expr(layout_list, parsed_basic_table_args)
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_cols_by(var = arm_var),
      env = list(arm_var = arm_var[[1]])
    )
  )
  if (length(arm_var) == 2) {
    layout_list <- add_expr(
      layout_list,
      if (drop_arm_levels) {
        substitute(
          expr = rtables::split_cols_by(nested_col, split_fun = drop_split_levels),
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

  layout_list <- add_expr(
    layout_list,
    quote(rtables::add_colcounts())
  )

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = rtables::add_overall_col(label = total_label),
        env = list(total_label = total_label)
      )
    )
  }

  unique_label <- paste0("Total number of patients with at least one ", event_type)
  nonunique_label <- paste0("Overall total number of ", event_type, "s")

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_num_patients(
        var = "USUBJID",
        .stats = c("unique", "nonunique"),
        .labels = c(
          unique = unique_label,
          nonunique = nonunique_label
        )
      ),
      env = list(unique_label = unique_label, nonunique_label = nonunique_label)
    )
  )

  one_term <- is.null(hlt) || is.null(llt)

  if (one_term) {
    term_var <- ifelse(is.null(hlt), llt, hlt)

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = count_occurrences(vars = term_var, .indent_mods = -1L) %>%
          append_varlabels(dataname, term_var),
        env = list(
          term_var = term_var,
          dataname = as.name(dataname)
        )
      )
    )
  } else {
    # Case when both hlt and llt are used.

    y$layout_prep <- quote(split_fun <- drop_split_levels)

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr =
          rtables::split_rows_by(
            hlt,
            child_labels = "visible",
            nested = FALSE,
            indent_mod = -1L,
            split_fun = split_fun,
            label_pos = "topleft",
            split_label = formatters::var_labels(dataname[hlt])
          ) %>%
            summarize_num_patients(
              var = "USUBJID",
              .stats = c("unique", "nonunique"),
              .labels = c(
                unique = unique_label,
                nonunique = nonunique_label
              )
            ) %>%
            count_occurrences(vars = llt, .indent_mods = -1L) %>%
            append_varlabels(dataname, llt, indent = 1L),
        env = list(
          dataname = as.name(dataname),
          hlt = hlt,
          llt = llt,
          unique_label = unique_label,
          nonunique_label = nonunique_label
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  # Full table.
  y$table <- substitute(
    expr = result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent),
    env = list(parent = as.name(parentname))
  )

  # Start pruning table.
  prune_list <- list()
  prune_list <- add_expr(
    prune_list,
    quote(
      pruned_result <- result %>% rtables::prune_table()
    )
  )

  if (prune_freq > 0 || prune_diff > 0) {
    # Do not use "All Patients" column for pruning conditions.
    prune_list <- add_expr(
      prune_list,
      substitute(
        expr = col_indices <- 1:(ncol(result) - add_total),
        env = list(add_total = add_total)
      )
    )

    if (prune_freq > 0 && prune_diff == 0) {
      prune_list <- add_expr(
        prune_list,
        substitute(
          expr = row_condition <- has_fraction_in_any_col(atleast = prune_freq, col_indices = col_indices),
          env = list(prune_freq = prune_freq)
        )
      )
    } else if (prune_freq == 0 && prune_diff > 0) {
      prune_list <- add_expr(
        prune_list,
        substitute(
          expr = row_condition <- has_fractions_difference(atleast = prune_diff, col_indices = col_indices),
          env = list(prune_diff = prune_diff)
        )
      )
    } else if (prune_freq > 0 && prune_diff > 0) {
      prune_list <- add_expr(
        prune_list,
        substitute(
          expr = row_condition <- has_fraction_in_any_col(atleast = prune_freq, col_indices = col_indices) &
            has_fractions_difference(atleast = prune_diff, col_indices = col_indices),
          env = list(prune_freq = prune_freq, prune_diff = prune_diff)
        )
      )
    }

    # Apply pruning conditions.
    prune_list <- add_expr(
      prune_list,
      substitute(
        expr = pruned_result <- pruned_result %>% rtables::prune_table(keep_rows(row_condition))
      )
    )
  }

  y$prune <- bracket_expr(prune_list)

  # Start sorting pruned table.
  sort_list <- list()

  if (sort_criteria == "alpha") {
    if (prune_freq == 0 && prune_diff == 0) {
      # This is just a dummy step to get the right variable result.
      # No additional sorting is needed because during the data pre-processing step,
      # llt and/or hlt are converted to factors with alphabetically sorted levels.
      # So the order in y$table table is already alphabetically sorted.
      sort_list <- add_expr(
        sort_list,
        quote({
          pruned_and_sorted_result <- pruned_result
          pruned_and_sorted_result
        })
      )
    } else {
      sort_list <- add_expr(
        sort_list,
        quote(
          criteria_fun <- function(tr) {
            inherits(tr, "ContentRow")
          }
        )
      )

      sort_list <- add_expr(
        sort_list,
        quote({
          pruned_and_sorted_result <- rtables::trim_rows(pruned_result, criteria = criteria_fun)
          pruned_and_sorted_result
        })
      )
    }
  } else {
    # Sort by decreasing frequency.

    # When the "All Patients" column is present we only use that for scoring.
    scorefun_hlt <- if (add_total) {
      quote(cont_n_onecol(ncol(result)))
    } else {
      quote(cont_n_allcols)
    }
    scorefun_llt <- if (add_total) {
      quote(score_occurrences_cols(col_indices = seq(1, ncol(result))))
    } else {
      quote(score_occurrences)
    }

    if (one_term) {
      term_var <- ifelse(is.null(hlt), llt, hlt)

      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = {
            pruned_and_sorted_result <- pruned_result %>%
              sort_at_path(path = c(term_var), scorefun = scorefun_llt)
            pruned_and_sorted_result
          },
          env = list(
            term_var = term_var,
            scorefun_llt = scorefun_llt
          )
        )
      )
    } else {
      sort_list <- add_expr(
        sort_list,
        substitute(
          expr = {
            pruned_and_sorted_result <- pruned_result %>%
              sort_at_path(path = c(hlt), scorefun = scorefun_hlt) %>%
              sort_at_path(path = c(hlt, "*", llt), scorefun = scorefun_llt)
          },
          env = list(
            llt = llt,
            hlt = hlt,
            scorefun_hlt = scorefun_hlt,
            scorefun_llt = scorefun_llt
          )
        )
      )

      if (prune_freq > 0 || prune_diff > 0) {
        sort_list <- add_expr(
          sort_list,
          quote(
            criteria_fun <- function(tr) {
              inherits(tr, "ContentRow")
            }
          )
        )

        sort_list <- add_expr(
          sort_list,
          quote(
            pruned_and_sorted_result <- rtables::trim_rows(pruned_and_sorted_result, criteria = criteria_fun)
          )
        )
      }

      sort_list <- add_expr(
        sort_list,
        quote(pruned_and_sorted_result)
      )
    }
  }
  y$sort <- bracket_expr(sort_list)

  y
}

#' Teal Module: Events by Term
#'
#' @inheritParams module_arguments
#' @inheritParams template_events
#'
#' @export
#'
#' @examples
#' adsl <- tmc_ex_adsl
#' adae <- tmc_ex_adae
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAE", adae)
#'   ),
#'   modules = modules(
#'     tm_t_events(
#'       label = "Adverse Event Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       llt = choices_selected(
#'         choices = variable_choices(adae, c("AETERM", "AEDECOD")),
#'         selected = c("AEDECOD")
#'       ),
#'       hlt = choices_selected(
#'         choices = variable_choices(adae, c("AEBODSYS", "AESOC")),
#'         selected = "AEBODSYS"
#'       ),
#'       add_total = TRUE,
#'       event_type = "adverse event"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_events <- function(label,
                        dataname,
                        parentname = ifelse(
                          inherits(arm_var, "data_extract_spec"),
                          teal.transform::datanames_input(arm_var),
                          "ADSL"
                        ),
                        arm_var,
                        hlt,
                        llt,
                        add_total = TRUE,
                        total_label = "All Patients",
                        event_type = "event",
                        sort_criteria = c("freq_desc", "alpha"),
                        prune_freq = 0,
                        prune_diff = 0,
                        drop_arm_levels = TRUE,
                        pre_output = NULL,
                        post_output = NULL,
                        basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_events")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(event_type)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_scalar(prune_freq)
  checkmate::assert_scalar(prune_diff)
  checkmate::assert_flag(drop_arm_levels)
  sort_criteria <- match.arg(sort_criteria)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname, multiple = TRUE, ordered = TRUE),
    hlt = cs_to_des_select(hlt, dataname = dataname),
    llt = cs_to_des_select(llt, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_t_events_byterm,
    server = srv_t_events_byterm,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        event_type = event_type,
        label = label,
        total_label = total_label,
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_events_byterm <- function(id, ...) {
  ns <- shiny::NS(id)
  a <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(a$arm_var, a$hlt, a$llt)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "hlt", "llt")]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("hlt"),
        label = "Event High Level Term",
        data_extract_spec = a$hlt,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("llt"),
        label = "Event Low Level Term",
        data_extract_spec = a$llt,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total),
      teal.widgets::panel_item(
        "Additional table settings",
        shiny::checkboxInput(
          ns("drop_arm_levels"),
          label = "Drop columns not in filtered analysis dataset",
          value = a$drop_arm_levels
        ),
        shiny::selectInput(
          inputId = ns("sort_criteria"),
          label = "Sort Criteria",
          choices = c(
            "Decreasing frequency" = "freq_desc",
            "Alphabetically" = "alpha"
          ),
          selected = a$sort_criteria,
          multiple = FALSE
        ),
        shiny::helpText("Pruning Options"),
        shiny::numericInput(
          inputId = ns("prune_freq"),
          label = "Minimum Incidence Rate(%) in any of the treatment groups",
          value = a$prune_freq,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        ),
        shiny::numericInput(
          inputId = ns("prune_diff"),
          label = "Minimum Difference Rate(%) between any of the treatment groups",
          value = a$prune_diff,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        )
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), button_label = "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_events_byterm <- function(id,
                                data,
                                filter_panel_api,
                                reporter,
                                dataname,
                                parentname,
                                event_type,
                                arm_var,
                                hlt,
                                llt,
                                drop_arm_levels,
                                label,
                                total_label,
                                basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(arm_var = arm_var, hlt = hlt, llt = llt),
      datasets = data,
      select_validation_rule = list(
        arm_var = ~ if (length(.) != 1 && length(.) != 2) {
          "Please select 1 or 2 treatment variable values"
        },
        hlt = ~ if (length(selector_list()$llt()$select) + length(.) == 0) {
          "Please select at least one of \"LOW LEVEL TERM\" or \"HIGH LEVEL TERM\" variables."
        },
        llt = ~ if (length(selector_list()$hlt()$select) + length(.) == 0) {
          "Please select at least one of \"LOW LEVEL TERM\" or \"HIGH LEVEL TERM\" variables."
        }
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("prune_freq", shinyvalidate::sv_required("Please provide an Incidence Rate between 0 and 100 (%)."))
      iv$add_rule(
        "prune_freq",
        shinyvalidate::sv_between(0, 100, message_fmt = "Please provide an Incidence Rate between 0 and 100 (%).")
      )
      iv$add_rule("prune_diff", shinyvalidate::sv_required("Please provide a Difference Rate between 0 and 100 (%)."))
      iv$add_rule(
        "prune_diff",
        shinyvalidate::sv_between(0, 100, message_fmt = "Please provide a Difference Rate between 0 and 100 (%).")
      )
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      join_keys = get_join_keys(data),
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL",
      join_keys = get_join_keys(data)
    )

    anl_q <- shiny::reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())

      adsl_filtered <- merged$anl_q()[[parentname]]
      anl_filtered <- merged$anl_q()[[dataname]]

      input_arm_var <- as.vector(merged$anl_input_r()$columns_source$arm_var)
      input_level_term <- c(
        as.vector(merged$anl_input_r()$columns_source$hlt),
        as.vector(merged$anl_input_r()$columns_source$llt)
      )

      shiny::validate(
        if (length(input_arm_var) >= 1) {
          shiny::need(is.factor(adsl_filtered[[input_arm_var[[1]]]]), "Treatment variable is not a factor.")
        },
        if (length(input_arm_var) == 2) {
          shiny::need(
            is.factor(adsl_filtered[[input_arm_var[[2]]]]) & all(!adsl_filtered[[input_arm_var[[2]]]] %in% c(
              "", NA
            )),
            "Please check nested treatment variable which needs to be a factor without NA or empty strings."
          )
        }
      )

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_level_term),
        arm_var = input_arm_var[[1]]
      )
    })

    # The R-code corresponding to the analysis.
    table_q <- shiny::reactive({
      validate_checks()
      ANL <- merged$anl_q()[["ANL"]] # nolint

      input_hlt <- as.vector(merged$anl_input_r()$columns_source$hlt)
      input_llt <- as.vector(merged$anl_input_r()$columns_source$llt)
      label_hlt <- if (length(input_hlt) != 0) attributes(ANL[[input_hlt]])$label else NULL
      label_llt <- if (length(input_llt) != 0) attributes(ANL[[input_llt]])$label else NULL

      my_calls <- template_events(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(merged$anl_input_r()$columns_source$arm_var),
        hlt = if (length(input_hlt) != 0) input_hlt else NULL,
        llt = if (length(input_llt) != 0) input_llt else NULL,
        label_hlt = label_hlt,
        label_llt = label_llt,
        add_total = input$add_total,
        total_label = total_label,
        event_type = event_type,
        sort_criteria = input$sort_criteria,
        prune_freq = input$prune_freq / 100,
        prune_diff = input$prune_diff / 100,
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = basic_table_args
      )

      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
    })

    # Outputs to render.
    table_r <- shiny::reactive({
      table_q()[["pruned_and_sorted_result"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(table_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(table_q())))
    )

    # Render R code.
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(table_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Events by Term Table")
        card$append_text("Events by Term Table", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(table_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
