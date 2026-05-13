#' Template: Multiple Events by Term
#'
#' Creates a valid expression to generate a table of multiple events by term.
#'
#' @inheritParams template_arguments
#' @param seq_var (`character`)\cr name of analysis sequence number variable. Used for counting the unique number
#'   of events.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_mult_events()]
#'
#' @keywords internal
template_mult_events <- function(dataname,
                                 parentname,
                                 arm_var,
                                 seq_var,
                                 hlt,
                                 llt,
                                 add_total = TRUE,
                                 total_label = default_total_label(),
                                 na_level = tern::default_na_str(),
                                 event_type = "event",
                                 drop_arm_levels = TRUE,
                                 basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(arm_var)
  checkmate::assert_string(seq_var)
  checkmate::assert_character(hlt, null.ok = TRUE)
  checkmate::assert_string(llt, null.ok = FALSE)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_string(event_type)
  checkmate::assert_flag(drop_arm_levels)

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
      arm_var = arm_var,
      drop_arm_levels = drop_arm_levels
    )
  )

  if (is.null(hlt)) {
    term_vars <- c(llt)
  } else {
    term_vars <- c(hlt, llt)
  }

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- anl %>%
        tern::df_explicit_na(omit_columns = setdiff(names(anl), term_vars)),
      env = list(
        term_vars = term_vars
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute_names(
      expr = anl <- anl %>%
        dplyr::mutate(seq_var = as.factor(seq_var)),
      names = list(
        seq_var = as.name(seq_var)
      )
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

  y$layout_prep <- quote(split_fun <- rtables::drop_split_levels)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(show_colcounts = TRUE)
    )
  )

  # Start layout steps.
  layout_list <- list()

  layout_list <- add_expr(layout_list, parsed_basic_table_args)
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_cols_by(var = arm_var),
      env = list(arm_var = arm_var)
    )
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
  nonunique_label <- paste0("Total number of ", event_type, "s")

  layout_list <- add_expr(
    layout_list,
    substitute(
      tern::summarize_num_patients(
        var = "USUBJID",
        count_by = seq_var,
        .stats = c("unique", "nonunique"),
        .labels = c(
          unique = unique_label,
          nonunique = nonunique_label
        )
      ),
      env = list(unique_label = unique_label, nonunique_label = nonunique_label, seq_var = seq_var)
    )
  )

  if (is.null(hlt)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::count_occurrences(vars = llt, .indent_mods = -1L) %>%
          tern::append_varlabels(dataname, llt, indent = 0L),
        env = list(
          dataname = as.name(dataname), llt = llt
        )
      )
    )
  } else {
    lbl_lst <- list()

    for (ii in seq_along(hlt)) {
      hlt_new <- hlt[ii]

      lbl_lst <- add_expr(
        lbl_lst,
        substitute(
          expr = attr(dataname$hlt_new, which = "label"),
          env = list(
            dataname = as.name(dataname),
            hlt_new = hlt_new
          )
        )
      )

      nested <- ifelse(ii == 1, FALSE, TRUE)
      indent_mod <- ifelse(ii == 1, -1L, 0L)

      layout_list <- add_expr(
        layout_list,
        substitute(
          expr =
            rtables::split_rows_by(
              hlt,
              child_labels = "visible",
              nested = nested,
              indent_mod = indent_mod,
              split_fun = split_fun,
              label_pos = "topleft",
              split_label = teal.data::col_labels(dataname[hlt_new])
            ),
          env = list(
            hlt = hlt_new,
            nested = nested,
            indent_mod = indent_mod,
            dataname = as.name(dataname),
            hlt_new = hlt_new
          )
        )
      )
    }

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = tern::summarize_num_patients(
          var = "USUBJID",
          count_by = seq_var,
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = unique_label,
            nonunique = nonunique_label
          )
        ) %>%
          tern::count_occurrences(vars = llt, .indent_mods = -1L) %>%
          tern::append_varlabels(dataname, llt, indent = indent_space),
        env = list(
          dataname = as.name(dataname), llt = llt,
          unique_label = unique_label, nonunique_label = nonunique_label,
          seq_var = seq_var,
          indent_space = length(hlt)
        )
      )
    )
  }

  lyt <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$layout <- lyt

  # Table
  y$table <- substitute(
    expr = result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent),
    env = list(
      parent = as.name(parentname)
    )
  )

  # Start sorting table
  if (is.null(hlt)) {
    pth <- c(llt)
  } else {
    pth <- c(rbind(hlt, rep("*", length(hlt))), llt)
  }

  sort_list <- list()

  sort_list <- add_expr(
    sort_list,
    substitute(
      expr = sorted_result <- result %>%
        rtables::sort_at_path(path = pth, scorefun = tern::score_occurrences),
      env = list(pth = pth)
    )
  )

  y$table_sorted <- bracket_expr(sort_list)

  # Combine tables.
  y$final_table <- quote(
    expr = {
      table <- sorted_result
    }
  )

  y
}

#' teal Module: Multiple Events by Term
#'
#' This module produces a table of multiple events by term.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_mult_events
#' @param seq_var ([teal.picks::variables()]; legacy `teal.transform` objects are deprecated but still accepted)\cr
#'   object with all available choices and preselected option for variable names that can be used as analysis
#'   sequence number variable. Used for counting the unique number of events.
#' @param title_text (`string`)\cr text to display as the first part of the dynamic table title. The table title is
#'   constructed as follows: "`title_text` by `hlt` and `llt`". Defaults to `"Concomitant Medications"`.
#'
#' @inherit module_arguments return seealso
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
#' tm_t_mult_events(
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
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADCM <- tmc_ex_adcm
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#' join_keys(data)["ADCM", "ADCM"] <- adcm_keys
#'
#' ADSL <- data[["ADSL"]]
#' ADCM <- data[["ADCM"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_mult_events(
#'       label = "Concomitant Medications by Medication Class and Preferred Name",
#'       dataname = "ADCM",
#'       arm_var = variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
#'       seq_var = variables(choices = "CMSEQ", selected = "CMSEQ", fixed = TRUE),
#'       hlt = variables(
#'         choices = c("ATC1", "ATC2", "ATC3", "ATC4"),
#'         selected = c("ATC1", "ATC2", "ATC3", "ATC4")
#'       ),
#'       llt = variables(
#'         choices = "CMDECOD",
#'         selected = "CMDECOD"
#'       ),
#'       add_total = TRUE,
#'       event_type = "treatment"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_t_mult_events <- function(label,
                             dataname,
                             parentname = "ADSL",
                             arm_var,
                             seq_var,
                             hlt,
                             llt,
                             add_total = TRUE,
                             total_label = default_total_label(),
                             na_level = tern::default_na_str(),
                             event_type = "event",
                             title_text = "Concomitant Medications",
                             drop_arm_levels = TRUE,
                             pre_output = NULL,
                             post_output = NULL,
                             basic_table_args = teal.widgets::basic_table_args(),
                             transformators = list(),
                             decorators = list()) {
  message("Initializing tm_t_mult_events")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)

  arm_var <- migrate_choices_selected_to_variables(arm_var, "arm_var")
  seq_var <- migrate_choices_selected_to_variables(seq_var, "seq_var")
  hlt <- migrate_choices_selected_to_variables(hlt, "hlt")
  llt <- migrate_choices_selected_to_variables(llt, "llt")

  checkmate::assert_string(event_type)
  checkmate::assert_string(title_text)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  teal::assert_decorators(decorators, "table")

  arm_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), arm_var)
  seq_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), seq_var)
  hlt <- create_picks_helper(teal.picks::datasets(dataname, dataname), hlt)
  llt <- create_picks_helper(teal.picks::datasets(dataname, dataname), llt)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_mult_events_byterm,
    server = srv_t_mult_events_byterm,
    ui_args = args[names(args) %in% names(formals(ui_t_mult_events_byterm))],
    server_args = args[names(args) %in% names(formals(srv_t_mult_events_byterm))],
    transformators = transformators,
    datanames = union(parentname, dataname)
  )
}

#' @keywords internal
ui_t_mult_events_byterm <- function(id,
                                    arm_var,
                                    seq_var,
                                    hlt,
                                    llt,
                                    add_total,
                                    drop_arm_levels,
                                    pre_output,
                                    post_output,
                                    decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Select Treatment Variable"),
        teal.picks::picks_ui(ns("arm_var"), arm_var)
      ),
      tags$div(
        tags$label("Event High Level Term"),
        teal.picks::picks_ui(ns("hlt"), hlt)
      ),
      tags$div(
        tags$label("Event Low Level Term"),
        teal.picks::picks_ui(ns("llt"), llt)
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = add_total),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = drop_arm_levels
          )
        )
      ),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          tags$div(
            tags$label("Analysis Sequence Number"),
            teal.picks::picks_ui(ns("seq_var"), seq_var)
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @keywords internal
srv_t_mult_events_byterm <- function(id,
                                     data,
                                     dataname,
                                     parentname,
                                     event_type,
                                     title_text,
                                     arm_var,
                                     seq_var,
                                     hlt,
                                     llt,
                                     drop_arm_levels,
                                     label,
                                     total_label,
                                     na_level,
                                     basic_table_args,
                                     decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        arm_var = arm_var,
        seq_var = seq_var,
        hlt = hlt,
        llt = llt
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
      "data_with_card",
      data = data_with_card,
      selectors = anl_selectors,
      output_name = "ANL"
    )

    merged_adsl_anl <- merge_srv(
      "merge_adsl_anl",
      data = merged_anl$data,
      selectors = adsl_selectors,
      output_name = "ANL_ADSL"
    )

    anl_selectors <- selectors
    adsl_selectors <- selectors["arm_var"]


    anl_q <- merged_adsl_anl$data

    validate_checks <- reactive({
      adsl_filtered <- anl_q()[[parentname]]
      anl_filtered <- anl_q()[[dataname]]

      input_arm_var <- as.vector(anl_selectors$arm_var()$variables$selected)
      input_seq_var <- as.vector(anl_selectors$seq_var()$variables$selected)
      input_hlt <- as.vector(anl_selectors$hlt()$variables$selected)
      input_llt <- as.vector(anl_selectors$llt()$variables$selected)

      validate(
        need(length(input_arm_var) >= 1L, "Please select a treatment variable."),
        need(length(input_llt) >= 1L, "Please select a \"LOW LEVEL TERM\" variable")
      )


      validate(
        need(is.factor(adsl_filtered[[input_arm_var]]), "Treatment variable is not a factor.")
      )
      validate(
        need(is.integer(anl_filtered[[input_seq_var]]), "Analysis sequence variable is not an integer.")
      )

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_seq_var, input_hlt, input_llt),
        arm_var = input_arm_var
      )
    })

    # The R-code corresponding to the analysis.
    all_q <- reactive({
      validate_checks()

      anl_q <- anl_q()

      input_hlt <- anl_selectors$hlt()$variables$selected
      input_llt <- anl_selectors$llt()$variables$selected

      hlt_labels <- mapply(function(x) rtables::obj_label(anl_q[["ANL"]][[x]]), input_hlt)
      llt_labels <- mapply(function(x) rtables::obj_label(anl_q[["ANL"]][[x]]), input_llt)

      basic_table_args$title <- ifelse(
        is.null(basic_table_args$title),
        paste(title_text, "by", paste(hlt_labels, collapse = ", "), "and", paste(llt_labels, collapse = ", ")),
        basic_table_args$title
      )

      my_calls <- template_mult_events(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = anl_selectors$arm_var()$variables$selected,
        seq_var = anl_selectors$seq_var()$variables$selected,
        hlt = if (length(input_hlt) != 0) input_hlt else NULL,
        llt = input_llt,
        add_total = input$add_total,
        total_label = total_label,
        na_level = na_level,
        event_type = event_type,
        drop_arm_levels = input$drop_arm_levels,
        basic_table_args = basic_table_args
      )
      obj <- anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    decorated_table_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = all_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    # Outputs to render.
    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(id = "table", table_r = table_r)

    decorated_table_q
  })
}
