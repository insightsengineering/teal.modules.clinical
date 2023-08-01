#' Adverse Events Table by Standardized `MedDRA` Query
#'
#' @inheritParams template_arguments
#' @param sort_criteria (`character`)\cr how to sort the final table. Default option `freq_desc` sorts
#'  by decreasing total number of patients with event. Alternative option `alpha` sorts events
#'  alphabetically.
#' @param smq_varlabel (`character`)\cr label of the new column `SMQ`
#' created by [tern::h_stack_by_baskets()].
#' @param baskets (`character`)\cr
#' variable names of the selected Standardized/Customized queries
#'
#' @seealso [tm_t_smq()]
#' @keywords internal
#'
template_smq <- function(dataname,
                         parentname,
                         arm_var,
                         llt = "AEDECOD",
                         add_total = TRUE,
                         total_label = "All Patients",
                         sort_criteria = c("freq_desc", "alpha"),
                         drop_arm_levels = TRUE,
                         na_level = "<Missing>",
                         smq_varlabel = "Standardized MedDRA Query",
                         baskets = c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"),
                         id_var = "USUBJID",
                         basic_table_args = teal.widgets::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(parentname),
    assertthat::is.string(dataname),
    is.character(arm_var) && length(arm_var) %in% c(1, 2),
    assertthat::is.string(id_var),
    assertthat::is.string(llt),
    assertthat::is.flag(add_total),
    assertthat::is.string(total_label),
    assertthat::is.flag(drop_arm_levels),
    assertthat::is.string(na_level),
    assertthat::is.string(smq_varlabel),
    is.character(baskets)
  )

  sort_criteria <- match.arg(sort_criteria)

  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- dataname,
      env = list(
        dataname = as.name(dataname)
      )
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
      anl <- h_stack_by_baskets(
        df = dataname,
        baskets = baskets,
        smq_varlabel = smq_varlabel,
        keys = unique(c("STUDYID", id_var, arm_var, llt))
      ),
      env = list(
        dataname = as.name("anl"),
        baskets = baskets,
        smq_varlabel = smq_varlabel,
        id_var = id_var,
        arm_var = arm_var,
        llt = llt
      )
    )
  )

  data_list <- add_expr(
    data_list,
    quote(
      if (nrow(anl) == 0) {
        stop("Analysis dataset contains only missing values")
      }
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- df_explicit_na(
        dataname,
        na_level = na_level
      ),
      env = list(
        dataname = as.name("anl"),
        na_level = na_level
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- df_explicit_na(
        parentname,
        na_level = na_level
      ),
      env = list(
        parentname = as.name(parentname),
        na_level = na_level
      )
    )
  )

  y$data <- bracket_expr(data_list)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args
    )
  )

  # Start layout steps.
  layout_list <- list()
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
          expr = rtables::split_cols_by(var = nested_col, split_fun = drop_split_levels),
          env = list(nested_col = arm_var[[2]])
        )
      } else {
        substitute(
          expr = rtables::split_cols_by(var = nested_col),
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
        expr = rtables::add_overall_col(total_label),
        env = list(total_label = total_label)
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = summarize_num_patients(
        var = id_var,
        .stats = c("unique"),
        .labels = c(
          unique = "Total number of patients with at least one adverse event"
        )
      ),
      env = list(
        id_var = id_var
      )
    )
  )

  split_label <- substitute(
    expr = formatters::var_labels(dataname, fill = FALSE)[["SMQ"]],
    env = list(
      dataname = as.name("anl")
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_rows_by(
        "SMQ",
        child_labels = "visible",
        nested = FALSE,
        split_fun = trim_levels_in_group(llt, drop_outlevs = FALSE),
        indent_mod = -1L,
        label_pos = "topleft",
        split_label = split_label
      ),
      env = list(
        llt = llt,
        split_label = split_label
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = summarize_num_patients(
        var = id_var,
        .stats = c("unique", "nonunique"),
        .labels = c(
          unique = "Total number of patients with at least one adverse event",
          nonunique = "Total number of events"
        )
      ),
      env = list(
        id_var = id_var
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = count_occurrences(vars = llt, drop = FALSE),
      env = list(
        llt = llt
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = append_varlabels(dataname, llt, indent = 1L),
      env = list(
        dataname = as.name("anl"),
        llt = llt
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = {
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = parent)
    },
    env = list(parent = as.name(parentname))
  )

  if (sort_criteria == "freq_desc") {
    y$sort <- substitute(
      expr = {
        sorted_result <- result %>%
          sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
          sort_at_path(path = c("SMQ", "*", llt), scorefun = score_occurrences, na.pos = "last")
      },
      env = list(llt = llt)
    )
  } else {
    y$sort <- quote(
      sorted_result <- result
    )
  }

  y$sort_and_prune <- quote(
    expr = {
      all_zero <- function(tr) {
        !inherits(tr, "ContentRow") && rtables::all_zero_or_na(tr)
      }
      pruned_and_sorted_result <- sorted_result %>% rtables::trim_rows(criteria = all_zero)
      pruned_and_sorted_result
    }
  )

  y
}

#' Teal Module: `SMQ` Table
#'
#' @description Adverse Events Table by Standardized `MedDRA` Query.
#' @inheritParams module_arguments
#' @inheritParams template_smq
#' @param baskets ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' object with all available choices and preselected options for Standardized/Customized queries
#' @param scopes ([teal.transform::choices_selected()] or [teal.transform::data_extract_spec()])\cr
#' object with all available choices for the scopes of Standardized queries.
#'
#' @export
#'
#' @examples
#' adsl <- tmc_ex_adsl
#' adae <- tmc_ex_adae
#'
#' names_baskets <- grep("^(SMQ|CQ).*NAM$", names(adae), value = TRUE)
#' names_scopes <- grep("^SMQ.*SC$", names(adae), value = TRUE)
#'
#' cs_baskets <- choices_selected(
#'   choices = variable_choices(adae, subset = names_baskets),
#'   selected = names_baskets
#' )
#'
#' cs_scopes <- choices_selected(
#'   choices = variable_choices(adae, subset = names_scopes),
#'   selected = names_scopes,
#'   fixed = TRUE
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAE", adae)
#'   ),
#'   modules = modules(
#'     tm_t_smq(
#'       label = "Adverse Events by SMQ Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("ARM", "SEX")),
#'         selected = "ARM"
#'       ),
#'       add_total = FALSE,
#'       baskets = cs_baskets,
#'       scopes = cs_scopes,
#'       llt = choices_selected(
#'         choices = variable_choices(adae, subset = c("AEDECOD")),
#'         selected = "AEDECOD"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_t_smq <- function(label,
                     dataname,
                     parentname = ifelse(
                       inherits(arm_var, "data_extract_spec"),
                       teal.transform::datanames_input(arm_var),
                       "ADSL"
                     ),
                     arm_var,
                     id_var = teal.transform::choices_selected(
                       teal.transform::variable_choices(dataname, subset = "USUBJID"),
                       selected = "USUBJID", fixed = TRUE
                     ),
                     llt,
                     add_total = TRUE,
                     total_label = "All Patients",
                     sort_criteria = c("freq_desc", "alpha"),
                     drop_arm_levels = TRUE,
                     na_level = "<Missing>",
                     smq_varlabel = "Standardized MedDRA Query",
                     baskets,
                     scopes,
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_t_smq")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(llt, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_arm_levels)
  sort_criteria <- match.arg(sort_criteria)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname, multiple = TRUE, ordered = TRUE),
    id_var = cs_to_des_select(id_var, dataname = dataname),
    baskets = cs_to_des_select(baskets, dataname = dataname, multiple = TRUE),
    scopes = cs_to_des_select(scopes, dataname = dataname, multiple = TRUE),
    llt = cs_to_des_select(llt, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_t_smq,
    server = srv_t_smq,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        na_level = na_level,
        label = label,
        total_label = total_label,
        basic_table_args = basic_table_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_smq <- function(id, ...) {
  ns <- shiny::NS(id)
  a <- list(...) # module args


  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$id_var,
    a$baskets,
    a$scopes,
    a$llt
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(teal.widgets::table_with_settings_ui(ns("table"))),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c(
        "arm_var", "baskets", "llt", "id_var", "scopes"
      )]),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("llt"),
        label = "Select the low level term",
        data_extract_spec = a$llt,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.transform::data_extract_ui(
        id = ns("baskets"),
        label = "Select the SMQXXNAM/CQXXNAM baskets",
        data_extract_spec = a$baskets,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional Variables Info",
          shiny::checkboxInput(
            ns(
              "drop_arm_levels"
            ),
            "Drop arm levels not in filtered analysis dataset",
            value = a$drop_arm_levels
          ),
          teal.transform::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.transform::data_extract_ui(
            id = ns("scopes"),
            label = "Scope variables available",
            data_extract_spec = a$scopes,
            is_single_dataset = is_single_dataset_value
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
          )
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

srv_t_smq <- function(id,
                      data,
                      reporter,
                      filter_panel_api,
                      dataname,
                      parentname,
                      arm_var,
                      llt,
                      id_var,
                      baskets,
                      scopes,
                      na_level,
                      label,
                      total_label,
                      basic_table_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")
  shiny::moduleServer(id, function(input, output, session) {
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        scopes = scopes,
        llt = llt,
        arm_var = arm_var,
        id_var = id_var,
        baskets = baskets
      ),
      datasets = data,
      select_validation_rule = list(
        scopes = shinyvalidate::sv_required("A scope variable is required"),
        llt = shinyvalidate::sv_required("A low level term variable is required"),
        arm_var = shinyvalidate::compose_rules(
          shinyvalidate::sv_required("At least one treatment variable is required"),
          ~ if (length(.) > 2) "Please select no more than two treatment variables"
        ),
        id_var = shinyvalidate::sv_required("An id variable is required"),
        baskets = shinyvalidate::sv_required("At least one basket is required")
      )
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      join_keys = get_join_keys(data),
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      join_keys = get_join_keys(data),
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
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

      input_arm_var <- names(merged$anl_input_r()$columns_source$arm_var)
      input_id_var <- names(merged$anl_input_r()$columns_source$id_var)
      input_baskets <- names(merged$anl_input_r()$columns_source$baskets)
      input_scopes <- names(merged$anl_input_r()$columns_source$scopes)
      input_llt <- names(merged$anl_input_r()$columns_source$llt)

      # validate inputs
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var),
        anl = anl_filtered,
        anlvars = c(
          "USUBJID", "STUDYID", input_id_var, input_baskets,
          input_scopes, input_llt
        ),
        arm_var = input_arm_var[[1]]
      )
    })

    all_q <- shiny::reactive({
      validate_checks()

      my_calls <- template_smq(
        parentname = "ANL_ADSL",
        dataname = "ANL",
        arm_var = names(merged$anl_input_r()$columns_source$arm_var),
        llt = names(merged$anl_input_r()$columns_source$llt),
        add_total = input$add_total,
        total_label = total_label,
        sort_criteria = input$sort_criteria,
        drop_arm_levels = input$drop_arm_levels,
        baskets = names(merged$anl_input_r()$columns_source$baskets),
        na_level = na_level,
        id_var = names(merged$anl_input_r()$columns_source$id_var),
        basic_table_args = basic_table_args
      )

      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
    })

    # Outputs to render.
    table_r <- shiny::reactive(all_q()[["pruned_and_sorted_result"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(all_q())))
    )

    # Render R code.
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("SMQ Table")
        card$append_text("Adverse Events Table by Standardized `MedDRA` Query (SMQ)", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Table", "header3")
        card$append_table(table_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(all_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
