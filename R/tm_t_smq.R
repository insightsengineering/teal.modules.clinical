#' Template: Adverse Events Table by Standardized MedDRA Query
#'
#' Creates a valid expression to generate an adverse events table by Standardized MedDRA Query.
#'
#' @inheritParams template_arguments
#' @param smq_varlabel (`character`)\cr label to use for new column `SMQ` created by [tern::h_stack_by_baskets()].
#' @param baskets (`character`)\cr names of the selected standardized/customized queries variables.
#'
#' @inherit template_arguments return
#'
#' @seealso [tm_t_smq()]
#'
#' @keywords internal
template_smq <- function(dataname,
                         parentname,
                         arm_var,
                         llt = "AEDECOD",
                         add_total = TRUE,
                         total_label = default_total_label(),
                         sort_criteria = c("freq_desc", "alpha"),
                         drop_arm_levels = TRUE,
                         na_level = tern::default_na_str(),
                         smq_varlabel = "Standardized MedDRA Query",
                         baskets = c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"),
                         id_var = "USUBJID",
                         basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_string(parentname)
  checkmate::assert_string(dataname)
  checkmate::assert_character(arm_var, min.len = 1, max.len = 2)
  checkmate::assert_string(id_var)
  checkmate::assert_string(llt)
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_string(na_level)
  checkmate::assert_string(smq_varlabel)
  checkmate::assert_character(baskets)

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
      anl <- tern::h_stack_by_baskets(
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
      anl <- tern::df_explicit_na(
        dataname,
        na_level = na_str
      ),
      env = list(
        dataname = as.name("anl"),
        na_str = na_level
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parentname <- tern::df_explicit_na(
        parentname,
        na_level = na_str
      ),
      env = list(
        parentname = as.name(parentname),
        na_str = na_level
      )
    )
  )

  y$data <- bracket_expr(data_list)

  parsed_basic_table_args <- teal.widgets::parse_basic_table_args(
    teal.widgets::resolve_basic_table_args(
      user_table = basic_table_args,
      module_table = teal.widgets::basic_table_args(show_colcounts = TRUE)
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
          expr = rtables::split_cols_by(var = nested_col, split_fun = rtables::drop_split_levels),
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
      expr = tern::summarize_num_patients(
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
    expr = teal.data::col_labels(dataname, fill = FALSE)[["SMQ"]],
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
        split_fun = rtables::trim_levels_in_group(llt, drop_outlevs = FALSE),
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
      expr = tern::summarize_num_patients(
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
      expr = tern::count_occurrences(vars = llt, drop = FALSE),
      env = list(
        llt = llt
      )
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = tern::append_varlabels(dataname, llt, indent = 1L),
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
          rtables::sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
          rtables::sort_at_path(path = c("SMQ", "*", llt), scorefun = tern::score_occurrences, na.pos = "last")
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
      table <- sorted_result %>% rtables::trim_rows(criteria = all_zero)
    }
  )

  y
}

#' teal Module: Adverse Events Table by Standardized MedDRA Query
#'
#' This module produces an adverse events table by Standardized MedDRA Query.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#' @inheritParams template_smq
#' @param arm_var ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected option for variable names that can be used as `arm_var`.
#'   It defines the grouping variable(s) in the results table.
#'   If there are two elements selected for `arm_var`,
#'   second variable will be nested under the first variable.
#' @param baskets ([teal.transform::choices_selected()])\cr object with all
#'   available choices and preselected options for standardized/customized queries.
#' @param scopes ([teal.transform::choices_selected()])\cr object with all
#'   available choices for the scopes of standardized queries.
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
#' tm_t_smq(
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
#' @examplesShinylive
#' library(teal.modules.clinical)
#' interactive <- function() TRUE
#' {{ next_example }}
#'
#' @examples
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#'   ADAE <- tmc_ex_adae
#'
#'   .names_baskets <- grep("^(SMQ|CQ).*NAM$", names(ADAE), value = TRUE)
#'   .names_scopes <- grep("^SMQ.*SC$", names(ADAE), value = TRUE)
#'
#'   .cs_baskets <- choices_selected(
#'     choices = variable_choices(ADAE, subset = .names_baskets),
#'     selected = .names_baskets
#'   )
#'
#'   .cs_scopes <- choices_selected(
#'     choices = variable_choices(ADAE, subset = .names_scopes),
#'     selected = .names_scopes,
#'     fixed = TRUE
#'   )
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_smq(
#'       label = "Adverse Events by SMQ Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(
#'         choices = variable_choices(data[["ADSL"]], subset = c("ARM", "SEX")),
#'         selected = "ARM"
#'       ),
#'       add_total = FALSE,
#'       baskets = data[[".cs_baskets"]],
#'       scopes = data[[".cs_scopes"]],
#'       llt = choices_selected(
#'         choices = variable_choices(data[["ADAE"]], subset = c("AEDECOD")),
#'         selected = "AEDECOD"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
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
                     total_label = default_total_label(),
                     sort_criteria = c("freq_desc", "alpha"),
                     drop_arm_levels = TRUE,
                     na_level = tern::default_na_str(),
                     smq_varlabel = "Standardized MedDRA Query",
                     baskets,
                     scopes,
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.widgets::basic_table_args(),
                     transformators = list(),
                     decorators = list()) {
  message("Initializing tm_t_smq")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(arm_var, "choices_selected")
  checkmate::assert_class(id_var, "choices_selected")
  checkmate::assert_class(llt, "choices_selected")
  checkmate::assert_class(baskets, "choices_selected")
  checkmate::assert_class(scopes, "choices_selected")
  checkmate::assert_flag(add_total)
  checkmate::assert_string(total_label)
  checkmate::assert_flag(drop_arm_levels)
  sort_criteria <- match.arg(sort_criteria)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")
  assert_decorators(decorators, "table")

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
        basic_table_args = basic_table_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @keywords internal
ui_t_smq <- function(id, ...) {
  ns <- NS(id)
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
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
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
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.transform::data_extract_ui(
        id = ns("baskets"),
        label = "Select the SMQXXNAM/CQXXNAM baskets",
        data_extract_spec = a$baskets,
        is_single_dataset = is_single_dataset_value
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(a$decorators, "table")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Additional Variables Info",
          checkboxInput(
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
          selectInput(
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
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @keywords internal
srv_t_smq <- function(id,
                      data,
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
                      basic_table_args,
                      decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.clinical")
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

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join"
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    anl_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Adverse Events Table by Standardized MedDRA Query (SMQ)"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      obj %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr)) %>%
        teal.code::eval_code(as.expression(adsl_inputs()$expr))
    })

    merged <- list(
      anl_input_r = anl_inputs,
      adsl_input_r = adsl_inputs,
      anl_q = anl_q
    )

    validate_checks <- reactive({
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

    # Generate r code for the analysis.
    all_q <- reactive({
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

      obj <- merged$anl_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Table")
      teal.code::eval_code(obj, as.expression(unlist(my_calls)))
    })

    # Decoration of table output.
    decorated_table_q <- srv_decorate_teal_data(
      id = "decorator",
      data = all_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    # Outputs to render.
    table_r <- reactive(decorated_table_q()[["table"]])

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_table_q
  })
}
