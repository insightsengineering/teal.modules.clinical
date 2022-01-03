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
#'
template_smq <- function(dataname,
                         parentname,
                         arm_var,
                         llt = "AEDECOD",
                         add_total = TRUE,
                         sort_criteria = c("freq_desc", "alpha"),
                         drop_arm_levels = TRUE,
                         na_level = "<Missing>",
                         smq_varlabel = "Standardized MedDRA Query",
                         baskets = c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"),
                         id_var = "USUBJID",
                         basic_table_args = teal.devel::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(parentname),
    assertthat::is.string(dataname),
    is.character(arm_var) && length(arm_var) %in% c(1, 2),
    assertthat::is.string(id_var),
    assertthat::is.string(llt),
    assertthat::is.flag(add_total),
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

  parsed_basic_table_args <- teal.devel::parse_basic_table_args(
    teal.devel::resolve_basic_table_args(
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
      quote(
        rtables::add_overall_col(label = "All Patients")
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
    expr = rtables::var_labels(dataname)[["SMQ"]],
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
#' @inheritParams module_arguments
#' @inheritParams template_smq
#' @param baskets ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#' object with all available choices and preselected options for Standardized/Customized queries
#' @param scopes ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#' object with all available choices for the scopes of Standardized queries.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adae <- synthetic_cdisc_data("latest")$adae
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
#'     cdisc_dataset("ADSL", adsl,
#'       code = "ADSL <- synthetic_cdisc_data('latest')$adsl"
#'     ),
#'     cdisc_dataset("ADAE", adae,
#'       code = "ADAE <- synthetic_cdisc_data('latest')$adae"
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_smq(
#'       label = "Adverse events by `SMQ` Table",
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_smq <- function(label,
                     dataname,
                     parentname = ifelse(
                       inherits(arm_var, "data_extract_spec"),
                       teal.devel::datanames_input(arm_var),
                       "ADSL"
                     ),
                     arm_var,
                     id_var = choices_selected(
                       variable_choices(dataname, subset = "USUBJID"),
                       selected = "USUBJID", fixed = TRUE
                     ),
                     llt,
                     add_total = TRUE,
                     sort_criteria = c("freq_desc", "alpha"),
                     drop_arm_levels = TRUE,
                     na_level = "<Missing>",
                     smq_varlabel = "Standardized MedDRA Query",
                     baskets,
                     scopes,
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_smq")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  utils.nest::stop_if_not(
    is.choices_selected(arm_var),
    assertthat::is.flag(add_total),
    assertthat::is.flag(drop_arm_levels),
    is.choices_selected(id_var),
    is.choices_selected(llt)
  )
  sort_criteria <- match.arg(sort_criteria)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_outpput, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname, multiple = TRUE),
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
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_smq <- function(id, ...) {
  ns <- NS(id)
  a <- list(...) # module args


  is_single_dataset_value <- teal.devel::is_single_dataset(
    a$arm_var,
    a$id_var,
    a$baskets,
    a$scopes,
    a$llt
  )

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(teal.devel::table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(a[c(
        "arm_var", "baskets", "llt", "id_var", "scopes"
      )]),
      teal.devel::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("llt"),
        label = "Select the low level term",
        data_extract_spec = a$llt,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total),
      teal.devel::data_extract_ui(
        id = ns("baskets"),
        label = "Select the SMQXXNAM/CQXXNAM baskets",
        data_extract_spec = a$baskets,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional Variables Info",
          checkboxInput(ns(
            "drop_arm_levels"
          ),
          "Drop arm levels not in filtered analysis dataset",
          value = a$drop_arm_levels
          ),
          teal.devel::data_extract_ui(
            id = ns("id_var"),
            label = "Subject Identifier",
            data_extract_spec = a$id_var,
            is_single_dataset = is_single_dataset_value
          ),
          teal.devel::data_extract_ui(
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
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_t_smq <- function(input,
                      output,
                      session,
                      datasets,
                      dataname,
                      parentname,
                      arm_var,
                      llt,
                      id_var,
                      baskets,
                      scopes,
                      na_level,
                      label,
                      basic_table_args) {
  stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

  anl_selectors <- teal.devel::data_extract_multiple_srv(
    list(
      arm_var = arm_var,
      id_var = id_var,
      baskets = baskets,
      scopes = scopes,
      llt = llt
    ),
    datasets = datasets
  )

  anl_merged <- teal.devel::data_merge_srv(
    selector_list = anl_selectors,
    datasets = datasets,
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
    anl_m <- anl_merged()
    anl_adsl <- adsl_merged()

    input_arm_var <- anl_selectors()$arm_var()$select_ordered
    input_id_var <- as.vector(anl_m$columns_source$id_var)
    input_baskets <- as.vector(anl_m$columns_source$baskets)
    input_scopes <- as.vector(anl_m$columns_source$scopes)
    input_llt <- as.vector(anl_m$columns_source$llt)

    validate(
      need(input_id_var, "Please select a subject identifier."),
      need(length(input_arm_var) <= 2, "Please limit arm variables within two"),
      need(input_baskets, "Please select the SMQ/CQ baskets."),
      need(input_scopes, "Please select the scope variables."),
      need(input_llt, "Please select the low level term."),
      need(input_arm_var, "Please select the arm variable.")
    )
    # validate inputs
    teal.devel::validate_standard_inputs(
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

  call_preparation <- reactive({
    validate_checks()

    teal.devel::chunks_reset()
    anl_m <- anl_merged()
    teal.devel::chunks_push_data_merge(anl_m)
    teal.devel::chunks_push_new_line()
    anl_adsl <- adsl_merged()
    teal.devel::chunks_push_data_merge(anl_adsl)
    teal.devel::chunks_push_new_line()

    my_calls <- template_smq(
      parentname = "ANL_ADSL",
      dataname = "ANL",
      arm_var = anl_selectors()$arm_var()$select_ordered,
      llt = as.vector(anl_m$columns_source$llt),
      add_total = input$add_total,
      sort_criteria = input$sort_criteria,
      drop_arm_levels = input$drop_arm_levels,
      baskets = as.vector(anl_m$columns_source$baskets),
      na_level = na_level,
      id_var = as.vector(anl_m$columns_source$id_var),
      basic_table_args = basic_table_args
    )
    mapply(expression = my_calls, teal.devel::chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    teal.devel::chunks_safe_eval()
    teal.devel::chunks_get_var("pruned_and_sorted_result")
  })

  callModule(
    teal.devel::table_with_settings_srv,
    id = "table",
    table_r = table
  )

  # Render R code.
  callModule(
    module = teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(
      list(arm_var, id_var, baskets, scopes, llt)
    ),
    modal_title = "R Code for SMQ tables",
    code_header = label
  )
}
