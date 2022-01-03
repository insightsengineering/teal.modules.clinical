#' Template: Events by Term
#'
#' @inheritParams template_arguments
#' @param event_type (`character`)\cr type of event that is summarized (e.g. adverse event, treatment).
#'   Default is "event".
#' @param seq_var (`numeric`) \cr Analysis Sequence Number. Used for counting the unique number of events.
#'
#' @seealso [tm_t_mult_events()]
#'
template_mult_events <- function(dataname,
                                 parentname,
                                 arm_var,
                                 seq_var,
                                 hlt,
                                 llt,
                                 add_total = TRUE,
                                 event_type = "event",
                                 drop_arm_levels = TRUE,
                                 basic_table_args = teal.devel::basic_table_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(seq_var),
    !is.null(llt),
    is.null(hlt) || is.character(hlt),
    assertthat::is.string(llt),
    assertthat::is.flag(add_total),
    assertthat::is.string(event_type),
    assertthat::is.flag(drop_arm_levels)
  )

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
        df_explicit_na(omit_columns = setdiff(names(anl), term_vars)),
      env = list(
        term_vars = term_vars
      )
    )
  )

  data_list <- add_expr(
    data_list,
    utils.nest::substitute_names(
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
      parentname <- df_explicit_na(parentname, na_level = ""),
      env = list(parentname = as.name(parentname))
    )
  )

  y$data <- bracket_expr(data_list)

  y$layout_prep <- quote(split_fun <- drop_split_levels)

  parsed_basic_table_args <- teal.devel::parse_basic_table_args(
    teal.devel::resolve_basic_table_args(
      user_table = basic_table_args
    )
  )

  # Start layout steps.
  layout_list <- list()

  layout_list <- add_expr(layout_list, parsed_basic_table_args)
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = rtables::split_cols_by(var = arm_var) %>%
        rtables::add_colcounts(),
      env = list(arm_var = arm_var)
    )
  )

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      quote(
        rtables::add_overall_col(label = "All Patients")
      )
    )
  }

  unique_label <- paste0("Total number of patients with at least one ", event_type)
  nonunique_label <- paste0("Total number of ", event_type, "s")

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_num_patients(
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
        expr =
          count_occurrences(vars = llt, .indent_mods = -1L) %>%
            append_varlabels(dataname, llt, indent = 0L),
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
        substitute( # nolint
          expr =
            attr(dataname$hlt_new, which = "label"),
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
              split_label = rtables::var_labels(dataname[hlt_new], fill = TRUE)
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
        expr =
          summarize_num_patients(
            var = "USUBJID",
            count_by = seq_var,
            .stats = c("unique", "nonunique"),
            .labels = c(
              unique = unique_label,
              nonunique = nonunique_label
            )
          ) %>%
            count_occurrences(vars = llt, .indent_mods = -1L) %>%
            append_varlabels(dataname, llt, indent = indent_space),
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
        sort_at_path(path = pth, scorefun = score_occurrences),
      env = list(pth = pth)
    )
  )

  y$table_sorted <- bracket_expr(sort_list)

  # Combine tables.
  y$final_table <- quote(
    expr = {
      result <- sorted_result
      result
    }
  )

  y
}

#' Teal Module: Multiple Events by Term
#'
#' @inheritParams module_arguments
#' @inheritParams template_mult_events
#'
#' @param seq_var ([teal::choices_selected()] or [teal::data_extract_spec()])\cr
#' Analysis Sequence Number. Used for counting the unique number of events.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADCM <- synthetic_cdisc_data("latest")$adcm
#' adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- synthetic_cdisc_data("latest")$adsl'),
#'     cdisc_dataset("ADCM", ADCM,
#'       code = 'ADCM <- synthetic_cdisc_data("latest")$adcm',
#'       keys = adcm_keys
#'     )
#'   ),
#'   modules = root_modules(
#'     tm_t_mult_events(
#'       label = "Concomitant Medications by Medication Class and Preferred Name",
#'       dataname = "ADCM",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       seq_var = choices_selected("ASEQ", selected = "ASEQ", fixed = TRUE),
#'       hlt = choices_selected(
#'         choices = variable_choices(ADCM, c("ATC1", "ATC2", "ATC3", "ATC4")),
#'         selected = c("ATC1", "ATC2", "ATC3", "ATC4")
#'       ),
#'       llt = choices_selected(
#'         choices = variable_choices(ADCM, c("CMDECOD")),
#'         selected = c("CMDECOD")
#'       ),
#'       add_total = TRUE,
#'       event_type = "treatment"
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_mult_events <- function(label, # nolint
                             dataname,
                             parentname = ifelse(
                               inherits(arm_var, "data_extract_spec"),
                               teal.devel::datanames_input(arm_var),
                               "ADSL"
                             ),
                             arm_var,
                             seq_var,
                             hlt,
                             llt,
                             add_total = TRUE,
                             event_type = "event",
                             drop_arm_levels = TRUE,
                             pre_output = NULL,
                             post_output = NULL,
                             basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_mult_events")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_string(event_type)
  checkmate::assert_flag(add_total)
  checkmate::assert_flag(drop_arm_levels)
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    seq_var = cs_to_des_select(seq_var, dataname = dataname),
    hlt = cs_to_des_select(hlt, dataname = dataname, multiple = TRUE),
    llt = cs_to_des_select(llt, dataname = dataname)
  )

  module(
    label = label,
    ui = ui_t_mult_events_byterm,
    server = srv_t_mult_events_byterm,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        event_type = event_type,
        label = label,
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_mult_events_byterm <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- teal.devel::is_single_dataset(a$arm_var, a$seq_var, a$hlt, a$llt)

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(
      teal.devel::table_with_settings_ui(ns("table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(a[c("arm_var", "seq_var", "hlt", "llt")]),
      teal.devel::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("hlt"),
        label = "Event High Level Term",
        data_extract_spec = a$hlt,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("llt"),
        label = "Event Low Level Term",
        data_extract_spec = a$llt,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional table settings",
          checkboxInput(
            ns("drop_arm_levels"),
            label = "Drop columns not in filtered analysis dataset",
            value = a$drop_arm_levels
          )
        )
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          "Additional Variables Info",
          teal.devel::data_extract_ui(
            id = ns("seq_var"),
            label = "Analysis Sequence Number",
            data_extract_spec = a$seq_var,
            is_single_dataset = is_single_dataset_value
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
srv_t_mult_events_byterm <- function(input,
                                     output,
                                     session,
                                     datasets,
                                     dataname,
                                     parentname,
                                     event_type,
                                     arm_var,
                                     seq_var,
                                     hlt,
                                     llt,
                                     drop_arm_levels,
                                     label,
                                     basic_table_args) {
  stopifnot(is_cdisc_data(datasets))

  teal.devel::init_chunks()

  anl_selectors <- teal.devel::data_extract_multiple_srv(
    list(
      arm_var = arm_var,
      seq_var = seq_var,
      hlt = hlt,
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
    data_extract = list(arm_var = arm_var),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_seq_var <- as.vector(anl_m$columns_source$seq_var)

    input_hlt <- anl_selectors()$hlt()$select_ordered
    input_llt <- as.vector(anl_m$columns_source$llt)

    validate(need(input_arm_var, "Please select a treatment variable"))
    validate(need(input_llt, "Please select a \"LOW LEVEL TERM\" variable"))

    validate(
      need(is.factor(adsl_filtered[[input_arm_var]]), "Treatment variable is not a factor.")
    )
    validate(
      need(is.integer(anl_filtered[[input_seq_var]]), "Analysis sequence variable is not an integer.")
    )

    # validate inputs
    teal.devel::validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_seq_var, input_hlt, input_llt),
      arm_var = input_arm_var
    )
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    validate_checks()

    teal.devel::chunks_reset()
    anl_m <- anl_merged()
    teal.devel::chunks_push_data_merge(anl_m)
    teal.devel::chunks_push_new_line()

    anl_adsl <- adsl_merged()
    teal.devel::chunks_push_data_merge(anl_adsl)
    teal.devel::chunks_push_new_line()

    input_hlt <- anl_selectors()$hlt()$select_ordered
    input_llt <- as.vector(anl_m$columns_source$llt)

    my_calls <- template_mult_events(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      seq_var = as.vector(anl_m$columns_source$seq_var),
      hlt = if (length(input_hlt) != 0) input_hlt else NULL,
      llt = input_llt,
      add_total = input$add_total,
      event_type = event_type,
      drop_arm_levels = input$drop_arm_levels,
      basic_table_args = basic_table_args
    )
    mapply(expression = my_calls, teal.devel::chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    teal.devel::chunks_safe_eval()
    teal.devel::chunks_get_var("result")
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
    datanames = teal.devel::get_extract_datanames(list(arm_var, seq_var, hlt, llt)),
    modal_title = "Event Table",
    code_header = label
  )
}
