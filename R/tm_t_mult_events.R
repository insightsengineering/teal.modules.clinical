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
                                 event_type = "event") {
  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(seq_var),
    !is.null(llt),
    is.null(hlt) || is.character(hlt),
    is.string(llt),
    is.flag(add_total),
    is.string(event_type)
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
    substitute(
      expr = anl <- anl %>%
        mutate(USUBJID2 = paste0(USUBJID, "@", seq_var)),
      env = list(
        seq_var = as.name(seq_var)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  y$layout_prep <- quote(split_fun <- drop_split_levels)

  # Start layout steps.
  layout_list <- list()
  layout_list <- add_expr(layout_list, quote(basic_table()))
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(var = arm_var) %>%
        add_colcounts(),
      env = list(arm_var = arm_var)
    )
  )

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      quote(
        add_overall_col(label = "All Patients")
      )
    )
  }

  unique_label <- paste0("Total number of patients with at least one ", event_type)

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_num_patients(
        var = "USUBJID",
        .stats = "unique",
        .labels = c(
          unique = unique_label
        )
      ),
      env = list(unique_label = unique_label)
    )
  )


  lyt_1 <- substitute(
    expr = lyt_1 <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$layout1 <- lyt_1

  # Re-start layout steps.
  layout_list <- list()

  layout_list <- add_expr(layout_list, quote(basic_table()))
  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(var = arm_var) %>%
        add_colcounts(),
      env = list(arm_var = arm_var)
    )
  )

  if (add_total) {
    layout_list <- add_expr(
      layout_list,
      quote(
        add_overall_col(label = "All Patients")
      )
    )
  }

  unique_count_label <- paste0("Total number of ", event_type, "s")

  layout_list <- add_expr(
    layout_list,
    substitute(
      summarize_num_patients(
        var = "USUBJID2",
        .stats = "unique_count",
        .labels = c(
          unique_count = unique_count_label
        )
      ),
      env = list(unique_count_label = unique_count_label)
    )
  )

  if (is.null(hlt)) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr =
          count_occurrences(vars = llt, .indent_mods = -1L) %>%
          append_varlabels(dataname, llt, indent = FALSE),
        env = list(
          dataname = as.name(dataname), llt = llt
        )
      )
    )
  } else {
    lbl_lst <- list()

    for (ii in 1:length(hlt)) {
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
            split_rows_by(
              hlt,
              child_labels = "visible",
              nested = nested,
              indent_mod = indent_mod,
              split_fun = split_fun
            ),
          env = list(
            hlt = hlt_new,
            nested = nested,
            indent_mod = indent_mod
          )
        )
      )
    }

    top_left_lbl <- substitute(
      expr =
        paste(vapply(lbl_lst, eval, FUN.VALUE = character(1)), collapse = "/"),
      env = list(
        lbl_lst = lbl_lst
      )
    )

    nonunique_label <- unique_count_label

    layout_list <- add_expr(
      layout_list,
      substitute(
        expr =
          summarize_num_patients(
            var = "USUBJID",
            .stats = c("unique", "nonunique"),
            .labels = c(
              unique = unique_label,
              nonunique = nonunique_label
            )
          ) %>%
          count_occurrences(vars = llt, .indent_mods = -1L) %>%
          append_topleft(top_left_lbl) %>%
          append_varlabels(dataname, llt, indent = TRUE),
        env = list(
          dataname = as.name(dataname), llt = llt,
          unique_label = unique_label, nonunique_label = nonunique_label,
          top_left_lbl = top_left_lbl
        )
      )
    )
  }

  lyt_2 <- substitute(
    expr = lyt_2 <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$layout2 <- lyt_2

  y$table1 <- substitute(
    expr = result_1 <- build_table(lyt = lyt_1, df = anl, alt_counts_df = parent),
    env = list(
      parent = as.name(parentname)
    )
  )

  # Table 2.
  y$table2 <- substitute(
    expr = result_2 <- build_table(lyt = lyt_2, df = anl, alt_counts_df = parent),
    env = list(
      parent = as.name(parentname)
    )
  )

  # Start sorting table 2.
  if (is.null(hlt)) {
    pth <- c(llt)
  } else {
    pth <- c(rbind(hlt, rep("*", length(hlt))), llt)
  }

  sort_list <- list()

  sort_list <- add_expr(
    sort_list,
    substitute(
      expr = sorted_result_2 <- result_2 %>%
        sort_at_path(path = pth, scorefun = score_occurrences),
      env = list(pth = pth)
    )
  )

  y$table2_sorted <- bracket_expr(sort_list)

  # Combine tables.
  y$final_table <- quote(
    expr = {
      col_info(result_1) <- col_info(sorted_result_2)
      result <- rbind(result_1, sorted_result_2)
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
#' library(random.cdisc.data)
#'
#' adsl <- radsl(N = 400, study_duration = 2, seed = 1234)
#' adcm <- radcm(ADSL = adsl, seed = 1234, cached = FALSE, who_coding = TRUE)
#' adcm_keys <- keys(
#'   primary = c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4"),
#'   foreign = c("STUDYID", "USUBJID"),
#'   parent = "ADSL"
#' )
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = "ADSL <- radsl(N = 400, study_duration = 2, seed = 1234)"),
#'     cdisc_dataset("ADCM", adcm,
#'       code = "ADCM <- radcm(ADSL = ADSL, seed = 1234, cached = FALSE, who_coding = TRUE)",
#'       keys = adcm_keys
#'     )
#'   ),
#'   modules = root_modules(
#'     tm_t_mult_events(
#'       label = "Concomitant Medications by Medication Class and Preferred Name",
#'       dataname = "ADCM",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       hlt = choices_selected(
#'         choices = variable_choices(adcm, c("ATC1", "ATC2", "ATC3", "ATC4")),
#'         selected = c("ATC1", "ATC2", "ATC3", "ATC4")
#'       ),
#'       llt = choices_selected(
#'         choices = variable_choices(adcm, c("CMDECOD")),
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
                             parentname = ifelse(is(arm_var, "data_extract_spec"), datanames_input(arm_var), "ADSL"),
                             arm_var,
                             seq_var = choices_selected("ASEQ", selected = "ASEQ", fixed = TRUE),
                             hlt,
                             llt,
                             add_total = TRUE,
                             event_type = "event",
                             pre_output = NULL,
                             post_output = NULL) {
  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_logical_single(add_total),
    is_character_single(event_type),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

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
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_mult_events_byterm <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(a$arm_var, a$seq_var, a$hlt, a$llt)

  standard_layout(
    output = white_small_well(
      uiOutput(ns("table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "seq_var", "hlt", "llt")]),
      helpText("Analysis data:", tags$code(a$dataname)),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("hlt"),
        label = "Event High Level Term",
        data_extract_spec = a$hlt,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("llt"),
        label = "Event Low Level Term",
        data_extract_spec = a$llt,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total),
      panel_group(
        panel_item(
          "Additional Variables Info",
          data_extract_input(
            id = ns("seq_var"),
            label = "Analysis Sequence Number",
            data_extract_spec = a$seq_var,
            is_single_dataset = is_single_dataset_value
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
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
                                     label) {
  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, seq_var, hlt, llt),
    input_id = c("arm_var", "seq_var", "hlt", "llt"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var),
    input_id = c("arm_var"),
    anl_name = "ANL_ADSL"
  )

  validate_checks <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_seq_var <- as.vector(anl_m$columns_source$seq_var)

    input_hlt <- as.vector(anl_m$columns_source$hlt)
    input_llt <- as.vector(anl_m$columns_source$llt)

    validate(need(input_arm_var, "Please select an ARM variable"))
    validate(need(input_llt, "Please select a \"LOW LEVEL TERM\" variable"))

    validate(
      need(
        nrow(anl_filtered[input_hlt]) > 0,
        "Not enough observations in \"HIGH LEVEL TERM\" variable(s)."
      )
    )
    validate(
      need(
        nrow(anl_filtered[input_llt]) > 0,
        "Not enough observations in \"LOW LEVEL TERM\" variable."
      )
    )

    validate(
      need(is.factor(adsl_filtered[[input_arm_var]]), "Arm variable is not a factor.")
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
      arm_var = input_arm_var,
      max_n_levels_armvar = NULL,
      min_nrow = 1
    )
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    input_hlt <- as.vector(anl_m$columns_source$hlt)
    input_llt <- as.vector(anl_m$columns_source$llt)

    my_calls <- template_mult_events(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      seq_var = as.vector(anl_m$columns_source$seq_var),
      hlt = if (length(input_hlt) != 0) input_hlt else NULL,
      llt = input_llt,
      add_total = input$add_total,
      event_type = event_type
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  output$table <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  # Render R code.
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = "Event Table",
    code_header = label
  )
}
