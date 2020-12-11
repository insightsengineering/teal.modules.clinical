#' Template: Events by Grade
#'
#' @inheritParams template_arguments
#' @param grade (`character`) \cr name of the severity level variable.
#'
#' @seealso [tm_t_events_by_grade()]
#'
template_events_by_grade <- function(dataname,
                                     parentname,
                                     arm_var,
                                     hlt,
                                     llt,
                                     grade,
                                     add_total = TRUE) {
  assert_that(
    is.string(dataname),
    is.string(parentname),
    is.string(arm_var),
    is.string(hlt) || is.null(hlt),
    is.string(llt) || is.null(llt),
    is.string(grade),
    is.flag(add_total)
  )

  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- dataname,
      env = list(
        dataname = as.name(dataname)
      )
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      expr = grade_groups <- list("- Any Intensity -" = levels(dataname$grade)),
      env = list(
        dataname = as.name(dataname),
        grade = grade
      )
    )
  )
  data_list <- add_expr(
    data_list,
    substitute(
      expr = col_counts <- table(parentname$arm_var),
      env = list(
        parentname = as.name(parentname),
        arm_var = as.name(arm_var)
      )
    )
  )
  if (add_total) {
    data_list <- add_expr(
      data_list,
      quote(
        col_counts <- c(col_counts, "All Patients" = sum(col_counts))
      )
    )
  }
  y$data <- bracket_expr(data_list)

  y$layout_prep <- quote(split_fun <- drop_split_levels)

  layout_list <- list()

  layout_list <- add_expr(
    layout_list,
    quote(
      basic_table()
    )
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = split_cols_by(arm_var),
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

  one_term <- is.null(hlt) || is.null(llt)

  if (one_term) {
    term_var <- ifelse(is.null(hlt), llt, hlt)
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = add_colcounts() %>%
          summarize_occurrences_by_grade(
            var = grade,
            grade_groups = grade_groups
          ) %>%
          split_rows_by(
            term_var,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L,
            split_fun = split_fun
          ) %>%
          summarize_num_patients(
            var = "USUBJID",
            .stats = "unique",
            .labels = c("- Any Intensity -")
          ) %>%
          count_occurrences_by_grade(var = grade, .indent_mods = -1L) %>%
          append_varlabels(dataname, c(term_var, grade)),
        env = list(
          arm_var = arm_var,
          term_var = term_var,
          grade = grade,
          dataname = as.name(dataname)
        )
      )
    )
  } else {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = add_colcounts() %>%
          summarize_occurrences_by_grade(
            var = grade,
            grade_groups = grade_groups
          ) %>%
          split_rows_by(
            hlt,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L,
            split_fun = split_fun
          ) %>%
          append_varlabels(dataname, hlt) %>%
          summarize_occurrences_by_grade(
            var = grade,
            grade_groups = grade_groups
          ) %>%
          split_rows_by(
            llt,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L,
            split_fun = split_fun
          ) %>%
          append_varlabels(dataname, c(llt, grade), indent = TRUE) %>%
          summarize_num_patients(
            var = "USUBJID",
            .stats = "unique",
            .labels = c("- Any Intensity -")
          ) %>%
          count_occurrences_by_grade(var = grade, .indent_mods = -1L),
        env = list(
          arm_var = arm_var,
          hlt = hlt,
          llt = llt,
          grade = grade,
          dataname = as.name(dataname)
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- quote(
    result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
  )

  scorefun <- if (add_total) {
    quote(cont_n_onecol(length(col_counts)))
  } else {
    quote(cont_n_allcols)
  }
  if (one_term) {
    term_var <- ifelse(is.null(hlt), llt, hlt)
    y$pruned_and_sorted_result <- substitute(
      expr = result <- result %>%
        trim_rows() %>%
        sort_at_path(
          path = term_var,
          scorefun = scorefun,
          decreasing = TRUE
        ),
      env = list(
        term_var = term_var,
        scorefun = scorefun
      )
    )
  } else {
    y$pruned_and_sorted_result <- substitute(
      expr = result <- result %>%
        trim_rows() %>%
        sort_at_path(
          path = hlt,
          scorefun = scorefun,
          decreasing = TRUE
        ) %>%
        sort_at_path(
          path = c(hlt, "*", llt),
          scorefun = scorefun,
          decreasing = TRUE
        ),
      env = list(
        hlt = hlt,
        llt = llt,
        scorefun = scorefun
      )
    )
  }
  y
}


#' Teal Module: Events by Grade
#'
#' @inheritParams module_arguments
#' @inheritParams template_events_by_grade
#'
#' @export
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' library(tern)
#'
#' adsl <- radsl(cached = TRUE)
#' adae <- radae(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADAE", adae),
#'     code =
#'       "ADSL <- radsl(cached = TRUE)
#'       ADAE <- radae(cached = TRUE)"
#'   ),
#'   modules = root_modules(
#'     tm_t_events_by_grade(
#'       label = "Adverse Events by Grade Table",
#'       dataname = 'ADAE',
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       llt = choices_selected(
#'         choices = variable_choices(adae, c("AETERM", "AEDECOD")),
#'         selected = c("AEDECOD")
#'       ),
#'       hlt = choices_selected(
#'         choices = variable_choices(adae, c("AEBODSYS", "AESOC")),
#'         selected = "AEBODSYS"
#'       ),
#'       grade = choices_selected(
#'         choices = variable_choices(adae, c("AETOXGR", "AESEV")),
#'         selected = "AESEV"
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_events_by_grade <- function(label,
                                 dataname,
                                 parentname = ifelse(
                                   is(arm_var, "data_extract_spec"),
                                   datanames_input(arm_var),
                                   "ADSL"
                                   ),
                                 arm_var,
                                 hlt,
                                 llt,
                                 grade,
                                 add_total = TRUE,
                                 pre_output = NULL,
                                 post_output = NULL) {

  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is_logical_single(add_total),
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
    hlt = cs_to_des_select(hlt, dataname = dataname),
    llt = cs_to_des_select(llt, dataname = dataname),
    grade = cs_to_des_select(grade, dataname = dataname)
  )

  module(
    label = label,
    server = srv_t_events_by_grade,
    ui = ui_t_events_by_grade,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_t_events_by_grade <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(a$arm_var, a$hlt, a$llt, a$grade)

  standard_layout(
    output = white_small_well(uiOutput(ns("as_html"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("arm_var", "hlt", "llt", "grade")]),
      helpText("Analysis data:", code(a$dataname)),
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
      data_extract_input(
        id = ns("grade"),
        label = "Event Grade",
        data_extract_spec = a$grade,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(
        ns("add_total"),
        "Add All Patients column",
        value = a$add_total)
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_events_by_grade <- function(input,
                                  output,
                                  session,
                                  datasets,
                                  dataname,
                                  parentname,
                                  label,
                                  arm_var,
                                  hlt,
                                  llt,
                                  grade) {

  init_chunks()

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, hlt, llt, grade),
    input_id = c("arm_var", "hlt", "llt", "grade"),
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
    input_level_term <- c(
      as.vector(anl_m$columns_source$hlt),
      as.vector(anl_m$columns_source$llt)
    )
    input_grade <- as.vector(anl_m$columns_source$grade)

    validate(
      need(input_arm_var, "Please select an ARM variable"),
      need(input_grade, "Please select a grade variable")
      )
    teal.devel::validate_has_elements(
      input_level_term,
      "Please select at least one of \"LOW LEVEL TERM\" or \"HIGH LEVEL TERM\" variables."
      )
    validate(
      need(
        nrow(anl_filtered[input_level_term]) > 0,
        "Not enough observations in  \"LOW LEVEL TERM\" or \"HIGH LEVEL TERM\" variables."
        )
      )
    validate(
      need(is.factor(adsl_filtered[[input_arm_var]]), "Arm variable is not a factor.")
    )

    # validate inputs
    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_level_term, input_grade),
      arm_var = input_arm_var,
      max_n_levels_armvar = NULL,
      min_nrow = 1
    )
  })

  call_preparation <- reactive({
    validate_checks()

    chunks_reset()
    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    input_hlt <- anl_m$columns_source$hlt
    input_llt <- anl_m$columns_source$llt

    my_calls <- template_events_by_grade(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      hlt = if (length(input_hlt) != 0) input_hlt else NULL,
      llt = if (length(input_llt) != 0) input_llt else NULL,
      grade = as.vector(anl_m$columns_source$grade),
      add_total = input$add_total
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  output$as_html <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })

  # Render R code.
  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm, hlt, llt, grade)
    ),
    modal_title = "AE by Grade Table",
    code_header = label
  )
}
