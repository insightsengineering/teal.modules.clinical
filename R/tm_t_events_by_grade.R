#' Teal Module: Events by Grade
#'
#' @name events_by_grade
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#'
NULL

#' @describeIn events_by_grade creates the expression corresponding to the
#'   analysis.
#' @param grade (`character`) \cr name of the severity level variable.
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

  if (one_term){
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
            indent_mod = -1L
          ) %>%
          summarize_num_patients(
            var = "USUBJID",
            .stats = "unique",
            .labels = c("- Any Intensity -")
          ) %>%
          count_occurrences_by_grade(var = grade, .indent_mods = -1L),
        env = list(
          arm_var = arm_var,
          term_var = term_var,
          grade = grade
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
            indent_mod = -1L
          ) %>%
          summarize_occurrences_by_grade(
            var = grade,
            grade_groups = grade_groups
          ) %>%
          split_rows_by(
            llt,
            child_labels = "visible",
            nested = TRUE,
            indent_mod = -1L
          ) %>%
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
          grade = grade
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
  if (one_term){
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


#' @describeIn events_by_grade teal module for event by grade table.
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
                                 arm_var,
                                 hlt,
                                 llt,
                                 grade,
                                 add_total = TRUE) {

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_events_by_grade,
    ui = ui_t_events_by_grade,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )
}

#' @noRd
ui_t_events_by_grade <- function(id, ...) {

  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("as_html"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(args$dataname)),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        args$arm_var$choices,
        args$arm_var$selected,
        multiple = FALSE,
        fixed = args$arm_var$fixed),
      optionalSelectInput(
        ns("hlt"),
        "Event High Level Term",
        args$hlt$choices,
        args$hlt$selected,
        multiple = FALSE,
        fixed = args$hlt$fixed),
      optionalSelectInput(
        ns("llt"),
        "Event Low Level Term",
        args$llt$choices,
        args$llt$selected,
        multiple = FALSE,
        fixed = args$llt$fixed),
      optionalSelectInput(
        ns("grade"),
        "Event Grade",
        args$grade$choices,
        args$grade$selected,
        multiple = FALSE,
        fixed = args$grade$fixed),
      checkboxInput(
        ns("add_total"),
        "Add All Patients column",
        value = args$add_total)
    ),
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    )
  )
}

#' @noRd
srv_t_events_by_grade <- function(input,
                                  output,
                                  session,
                                  datasets,
                                  dataname,
                                  label) {

  init_chunks()

  # Prepare the analysis environment (filter data, check data, populate envir).
  prepared_env <- reactive({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    arm_var <- input$arm_var
    hlt <- input$hlt
    llt <- input$llt

    teal.devel::validate_has_elements(
      c(llt, hlt),
      "Please select at least one of \"LOW LEVEL TERM\"
      or \"HIGH LEVEL TERM\" variables."
    )
    validate(
      need(is.factor(adsl_filtered[[arm_var]]), "Arm variable is not a factor.")
    )

    # Send data where the analysis lives.
    e <- new.env()
    e[[paste0(dataname, "_FILTERED")]] <- anl_filtered
    e$ADSL_FILTERED <- adsl_filtered #nolint
    e
  })

  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_events_by_grade(
      dataname = paste0(dataname, "_FILTERED"),
      parentname = "ADSL_FILTERED",
      arm_var = input$arm_var,
      hlt = input$hlt,
      llt = input$llt,
      grade = input$grade,
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
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "AE by Grade Table",
      rcode = get_rcode(datasets = datasets, title = input$label)
    )
  })
}
