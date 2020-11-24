#' Teal Module: Abnormality Summary Table
#'
#' @name abnormality
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#'
NULL

#' @describeIn abnormality create the expressions corresponding to the analysis.
#' @param by_vars (`character`)\cr variable names of the row by variables.
#' @param treatment_flag_var (`string`)\cr variable name of the treatment flag variable.
#' @param treatment_flag (`string`)\cr value indicating on treatment.
#' @param exclude_base_abn (`flag`)\cr whether to exclude patients who had abnormal values at baseline.
#'
template_abnormality <- function(parentname,
                                 dataname,
                                 arm_var,
                                 by_vars,
                                 abnormal,
                                 grade = "ANRIND",
                                 treatment_flag_var = "ONTRTFL",
                                 treatment_flag = "Y",
                                 add_total = FALSE,
                                 exclude_base_abn = FALSE) {
  y <- list()

  data_list <- list()

  data_list <- add_expr(
    data_list,
    substitute(
      expr = anl <- df %>%
        filter(treatment_flag_var == treatment_flag & !is.na(grade)),
      env = list(
        df = as.name(dataname),
        grade = as.name(grade),
        treatment_flag_var = as.name(treatment_flag_var),
        treatment_flag = treatment_flag
      )
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      expr = n_col_counts <- table(df$arm_var),
      env = list(
        df = as.name(parentname),
        arm_var = arm_var
      )
    )
  )

  if (add_total) {
    data_list <- add_expr(
      data_list,
      quote(n_col_counts <- c(n_col_counts, Total = sum(n_col_counts)))
    )
  }

  y$data <- bracket_expr(data_list)

  layout_list <- list()

  layout_list <- add_expr(
    layout_list,
    if (add_total) {
      substitute(
        expr = basic_table() %>%
          split_cols_by(
            var = arm_var,
            split_fun = add_overall_level("All Patients", first = FALSE)
          ) %>%
          add_colcounts(),
        env = list(arm_var = arm_var)
      )
    } else {
      substitute(
        expr = basic_table() %>%
          split_cols_by(var = arm_var) %>%
          add_colcounts(),
        env = list(arm_var = arm_var)
      )
    }
  )

  for (by_var in by_vars) {
    split_label <- substitute(
      expr = var_labels(dataname)[[by_var]],
      env = list(
        dataname = as.name(dataname),
        by_var = by_var
      )
    )
    layout_list <- add_expr(
      layout_list,
      substitute(
        split_rows_by(
          by_var,
          split_label = split_label,
          visible_label = TRUE
        ),
        env = list(
          by_var = by_var,
          split_label = split_label
        )
      )
    )
  }

  layout_list <- add_expr(
    layout_list,
    substitute(
      expr = count_abnormal(grade, abnormal = abnormal, exclude_base_abn = exclude_base_abn),
      env = list(
        grade = grade,
        abnormal = setNames(abnormal, tolower(abnormal)),
        exclude_base_abn = exclude_base_abn
      )
    )
  )

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- quote(
    result <- build_table(lyt = lyt, df = anl, col_counts = n_col_counts) %>%
      prune_table()
  )

  y
}


#' @describeIn abnormality teal module for abnormality table.
#' @param grade (`choices_selected`)\cr object with all available
#'   choices and preselected option for variable names that can be used to
#'   specify the abnormality grade. Variable must be factor.
#' @param abnormal (`choices_selected`)\cr indicating abnormality grade.
#'
#' @note Patients with the same abnormality at baseline as on the treatment visit are automatically
#'   excluded in accordance with GDSR specifications.
#'
#' @export
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' adsl <- radsl(cached = TRUE)
#' adlb <- radlb(cached = TRUE) %>%
#'   var_relabel(
#'     PARAM = "Parameters",
#'     BNRIND = "Baseline Reference Range Indicator",
#'     ANRIND = "Analysis Reference Range Indicator",
#'     AVISIT = "Analysis Visit"
#'   )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADLB", adlb,
#'       code = 'ADLB <- radlb(cached = TRUE) %>%
#'               var_relabel(
#'                 PARAM = "Parameters",
#'                 BNRIND = "Baseline Reference Range Indicator",
#'                 ANRIND = "Analysis Reference Range Indicator",
#'                 AVISIT = "Analysis Visit"
#'               )'
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_abnormality(
#'       label = "Abnormality Table",
#'       dataname = "ADLB",
#'       arm_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       id_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("USUBJID", "SUBJID")),
#'         selected = "USUBJID",
#'         fixed = TRUE
#'       ),
#'       by_vars = choices_selected(
#'         choices = variable_choices(adlb, subset = c("LBCAT", "PARAM", "AVISIT")),
#'         selected = c("PARAM", "AVISIT"),
#'         keep_order = TRUE
#'       ),
#'       grade = choices_selected(
#'         choices = variable_choices(adlb, subset = "ANRIND"),
#'         selected = "ANRIND",
#'         fixed = TRUE
#'       ),
#'       abnormal = c("LOW", "HIGH"),
#'       treatment_flag_var = choices_selected(
#'         choices = variable_choices(adlb, subset = "ONTRTFL"),
#'         selected = "ONTRTFL",
#'         fixed = TRUE
#'       ),
#'       treatment_flag = choices_selected(
#'         choices = "Y",
#'         selected = "Y",
#'         fixed = TRUE
#'       ),
#'       exclude_base_abn = FALSE
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_abnormality <- function(label,
                             dataname,
                             arm_var,
                             id_var,
                             by_vars,
                             grade,
                             abnormal,
                             treatment_flag_var,
                             treatment_flag,
                             exclude_base_abn,
                             pre_output = NULL,
                             post_output = NULL) {
  stopifnot(is.string(dataname))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(id_var))
  stopifnot(is.choices_selected(by_vars))
  stopifnot(is.choices_selected(grade))
  stopifnot(is_character_vector(abnormal))
  stopifnot(is.choices_selected(treatment_flag_var))
  stopifnot(is.choices_selected(treatment_flag))
  stopifnot(is_logical_single(exclude_base_abn))

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_t_abnormality,
    server = srv_t_abnormality,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      abnormal = abnormal
    ),
    filters = dataname
  )
}

#' @noRd
ui_t_abnormality <- function(id, ...) {
  ns <- NS(id)
  a <- list(...) # module args

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        a$arm_var$choices,
        a$arm_var$selected,
        multiple = FALSE,
        fixed = a$arm_var$fixed
      ),
      checkboxInput(ns("add_total"), "Add All Patients column", value = TRUE),
      optionalSelectInput(
        ns("id_var"),
        "Subject Identifier",
        a$id_var$choices,
        a$id_var$selected,
        multiple = FALSE,
        fixed = a$id_var$fixed
      ),
      optionalSelectInput(
        ns("by_vars"),
        "Row By Variable",
        a$by_vars$choices,
        a$by_vars$selected,
        multiple = TRUE,
        fixed = a$by_vars$fixed
      ),
      optionalSelectInput(
        ns("grade"),
        "Grade Variable",
        a$grade$choices,
        a$grade$selected,
        multiple = FALSE,
        fixed = a$grade$fixed
      ),
      selectInput(
        ns("abnormal_values"),
        "Abnormality Indicator",
        choices = c("LOW", "HIGH"),
        selected = "LOW",
        multiple = TRUE
      ),
      optionalSelectInput(
        ns("treatment_flag_var"),
        "Treatment Flag Variable",
        a$treatment_flag_var$choices,
        a$treatment_flag_var$selected,
        multiple = FALSE,
        fixed = a$treatment_flag_var$fixed
      ),
      optionalSelectInput(
        ns("treatment_flag"),
        "Value indicating On Treatment",
        a$treatment_flag$choices,
        a$treatment_flag$selected,
        multiple = FALSE,
        fixed = a$treatment_flag$fixed
      ),
      checkboxInput(
        ns("exclude_base_abn"),
        "Exclude subjects whose baseline grade is the same as abnormal grade",
        value = a$exclude_base_abn
        )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @noRd
srv_t_abnormality <- function(input,
                              output,
                              session,
                              datasets,
                              dataname,
                              abnormal) {
  init_chunks()

  # Update UI choices depending on selection of previous options
  observe({
    anl <- datasets$get_data(dataname, filtered = FALSE)

    validate_has_elements(input$grade, "please select 'Grade Variable'")
    choices <- unique(anl[[input$grade]][!is.na(anl[[input$grade]])])

    updateSelectInput(
      session,
      "abnormal_values",
      choices = choices,
      selected = if (is.null(abnormal) |
                     length(intersect(abnormal, choices)) <= 0) {
        choices[1]
      } else {
        intersect(abnormal, choices)
      }

    )
  })

  prepared_env <- reactive({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    arm_var <- input$arm_var
    id_var <- input$id_var
    by_vars <- input$by_vars
    grade <- input$grade
    add_total <- input$add_total
    exclude_base_abn <- input$exclude_base_abn

    validate_has_data(adsl_filtered, 1)
    validate_has_data(anl_filtered, 1)
    validate(need(is_logical_single(add_total), "add_total is not logical"))
    validate(need(is_logical_single(exclude_base_abn), "exclude_base_abn is not logical"))
    validate(need(arm_var, "please select 'Arm variable'"))
    validate(need(id_var, "please select 'Subject Identifier'"))
    validate(need(grade, "please select 'Grade Variable'"))

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = unique(c("USUBJID", "STUDYID", arm_var, id_var)),
      anl = anl_filtered,
      anlvars = unique(c("USUBJID", "STUDYID", by_vars, grade)),
      arm_var = arm_var,
      min_n_levels_armvar = 1,
      min_nrow = 1
    )

    anl_name <- paste0(dataname, "_FILTERED")

    # Send data where the analysis lives.
    e <- new.env()
    e[[anl_name]] <- anl_filtered
    e$ADSL_FILTERED <- adsl_filtered # nolint
    e
  })

  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_abnormality(
      parentname = "ADSL_FILTERED",
      dataname = paste0(dataname, "_FILTERED"),
      arm_var = input$arm_var,
      by_vars = input$by_vars,
      abnormal = input$abnormal_values,
      grade = input$grade,
      treatment_flag_var = input$treatment_flag_var,
      treatment_flag = input$treatment_flag,
      add_total = input$add_total,
      exclude_base_abn = input$exclude_base_abn
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
    modal_title = "R Code for Abnormality Table",
    code_header = input$label
  )
}
