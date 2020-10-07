#'
#' This module produces a summary table that includes abnormality counts based
#' on \code{\link[tern]{t_abnormality}}. For correct results, please use the
#' filter panel to select only post-baseline records.
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param dataname (\code{character}) analysis data used in teal module, needs
#'   to be available in the list passed to the \code{data} argument of
#'   \code{\link[teal]{init}}.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available
#'   choices and preselected option for variable names that can be used as
#'   \code{arm}
#' @param id_var \code{\link[teal]{choices_selected}} object specifying the
#'   variable name for subject id.
#' @param by_vars \code{\link[teal]{choices_selected}} object with all available
#'   choices and preselected option for variable names used to split the summary
#'   by rows.
#' @param grade \code{\link[teal]{choices_selected}} object with all available
#'   choices and preselected option for variable names that can be used to
#'   specify the abnormality grade. Variable must be factor.
#' @param abnormal (\code{vector}) to specify the default preselected values
#'   indicating abnormality grade.
#' @param baseline \code{\link[teal]{choices_selected}} object with all
#'   available choices and preselected option for variable names that can be
#'   used to specify the baseline abnormality.
#' @param exclude_base_abn (\code{boolean}) to specify the default option in UI
#'   whether to exclude subjects with baseline abnormality in denominator.
#'   Default is \code{FALSE}.
#'
#' @return a \code{\link[teal]{module}} object
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#' ADLB <- ADLB %>% dplyr::filter(ABLFL == "Y")
#'
#' ADLB <- ADLB %>%
#'   var_relabel(
#'     PARAM = "Parameters",
#'     BNRIND = "Baseline Reference Range Indicator",
#'     ANRIND = "Analysis Reference Range Indicator",
#'     AVISIT = "Analysis Visit"
#'   )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = 'ADSL <- radsl(cached = TRUE)'),
#'     cdisc_dataset("ADLB", ADLB,
#'       code = 'ADLB <- radlb(cached = TRUE)
#'               ADLB <- ADLB %>% dplyr::filter(ABLFL == "Y")
#'               ADLB <- ADLB %>% var_relabel(
#'               PARAM = "Parameters",
#'               BNRIND = "Baseline Reference Range Indicator",
#'               ANRIND = "Analysis Reference Range Indicator",
#'               AVISIT = "Analysis Visit"
#'             )'
#'     ),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_abnormality(
#'       label = "Abnormality Table",
#'       dataname = 'ADLB',
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADSL, subset = c("ARM", "ARMCD")),
#'         selected = "ARM"
#'       ),
#'       id_var = choices_selected(
#'         choices = variable_choices(ADSL, subset = c("USUBJID", "SUBJID")),
#'         selected = "USUBJID",
#'         fixed = TRUE
#'       ),
#'       by_vars = choices_selected(
#'         choices = variable_choices(ADLB, subset = c("LBCAT", "PARAM", "AVISIT")),
#'         selected = c("PARAM", "AVISIT"),
#'         keep_order = TRUE
#'       ),
#'       grade = choices_selected(
#'         choices = variable_choices(ADLB, subset = "ANRIND"),
#'         selected = "ANRIND",
#'         fixed = TRUE
#'       ),
#'       abnormal = c("LOW", "HIGH"),
#'       baseline = choices_selected(
#'         choices = variable_choices(ADLB, subset = "BNRIND"),
#'         selected = "BNRIND",
#'         fixed = TRUE
#'       ),
#'       exclude_base_abn = FALSE
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_abnormality <- function(label,
                             dataname,
                             arm_var,
                             id_var,
                             by_vars,
                             grade,
                             abnormal,
                             baseline,
                             exclude_base_abn = FALSE,
                             pre_output = NULL,
                             post_output = NULL
) {

  stopifnot(length(dataname) == 1)
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(id_var))
  stopifnot(is.choices_selected(by_vars))
  stopifnot(is.choices_selected(grade))
  stopifnot(is.choices_selected(baseline))
  stopifnot(is_character_vector(abnormal))
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
      optionalSelectInput(
        ns("baseline"),
        "Variable for Baseline Grade",
        a$baseline$choices,
        a$baseline$selected,
        multiple = FALSE,
        fixed = a$baseline$fixed
      ),
      selectInput(
        ns("abnormal_values"),
        "Abnormality Indicator",
        choices = NULL,
        selected = NULL,
        multiple = TRUE),
      checkboxInput(
        ns("exclude_base_abn"),
        "Exclude subjects whose baseline grade is the same as abnormal grade",
        value = a$exclude_base_abn),
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


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

    validate_has_elements(input$grade, "plesae select 'Grade Variable'")
    choices <- unique(anl[[input$grade]][!is.na(anl[[input$grade]])])

    updateSelectInput(
      session, "abnormal_values",
      choices = choices,
      selected = if (is.null(abnormal) |
                     length(intersect(abnormal, choices)) <= 0) {
        choices[1]
      } else {
        intersect(abnormal, choices)
      }

    )
  })

  # Create output
  output$table <- renderUI({

    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    arm_var <- input$arm_var
    id_var <- input$id_var
    by_vars <- input$by_vars
    grade <- input$grade
    baseline <- input$baseline
    abnormal_values <- input$abnormal_values
    add_total <- input$add_total
    exclude_base_abn <- input$exclude_base_abn

    # validate your input values
    validate_has_data(adsl_filtered, 1)
    validate_has_data(anl_filtered, 1)

    validate(need(is_logical_single(add_total), "add_total is not logical"))
    validate(need(is_logical_single(exclude_base_abn), "exclude_base_abn is not logical"))
    validate_has_elements(arm_var, "please select 'Arm variable'")
    validate_has_elements(id_var, "please select 'Subject Identifier'")
    validate_has_elements(grade, "plesae select 'Grade Variable'")
    validate_has_elements(baseline, "plesae select 'Variable for Baseline Grade'")
    validate_has_elements(abnormal_values, "please select 'Abnormality Indicator'")

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = unique(c("USUBJID", "STUDYID", arm_var, id_var)),
      anl = anl_filtered,
      anlvars = unique(c("USUBJID", "STUDYID", by_vars, grade, baseline)),
      arm_var = arm_var,
      min_n_levels_armvar = 1,
      min_nrow = 1
    )

    # do analysis

    adsl_name <- "ADSL_FILTERED"
    assign(adsl_name, adsl_filtered)
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, id_var))
    anl_vars <- unique(c("USUBJID", "STUDYID", by_vars, grade, baseline))

    #nolint start
    chunks_push(bquote({
      ADSL_S <- ADSL_FILTERED[, .(adsl_vars)]
      ANL_S <- .(as.name(anl_name))[, .(anl_vars)]
      ANL_MERGED <- merge(ADSL_S, ANL_S,
                          all.x = FALSE, all.y = FALSE, by = c("STUDYID", "USUBJID"))
    })
    )
    #nolint end

    chunks_push(
      call(
        "<-",
        as.name("ANL_MERGED"),
        call(
          "%>%",
          as.name("ANL_MERGED"),
          teal.devel::get_relabel_call(
            labels = c(
              datasets$get_variable_labels("ADSL", adsl_vars),
              datasets$get_variable_labels(dataname, anl_vars)
            )
          )
        )
      )
    )

    total <- if (add_total) "All Patients" else NULL #nolint

    if (is.null(by_vars)) {
      chunks_push(bquote(row_by <- factor(rep("All", nrow(.(as.name("ANL_MERGED")))))))
    } else {
      chunks_push(bquote(row_by <- .(as.name("ANL_MERGED"))[, .(by_vars), drop = FALSE]
      ))
    }

    chunks_push(bquote({

      tbl <- t_abnormality(
        grade = ANL_MERGED[[.(grade)]],
        abnormal = .(abnormal_values),
        baseline = ANL_MERGED[[.(baseline)]],
        id = ANL_MERGED[[.(id_var)]],
        exclude_base_abn = .(exclude_base_abn),
        col_by = as.factor(ANL_MERGED[[.(arm_var)]]),
        row_by = row_by,
        col_N = table(ADSL_S[[.(arm_var)]]),
        total = .(total),
        table_tree = FALSE
      )
      tbl

    }))

    chunks_safe_eval()

    tbl <- chunks_get_var("tbl")

    div(
      as_html(tbl),
      tags$p("Denominator for each abnormal grade is defined as number of
             subjects with non-missing grade")
    )
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Abnormality Table",
      rcode = get_rcode(
        datasets = datasets,
        datanames = union("ADSL", dataname),
        title = "Abnormality Table"
      )
    )
  })
}
