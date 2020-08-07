#' Adverse Events Table Teal Module
#'
#' This module produces a Adverse Events summary table that matches with multiple STREAM Adverse Events templates.
#'
#' @param label (\code{character}) module label displayed in UI.
#' @param dataname (\code{character}) analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as \code{arm_var}.
#' @param hlt \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used to specify the high level term for events.
#' @param llt \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used to specify the low level term for events.
#' @param grade \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used to specify the event grade.
#' @param add_total (\code{logical}) optional, whether show column with total number of patients
#'   (\code{TRUE} on default).
#'
#' @return a \code{\link[teal]{module}} object.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADAE", ADAE),
#'     code = 'ADSL <- radsl(cached = TRUE)
#'             ADAE <- radae(cached = TRUE)',
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_t_events_by_grade(
#'       label = "AE Table by Grade",
#'       dataname = 'ADAE',
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       llt = choices_selected(
#'         choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
#'         selected = c("AEDECOD")
#'        ),
#'       hlt = choices_selected(
#'         choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
#'         selected = "AEBODSYS"
#'        ),
#'       grade = choices_selected(
#'         choices = variable_choices(ADAE, c("AETOXGR")),
#'         selected = "AETOXGR"
#'       ),
#'       add_total = TRUE
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_events_by_grade <- function(label,
                                 dataname,
                                 arm_var,
                                 hlt,
                                 llt,
                                 grade,
                                 add_total = TRUE) {

  stop_if_not(list(is_character_single(label), "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is_character_vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(hlt))
  stopifnot(is.choices_selected(llt))
  stopifnot(is.choices_selected(grade))

  stopifnot(is_logical_single(add_total))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_events_by_grade,
    ui = ui_t_events_by_grade,
    ui_args = args,
    server_args = list(
      dataname = dataname
    ),
    filters = dataname
  )

}

ui_t_events_by_grade <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var$choices,
                          a$arm_var$selected,
                          multiple = FALSE,
                          fixed = a$arm_var$fixed),
      optionalSelectInput(ns("hlt"),
                          "Event High Level Term",
                          a$hlt$choices,
                          a$hlt$selected,
                          multiple = FALSE,
                          fixed = a$hlt$fixed),
      optionalSelectInput(ns("llt"),
                          "Event Low Level Term",
                          a$llt$choices,
                          a$llt$selected,
                          multiple = FALSE,
                          fixed = a$llt$fixed),
      optionalSelectInput(ns("grade"),
                          "Event Grade",
                          a$grade$choices,
                          a$grade$selected,
                          multiple = FALSE,
                          fixed = a$grade$fixed),
      checkboxInput(ns("add_total"),
                    "Add All Patients column",
                    value = a$add_total)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @importFrom dplyr filter mutate select
#' @importFrom rtables var_relabel
#' @importFrom tern t_events_per_term_grade_id
srv_t_events_by_grade <- function(input, output, session, datasets, dataname) {

  init_chunks()

  output$table <- renderUI({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    arm_var <- input$arm_var
    hlt <- input$hlt
    llt <- input$llt
    grade <- input$grade
    add_total <- input$add_total

    validate_has_data(adsl_filtered, 1)
    validate_has_data(anl_filtered, 1)
    validate_has_elements(arm_var, "Please select \"ARM\" variable")
    validate_has_elements(llt, "Please select \"LOW LEVEL TERM\" variable")
    validate_has_elements(grade, "Please select \"GRADE\" variable")

    validate_standard_inputs(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", arm_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", llt, hlt, grade),
      arm_var = arm_var,
      max_n_levels_armvar = NULL,
      min_nrow = 1
    )

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)
    adsl_name <- "ADSL_FILTERED"
    assign(adsl_name, adsl_filtered)

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    anl_vars <- unique(c("USUBJID", "STUDYID", llt, hlt, grade))

    # Select only adsl_vars from as.name(adsl_name)
    chunks_push(
      call(
        "<-",
        as.name("ADSL_P"),
        call(
          "%>%",
          as.name(adsl_name),
          as.call(c(
            list(quote(dplyr::select)),
            lapply(adsl_vars, as.name)
          ))
        )
      )
    )

    # Convert grade to factor, and filter out missing grade, and select only anl_vars fromas.name(anl_name)
    chunks_push(
      call(
        "<-",
        as.name("ANL_ENDPOINT"),
        Reduce(
          function(x, y) call("%>%", x, y),
          c(
            as.name(anl_name),
            as.call(append(
              quote(dplyr::select),
              lapply(anl_vars, as.name)
            ))
          )
        )
      )
    )

    # Merge ANL_ENDPOINT and ADSL_P, relable original datasets labels
    chunks_push(
      call(
        "<-",
        as.name("ANL"),
        call(
          "%>%",
          call(
            "%>%",
            as.call(c(
              quote(merge),
              list(
                x = as.name("ADSL_P"),
                y = as.name("ANL_ENDPOINT"),
                all.x = FALSE,
                all.y = FALSE,
                by = c("USUBJID", "STUDYID")
              )
            )),
            as.call(c(
              quote(df_explicit_na),
              list(
                omit_columns = c("USUBJID", "STUDYID", arm_var, grade),
                char_as_factor =  FALSE
              )
            ))
          ),
          teal.devel::get_relabel_call(
            labels = c(
              datasets$get_variable_labels("ADSL", adsl_vars),
              datasets$get_variable_labels(dataname, anl_vars)
            )
          )
        )
      )
    )

    chunks_safe_eval()
    validate_has_data(chunks_get_var("ANL"), 1)

    total <- if (add_total) {
      "All Patients"
    } else {
      NULL
    }

    chunks_push(bquote({
      tbl <- t_events_per_term_grade_id(
        terms = ANL[, .(c(hlt, llt)), drop = FALSE],
        id = ANL[["USUBJID"]],
        grade = ANL[[.(grade)]],
        col_by = as.factor(.(as.name("ANL"))[[.(arm_var)]]),
        col_N = table(ADSL_P[[.(arm_var)]]),
        total = .(total)
      )
      tbl
    }))

    chunks_safe_eval()

    tbl <- chunks_get_var("tbl")
    as_html(tbl)
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Event by grade",
      rcode = get_rcode(
        datasets = datasets,
        datanames = union("ADSL", dataname),
        title = "Event table by grade"
      )
    )
  })
}
