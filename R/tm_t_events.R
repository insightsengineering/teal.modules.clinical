#' Adverse Events by Term Table Teal Module
#'
#' @description This module produces an Adverse Event summary table that matches the
#'   STREAM template \code{aet02} or \code{cmt01}
#'
#' @param label menu item label of the module in the teal app
#' @param dataname (\code{character}) analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as \code{arm_var}
#' @param hlt \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used to specify the high level term for events
#' @param llt \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used to specify the low level term for events
#' @param add_total (\code{logical}) optional, whether show column with total number of patients
#' @param event_type  type of event that is summarized (e.g. adverse event, treatment). Default is "event".
#'
#' @return an \code{\link[teal]{module}} object
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(teal.modules.clinical)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADAE", ADAE),
#'     code = "ADSL <- radsl(cached = TRUE)
#'             ADAE <- radae(cached = TRUE)",
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_t_events(
#'       label = "Adverse Event Table",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       llt = choices_selected(
#'         choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
#'         selected = c("AEDECOD")
#'        ),
#'       hlt = choices_selected(
#'         choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
#'         selected = "AEBODSYS"
#'        ),
#'       add_total = TRUE,
#'       event_type = "adverse event"
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_events <- function(label,
                        dataname,
                        arm_var,
                        hlt,
                        llt,
                        add_total = TRUE,
                        event_type = "event"){

  stop_if_not(list(is_character_single(label), "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is_character_vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(hlt))
  stopifnot(is.choices_selected(llt))
  stopifnot(is_logical_single(add_total))
  stop_if_not(is_character_single(event_type))
  args <- as.list(environment())
  module(
    label = label,
    ui = ui_t_events_byterm,
    server = srv_t_events_byterm,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      event_type = event_type
    ),
    filters = dataname
  )
}

ui_t_events_byterm <- function(id, ...){
  ns <- NS(id)
  a <- list(...)
  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding = div(
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
      checkboxInput(ns("add_total"), "Add All Patients columns", value = a$add_total)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_t_events_byterm <- function(input, output, session, datasets, dataname, event_type){
  init_chunks()
  output$table <- renderUI({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    arm_var <- input$arm_var
    add_total <- input$add_total
    hlt <- input$hlt
    llt <- input$llt

    validate(need(is.logical(add_total), "add total is not logical"))
    validate_has_elements(llt, "Please select \"LOW LEVEL TERM\" variable")
    validate_has_elements(arm_var, "please select 'arm variables'")
    validate_has_variable(adsl_filtered, arm_var, "arm variable does not exist")
    validate_has_data(adsl_filtered, min_nrow = 1)
    validate_has_data(anl_filtered, min_nrow = 1)

    adsl_name <- "ADSL_FILTERED"
    assign(adsl_name, adsl_filtered)
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    anl_vars <- c("USUBJID", "STUDYID", llt, hlt)

    chunks_push(
      call(
        "<-",
        as.name("ADSL_S"),
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

    chunks_push(
      call(
        "<-",
        as.name("ANL_S"),
        call(
          "%>%",
          as.name(anl_name),
          as.call(c(
            list(quote(dplyr::select)),
            lapply(anl_vars, as.name)
          ))
        )
      )
    )

    chunks_push(
      call(
        "<-",
        as.name("ANL_MERGED"),
        call(
          "%>%",
          call(
            "%>%",
            as.call(c(
              quote(merge),
              list(
                x = as.name("ADSL_S"),
                y = as.name("ANL_S"),
                all.x = FALSE,
                all.y = FALSE,
                by = c("USUBJID", "STUDYID")
              )
            )),
            as.call(c(
              quote(df_explicit_na),
              list(
                omit_columns = c("USUBJID", "STUDYID", arm_var),
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

    total <- if (add_total) "All Patients" else NULL # nolint

    chunks_push(bquote({
      tbl <- t_events_per_term_id(
        terms = ANL_MERGED[, .(c(hlt, llt)), drop = FALSE],
        id = ANL_MERGED[["USUBJID"]],
        col_by = as.factor(ANL_MERGED[[.(arm_var)]]),
        col_N = table(.(as.name(adsl_name))[[.(arm_var)]]),
        total = .(total),
        event_type = .(event_type)
      )
    }))

    chunks_safe_eval()
    tbl <- chunks_get_var("tbl")
    as_html(tbl)
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Summary",
      rcode = get_rcode(
        datasets = datasets,
        title = "Event Table"
      )
    )
  })
}
