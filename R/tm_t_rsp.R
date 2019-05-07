#' @title Response Table Teal Module
#'
#' @description This module produces a response summary table that matches the
#'   STREAM template rspt01
#'
#' @inheritParams tm_t_tte
#'
#' @details Additional standard UI inputs include \code{responders},
#'   \code{ref_arm}, \code{comp_arm} and \code{combine_arm} (default FALSE)
#'
#'   Default values of the inputs \code{var_arm}, \code{ref_arm} and
#'   \code{comp_arm} are set to NULL, and updated accordingly based on seletion
#'   of \code{paramcd} and \code{var_arm}
#'
#'   This display order of response categories in partitioned statistics section
#'   inherits the factor level order of the source data. Use
#'   \code{\link[base]{factor}} and its \code{levels} argument to manipulate
#'   the source data in order to include/exclude or re-categorize response
#'   categories and arrange the display order. If response categories are
#'   "Missing" or "Not Evaluable (NE)" or "Missing or unevaluable", 95\%
#'   confidence interval will not be calculated.
#'
#'   Reference arms automatically combined if multiple arms selected as
#'   reference group.
#'
#' @return an \code{\link[teal]{module}} object
#'
#' @template author_liaoc10
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' asl <- radsl(seed = 1)
#' keys(asl) <- c("STUDYID", "USUBJID")
#'
#' ars <- subset(radrs(asl, seed = 1), AVISIT == "Follow Up")
#' keys(ars) <- c("STUDYID", "USUBJID")
#'
#' x <- teal::init(
#'   data = cdisc_data(
#'     ASL = asl,
#'     ARS = ars,
#'     code = "
#'       asl <- radsl(seed = 1)
#'       keys(asl) <- c('STUDYID', 'USUBJID')
#'       ars <- subset(radrs(asl, seed = 1), AVISIT == 'Follow Up')
#'       keys(ars) <- c('STUDYID', 'USUBJID')
#'     "),
#'   modules = root_modules(
#'     tm_t_rsp(
#'       label = "Response Table",
#'       dataname = 'ARS',
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       paramcd = choices_selected(unique(ars$PARAMCD), "BESRSPI"),
#'       strata_var = choices_selected(c("SEX", "BMRKR2"), "SEX")
#'     )
#'   )
#' )
#'
#' \dontrun{
#'
#' shinyApp(x$ui, x$server)
#'
#' }
#'
tm_t_rsp <- function(label,
                     dataname,
                     arm_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     strata_var,
                     pre_output = NULL,
                     post_output = NULL) {

  stop_if_not(list(is.character.single(label), "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is.character.vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.null(arm_ref_comp) || is.choices_selected(arm_ref_comp))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(strata_var))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_rsp,
    ui = ui_t_rsp,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      arm_ref_comp = arm_ref_comp
    ),
    filters = dataname
  )

}


#' UI part for response table teal module
#'
#' @inheritParams tm_response_table
#' @param id namespace id
#'
#' @details Additional standard UI inputs include \code{responders},
#' \code{incl_missing} (default TRUE), \code{ref_arm}, \code{comp_arm} and
#' \code{combin_arm} (default FALSE)
#'
#' Default values of the inputs \code{var_arm}, \code{ref_arm} and
#' \code{comp_arm} are set to NULL, and updated accordingly based on seletion of
#' \code{paramcd} and \code{arm.var}
#'
#' @noRd
#'
ui_t_rsp <- function(id, ...) {

  ns <- NS(id)

  a <- list(...)

  standard_layout(
    output = teal.devel::white_small_well(uiOutput(ns("response_table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      #Response related parameters
      optionalSelectInput(
        ns("paramcd"),
        div("PARAMCD", tags$br(), helpText("Select one type of response to analyze.")),
        choices = a$paramcd$choices,
        selected = a$paramcd$selected,
        multiple = FALSE
      ),
      selectInput(
        ns("responders"),
        "Responders",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      #Arm related parameters
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        choices = a$arm_var$choices,
        selected = a$arm_var$selected,
        multiple = FALSE
      ),
      selectInput(
        ns("ref_arm"),
        "Reference Group",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      helpText("Multiple reference groups are automatically combined into a single group."),
      selectInput(
        ns("comp_arm"),
        "Comparison Group",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      checkboxInput(
        ns("combine_comp_arms"),
        "Combine all comparison groups?",
        value = FALSE
      ),
      #Stratification related parameters
      optionalSelectInput(
        ns("strata_var"),
        "Stratification Factors",
        choices = a$strata_var$choices,
        selected = a$strata_var$selected,
        multiple = TRUE,
        label_help = helpText("taken from:", tags$code("ASL"))
      )
    ),
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


#' Server part for response table teal module
#'
#' @inheritParams tm_response_table
#' @param id namespace id
#'
#' @details
#'
#' Selection for standard UI inputs \code{responders}, \code{ref_arm} and
#' \code{comp_arm} are updated upon selection of \code{paramcd} and
#' \code{arm.var}.
#'
#' Package \code{forcats} used to re-format arm data into leveled factors.
#' Reference arms automatically combined if multiple arms selected as reference
#' group.
#'
#' @importFrom forcats fct_relevel fct_collapse
#' @importFrom rtables as_html
#' @noRd
#'
srv_t_rsp <- function(input,
                      output,
                      session,
                      datasets,
                      dataname,
                      arm_ref_comp) {

  use_chunks(session)

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    asl = datasets$get_data("ASL", filtered = FALSE, reactive = FALSE),
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_rsp"
  )


  # Update UI choices depending on selection of previous options
  observe({
    anl <- datasets$get_data(dataname, filtered = FALSE, reactive = FALSE)
    paramcd <- input$paramcd

    responder_choices <- unique(anl$AVALC[anl$PARAMCD == paramcd])

    updateSelectInput(
      session, "responders",
      choices = responder_choices,
      selected = intersect(c("CR", "PR"), responder_choices)
    )
  })

  tm_t_rsp_call <- reactive({
    asl_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    paramcd <- input$paramcd
    responders <- input$responders
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_comp_arms <- input$combine_comp_arms
    strata_var <- input$strata_var

    if (length(strata_var) == 0) {
      strata_var <- NULL
    }

    # Validate your input
    validate_standard_inputs(
      asl = asl_filtered,
      aslvars = c("USUBJID", "STUDYID", arm_var, strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "AVALC"),
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm
    )

    teal.devel::validate_in(responders, anl_filtered$AVALC, "responder values do not exist")
    validate(need(is.logical(combine_comp_arms), "need combine arm information"))


    # perform analysis
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)
    asl_name <- "ASL_FILTERED"
    assign(asl_name, asl_filtered)

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, strata_var)) # nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALC", "PARAMCD") # nolint


    ## Now comes the analysis code
    renew_chunk_environment(envir = environment())
    renew_chunks()

    chunk_call_asl_p <- bquote(
      asl_p <- subset(
        .(as.name(asl_name)),
        .(as.name(arm_var)) %in% c(.(ref_arm), .(comp_arm))
      )
    )
    set_chunk("tm_t_rsp_asl_p", chunk_call_asl_p)

    chunk_call_anl_endpoint <- bquote(
      anl_endpoint <- subset(
        .(as.name(anl_name)),
        PARAMCD == .(paramcd)
      )
    )
    set_chunk("tm_t_rsp_anl_endpoint", chunk_call_anl_endpoint)

    chunk_call_anl <- bquote(
      anl <- merge(
        x = asl_p[, .(asl_vars), drop = FALSE],
        y = anl_endpoint[, .(anl_vars), drop = FALSE],
        all.x = FALSE,
        all.y = FALSE,
        by = c("USUBJID", "STUDYID")
      )
    )
    set_chunk("tm_t_rsp_anl", chunk_call_anl)

    chunk_call_arm <- bquote({
      arm <- relevel(as.factor(anl[[.(arm_var)]]), .(ref_arm)[1])
      arm <- combine_levels(arm, .(ref_arm))
    })
    if (combine_comp_arms) {
      chunk_call_arm <- bquote({
        .(chunk_call_arm)
        arm <- combine_levels(arm, .(comp_arm))
      })
    }
    set_chunk("tm_t_rsp_arm", chunk_call_arm)

    chunk_call_arm_var <- bquote(
      anl[[.(arm_var)]] <- droplevels(arm)
    )
    set_chunk("tm_t_rsp_arm_var", chunk_call_arm_var)

    table_call <- call(
      "t_rsp",
      rsp = bquote(anl$AVALC %in% .(responders)),
      col_by = bquote(anl[[.(arm_var)]]),
      partition_rsp_by = bquote(as.factor(anl$AVALC)),
      strata_data = if (length(strata_var) > 0) {
        bquote(anl[, .(strata_var), drop = FALSE])
      } else {
        NULL
      }
    )
    set_chunk("tm_t_rsp", table_call)

    invisible(NULL)
  })


  output$response_table <- renderUI({
    tm_t_rsp_call()

    eval_chunk("tm_t_rsp_asl_p")

    anl_endpoint <- eval_chunk("tm_t_rsp_anl_endpoint")
    if (any(duplicated(anl_endpoint[, c("USUBJID", "STUDYID")]))) {
      stop("only one row per patient expected")
    }

    anl <- eval_chunk("tm_t_rsp_anl")
    validate(need(nrow(anl) > 15, "need at least 15 data points"))
    validate(need(!any(duplicated(anl$USUBJID)), "patients have multiple records in the analysis data."))

    eval_chunk("tm_t_rsp_arm")

    eval_chunk("tm_t_rsp_arm_var")

    tbl <- eval_chunk("tm_t_rsp")
    if (is(tbl, "try-error")) {
      validate(need(FALSE, paste0("could not calculate response table:\n\n", tbl)))
    }

    rtables::as_html(tbl)
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Summary",
      rcode = get_rcode(
        datasets = datasets,
        dataname = dataname,
        title = "Response Table"
      )
    )
  })

}
