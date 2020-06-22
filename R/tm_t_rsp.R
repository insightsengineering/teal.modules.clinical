#' @title Response Table Teal Module
#'
#' @description This module produces a response summary table that matches the
#'   STREAM template \code{rspt01}
#'
#' @inheritParams tm_t_tte
#'
#' @details Additional standard UI inputs include \code{responders},
#'   \code{ref_arm}, \code{comp_arm} and \code{combine_arm} (default FALSE)
#'
#'   Default values of the inputs \code{var_arm}, \code{ref_arm} and
#'   \code{comp_arm} are set to NULL, and updated accordingly based on selection
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
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL$Dum_ARM <- factor(rep("Single ARM", nrow(ADSL)))
#' ADRS <- radrs(ADSL, cached = TRUE) %>% dplyr::filter(AVISIT == "FOLLOW UP")
#' ADRS$Dum_ARM <- factor(rep("Single ARM", nrow(ADRS)))
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = 'ADSL <- radsl(cached = TRUE)
#'             ADSL$Dum_ARM <- factor(rep("Single ARM", nrow(ADSL)))
#'             ADRS <- radrs(ADSL, cached = TRUE) %>% dplyr::filter(AVISIT == "FOLLOW UP")
#'             ADRS$Dum_ARM <- factor(rep("Single ARM", nrow(ADRS)))',
#'      check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_t_rsp(
#'       label = "Response Table",
#'       dataname = 'ADRS',
#'       arm_var = choices_selected(c("ARM", "ARMCD", "Dum_ARM"), "ARM"),
#'       paramcd = choices_selected(levels(ADRS$PARAMCD), "BESRSPI"),
#'       strata_var = choices_selected(c("SEX", "BMRKR2"), NULL)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' @importFrom methods substituteDirect
tm_t_rsp <- function(label,
                     dataname,
                     arm_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     strata_var,
                     pre_output = NULL,
                     post_output = NULL) {

  stop_if_not(list(is_character_single(label), "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is_character_vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
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
#' \code{comp_arm} are set to NULL, and updated accordingly based on selection of
#' \code{paramcd} and \code{arm.var}
#'
#' @noRd
#'
ui_t_rsp <- function(id, ...) {

  ns <- NS(id)

  a <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("response_table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      #Response related parameters
      optionalSelectInput(
        ns("paramcd"),
        "PARAMCD",
        choices = a$paramcd$choices,
        selected = a$paramcd$selected,
        multiple = FALSE,
        fixed = a$paramcd$fixed,
        label_help = helpText("Select one type of response to analyze.")
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
        multiple = FALSE,
        fixed = a$arm_var$fixed
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
        label_help = helpText("taken from:", tags$code("ADSL")),
        fixed = a$strata_var$fixed
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
#' @importFrom methods substituteDirect
#' @noRd
#'
srv_t_rsp <- function(input,
                      output,
                      session,
                      datasets,
                      dataname,
                      arm_ref_comp) {

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    adsl = datasets$get_data("ADSL", filtered = FALSE),
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_rsp"
  )


  # Update UI choices depending on selection of previous options
  observe({
    anl <- datasets$get_data(dataname, filtered = FALSE)
    paramcd <- input$paramcd

    responder_choices <- unique(anl$AVALC[anl$PARAMCD == paramcd])

    updateSelectInput(
      session, "responders",
      choices = responder_choices,
      selected = intersect(c("CR", "PR"), responder_choices)
    )
  })

  tm_t_rsp_call <- reactive({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

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

    if (length(unique(adsl_filtered[[arm_var]])) == 1){
      # Validate your input
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", arm_var, strata_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "AVALC"),
        arm_var = arm_var,
        ref_arm = ref_arm,
        min_n_levels_armvar = NULL
      )
    } else {
      # Validate your input
      validate_standard_inputs(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", arm_var, strata_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "AVALC"),
        arm_var = arm_var,
        ref_arm = ref_arm,
        comp_arm = comp_arm
      )
    }

    validate_in(responders, anl_filtered$AVALC, "responder values do not exist")
    validate(need(is.logical(combine_comp_arms), "need combine arm information"))


    # perform analysis
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_filtered)
    adsl_name <- "ADSL_FILTERED"
    assign(adsl_name, adsl_filtered)

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, strata_var)) # nolint
    anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALC", "PARAMCD") # nolint


    chunks_reset(envir = environment())
    chunks_push(bquote({
      anl_endpoint <- subset(
        .(as.name(anl_name)),
        PARAMCD == .(paramcd)
      )
    }))
    if (length(unique(adsl_filtered[[arm_var]])) == 1){
      chunks_push(bquote({
        anl <- merge(
          x = ADSL_FILTERED[, .(adsl_vars), drop = FALSE],
          y = anl_endpoint[, .(anl_vars), drop = FALSE],
          all.x = FALSE,
          all.y = FALSE,
          by = c("USUBJID", "STUDYID")
        )
      }))

      chunks_push(bquote({
        anl[[.(arm_var)]] <- as.factor(anl[[.(arm_var)]])
        anl[[.(arm_var)]] <- droplevels(anl[[.(arm_var)]])
        tbl <- t_rsp(
          rsp = anl$AVALC %in% .(responders),
          col_by = anl[[.(arm_var)]],
          partition_rsp_by = as.factor(anl$AVALC),
          strata_data = NULL
        )
        tbl
      }))
    } else {
      chunks_push(bquote({
        adsl_p <- subset(
          .(as.name(adsl_name)),
          .(as.name(arm_var)) %in% c(.(ref_arm), .(comp_arm))
        )
      }))

      chunks_push(bquote({
        anl <- merge(
          x = adsl_p[, .(adsl_vars), drop = FALSE],
          y = anl_endpoint[, .(anl_vars), drop = FALSE],
          all.x = FALSE,
          all.y = FALSE,
          by = c("USUBJID", "STUDYID")
        )
      }))

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
      chunks_push(chunk_call_arm)

      chunks_push(bquote(
        anl[[.(arm_var)]] <- droplevels(arm)
      ))

      strata_data <- if (length(strata_var) > 0) {
        quote(anl[, strata_var, drop = FALSE]) %>%
          substituteDirect(list(strata_var = strata_var))
      } else {
        NULL
      }

      chunks_push(bquote({
        tbl <- t_rsp(
          rsp = anl$AVALC %in% .(responders),
          col_by = anl[[.(arm_var)]],
          partition_rsp_by = as.factor(anl$AVALC),
          strata_data = .(strata_data)
        )
        tbl
      }))
    }
    invisible(NULL)
  })


  output$response_table <- renderUI({
    tm_t_rsp_call()

    chunks_safe_eval()

    anl_endpoint <- chunks_get_var("anl_endpoint")
    if (any(duplicated(anl_endpoint[, c("USUBJID", "STUDYID")]))) {
      stop("only one row per patient expected")
    }

    anl <- chunks_get_var("anl")
    validate(need(nrow(anl) > 15, "need at least 15 data points"))
    validate(need(!any(duplicated(anl$USUBJID)), "patients have multiple records in the analysis data."))

    tbl <- chunks_get_var("tbl")
    validate(need(is(tbl, "rtable"), "Evaluation with tern t_rsp failed."))

    as_html(tbl)
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Summary",
      rcode = get_rcode(
        datasets = datasets,
        title = "Response Table"
      )
    )
  })

}
