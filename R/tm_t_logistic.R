#' teal module for multi-variable logistic regression
#'
#' @description This module produces a multi-variable logistic regression table that matches the
#'   STREAM template \code{lgrt02}
#'
#' @inheritParams tm_t_rsp
#' @param covariate_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for covariates selection
#' @param interaction_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for interaction variable selection
#' @param conf_level \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used for confidence level for computation of the confidence intervals.
#' @importFrom rtables var_labels "var_labels<-"
#'
#' @export
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' library(teal.modules.clinical)
#' ADSL <- radsl(cached = TRUE)
#' ADRS <- radrs(ADSL, seed = 2)
#' varlabel_adrs <- var_labels(ADRS)
#' ADRS <- subset(ADRS, PARAMCD %in% c("BESRSPI", "INVET"))
#' ADRS$PARAMCD <- droplevels(ADRS$PARAMCD)
#' var_labels(ADRS) <- varlabel_adrs
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL), cdisc_dataset("ADRS", ADRS),
#'     code = 'ADSL <- radsl(cached = TRUE)
#'             ADRS <- radrs(ADSL, seed = 2)
#'             varlabel_adrs <- var_labels(ADRS)
#'             ADRS <- subset(ADRS, PARAMCD %in% c("BESRSPI", "INVET"))
#'             ADRS$PARAMCD <- droplevels(ADRS$PARAMCD)
#'             var_labels(ADRS) <- varlabel_adrs',
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_t_logistic(
#'       label = "Logistic Regression",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       arm_ref_comp = NULL,
#'       paramcd = choices_selected(levels(ADRS$PARAMCD), "BESRSPI"),
#'       covariate_var = choices_selected(c("SEX", "AGE", "BMRKR1", "BMRKR2"), "SEX"),
#'       interaction_var = choices_selected(c("SEX", "AGE", "BMRKR1", "BMRKR2"), NULL)
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(ui = app$ui, server = app$server)
#' }
#'
tm_t_logistic <- function(label,
                          dataname,
                          arm_var,
                          arm_ref_comp = NULL,
                          paramcd,
                          covariate_var,
                          interaction_var,
                          conf_level = choices_selected(c(0.8, 0.85, 0.90, 0.95, 0.99, 0.995), 0.95, keep_order = TRUE),
                          pre_output = NULL,
                          post_output = NULL){

  stop_if_not(list(is_character_single(label), "Label should be single (i.e. not vector) character type of object"))
  stop_if_not(list(is_character_vector(dataname), "Dataname should vector of characters"))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(covariate_var))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_logistic,
    ui = ui_t_logistic,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      arm_ref_comp = arm_ref_comp
    ),
    filters = dataname
  )
}

#' UI part for tm_t_logistic
#'
#' @inheritParams tm_t_logistic
#' @param id namespace id
#'
#' @details Additional standard UI inputs include \code{events},
#' \code{incl_missing} (default TRUE), \code{ref_arm}, \code{comp_arm} and
#' \code{combin_arm} (default FALSE)
#'
#' Default values of the inputs \code{var_arm}, \code{ref_arm} and
#' \code{comp_arm} are set to NULL, and updated accordingly based on selection of
#' \code{paramcd} and \code{arm.var}
#'
#' @noRd
#'
ui_t_logistic <- function(id, ...){
  ns <- NS(id)

  a <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("logistic_table"))),
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
        ns("events"),
        "Events",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      #Arm related parameters
      optionalSelectInput(
        ns("arm_var"),
        "Treatment Variable",
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
      optionalSelectInput(
        ns("covariate_var"),
        "Covariates",
        choices = a$covariate_var$choices,
        selected = a$covariate_var$selected,
        multiple = TRUE,
        label_help = helpText("taken from:", tags$code("ADSL")),
        fixed = a$covariate_var$fixed
      ),
      optionalSelectInput(
        ns("interaction_var"),
        "Interact with Treatment",
        choices = a$interaction_var$choices,
        selected = a$interaction_var$selected,
        multiple = FALSE,
        label_help = helpText("taken from:", tags$code("ADSL")),
        fixed = a$interaction_var$fixed
      ),
      uiOutput(ns("interaction_input")),
      optionalSelectInput(ns("conf_level"),
                          "Level of Confidence",
                          a$conf_level$choices,
                          a$conf_level$selected,
                          multiple = FALSE,
                          fixed = a$conf_level$fixed
      ),
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

#' server part of tm_t_logistic
#'
#' @importFrom stats median
#'
#' @noRd
srv_t_logistic <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           arm_ref_comp){
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
    module = "tm_t_logistic"
  )
  # Update UI choices depending on selection of previous options
  observe({
    anl <- datasets$get_data(dataname, filtered = FALSE)
    paramcd <- input$paramcd

    responder_choices <- unique(anl$AVALC[anl$PARAMCD == paramcd])

    updateSelectInput(
      session, "events",
      choices = responder_choices,
      selected = intersect(c("CR", "PR"), responder_choices)
    )
  })

  output$interaction_input <- renderUI({
    adsl <- datasets$get_data("ADSL", filtered = FALSE)
    interaction_var <- input$interaction_var

    if (length(interaction_var) != 0) {
      validate(need(interaction_var %in% colnames(adsl),
                    paste0(interaction_var, " is not in ADSL")))
      if (is.numeric(adsl[[interaction_var]])) {
        def_val <- ceiling(stats::median(adsl[[interaction_var]], na.rm = TRUE))
        return(
          tagList(textInput(session$ns("interaction_values"),
                            label = paste0("Specify ", interaction_var, " values (for treatment ORs calculation):"),
                            value = as.character(def_val)))
        )
      } else {
        return(NULL)
      }
    }
  })

 output$logistic_table <- renderUI({
   adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
   var_labels(adsl_filtered) <- var_labels(
     datasets$get_data("ADSL", filtered = FALSE)
   )
   anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
   var_labels(anl_filtered) <- var_labels(
     datasets$get_data(dataname, filtered = FALSE)
   )
   paramcd <- input$paramcd
   events <- input$events
   arm_var <- input$arm_var
   ref_arm <- input$ref_arm
   comp_arm <- input$comp_arm
   combine_comp_arms <- input$combine_comp_arms
   covariate_var <- input$covariate_var
   if (length(covariate_var) == 0) {
     covariate_var <- NULL
   }
   interaction_var <- input$interaction_var
   if (length(interaction_var) == 0) {
     interaction_var <- NULL
     increments <- NULL
   } else {
     validate(need(interaction_var %in% colnames(adsl_filtered),
                   paste0(interaction_var, " is not in ADSL")))
     if (is.numeric(adsl_filtered[[interaction_var]])) {
       increments <- gsub(";", ",", trimws(input$interaction_values)) %>%
         strsplit(",") %>%
         unlist() %>%
         as.numeric()
       if (length(increments) == 0) {
         increments <- NULL
       } else {
         validate(need(all(!is.na(increments)), "Not all values entered were numeric"))
         increments <- list(increments)
         names(increments) <- interaction_var
       }
     } else {
       validate(
         need(all(table(adsl_filtered[[interaction_var]], adsl_filtered[[arm_var]]) > 0),
              paste0("0 count for some cells of ", arm_var, "*", interaction_var))
       )
       increments <- NULL
     }
   }
   conf_level <- as.numeric(input$conf_level) # nolint
   # Validate your input
   validate_standard_inputs(
     adsl = adsl_filtered,
     adslvars = c("USUBJID", "STUDYID", arm_var, covariate_var, interaction_var),
     anl = anl_filtered,
     anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "AVALC"),
     arm_var = arm_var,
     ref_arm = ref_arm,
     comp_arm = comp_arm
   )

   anl_name <- paste0(dataname, "_FILTERED")
   assign(anl_name, anl_filtered)
   adsl_name <- "ADSL_FILTERED"
   assign(adsl_name, adsl_filtered)
   adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, covariate_var, interaction_var)) # nolint
   anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALC", "PARAMCD") # nolint

   validate_in(events, anl_filtered$AVALC, "event values do not exist")
   validate(need(is.logical(combine_comp_arms), "need combine arm information"))


   chunks_reset(envir = environment())

   chunks_push(bquote({
     adsl_p <- subset(
       .(as.name(adsl_name)),
       .(as.name(arm_var)) %in% c(.(ref_arm), .(comp_arm))
     )
   }))

   chunks_push(bquote({
     anl_endpoint <- subset(
       .(as.name(anl_name)),
       PARAMCD == .(paramcd)
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

   chunks_push(bquote(events <- .(events)))


   chunks_push(bquote({
     anl <- mutate(anl, EVENT = case_when(AVALC %in% events ~ 1, TRUE ~ 0))
     var_labels(anl) <- .(
       c(
         var_labels(adsl_filtered[adsl_vars]),
         var_labels(anl_filtered[c("AVAL", "AVALC", "PARAMCD")]),
         "Responders"
       )
     )
   }))

   if (!is.null(interaction_var)) {
     if (!is.null(covariate_var)) {
       if (interaction_var %in% covariate_var) {
         covariate_var <- covariate_var[which(covariate_var != interaction_var)]
         if (length(covariate_var) == 0) covariate_var <- NULL
       }
     }
   }
   chunks_push(bquote({

     formula_glm <- as.formula(
       .(paste0(
         "EVENT ~ ", arm_var,
         ifelse(is.null(covariate_var), "", paste0(" + ", paste(covariate_var, collapse = " + "))),
         ifelse(is.null(interaction_var), "", paste0(" + ", interaction_var, " + ", arm_var, "*", interaction_var))
       ))
     )
   }))

  chunks_push(bquote(increments <- .(increments)))

   chunks_push(bquote({
     tbl <- t_logistic(
       formula = formula_glm,
       data = anl,
        increments = increments,
        conf_level = .(conf_level)

     )
     tbl
   }))

   chunks_safe_eval()
   tbl <- chunks_get_var("tbl")
   as_html(tbl)

 })

 observeEvent(input$show_rcode, {
   show_rcode_modal(
     title = "Logistic Table",
     rcode = get_rcode(
       datasets = datasets,
       datanames = union("ADSL", dataname),
       title = "Logistic Regression Table"
     )
   )
 })


}
