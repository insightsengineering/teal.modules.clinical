#' Validate standard input values for a teal module
#'
#' @description `r lifecycle::badge("stable")`
#' @param adsl data.frame with subject-level data
#' @param adslvars required variables from \code{ADSL}
#' @param anl data.frame with analysis data
#' @param anlvars required variables from \code{ANL}
#' @param need_arm flag indicating whether grouping variable \code{arm_var}
#' is required or can be optionally \code{NULL}.
#' @param arm_var character with name of grouping variable, typically arm
#' @param ref_arm character with name of reference level in \code{arm_var}
#' @param comp_arm character with name for comparison level in \code{arm_var}
#' @param min_n_levels_armvar minimum number of levels in grouping variable \code{arm_var}.
#'   Defaults to 1, \code{NULL} for no minimum.
#' @param max_n_levels_armvar maximum number of levels in grouping variable \code{arm_var}.
#'   Use \code{NULL} for no maximum.
#' @param min_nrow minimum number of observations in \code{ADSL} and \code{ANL}
#'
#' @keywords internal
#'
#' @examples
#'
#'
#' library(scda)
#' library(shiny)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
#'
#' ui <- fluidPage(
#'   shiny::sliderInput("obs", "Max Age",
#'     min = 0, max = 100, value = 100
#'   ),
#'   shiny::sliderInput("maxgr", "Max Grade",
#'     min = 0, max = 5, value = 5
#'   ),
#'   plotOutput("plot")
#' )
#'
#' server <- function(input, output) {
#'   output$plot <- renderPlot({
#'     keep_adsl <- c("USUBJID", "STUDYID", "ARMCD", "AGE", "ARM")
#'     keep_adae <- c("USUBJID", "STUDYID", "AETOXGR")
#'
#'     ADSL_f <- ADSL[ADSL$AGE <= input$obs, keep_adsl]
#'     ADAE_f <- ADAE[as.numeric(ADAE$AETOXGR) <= input$maxgr, keep_adae]
#'
#'     validate_standard_inputs(
#'       adsl = ADSL_f,
#'       adslvars = keep_adsl,
#'       anl = ADAE_f,
#'       anlvars = keep_adae,
#'       arm_var = "ARM",
#'       need_arm = TRUE
#'     )
#'
#'     ANL <- merge(ADSL_f, ADAE_f, by = c("USUBJID", "STUDYID"))
#'
#'
#'     plot(ANL$AGE, jitter(as.numeric(ANL$AETOXGR)), xlab = "AGE", ylab = "AETOXGR")
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
#'
#' server2 <- function(input, output) {
#'   output$plot <- renderPlot({
#'     keep_adsl <- c("USUBJID", "STUDYID", "ARMCD", "AGE", "ARM")
#'     keep_adae <- c("USUBJID", "STUDYID", "AETOXGR")
#'
#'     ADSL_f <- ADSL[ADSL$AGE <= input$obs, keep_adsl]
#'     ADAE_f <- ADAE[as.numeric(ADAE$AETOXGR) <= input$maxgr, keep_adae]
#'
#'     validate_standard_inputs(
#'       adsl = ADSL_f,
#'       adslvars = keep_adsl,
#'       anl = ADAE_f,
#'       anlvars = keep_adae,
#'       arm_var = NULL,
#'       need_arm = FALSE
#'     )
#'
#'     ANL <- merge(ADSL_f, ADAE_f, by = c("USUBJID", "STUDYID"))
#'
#'     plot(ANL$AGE, jitter(as.numeric(ANL$AETOXGR)), xlab = "AGE", ylab = "AETOXGR")
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server2)
#' }
#'
#' server3 <- function(input, output) {
#'   output$plot <- renderPlot({
#'     keep_adsl <- c("USUBJID", "STUDYID", "ARMCD", "AGE", "ARM")
#'     keep_adae <- c("USUBJID", "STUDYID", "AETOXGR")
#'
#'     ADSL_f <- ADSL[ADSL$AGE <= input$obs, keep_adsl]
#'     ADAE_f <- ADAE[as.numeric(ADAE$AETOXGR) <= input$maxgr, keep_adae]
#'
#'     validate_standard_inputs(
#'       adsl = ADSL_f,
#'       adslvars = keep_adsl,
#'       anl = ADAE_f,
#'       anlvars = keep_adae,
#'       arm_var = "HELLO",
#'       need_arm = FALSE
#'     )
#'
#'     ANL <- merge(ADSL_f, ADAE_f, by = c("USUBJID", "STUDYID"))
#'
#'     plot(ANL$AGE, jitter(as.numeric(ANL$AETOXGR)), xlab = "AGE", ylab = "AETOXGR")
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server3)
#' }
#'
validate_standard_inputs <- function(adsl,
                                     adslvars = character(0),
                                     anl,
                                     anlvars = character(0),
                                     need_arm = TRUE,
                                     arm_var,
                                     ref_arm,
                                     comp_arm,
                                     min_n_levels_armvar = 1L,
                                     max_n_levels_armvar = 100L,
                                     min_nrow = 1) {
  teal::validate_has_data(adsl, min_nrow = min_nrow)
  teal::validate_has_data(anl, min_nrow = min_nrow)

  if (length(adslvars) > 0) {
    teal::validate_has_variable(adsl, c(adslvars, arm_var))
  }
  if (length(anlvars) > 0) {
    teal::validate_has_variable(anl, anlvars)
  }

  if (need_arm || (!(need_arm) && !is.null(arm_var))) {
    teal::validate_has_elements(arm_var, "Treatment variable name is empty.")
    teal::validate_has_variable(adsl, arm_var, "Treatment variable not found.")

    validate_n_levels(
      adsl[[arm_var]],
      min_levels = min_n_levels_armvar,
      max_levels = max_n_levels_armvar,
      var_name = arm_var
    )

    shiny::validate(shiny::need(!("" %in% adsl[[arm_var]]), "Treatment values can not contain empty strings (i.e. '')."))

    if (!missing(comp_arm)) {
      teal::validate_has_elements(comp_arm, "Comparison treatments selection is empty.")
    }
    if (!missing(ref_arm)) {
      teal::validate_has_elements(ref_arm, "Reference treatments selection is empty.")
    }

    if (!missing(comp_arm) && !missing(ref_arm)) {
      teal::validate_no_intersection(comp_arm, ref_arm, "Reference and comparison treatments cannot overlap.")
      teal::validate_in(
        c(comp_arm, ref_arm), adsl[[arm_var]],
        "Current ADSL data does not have observations from the reference and comparison treatments."
      )
    }
  }
}
