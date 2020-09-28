#' Teal module for Mixed Model Repeated Measurements (MMRM) analysis.
#'
#' @inheritParams teal.devel::standard_layout
#' @param label menu item label of the module in the teal app.
#' @param dataname (\code{character}) analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form where each subject has
#'   repeated measured at different timepoints. At each visit, there is only one
#'   record for each subject.
#' @param response_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#'   for numeric variables that can be used as \code{response} variable.
#' @param id_var \code{\link[teal]{choices_selected}} object specifying the variable name for subject ID.
#'   (This will be converted to a factor.)
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#'   for variable names that can be used as \code{arm} variable. (This will be converted to a factor.)
#' @param visit_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#'   for variable names that can be used as \code{visit} variable. Must be a factor in \code{dataname}.
#' @param covariate_vars \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#'   for variable names and interaction combinations (using \code{A*B} and \code{A:B} notation) that can be used as
#'   \code{covariate} variables. Variables can be numeric or factor.
#' @param arm_ref_comp (\code{\link[teal]{choices_selected}}) optional, if specified it must be a named list with each
#'   element corresponding to an arm variable in \code{ADSL} and the element must
#'   be another list with the elements named \code{ref} and \code{comp} that the
#'   defined the default reference and comparison arms when the arm variable is
#'   changed.
#' @param paramcd \code{\link[teal]{choices_selected}} object with all available choices and preselected option for
#'   variable names that can be used as \code{PARAMCD} variable (i.e., types of response that can be selected).
#' @param conf_level \code{\link[teal]{choices_selected}} object specifying the confidence level (numeric values,
#'   need to be greater than 0 and less than 1).
#'
#' @details
#' This modules expects that the analysis data has the following variables:
#'
#' \tabular{ll}{
#'  \code{PARAMCD} \tab variable used to filter for endpoint, after
#'  filtering for \code{paramcd} one observation per patient per time point is expected
#' }
#'
#' The arm, subject ID, and covariate variables are taken from the \code{ADSL} data.
#'
#' @note
#' The ordering of the input data sets can lead to slightly different numerical results or
#' different convergence behavior. This is a known observation with the used package
#' \code{lme4}. However, once convergence is achieved, the results are reliable up to
#' numerical precision.
#'
#' @export
#' @importFrom stats terms
#' @importFrom shinyjs show
#'
#' @examples
#'
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADQS <- radqs(cached = TRUE) %>%
#'   dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
#'   dplyr::mutate(
#'     AVISIT = as.factor(AVISIT),
#'     AVISITN = rank(AVISITN) %>%
#'       as.factor() %>%
#'       as.numeric() %>%
#'       as.factor() # making consecutive numeric factor
#'   )
#'
#' arm_ref_comp <- list(
#'   ARMCD = list(
#'     ref = "ARM A",
#'     comp = c("ARM B", "ARM C")
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADQS", ADQS),
#'     code = 'ADSL <- radsl(cached = TRUE)
#'             ADQS <- radqs(cached = TRUE) %>%
#'               dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
#'               dplyr::mutate(
#'                 AVISIT = as.factor(AVISIT),
#'                 AVISITN = rank(AVISITN) %>%
#'                   as.factor() %>%
#'                   as.numeric() %>%
#'                   as.factor() # making consecutive numeric factor
#'               )'
#'   ),
#'   modules = root_modules(
#'     tm_a_mmrm(
#'       label = "MMRM",
#'       dataname = 'ADQS',
#'       response_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
#'       id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARMCD"),
#'       visit_var = choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         choices = value_choices(ADQS, "PARAMCD", "PARAM"),
#'         selected = "FKSI-FWB"
#'       ),
#'       covariate_vars = choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL),
#'       conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_a_mmrm <- function(label,
                      dataname,
                      response_var,
                      id_var,
                      arm_var,
                      visit_var,
                      covariate_vars,
                      arm_ref_comp = NULL,
                      paramcd,
                      conf_level,
                      pre_output = NULL,
                      post_output = NULL
) {
  module(
    label = label,
    ui = function(id, datasets) {
      ns <- NS(id)
      htmlOutput(ns("tbd"))
    },
    server = function(input, output, session, datasets) {
      output$tbd <- renderUI({
        p("Module is currently refactored")
      })
    },
    filters = "ADSL"
  )
}


# REFACTOR
# nolint start
# tm_a_mmrm <- function(label,
#                       dataname,
#                       response_var,
#                       id_var,
#                       arm_var,
#                       visit_var,
#                       covariate_vars,
#                       arm_ref_comp = NULL,
#                       paramcd,
#                       conf_level,
#                       pre_output = NULL,
#                       post_output = NULL
# ) {
#
#   covariate_vars <- add_no_selected_choices(covariate_vars, multiple = TRUE)
#   stopifnot(length(dataname) == 1)
#   stopifnot(is.choices_selected(response_var))
#   stopifnot(is.choices_selected(id_var))
#   stopifnot(is.choices_selected(arm_var))
#   stopifnot(is.choices_selected(visit_var))
#   stopifnot(is.choices_selected(covariate_vars))
#   stopifnot(is.choices_selected(paramcd))
#   stopifnot(is.choices_selected(conf_level))
#
#   args <- as.list(environment())
#
#   module(
#     label = label,
#     server = srv_mmrm,
#     ui = ui_mmrm,
#     ui_args = args,
#     server_args = list(
#       dataname = dataname,
#       arm_ref_comp = arm_ref_comp,
#       label = label
#     ),
#     filters = dataname
#   )
# }
#
# ui_mmrm <- function(id, ...) {
#
#   a <- list(...) # module args
#
#   ns <- NS(id)
#
#   standard_layout(
#     output = white_small_well(
#       p(
#         id = ns("initial_message"),
#         paste(
#           "Please first specify 'Model Settings' and press 'Fit Model'.",
#           "Afterwards choose 'Output Type' and optional 'Output Settings'.",
#           "If changes to the 'Model Settings' or dataset (by filtering) are made,",
#           "then the 'Fit Model' button must be pressed again to update the MMRM model.",
#           "Note that the 'Show R Code' button can only be clicked if the model fit is up to date."
#         )
#       ),
#       h3(textOutput(ns("mmrm_title"))),
#       uiOutput(ns("mmrm_table")),
#       plotOutput(ns("mmrm_plot"))
#     ),
#     encoding = div(
#       tags$label("Encodings", class = "text-primary"),
#       panel_group(
#         panel_item(
#           "Model Settings",
#           helpText("Analysis data:", code(a$dataname)),
#           optionalSelectInput(
#             ns("response_var"),
#             "Select Response",
#             a$response_var$choices,
#             a$response_var$selected,
#             multiple = FALSE,
#             fixed = a$response_var$fixed
#           ),
#           optionalSelectInput(
#             ns("paramcd"),
#             "Select Parameter",
#             a$paramcd$choices,
#             a$paramcd$selected,
#             multiple = FALSE,
#             fixed = a$paramcd$fixed
#           ),
#           optionalSelectInput(
#             ns("arm_var"),
#             "Arm Variable",
#             a$arm_var$choices,
#             a$arm_var$selected,
#             multiple = FALSE,
#             fixed = a$arm_var$fixed
#           ),
#           optionalSelectInput(
#             ns("visit_var"),
#             "Visit Variable",
#             a$visit_var$choices,
#             a$visit_var$selected,
#             multiple = FALSE,
#             fixed = a$visit_var$fixed
#           ),
#           optionalSelectInput(
#             ns("covariate_vars"),
#             "Covariate Variables",
#             a$covariate_vars$choices,
#             a$covariate_vars$selected,
#             multiple = TRUE,
#             fixed = a$covariate_vars$fixed
#           ),
#           selectInput(
#             ns("ref_arm"),
#             "Reference Group",
#             choices = NULL,
#             selected = NULL,
#             multiple = TRUE
#           ),
#           helpText("Multiple reference groups are automatically combined into a single group."),
#           selectInput(
#             ns("comp_arm"),
#             "Comparison Group",
#             choices = NULL,
#             selected = NULL,
#             multiple = TRUE
#           ),
#           checkboxInput(
#             ns("combine_comp_arms"),
#             "Combine all comparison groups?",
#             value = FALSE
#           ),
#           optionalSelectInput(
#             ns("id_var"),
#             "Subject Identifier",
#             a$id_var$choices,
#             a$id_var$selected,
#             multiple = FALSE,
#             fixed = a$id_var$fixed
#           ),
#           selectInput(
#             ns("weights_emmeans"),
#             "Weights for LS means",
#             choices = c("proportional", "equal"),
#             selected = "proportional",
#             multiple = FALSE
#           ),
#           selectInput(
#             ns("cor_struct"),
#             "Correlation Structure",
#             choices = c("unstructured", "random-quadratic", "random-slope", "compound-symmetry"),
#             selected = "unstructured",
#             multiple = FALSE
#           ),
#           optionalSelectInput(
#             ns("conf_level"),
#             "Confidence Level",
#             a$conf_level$choices,
#             a$conf_level$selected,
#             multiple = FALSE,
#             fixed = a$conf_level$fixed
#           ),
#           selectInput(
#             ns("optimizer"),
#             "Optimization Algorithm",
#             choices = c(
#               "automatic",
#               "nloptwrap_neldermead",
#               "nloptwrap_bobyqa",
#               "nlminbwrap",
#               "bobyqa",
#               "neldermead",
#               "nmkbw",
#               "optimx_lbfgsb"
#             ),
#             selected = NULL,
#             multiple = FALSE
#           ),
#           # Additional option for "automatic" optimizer.
#           checkboxInput(
#             ns("parallel"),
#             "Parallel Computing",
#             value = TRUE
#           ),
#           # Show here which automatic optimizer was used in the end.
#           textOutput(ns("optimizer_selected")),
#           collapsed = FALSE  # Start with having this panel opened.
#         )
#       ),
#       tags$style(".btn.disabled {
#          color: grey;
#          background-color: white
#       }"),
#       actionButton(
#         ns("button_start"),
#         "Fit Model",
#         icon = icon("calculator"),
#         width = "100%",
#         class = "btn action-button",
#         style = "color: black;
#                  background-color: orange"
#       ),
#       br(),
#       br(),
#       radioButtons(
#         ns("output_function"),
#         "Output Type",
#         choices = c(
#           "LS means table" = "t_mmrm_lsmeans",
#           "LS means plots" = "g_mmrm_lsmeans",
#           "Covariance estimate" = "t_mmrm_cov",
#           "Fixed effects" = "t_mmrm_fixed",
#           "Fit statistics" = "t_mmrm_diagnostic",
#           "Diagnostic plots" = "g_mmrm_diagnostic"
#         ),
#         selected = "t_mmrm_lsmeans"
#       ),
#       panel_group(
#         panel_item(
#           "Output Settings",
#           # Additional option for LS means table.
#           selectInput(
#             ns("t_mmrm_lsmeans_show_relative"),
#             "Show Relative Change",
#             choices = c("reduction", "increase", "none"),
#             selected = "reduction",
#             multiple = FALSE
#           ),
#           checkboxGroupInput(
#             ns("g_mmrm_lsmeans_select"),
#             "LS means plots",
#             choices = c(
#               "Estimates" = "estimates",
#               "Contrasts" = "contrasts"
#             ),
#             selected = c("estimates", "contrasts"),
#             inline = TRUE
#           ),
#           sliderInput(
#             ns("g_mmrm_lsmeans_width"),
#             "CI bar width",
#             min = 0.1,
#             max = 1,
#             value = 0.6
#           ),
#           checkboxInput(
#             ns("g_mmrm_lsmeans_contrasts_show_pval"),
#             "Show contrasts p-values",
#             value = FALSE
#           ),
#           # Additional options for diagnostic plots.
#           radioButtons(
#             ns("g_mmrm_diagnostic_type"),
#             "Diagnostic plot type",
#             choices = c(
#               "Fitted vs. Residuals" = "fit-residual",
#               "Normal Q-Q Plot of Residuals" = "q-q-residual"
#             ),
#             selected = NULL
#           ),
#           sliderInput(
#             ns("g_mmrm_diagnostic_z_threshold"),
#             "Label observations above this threshold",
#             min = 0.1,
#             max = 10,
#             value = 3
#           )
#         )
#       )
#     ),
#     forms = actionButton(
#       ns("show_rcode"),
#       "Show R Code",
#       width = "100%",
#       class = "btn action-button",
#       style = "color: black;
#                background-color: green"
#     ),
#     pre_output = a$pre_output,
#     post_output = a$post_output
#   )
# }
#
# srv_mmrm <- function(input,
#                      output,
#                      session,
#                      datasets,
#                      dataname,
#                      arm_ref_comp,
#                      label) {
#   init_chunks()
#
#   # Initially hide the output title because there is no output yet.
#   shinyjs::hide("mmrm_title")
#
#   # Setup arm variable selection, default reference arms, and default
#   # comparison arms for encoding panel.
#   arm_ref_comp_observer(
#     session, input,
#     id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",  # From UI.
#     adsl = datasets$get_data("ADSL", filtered = FALSE),
#     arm_ref_comp = arm_ref_comp,
#     module = "tm_mmrm"
#   )
#
#   # Event handler:
#   # Show either the plot or the table output.
#   observeEvent(input$output_function, {
#     output_function <- input$output_function
#     if (isTRUE(grepl("^t_", output_function))) {
#       shinyjs::hide("mmrm_plot")
#       shinyjs::show("mmrm_table")
#     } else if (isTRUE(grepl("^g_", output_function)))  {
#       shinyjs::hide("mmrm_table")
#       shinyjs::show("mmrm_plot")
#     } else {
#       stop("unknown output type")
#     }
#   })
#
#   # Event handler:
#   # Show or hide parallel computing option (and result).
#   observeEvent(input$optimizer, {
#     optimizer <- input$optimizer
#     if (isTRUE(optimizer == "automatic")) {
#       shinyjs::show("parallel")
#       shinyjs::show("optimizer_selected")
#     } else {
#       shinyjs::hide("parallel")
#       shinyjs::hide("optimizer_selected")
#     }
#   })
#
#   # Event handler:
#   # Show or hide LS means table option.
#   observeEvent(input$output_function, {
#     output_function <- input$output_function
#     if (isTRUE(output_function == "t_mmrm_lsmeans")) {
#       shinyjs::show("t_mmrm_lsmeans_show_relative")
#     } else {
#       shinyjs::hide("t_mmrm_lsmeans_show_relative")
#     }
#   })
#
#   # Event handler:
#   # Show or hide the LS means plot options.
#   observeEvent(list(input$output_function, input$g_mmrm_lsmeans_select), {
#     output_function <- input$output_function
#     g_mmrm_lsmeans_select <- input$g_mmrm_lsmeans_select
#     if (isTRUE(output_function == "g_mmrm_lsmeans")) {
#       shinyjs::show("g_mmrm_lsmeans_select")
#       shinyjs::show("g_mmrm_lsmeans_width")
#       if (isTRUE("contrasts" %in% g_mmrm_lsmeans_select)) {
#         shinyjs::show("g_mmrm_lsmeans_contrasts_show_pval")
#       } else {
#         shinyjs::hide("g_mmrm_lsmeans_contrasts_show_pval")
#       }
#     } else {
#       shinyjs::hide("g_mmrm_lsmeans_select")
#       shinyjs::hide("g_mmrm_lsmeans_width")
#       shinyjs::hide("g_mmrm_lsmeans_contrasts_show_pval")
#     }
#   })
#
#   # Event handler:
#   # Show or hide the diagnostic plot type option.
#   observeEvent(list(input$output_function, input$g_mmrm_diagnostic_type), {
#     output_function <- input$output_function
#     g_mmrm_diagnostic_type <- input$g_mmrm_diagnostic_type
#     if (isTRUE(output_function == "g_mmrm_diagnostic")) {
#       shinyjs::show("g_mmrm_diagnostic_type")
#       if (isTRUE(g_mmrm_diagnostic_type == "q-q-residual")) {
#         shinyjs::show("g_mmrm_diagnostic_z_threshold")
#       } else {
#         shinyjs::hide("g_mmrm_diagnostic_z_threshold")
#       }
#     } else {
#       shinyjs::hide("g_mmrm_diagnostic_type")
#       shinyjs::hide("g_mmrm_diagnostic_z_threshold")
#     }
#   })
#
#   # Event handler:
#   # When the "Fit Model" button is clicked, hide initial message, show title, disable model fit and enable
#   # show R code buttons.
#   shinyjs::onclick("button_start", {
#     shinyjs::hide("initial_message")
#     shinyjs::show("mmrm_title")
#     shinyjs::disable("button_start")
#     shinyjs::enable("show_rcode")
#   })
#
#   # Event handler:
#   # These trigger when we are out of sync and then enable the start button and
#   # disable the show R code button.
#   observeEvent(list(
#     # Data.
#     datasets$get_data("ADSL", filtered = TRUE),
#     datasets$get_data(dataname, filtered = TRUE),
#     # Relevant Encodings.
#     input$response_var,
#     input$paramcd,
#     input$arm_var,
#     input$ref_arm,
#     input$comp_arm,
#     input$combine_comp_arms,
#     input$visit_var,
#     input$covariate_vars,
#     input$id_var,
#     input$weights_emmeans,
#     input$cor_struct,
#     input$conf_level,
#     input$optimizer
#     # Note:
#     # input$parallel does not get us out of sync (it just takes longer to get to same result).
#   ), {
#     shinyjs::enable("button_start")
#     shinyjs::disable("show_rcode")
#   })
#
#
#
#   # Connector:
#   # Fit the MMRM, once the user clicks on the start button.
#   mmrm_fit <- eventReactive(input$button_start, {
#
#     # Create a private stack for this function only.
#     fit_stack <- chunks$new()
#     fit_stack_push <- function(...) {
#       chunks_push(..., chunks = fit_stack)
#     }
#     fit_stack_get_var <- function(...) {
#       chunks_get_var(..., chunks = fit_stack)
#     }
#
#     # First reassign reactive sources:
#
#     # Data.
#     # nolint start
#     adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
#     anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
#     # nolint end
#
#     # Input on model features.
#     # nolint start
#     response_var <- input$response_var
#     paramcd <- input$paramcd
#     arm_var <- input$arm_var
#     ref_arm <- input$ref_arm
#     comp_arm <- input$comp_arm
#     combine_comp_arms <- input$combine_comp_arms
#     visit_var <- input$visit_var
#     covariate_vars <- input$covariate_vars
#     covariate_vars <- no_selected_as_NULL(covariate_vars)
#     id_var <- input$id_var
#     weights_emmeans <- input$weights_emmeans
#     cor_struct <- input$cor_struct
#     conf_level <- as.numeric(input$conf_level)
#     optimizer <- input$optimizer
#     parallel <- input$parallel
#     # nolint end
#
#     # Validate the input variables.
#     validate_has_data(adsl_filtered, 1)
#     validate_has_data(anl_filtered, 1)
#     # Split the existing covariate strings in their variable parts, to allow "A*B" and "A:B" notations.
#     covariate_parts <- if_not_null(
#       covariate_vars,
#       unique(unlist(strsplit(covariate_vars, split = "\\*|:")))
#     )
#     all_x_vars <- c(arm_var, visit_var, covariate_parts)
#     all_x_vars_in_adsl <- intersect(
#       all_x_vars,
#       colnames(adsl_filtered)
#     )
#     all_x_vars_in_anl <- setdiff(
#       all_x_vars,
#       all_x_vars_in_adsl
#     )
#     validate_standard_inputs(
#       adsl = adsl_filtered,
#       adslvars = unique(c("USUBJID", "STUDYID", arm_var, id_var, all_x_vars_in_adsl)),
#       anl = anl_filtered,
#       anlvars = unique(c("USUBJID", "STUDYID", "PARAMCD", response_var, visit_var, all_x_vars_in_anl)),
#       arm_var = arm_var,
#       ref_arm = ref_arm,
#       comp_arm = comp_arm
#     )
#
#     # Assert combine arm info.
#     validate(need(is.logical(combine_comp_arms), "need combine arm information"))
#
#     # Assign data set names and variables.
#     anl_name <- paste0(dataname, "_FILTERED")
#     assign(anl_name, anl_filtered)
#     adsl_name <- "ADSL_FILTERED"
#     assign(adsl_name, adsl_filtered)
#
#     chunks_reset(chunks = fit_stack)
#
#     adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, id_var, all_x_vars_in_adsl))
#     anl_vars <- unique(c("USUBJID", "STUDYID", response_var, visit_var, all_x_vars_in_anl))
#
#     # Now comes the analysis code.
#     fit_stack_push(bquote(ref_arm <- .(ref_arm)))
#     fit_stack_push(bquote(comp_arm <- .(comp_arm)))
#     fit_stack_push(bquote(combine_comp_arms <- .(combine_comp_arms)))
#
#     # Create ADSL_P: Select only adsl_vars from as.name(adsl_name).
#     fit_stack_push(bquote(keep_adsl_columns <- .(adsl_vars)))
#     fit_stack_push(bquote({
#       ADSL_P <- .(as.name(adsl_name)) %>% # nolint
#         dplyr::filter(.(as.name(arm_var)) %in% .(c(ref_arm, comp_arm))) %>%
#         dplyr::select(!!!syms(keep_adsl_columns))
#     }))
#
#     chunks_safe_eval(chunks = fit_stack)
#     fit_stack_push(bquote(arm <- relevel(as.factor(ADSL_P[[.(arm_var)]]), ref_arm[1])))
#     fit_stack_push(bquote(arm <- combine_levels(arm, ref_arm)))
#
#     if (combine_comp_arms) {
#       fit_stack_push(bquote(arm <- combine_levels(arm, comp_arm)))
#     }
#
#     fit_stack_push(bquote(ADSL_P[[.(arm_var)]] <- droplevels(arm)))
#
#     # Create ANL_ENDPOINT: Filter PARAMCD and select only anl_vars from as.name(anl_name).
#     fit_stack_push(bquote(keep_anl_columns <- .(anl_vars)))
#     fit_stack_push(bquote({
#       ANL_ENDPOINT <- .(as.name(anl_name)) %>% # nolint
#         dplyr::filter(.(as.name("PARAMCD")) == .(paramcd)) %>%
#         dplyr::select(!!!syms(keep_anl_columns))
#     }))
#
#     # Merge ANL_ENDPOINT and ADSL_P, relabel original datasets labels.
#     fit_stack_push(
#       call(
#         "<-",
#         as.name("ANL"),
#         call(
#           "%>%",
#           as.call(append(
#             quote(merge),
#             list(
#               x = as.name("ADSL_P"),
#               y = as.name("ANL_ENDPOINT"),
#               all.x = FALSE,
#               all.y = FALSE,
#               by = c("USUBJID", "STUDYID")
#             )
#           )),
#           teal.devel::get_relabel_call(
#             labels = c(
#               datasets$get_variable_labels("ADSL", adsl_vars),
#               datasets$get_variable_labels(dataname, anl_vars)
#             )
#           )
#         )
#       )
#     )
#
#     chunks_safe_eval(chunks = fit_stack)
#
#     validate(need(nrow(fit_stack_get_var("ANL")) > 5, "need at least 5 data points"))
#     Map(function(visit_df, visit_name) {
#
#       dup <- any(duplicated(visit_df[[id_var]]))
#
#       validate(need(!dup, paste("Duplicated subject ID found at", visit_name)))
#
#     }, split(fit_stack_get_var("ANL"), fit_stack_get_var("ANL")[[visit_var]]),
#     levels(fit_stack_get_var("ANL")[[visit_var]]))
#
#     validate(need(
#       all(complete.cases(fit_stack_get_var("ANL"))),
#       paste(c("Missing values found in formula vars",
#               fit_stack_get_var("ANL")[!complete.cases(fit_stack_get_var("ANL")), ]))
#     ))
#
#     fit_stack_push(bquote({
#       fit <- s_mmrm(
#         vars = list(
#           response = .(response_var),
#           covariates = .(covariate_vars),
#           id = .(id_var),
#           arm = .(arm_var),
#           visit = .(visit_var)
#         ),
#         data = ANL,
#         conf_level = .(conf_level),
#         cor_struct = .(cor_struct),
#         weights_emmeans = .(weights_emmeans),
#         optimizer = .(optimizer),
#         parallel = .(parallel)
#       )
#     }))
#     fit_stack_push(bquote(fit[["col_N"]] <- table(ADSL_P[[.(arm_var)]])))
#
#     # Evaluate all code on the fit stack, and return the fit stack, so we can further push
#     # to it in the output rendering code below.
#     chunks_safe_eval(chunks = fit_stack)
#     fit_stack
#   })
#
#   # Endpoint:
#   # Output title.
#   output$mmrm_title <- renderText({
#
#     # Input on output type.
#     output_function <- input$output_function
#     g_mmrm_diagnostic_type <- input$g_mmrm_diagnostic_type
#     g_mmrm_lsmeans_select <- input$g_mmrm_lsmeans_select
#
#     output_title <- switch(
#       output_function,
#       "t_mmrm_cov" = "Residual covariance matrix estimate",
#       "t_mmrm_diagnostic" = "Model fit statistics",
#       "t_mmrm_fixed" = "Fixed effects estimates",
#       "t_mmrm_lsmeans" = "LS means and contrasts estimates",
#       "g_mmrm_diagnostic" = switch(
#         g_mmrm_diagnostic_type,
#         "fit-residual" = "Marginal fitted values vs. residuals",
#         "q-q-residual" = "Q-Q normal plot for standardized residuals"
#       ),
#       "g_mmrm_lsmeans" = if (setequal(g_mmrm_lsmeans_select, c("estimates", "contrasts"))) {
#         "LS means estimates and contrasts"
#       } else if (identical(g_mmrm_lsmeans_select, "estimates")) {
#         "LS means estimates"
#       } else {
#         "LS means contrasts"
#       }
#     )
#     output_title
#   })
#
#   # Endpoint:
#   # Table outputs.
#   output$mmrm_table <- renderUI({
#
#     # Input on output type.
#     output_function <- input$output_function
#     # Additional setting.
#     show_relative <- input$t_mmrm_lsmeans_show_relative # nolint
#
#     # If the output is not a table, stop here.
#     if (!isTRUE(grepl("^t_", output_function))) return(NULL)
#
#     # Reset global chunks. Needs to be done here so nothing yet in environment.
#     chunks_reset()
#
#     # Get the fit stack while evaluating the fit code at the same time.
#     fit_stack <- mmrm_fit()
#     fit <- chunks_get_var("fit", chunks = fit_stack)
#
#     # Start new private stack for the table code.
#     table_stack <- chunks$new()
#     table_stack_push <- function(...) {
#       chunks_push(..., chunks = table_stack)
#     }
#
#     # Depending on the table function type, produce different code.
#     if (output_function == "t_mmrm_lsmeans") {
#       table_stack_push(bquote({
#         tbl <- t_mmrm_lsmeans(
#           fit,
#           col_N = fit$col_N,
#           table_tree = FALSE,
#           show_relative = .(show_relative)
#         )
#       }))
#     } else if (output_function == "t_mmrm_diagnostic") {
#       table_stack_push(bquote(tbl <- t_mmrm_diagnostic(fit)))
#     } else if (output_function == "t_mmrm_fixed") {
#       table_stack_push(bquote(tbl <- t_mmrm_fixed(fit)))
#     } else if (output_function == "t_mmrm_cov") {
#       table_stack_push(bquote(tbl <- t_mmrm_cov(fit)))
#     }
#
#     # Get nice R code to show to the user.
#     chunks_push_chunks(fit_stack)
#     chunks_push_chunks(table_stack)
#
#     # Evaluate the code and the the table.
#     chunks_safe_eval()
#     tbl <- chunks_get_var("tbl")
#
#     # Just so that the shown R code prints the table at the end.
#     chunks_push(bquote(print(tbl)))
#
#     # Return in HTML.
#     div(
#       as_html(tbl)
#     )
#   })
#
#   # Endpoint:
#   # Plot outputs.
#   output$mmrm_plot <- renderPlot({
#
#     # Input on output type.
#     output_function <- input$output_function
#
#     # Input on output features.
#     # nolint start
#     select <- input$g_mmrm_lsmeans_select
#     width <- input$g_mmrm_lsmeans_width
#     show_pval <- input$g_mmrm_lsmeans_contrasts_show_pval
#     type <- input$g_mmrm_diagnostic_type
#     z_threshold <- input$g_mmrm_diagnostic_z_threshold
#     # nolint end
#
#     # Stop here if the output is not a plot.
#     if (!isTRUE(grepl("^g_", output_function))) return(NULL)
#
#     # Reset global chunks.
#     chunks_reset()
#
#     # Get the fit stack while evaluating the fit code at the same time.
#     fit_stack <- mmrm_fit()
#     fit <- chunks_get_var("fit", chunks = fit_stack)
#
#     # Start new private stack for the plot code.
#     plot_stack <- chunks$new()
#     plot_stack_push <- function(...) {
#       chunks_push(..., chunks = plot_stack)
#     }
#
#     # Depending on the plot function type, produce different code.
#     if (output_function == "g_mmrm_lsmeans") {
#       plot_stack_push(bquote({
#         plt <- g_mmrm_lsmeans(
#           fit,
#           select = .(select),
#           width = .(width),
#           show_pval = .(show_pval)
#         )
#       }))
#     } else if (output_function == "g_mmrm_diagnostic") {
#       plot_stack_push(bquote({
#         plt <- g_mmrm_diagnostic(
#           fit,
#           type = .(type),
#           z_threshold = .(z_threshold)
#         )
#       }))
#     }
#
#     # Get nice R code to show to the user.
#     chunks_push_chunks(fit_stack)
#     chunks_push_chunks(plot_stack)
#
#     # Get our plot as ggplot2 object.
#     chunks_safe_eval()
#     plt <- chunks_get_var("plt")
#
#     # Just so that the shown R code displays the plot at the end.
#     chunks_push(bquote(show(plt)))
#
#     # Return the plot.
#     plt
#   })
#
#   # Endpoint:
#   # Optimizer that was selected.
#   output$optimizer_selected <- renderText({
#     # First reassign reactive sources:
#
#     fit_stack <- mmrm_fit()  # Code creation.
#     fit <- chunks_get_var("fit", chunks = fit_stack)  # Extract variable.
#
#     # Inputs.
#     optimizer <- input$optimizer
#
#     result <- if (!inherits(fit, "try-error") && optimizer == "automatic") {
#       selected <- attr(fit$fit, "optimizer")
#       paste("Optimizer used:", selected)
#     } else {
#       NULL
#     }
#     return(result)
#   })
#
#   # Handler:
#   # Show R code once button is pressed.
#   observeEvent(input$show_rcode, {
#     show_rcode_modal(
#       title = "MMRM Analysis",
#       rcode = get_rcode(
#         datasets = datasets,
#         datanames = union("ADSL", dataname),
#         title = label
#       )
#     )
#   })
# }
# nolint end
