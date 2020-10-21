#' (Deprecated) Mix model with repeated measurements (\code{MMRM}) Table Teal Module
#'
#' \code{MMRM} as defined in \code{REFACTOR} in the
#' \code{tern} package
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param dataname (\code{character}) analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form where each subject has
#'   repeated measured at different timepoints. At each visit, there is only one
#'   record for each subject.
#' @param endpoint_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for numeric variables that can be used as \code{endpoint}
#' @param id_var \code{\link[teal]{choices_selected}} object specifying the variable name for subject id.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as \code{arm}
#' @param visit_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#' for variable names that can be used as visit. Must be factor and consecutive numbers if numeric in \code{dataname}.
#' @param arm_ref_comp (\code{\link[teal]{choices_selected}}) optional, if specified it must be a named list with each
#'   element corresponding to an arm variable in \code{ADSL} and the element must
#'   be another list with the elements named \code{ref} and \code{comp} that the
#'   defined the default reference and comparison arms when the arm variable is
#'   changed.
#' @param paramcd \code{\link[teal]{choices_selected}} object with all available choices and preselected option for
#'   variable names that can be used as \code{PARAMCD} variable
#' @param formula \code{\link[teal]{choices_selected}} object specifying the string type of options for module formula
#'   (regressors only).
#' @param mode \code{\link[teal]{choices_selected}} object specifying the algorithm for degree of freedom:
#'   \code{auto}, \code{df.error} or \code{boot-satterthwaite}.
#' @param conf_level \code{\link[teal]{choices_selected}} object specifying the confidence level. Greater than 0 and
#'   less than 1.
#' @param weights_emmeans \code{\link[teal]{choices_selected}} object specifying the \code{emmeans} weights:
#'   "proportional" or "equal".
#' @param cor_struct \code{\link[teal]{choices_selected}} object with \code{NULL} and other possible choices specifying
#'   the name of \code{\link[nlme]{corClasses}}.
#'
#' @section Warning:
#' This module has been deprecated and will eventually be removed. Please use instead
#' the module `tm_a_mmrm`.
#'
#' @details
#' This modules expects that the analysis data has the following variables
#'
#' \tabular{ll}{
#'  \code{PARAMCD} \tab variable used to filter for endpoint, after
#'  filtering for \code{paramcd} one observation per patient per time point is expected
#' }
#'
#' The arm, subject ID, baseline characteristics variables are taken from the \code{ADSL} data.
#'
#' @export
#' @importFrom stats terms
#'
#' @examples
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
#' arm_ref_comp = list(
#'   ARMCD = list(
#'     ref = "ARM A",
#'     comp = c("ARM B", "ARM C")
#'   )
#' )
#'
#' \dontrun{
#' app <- init(
#'     data = cdisc_data(
#'       cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'       cdisc_dataset("ADQS", ADQS,
#'         code = "ADQS <- radqs(cached = TRUE) %>%
#'                 dplyr::filter(ABLFL != 'Y' & ABLFL2 != 'Y') %>%
#'                 dplyr::mutate(
#'                   AVISIT = as.factor(AVISIT),
#'                   AVISITN = rank(AVISITN) %>%
#'                     as.factor() %>%
#'                     as.numeric() %>%
#'                     as.factor()
#'                 )"
#'       ),
#'       check = TRUE
#'     ),
#'     modules = root_modules(
#'         tm_t_mmrm(
#'             label = "MMRM",
#'             dataname = 'ADQS',
#'             endpoint_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
#'             id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
#'             arm_var = choices_selected(c("ARM", "ARMCD"), "ARMCD"),
#'             visit_var = choices_selected(c("AVISIT", "VISIT", "AVISITN"), "AVISIT"),
#'             arm_ref_comp = arm_ref_comp,
#'             paramcd = choices_selected(
#'               choices = value_choices(ADQS, "PARAMCD", "PARAM"),
#'               selected = "FKSI-FWB"
#'             ),
#'             formula = choices_selected(
#'               choices = c('BASE + AVISITN + ARMCD + ARMCD*AVISITN + SEX',
#'                           'BASE + AVISIT + ARMCD + ARMCD*AVISIT + SEX',
#'                           'BASE + AVISIT + ARM + ARM*AVISIT + SEX'),
#'               selected = 'BASE + AVISIT + ARMCD + ARMCD*AVISIT + SEX'),
#'             mode = choices_selected(
#'               choices = c("auto", "df.error", "boot-satterthwaite"),
#'               selected = "boot-satterthwaite"),
#'             conf_level = choices_selected(c("0.95", "0.9", "0.8"), "0.95"),
#'             weights_emmeans = choices_selected(c("proportional", "equal"), "proportional"),
#'             cor_struct = choices_selected(c("corSymm", "corAR1"), "corSymm")
#'         )
#'     )
#' )
#'
#' shinyApp(app$ui, app$server)
#' }
tm_t_mmrm <- function(label,
                      dataname,
                      endpoint_var,
                      id_var,
                      arm_var,
                      visit_var,
                      arm_ref_comp = NULL,
                      paramcd,
                      formula,
                      mode,
                      conf_level,
                      weights_emmeans,
                      cor_struct,
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
# tm_t_mmrm <- function(label,
#                       dataname,
#                       endpoint_var,
#                       id_var,
#                       arm_var,
#                       visit_var,
#                       arm_ref_comp = NULL,
#                       paramcd,
#                       formula,
#                       mode,
#                       conf_level,
#                       weights_emmeans,
#                       cor_struct,
#                       pre_output = NULL,
#                       post_output = NULL
# ) {
#
#
#   .Deprecated(
#     new = "tm_a_mmrm",
#     msg = paste(
#       "This module has been deprecated and will eventually be removed.",
#       "Please use instead the module `tm_a_mmrm`."
#     )
#   )
#
#   stopifnot(length(dataname) == 1)
#   stopifnot(is.choices_selected(endpoint_var))
#   stopifnot(is.choices_selected(id_var))
#   stopifnot(is.choices_selected(arm_var))
#   stopifnot(is.choices_selected(visit_var))
#   stopifnot(is.choices_selected(paramcd))
#   stopifnot(is.choices_selected(formula))
#   stopifnot(is.choices_selected(mode))
#   stopifnot(is.choices_selected(conf_level))
#   stopifnot(is.choices_selected(weights_emmeans))
#   stopifnot(is.choices_selected(cor_struct))
#
#
#   args <- as.list(environment())
#
#   module(
#     label = label,
#     server = srv_t_mmrm,
#     ui = ui_t_mmrm,
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
# ui_t_mmrm <- function(id, ...) {
#
#   a <- list(...) # module args
#
#   ns <- NS(id)
#
#   standard_layout(
#     output = white_small_well(uiOutput(ns("mmrm_table"))),
#     encoding = div(
#       tags$label("Encodings", class = "text-primary"),
#       helpText("Analysis data:", tags$code(a$dataname)),
#       optionalSelectInput(
#         ns("endpoint_var"),
#         "Select Endpoint",
#         a$endpoint_var$choices,
#         a$endpoint_var$selected,
#         multiple = FALSE,
#         fixed = a$endpoint_var$fixed
#       ),
#       optionalSelectInput(
#         ns("paramcd"),
#         "Select Parameter",
#         a$paramcd$choices,
#         a$paramcd$selected,
#         multiple = FALSE,
#         fixed = a$paramcd$fixed
#       ),
#       optionalSelectInput(
#         ns("formula"),
#         "Formula (regressors only)",
#         a$formula$choices,
#         a$formula$selected,
#         multiple = FALSE,
#         fixed = a$formula$fixed
#       ),
#       optionalSelectInput(
#         ns("arm_var"),
#         "Arm Variable (match the one in formula)",
#         a$arm_var$choices,
#         a$arm_var$selected,
#         multiple = FALSE,
#         fixed = a$arm_var$fixed
#       ),
#       optionalSelectInput(
#         ns("visit_var"),
#         "Visit Variable (match the one in formula)",
#         a$visit_var$choices,
#         a$visit_var$selected,
#         multiple = FALSE,
#         fixed = a$visit_var$fixed
#       ),
#       selectInput(
#         ns("ref_arm"),
#         "Reference Group",
#         choices = NULL,
#         selected = NULL,
#         multiple = TRUE),
#       helpText("Multiple reference groups are automatically combined into a single group."),
#       selectInput(
#         ns("comp_arm"),
#         "Comparison Group",
#         choices = NULL,
#         selected = NULL,
#         multiple = TRUE),
#       checkboxInput(
#         ns("combine_comp_arms"),
#         "Combine all comparison groups?",
#         value = FALSE),
#       optionalSelectInput(
#         ns("id_var"),
#         "Subject Identifier",
#         a$id_var$choices,
#         a$id_var$selected,
#         multiple = FALSE,
#         fixed = a$id_var$fixed
#       ),
#       optionalSelectInput(
#         ns("mode"),
#         "Mode",
#         a$mode$choices,
#         a$mode$selected,
#         multiple = FALSE,
#         fixed = a$mode$fixed
#       ),
#       optionalSelectInput(
#         ns("weights_emmeans"),
#         "Weights for emmeans",
#         a$weights_emmeans$choices,
#         a$weights_emmeans$selected,
#         multiple = FALSE,
#         fixed = a$weights_emmeans$fixed
#       ),
#       optionalSelectInput(
#         ns("cor_struct"),
#         "Correlation Structure",
#         a$cor_struct$choices,
#         a$cor_struct$selected,
#         multiple = FALSE,
#         fixed = a$cor_struct$fixed
#       ),
#       optionalSelectInput(
#         ns("conf_level"),
#         "Confidence Level",
#         a$conf_level$choices,
#         a$conf_level$selected,
#         multiple = FALSE,
#         fixed = a$conf_level$fixed
#       )
#     ),
#     forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
#     pre_output = a$pre_output,
#     post_output = a$post_output
#   )
# }
#
#
# srv_t_mmrm <- function(input,
#                        output,
#                        session,
#                        datasets,
#                        dataname,
#                        arm_ref_comp,
#                        label) {
#
#   init_chunks()
#
#   # Setup arm variable selection, default reference arms, and default
#   # comparison arms for encoding panel
#   arm_ref_comp_observer(
#     session, input,
#     id_ref = "ref_arm", id_comp = "comp_arm", id_arm_var = "arm_var",    # from UI
#     datasets = datasets,
#     arm_ref_comp = arm_ref_comp,
#     module = "tm_t_mmrm"
#   )
#
#   # Create output
#   table_reactive <- reactive({
#     # resolve all reactive expressions
#     # nolint start
#     adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
#     anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
#     # nolint end
#
#     endpoint_var <- input$endpoint_var
#     paramcd <- input$paramcd
#     arm_var <- input$arm_var
#     ref_arm <- input$ref_arm
#     comp_arm <- input$comp_arm
#     combine_comp_arms <- input$combine_comp_arms
#     visit_var <- input$visit_var
#     id_var <- input$id_var
#     mode <- input$mode # nolint
#     weights_emmeans <- input$weights_emmeans # nolint
#     cor_struct <- input$cor_struct # nolint
#     conf_level <- as.numeric(input$conf_level) # nolint
#
#     formula <- as.formula(
#       paste(endpoint_var, "~", input$formula)
#     )
#     mt <- terms(formula)
#     vars <- all.vars(attr(mt, "variables"))
#     regressor_vars <- vars[-1L]
#
#     validate(need(arm_var %in% regressor_vars, paste("Arm variable", arm_var, "does not exist in formula")))
#     validate(need(visit_var %in% regressor_vars, paste("Visit variable", visit_var, "does not exist in formula")))
#
#     # validate your input values
#     validate_has_data(adsl_filtered, 1)
#     validate_has_data(anl_filtered, 1)
#
#     regressor_vars_in_adsl <- intersect(regressor_vars, colnames(adsl_filtered))
#     regressor_vars_in_anl <- setdiff(regressor_vars, regressor_vars_in_adsl)
#
#
#     validate_standard_inputs(
#       adsl = adsl_filtered,
#       adslvars = unique(c("USUBJID", "STUDYID", arm_var, id_var, regressor_vars_in_adsl)),
#       anl = anl_filtered,
#       anlvars = unique(c("USUBJID", "STUDYID", "PARAMCD", endpoint_var, visit_var, regressor_vars_in_anl)),
#       arm_var = arm_var,
#       ref_arm = ref_arm,
#       comp_arm = comp_arm
#     )
#
#     validate(need(is.logical(combine_comp_arms), "need combine arm information"))
#
#     # do analysis
#
#     anl_name <- paste0(dataname, "_FILTERED")
#     assign(anl_name, anl_filtered)
#     adsl_name <- "ADSL_FILTERED"
#     assign(adsl_name, adsl_filtered)
#
#     chunks_reset(envir = environment())
#
#     adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, id_var, regressor_vars_in_adsl))
#     anl_vars <- unique(c("USUBJID", "STUDYID", endpoint_var, visit_var, regressor_vars_in_anl))
#
#     ## Now comes the analysis code
#     chunks_push(bquote(ref_arm <- .(ref_arm)))
#     chunks_push(bquote(comp_arm <- .(comp_arm)))
#     chunks_push(bquote(combine_comp_arms <- .(combine_comp_arms)))
#
#
#     # Select only adsl_vars from as.name(adsl_name)
#     chunks_push(
#       call(
#         "<-",
#         as.name("ADSL_P"),
#         Reduce(
#           function(x, y) call("%>%", x, y),
#           c(
#             as.name(adsl_name),
#             #filter ARM
#             as.call(append(
#               quote(dplyr::filter),
#               call("%in%", as.name(arm_var), c(ref_arm, comp_arm))
#             )),
#
#             #Select variables
#             as.call(append(
#               quote(dplyr::select),
#               lapply(adsl_vars, as.name)
#             ))
#           )
#         )
#       )
#     )
#
#     chunks_safe_eval()
#     chunks_push(bquote(arm <- relevel(as.factor(ADSL_P[[.(arm_var)]]), ref_arm[1])))
#     chunks_push(bquote(arm <- combine_levels(arm, ref_arm)))
#
#     if (combine_comp_arms) {
#       chunks_push(bquote(arm <- combine_levels(arm, comp_arm)))
#     }
#
#     chunks_push(bquote(ADSL_P[[.(arm_var)]] <- droplevels(arm)))
#
#
#     # Create ANL_ENDPOINT
#     chunks_push(
#       call(
#         "<-",
#         as.name("ANL_ENDPOINT"),
#         Reduce(
#           function(x, y) call("%>%", x, y),
#           c(
#             as.name(anl_name),
#             #filter PARAMCD
#             as.call(append(
#               quote(dplyr::filter),
#               call("==", as.name("PARAMCD"), paramcd)
#             )),
#
#             #Select variables
#             as.call(append(
#               quote(dplyr::select),
#               lapply(anl_vars, as.name)
#             ))
#           )
#         )
#       )
#     )
#
#
#     # Merge ANL_ENDPOINT and ADSL_P, relable original datasets labels
#     chunks_push(
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
#     chunks_safe_eval()
#
#
#     validate(need(nrow(chunks_get_var("ANL")) > 5, "need at least 5 data points"))
#     Map(function(visit_df, visit_name) {
#
#       dup <- any(duplicated(visit_df[[id_var]]))
#
#       validate(need(!dup, paste("Duplicated subject ID found at", visit_name)))
#
#     }, split(chunks_get_var("ANL"), chunks_get_var("ANL")[[visit_var]]),
#     levels(chunks_get_var("ANL")[[visit_var]]))
#
#     validate(need(
#       all(complete.cases(chunks_get_var("ANL"))),
#       paste(c("Missing values found in formula vars",
#               chunks_get_var("ANL")[!complete.cases(chunks_get_var("ANL")), ]))
#     ))
#
#     chunks_push(bquote({
#       tbl <- t_mmrm(
#         formula = .(formula),
#         data = ANL,
#         id_var = .(id_var),
#         arm_var = .(arm_var),
#         visit_var = .(visit_var),
#         col_N = table(ADSL_P[[.(arm_var)]] %>% droplevels()),
#         mode = .(mode),
#         conf_level = .(conf_level),
#         weights_emmeans = .(weights_emmeans),
#         corStruct = .(cor_struct),
#         table_tree = FALSE
#       )
#       tbl
#     }))
#   })
#
#   output$mmrm_table <- renderUI({
#     table_reactive()
#
#     chunks_safe_eval()
#
#     tbl <- chunks_get_var("tbl")
#
#     div(
#       tags$p("MMRM methodology in R is different from SAS. Please use for exploratory purpose."),
#       as_html(tbl)
#     )
#   })
#
#
#   observeEvent(input$show_rcode, {
#     show_rcode_modal(
#       title = "MMRM Table",
#       rcode = get_rcode(
#         datasets = datasets,
#         datanames = dataname,
#         title = label
#       )
#     )
#   })
#
# }
# nolint end
