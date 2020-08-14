#' @title Binary Outcome Table Teal Module
#'
#' @inheritParams tm_t_tte
#' @param  show_rsp_categories (\code{logical}) If \code{TRUE}, table includes a section for
#'  for each response category observed in the dataset for variable \code{AVALC}.
#'
#' @details This module produces a response summary table that is similar to
#'   STREAM template \code{rspt01}. The core functionality is based on
#'   \code{\link[tern]{t_binary_outcome}} from the \code{tern} package.
#'
#' @return an \code{\link[teal]{module}} object
#'
#' @export
#'
#' @examples
#' library(teal.modules.clinical)
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL$Dum_ARM <- factor(rep("Single ARM", nrow(ADSL)))
#' ADRS <- radrs(ADSL, cached = TRUE) %>%
#'   dplyr::filter(PARAMCD %in% c("BESRSPI", "INVET")) %>%
#'   droplevels()
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = 'ADSL <- radsl(cached = TRUE)
#'             ADSL$Dum_ARM <- factor(rep("Single ARM", nrow(ADSL)))
#'             ADRS <- radrs(ADSL, cached = TRUE) %>%
#'               dplyr::filter(PARAMCD %in% c("BESRSPI", "INVET")) %>%
#'               droplevels()',
#'      check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_t_binary_outcome(
#'       label = "Response Table",
#'       dataname = 'ADRS',
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADSL, subset = c("ARMCD", "ARM", "Dum_ARM")),
#'         selected = "ARMCD"),
#'       paramcd = choices_selected(
#'         choices = value_choices(ADRS, "PARAMCD", "PARAM"),
#'         selected = "BESRSPI"),
#'       strata_var = choices_selected(
#'         choices = variable_choices(ADSL, subset = c("STRATA1", "STRATA2")),
#'         selected = NULL)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' @importFrom methods substituteDirect
tm_t_binary_outcome <- function(label,
                                dataname,
                                arm_var,
                                arm_ref_comp = NULL,
                                paramcd,
                                strata_var,
                                show_rsp_categories = TRUE,
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
    server = srv_t_binary_outcome,
    ui = ui_t_binary_outcome,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      arm_ref_comp = arm_ref_comp
    ),
    filters = dataname
  )

}


#' UI part for binary outcome table teal module
#'
#' @importFrom shinyWidgets switchInput
#' @noRd
#'
ui_t_binary_outcome <- function(id, ...) {

  ns <- NS(id)

  a <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("binary_outcome_table"))),
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
      div(
        class = "arm-comp-box",
        tags$label("Compare Arms"),
        shinyWidgets::switchInput(inputId = ns("compare_arms"), value = !is.null(a$arm_ref_comp), size = "mini"),

        conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          div(
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
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        panel_group(
          panel_item(
            "Unstratified analysis settings",
            optionalSelectInput(
              ns("u_diff_ci"),
              label = "Method for Difference of Proportions CI",
              choices = c("Wald, without correction" = "wald",
                          "Wald, with correction" = "waldcc",
                          "Anderson-Hauck" = "anderson-hauck",
                          "Newcombe" = "newcombe"
              ),
              selected = "waldcc",
              multiple = FALSE,
              fixed = FALSE
            ),
            optionalSelectInput(
              ns("u_diff_test"),
              label = "Method for Difference of Proportions Test",
              choices = c("Chi-squared Test" = "chisq",
                          "Fisher's Exact Test" = "fisher",
                          "Chi-Squared Test with Schouten correction" = "schouten"),
              selected = "chisq",
              multiple = FALSE,
              fixed = FALSE
            ),
            tags$label("Odds Ratio Estimation"),
            shinyWidgets::switchInput(inputId = ns("u_odds_ratio"), value = TRUE, size = "mini")
          )
        ),
        panel_group(
          panel_item(
            "Stratified analysis settings",
            optionalSelectInput(
              ns("strata_var"),
              "Stratification Factors",
              choices = a$strata_var$choices,
              selected = a$strata_var$selected,
              multiple = TRUE,
              label_help = helpText("taken from:", tags$code("ADSL")),
              fixed = a$strata_var$fixed
            ),
            optionalSelectInput(
              ns("s_diff_ci"),
              label = "Method for Difference of Proportions CI",
              choices = c("Wald" = "wald",
                          "Wald, with correction" = "waldcc",
                          "CMH, without correction" = "cmh",
                          "Anderson-Hauck" = "anderson-hauck",
                          "Newcombe" = "newcombe"
              ),
              selected = "waldcc",
              multiple = FALSE,
              fixed = FALSE
            ),
            optionalSelectInput(
              ns("s_diff_test"),
              label = "Method for Difference of Proportions Test",
              choices = c("CMH Test" = "cmh"),
              selected = "cmh",
              multiple = FALSE,
              fixed = FALSE
            ),
            tags$label("Odds Ratio Estimation"),
            shinyWidgets::switchInput(inputId = ns("s_odds_ratio"), value = TRUE, size = "mini")
          )
        )
      ),
      panel_item(
        "Additional table settings",
        optionalSelectInput(
          inputId = ns("prop_ci_method"),
          label = "Method for Proportion CI",
          choices = c("Wald, without correction" = "wald",
                      "Wald, with correction" = "waldcc",
                      "Clopper-Pearson" = "clopper-pearson",
                      "Wilson" = "wilson",
                      "Jeffreys" = "jeffreys",
                      "Agresti-Coull" = "agresti-coull"
          ),
          selected = "waldcc",
          multiple = FALSE,
          fixed = FALSE
        ),
        numericInput(
          inputId = ns("conf_level"),
          label = "Confidence Level",
          value = 0.95,
          min = 0.01,
          max = 0.99,
          step = 0.01,
          width = "100%"
        ),
        tags$label("Show All Reponse Categories"),
        shinyWidgets::switchInput(
          inputId = ns("show_rsp_categories"),
          value = a$show_rsp_categories,
          size = "mini"
        )
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


#' Server part for binary outcome table teal module
#'
#' @importFrom rtables as_html
#' @importFrom methods substituteDirect
#' @noRd
#'
srv_t_binary_outcome <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 arm_ref_comp) {
  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel.
  arm_ref_comp_observer(
    session,
    input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    adsl = datasets$get_data("ADSL", filtered = FALSE),
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_binary_outcome",
    on_off = reactive(input$compare_arms)
  )

  # Update UI choices depending on selection of previous options.
  observe({
    anl <- datasets$get_data(dataname, filtered = FALSE)
    paramcd <- input$paramcd

    responder_choices <- unique(anl$AVALC[anl$PARAMCD == paramcd])

    updateSelectInput(
      session, "responders",
      choices = responder_choices,
      selected = intersect(c("CR", "PR", "Y"), responder_choices)
    )
  })

  tm_t_binary_outcome_call <- reactive({
    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    paramcd <- input$paramcd
    responders <- input$responders

    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_comp_arms <- input$combine_comp_arms
    compare_arms <- input$compare_arms

    strata_var <- input$strata_var
    conf_level <- input$conf_level #nolint
    prop_ci_method <- input$prop_ci_method #nolint
    u_analysis <- tern::control_binary_comparison(
      diff_ci = input$u_diff_ci,
      diff_test = input$u_diff_test,
      odds_ratio = input$u_odds_ratio
    )
    s_analysis <- tern::control_binary_comparison(
      diff_ci = input$s_diff_ci,
      diff_test = input$s_diff_test,
      odds_ratio = input$s_odds_ratio
    )
    show_rsp_categories <- input$show_rsp_categories


    if (length(strata_var) == 0) {
      strata_var <- NULL
    }

    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", arm_var, strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "AVALC"),
      arm_var = arm_var
    )

    if (length(unique(adsl_filtered[[arm_var]])) == 1) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      if (compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = ref_arm))
      }
    } else {
      if (compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = ref_arm, comp_arm = comp_arm))
      }
    }

    do.call(what = "validate_standard_inputs", validate_args)

    validate_in(responders, anl_filtered$AVALC, "responder values do not exist")
    validate(need(is.factor(anl_filtered$AVALC), "need AVALC to be a factor"))
    validate(need(is.logical(combine_comp_arms), "need combine arm information"))
    validate(need(is.logical(show_rsp_categories), "show_rsp_categories is not logical"))

    if (show_rsp_categories) {
      rsp_categories <- unique(anl_filtered$AVALC[anl_filtered$PARAMCD == paramcd])
      rsp_categories[trimws(rsp_categories) == ""] <- NA
      validate(
        need(all(!is.na(unique(rsp_categories))),
             paste("there is missing value in AVALC for the selected endpoint", paramcd))
      )
    }

    # Perform analysis.
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

    #do not compare arms
    if (isFALSE(compare_arms) || length(unique(adsl_filtered[[arm_var]])) == 1) {

      chunks_push(
        bquote({
          anl <- merge(
            x = ADSL_FILTERED[, .(adsl_vars), drop = FALSE],
            y = anl_endpoint[, .(anl_vars), drop = FALSE],
            all.x = FALSE,
            all.y = FALSE,
            by = c("USUBJID", "STUDYID")
          )
        })
      )

      if (show_rsp_categories) {
        chunks_push(
          bquote({
            rsp_categories <- anl$AVALC
            rsp_categories <- droplevels(rsp_categories)
          })
        )
      } else {
        chunks_push(
          bquote({
            rsp_categories <- NULL
          })
        )
      }

      chunks_push(
        bquote({
          anl[[.(arm_var)]] <- as.factor(anl[[.(arm_var)]])
          anl[[.(arm_var)]] <- droplevels(anl[[.(arm_var)]])

          tbl <- tern::t_binary_outcome(
            rsp = anl$AVALC %in% .(responders),
            col_by = anl[[.(arm_var)]],
            unstrat_analysis = NULL,
            strat_analysis = NULL,
            conf_level = .(conf_level),
            prop_ci_method = .(prop_ci_method),
            rsp_multinomial = rsp_categories
          )
          tbl
        })
      )

    } else {
      # Compare arms.

      chunks_push(
        bquote({
          adsl_p <- subset(
            .(as.name(adsl_name)),
            .(as.name(arm_var)) %in% c(.(ref_arm), .(comp_arm))
          )
        })
      )

      chunks_push(
        bquote({
          anl <- merge(
            x = adsl_p[, .(adsl_vars), drop = FALSE],
            y = anl_endpoint[, .(anl_vars), drop = FALSE],
            all.x = FALSE,
            all.y = FALSE,
            by = c("USUBJID", "STUDYID")
          )
        })
      )

      if (show_rsp_categories) {
        chunks_push(
          bquote({
            rsp_categories <- anl$AVALC
            rsp_categories <- droplevels(rsp_categories)
          })
        )
      } else {
        chunks_push(
          bquote({
            rsp_categories <- NULL
          })
        )
      }

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

      chunks_push(
        bquote({
          tbl <- tern::t_binary_outcome(
            rsp = anl$AVALC %in% .(responders),
            col_by = anl[[.(arm_var)]],
            strata_data = .(strata_data),
            conf_level = .(conf_level),
            prop_ci_method = .(prop_ci_method),
            unstrat_analysis = .(u_analysis),
            strat_analysis = .(s_analysis),
            rsp_multinomial = rsp_categories
          )
          tbl
        })
      )
    }
    invisible(NULL)
  })


  output$binary_outcome_table <- renderUI({
    tm_t_binary_outcome_call()

    chunks_safe_eval()

    anl_endpoint <- chunks_get_var("anl_endpoint")
    if (any(duplicated(anl_endpoint[, c("USUBJID", "STUDYID")]))) {
      stop("only one row per patient expected")
    }

    anl <- chunks_get_var("anl")
    validate(need(nrow(anl) > 5, "need at least 5 data points"))
    validate(need(!any(duplicated(anl$USUBJID)), "patients have multiple records in the analysis data."))

    tbl <- chunks_get_var("tbl")
    validate(need(is(tbl, "rtable"), "Evaluation with tern t_binary_outcome failed."))

    as_html(tbl)
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Summary",
      rcode = get_rcode(
        datasets = datasets,
        datanames = union("ADSL", dataname),
        title = "Binary Outcome Table"
      )
    )
  })
}
