#' @title Binary Outcome Table Teal Module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @inheritParams tm_t_rsp
#' @param show_rsp_categories (`flag`)\cr display the multinomial response
#'   estimations.
#' @return a [teal::module()] object
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(random.cdisc.data)
#' adsl <- radsl(cached = TRUE)
#' adrs <- radrs(cached = TRUE)
#' arm_ref_comp <- list(
#'   ARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
#'   ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
#' )
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADRS", adrs),
#'     code =
#'       "ADSL <- radsl(cached = TRUE)
#'     ADRS <- radrs(cached = TRUE)"
#'   ),
#'   modules = root_modules(
#'     tm_t_binary_outcome(
#'       label = "Binary responses",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARMCD"),
#'       paramcd = choices_selected(levels(adrs$PARAMCD), "BESRSPI"),
#'       arm_ref_comp = NULL,
#'       strata_var = choices_selected(
#'         choices = variable_choices(adsl, subset = c("STRATA1", "SEX")),
#'         selected = "STRATA1"
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_binary_outcome <- function(label,
                                dataname,
                                arm_var,
                                arm_ref_comp = NULL,
                                paramcd,
                                strata_var,
                                show_rsp_categories = TRUE,
                                pre_output = NULL,
                                post_output = NULL
) {

  args <- c(as.list(environment()))
  module(
    label = label,
    ui = ui_t_binary_outcome,
    ui_args = args,
    server = srv_t_binary_outcome,
    server_args = args,
    filters = dataname
  )
}

ui_t_binary_outcome <- function(id,
                                datasets,
                                dataname,
                                ...) {
  args <- list(...)
  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      verbatimTextOutput(ns("text")),
      uiOutput(ns("as_html"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(
        ns("paramcd"),
        "PARAMCD",
        choices = args$paramcd$choices,
        selected = args$paramcd$selected,
        multiple = FALSE,
        fixed = args$paramcd$fixed,
        label_help = helpText("Select one type of response to analyze.")
      ),
      selectInput(
        ns("responders"),
        "Responders",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      optionalSelectInput(
        ns("arm_var"), "Arm Variable",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected, multiple = FALSE,
        fixed = args$arm_var$fixed
      ),
      div(
        class = "arm-comp-box",
        tags$label("Compare Arms"),
        shinyWidgets::switchInput(
          inputId = ns("compare_arms"),
          value = !is.null(args$arm_ref_comp),
          size = "mini"
        ),
        conditionalPanel(
          condition = paste0("input['", ns("compare_arms"), "']"),
          selectInput(
            ns("ref_arm"),
            "Reference Group",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
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
      ),
      conditionalPanel(
        condition = paste0("input['", ns("compare_arms"), "']"),
        panel_group(
          panel_item(
            "Unstratified analysis settings",
            optionalSelectInput(
              ns("u_diff_ci"),
              label = "Method for Difference of Proportions CI",
              choices = c(
                "Wald, without correction" = "wald",
                "Wald, with correction" = "waldcc",
                "Anderson-Hauck" = "ha",
                "Newcombe" = "newcombe"
              ),
              selected = "waldcc",
              multiple = FALSE,
              fixed = FALSE
            ),
            optionalSelectInput(
              ns("u_diff_test"),
              label = "Method for Difference of Proportions Test",
              choices = c(
                "Chi-squared Test" = "chisq",
                "Fisher's Exact Test" = "fisher",
                "Chi-Squared Test with Schouten correction" = "schouten"
              ),
              selected = "chisq",
              multiple = FALSE,
              fixed = FALSE
            ),
            tags$label("Odds Ratio Estimation"),
            shinyWidgets::switchInput(
              inputId = ns("u_odds_ratio"), value = TRUE, size = "mini"
            )
          )
        ),
        panel_group(
          panel_item(
            "Stratified analysis settings",
            optionalSelectInput(
              ns("strata_var"),
              "Stratification Factors",
              choices = args$strata_var$choices,
              selected = args$strata_var$selected,
              multiple = FALSE,
              label_help = helpText("taken from:", tags$code("ADSL")),
              fixed = args$strata_var$fixed
            ),
            optionalSelectInput(
              ns("s_diff_ci"),
              label = "Method for Difference of Proportions CI",
              choices = c("CMH, without correction" = "cmh"),
              selected = "cmh",
              multiple = FALSE,
              fixed = TRUE
            ),
            optionalSelectInput(
              ns("s_diff_test"),
              label = "Method for Difference of Proportions Test",
              choices = c("CMH Test" = "cmh"),
              selected = "cmh",
              multiple = FALSE,
              fixed = TRUE
            )
          )
        )
      ),
      panel_item(
        "Additional table settings",
        optionalSelectInput(
          inputId = ns("prop_ci_method"),
          label = "Method for Proportion CI",
          choices = c(
            "Wald, without correction" = "wald",
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
          inputId = ns("show_rsp_cat"),
          value = args$show_rsp_categories,
          size = "mini"
        )
      )
    ),
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @noRd
srv_t_binary_outcome <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 label,
                                 arm_ref_comp,
                                 ...) {
  init_chunks()
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = "arm_var",
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_rsp"
  )
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

  # Prepare the analysis environment (filter data, check data, populate envir).
  prepared_env <- reactive({

    adsl_filtered <- datasets$get_data("ADSL", filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input$arm_var, input$strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", "AVAL", "AVALC"),
      arm_var = input$arm_var
    )

    if (
      length(input$arm_var) > 0 &&
      length(unique(adsl_filtered[[input$arm_var]])) == 1
    ) {
      validate_args <- append(validate_args, list(min_n_levels_armvar = NULL))
      if (input$compare_arms) {
        validate_args <- append(validate_args, list(ref_arm = input$ref_arm))
      }
    } else {
      if (input$compare_arms) {
        validate_args <- append(
          validate_args,
          list(ref_arm = input$ref_arm, comp_arm = input$comp_arm)
        )
      }
    }

    do.call(what = "validate_standard_inputs", validate_args)

    validate_in(
      input$responders, anl_filtered$AVALC, "responder values do not exist"
    )
    validate(
      need(is.factor(anl_filtered$AVALC), "need AVALC to be a factor"),
      need(is.logical(input$combine_comp_arms), "need combine arm information"),
      need(is.logical(input$show_rsp_cat), "show_rsp_categories is not logical")
    )

    if (input$show_rsp_cat) {
      rsp_categories <- unique(
        anl_filtered$AVALC[anl_filtered$PARAMCD == input$paramcd]
      )
      rsp_categories[trimws(rsp_categories) == ""] <- NA
      validate(
        need(
          all(!is.na(unique(rsp_categories))),
          paste(
            "there is missing value in AVALC for the selected endpoint",
            input$paramcd
          )
        )
      )
    }

    # Send data where the analysis lives.
    e <- new.env()
    e$ADSL_FILTERED <- adsl_filtered # nolint
    assign(x = paste0(dataname, "_FILTERED"), value = anl_filtered, env = e)
    e

  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_rsp(
      dataname = paste0(dataname, "_FILTERED"),
      param = input$paramcd,
      arm_var = input$arm_var,
      compare_arm = input$compare_arms,
      ref_arm = input$ref_arm,
      show_rsp_cat = input$show_rsp_cat,
      control = list(
        global = list(
          method = input$prop_ci_method,
          conf_level = as.numeric(input$conf_level)
        ),
        unstrat = list(
          method_ci = input$u_diff_ci,
          method_test = input$u_diff_test,
          odds = input$u_odds_ratio
        ),
        strat = list(
          method_test = input$s_diff_test,
          strat = input$strata_var
        )
      )
    )
    mapply(expression = my_calls, chunks_push)
  })
  # Outputs to render.
  output$as_html <- renderUI({
    call_preparation()
    chunks_safe_eval()
    as_html(chunks_get_var("result"))
  })
  # Render R code.
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Response",
      rcode = get_rcode(datasets = datasets, title = label)
    )
  })
}
