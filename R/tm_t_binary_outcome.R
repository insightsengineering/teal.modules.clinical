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
#' #'
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
#'       label = "Responders",
#'       dataname = "ADRS",
#'       paramcd = choices_selected(
#'         choices = value_choices(ADRS, "PARAMCD", "PARAM"),
#'         selected = "BESRSPI"
#'       ),
#'       arm_var = choices_selected(
#'         choices = variable_choices(ADRS, c("ARM", "ARMCD", "ACTARMCD")),
#'         selected = "ARM"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       strata_var = choices_selected(
#'         choices = variable_choices(ADRS, c("SEX", "BMRKR2")),
#'         select = NULL
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
                                parent_name = ifelse(
                                  test = is(arm_var, "data_extract_spec"),
                                  yes = datanames_input(arm_var),
                                  no = "ADSL"
                                ),
                                arm_var,
                                arm_ref_comp = NULL,
                                paramcd,
                                strata_var,
                                avalc_var = choices_selected(variable_choices(dataname, "AVALC"), "AVALC", fixed = TRUE),
                                pre_output = NULL,
                                post_output = NULL
) {

  stopifnot(
    length(dataname) == 1,
    length(parent_name) == 1,
    is.cs_or_des(arm_var),
    is.cs_or_des(paramcd),
    is.cs_or_des(avalc_var),
    is.cs_or_des(strata_var)
  )

  # Convert choices-selected to data_extract_spec
  if (is.choices_selected(arm_var)) {
    arm_var <- cs_to_des_select(arm_var, dataname = parent_name, multiple = FALSE)
  }
  if (is.choices_selected(paramcd)) {
    paramcd <- cs_to_des_filter(paramcd, dataname = dataname, multiple = FALSE)
  }
  if (is.choices_selected(avalc_var)) {
    avalc_var <- cs_to_des_select(avalc_var, dataname = dataname, multiple = FALSE)
  }
  if (is.choices_selected(strata_var)) {
    strata_var <- cs_to_des_select(strata_var, dataname = parent_name, multiple = TRUE)
  }
  args <- as.list(environment())

  data_extract_list <- list(
    arm = arm_var,
    paramcd = paramcd,
    avalc = avalc_var,
    strata = strata_var
  )

  module(
    label = label,
    ui = ui_t_binary_outcome,
    ui_args = args,
    server = srv_t_binary_outcome,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parent_name = parent_name,
        arm_ref_comp = arm_ref_comp,
        label = label
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

ui_t_binary_outcome <- function(id, ...) {
  a <- list(...)
  is_single_dataset_value <- is_single_dataset(
    a$paramcd, a$arm_var, a$avalc_var, a$strata_var
  )
  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      verbatimTextOutput(ns("text")),
      uiOutput(ns("as_html"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("paramcd")]),
      data_extract_input(
        id = ns("paramcd"),
        label = "Parameter",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      selectInput(
        ns("responders"),
        "Responders",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      data_extract_input(
        id = ns("arm_var"),
        label = "Arm Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      div(
        class = "arm-comp-box",
        tags$label("Compare Arms"),
        shinyWidgets::switchInput(
          inputId = ns("compare_arms"),
          value = !is.null(a$arm_ref_comp),
          size = "mini"
        ),
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
            data_extract_input(
              id = ns("strata_var"),
              label = "Stratification Factors",
              data_extract_spec = a$strata_var,
              is_single_dataset = is_single_dataset_value
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
          value = a$show_rsp_categories,
          size = "mini"
        )
      ),
      data_extract_input(
        id = ns("avalc"),
        label = "Analysis Variable",
        data_extract_spec = a$avalc_var,
        is_single_dataset = is_single_dataset_value
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

#' @noRd
srv_t_binary_outcome <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 parent_name,
                                 paramcd,
                                 avalc,
                                 arm,
                                 arm_ref_comp,
                                 strata,
                                 label) {
  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm", # from UI
    id_comp = "comp_arm", # from UI
    id_arm_var = paste0("arm_var-dataset_", parent_name, "_singleextract-select"), # from UI
    datasets = datasets,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte",
    on_off = reactive(input$compare_arms)
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm, paramcd, strata, avalc),
    input_id = c("arm_var", "paramcd", "strata_var", "avalc"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm, strata),
    input_id = c("arm_var", "strata_var"),
    anl_name = "ANL_ADSL"
  )

  # Because the AVALC values depends on the selected PARAMCD.
  observe({
    avalc_var <- anl_merged()$columns_source$avalc
    responder_choices <- unique(anl_merged()$data()[[avalc_var]])
    updateSelectInput(
      session, "responders",
      choices = responder_choices,
      selected = intersect(c("CR", "PR"), responder_choices)
    )
  })

  validate_check <- reactive({
    adsl_filtered <- datasets$get_data(parent_name, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)
    input_avalc_var <- as.vector(anl_m$columns_source$avalc_var)

    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", "PARAMCD", input_avalc_var),
      arm_var = input_arm_var
    )

    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- c(validate_args, list(min_n_levels_armvar = NULL))
      if (input$compare_arms) {
        validate_args <- c(validate_args, list(ref_arm = input$ref_arm))
      }
    } else if (input$compare_arms) {
      validate_args <- c(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
    }

    do.call(what = "validate_standard_inputs", validate_args)

    NULL
  })

  call_preparation <- reactive({
    validate_check()
    chunks_reset()

    anl_m <- anl_merged()
    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    anl <- chunks_get_var("ANL") # nolint
    validate_has_data(anl, 10)
    my_calls <- template_rsp(
      dataname = "ANL",
      arm_var = as.vector(anl_m$columns_source$arm),
      compare_arm = input$compare_arms,
      combine_arm = input$combine_comp_arms,
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
