#' @title Teal Module: Binary Outcome Table
#'
#' @inheritParams module_arguments
#' @inheritParams tm_t_rsp
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADRS <- synthetic_cdisc_data("latest")$adrs
#' arm_ref_comp <- list(
#'   ARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
#'   ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
#' )
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code =
#'       "ADSL <- synthetic_cdisc_data('latest')$adsl
#'        ADRS <- synthetic_cdisc_data('latest')$adrs"
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
                                parentname = ifelse(
                                  test = is(arm_var, "data_extract_spec"),
                                  yes = datanames_input(arm_var),
                                  no = "ADSL"
                                ),
                                arm_var,
                                arm_ref_comp = NULL,
                                paramcd,
                                strata_var,
                                aval_var = choices_selected(
                                  choices = variable_choices(dataname, c("AVALC", "SEX")),
                                  selected = "AVALC", fixed = FALSE),
                                conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                                default_responses =
                                  c("CR", "PR", "Y", "Complete Response (CR)", "Partial Response (PR)"),
                                add_total = FALSE,
                                pre_output = NULL,
                                post_output = NULL) {
  logger::log_info("Initializing tm_t_binary_outcome")
  stop_if_not(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(parentname),
    is.choices_selected(conf_level),
    is.flag(add_total),
    list(
      is.null(pre_output) || is(pre_output, "shiny.tag"),
      "pre_output should be either null or shiny.tag type of object"
    ),
    list(
      is.null(post_output) || is(post_output, "shiny.tag"),
      "post_output should be either null or shiny.tag type of object"
    )
  )

  assert_that(
    is.list(default_responses) ||
      is.null(default_responses) ||
      is.character(default_responses) ||
      is.numeric(default_responses),
    msg = "`default_responses` must be a named list or an array."
  )

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE)
  )

  module(
    label = label,
    ui = ui_t_binary_outcome,
    ui_args = c(data_extract_list, args),
    server = srv_t_binary_outcome,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        default_responses = default_responses
      )
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @importFrom shinyWidgets switchInput
ui_t_binary_outcome <- function(id, ...) {

  a <- list(...)
  is_single_dataset_value <- is_single_dataset(
    a$paramcd,
    a$arm_var,
    a$aval_var,
    a$strata_var
  )

  ns <- NS(id)
  standard_layout(
    output = white_small_well(table_with_settings_ui(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(a[c("paramcd", "arm_var", "aval_var", "strata_var")]),
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
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      div(
        class = "arm-comp-box",
        tags$label("Compare Treatments"),
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
      conditionalPanel(
        condition = paste0("!input['", ns("compare_arms"), "']"),
        checkboxInput(ns("add_total"), "Add All Patients column", value = a$add_total)
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
        optionalSelectInput(
          inputId = ns("conf_level"),
          label = "Confidence Level",
          a$conf_level$choices,
          a$conf_level$selected,
          multiple = FALSE,
          fixed = a$conf_level$fixed
        ),
        tags$label("Show All Response Categories"),
        shinyWidgets::switchInput(
          inputId = ns("show_rsp_cat"),
          value = a$show_rsp_categories,
          size = "mini"
        )
      ),
      data_extract_input(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
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
                                 parentname,
                                 paramcd,
                                 aval_var,
                                 arm_var,
                                 arm_ref_comp,
                                 strata_var,
                                 add_total,
                                 label,
                                 default_responses) {
  stopifnot(is_cdisc_data(datasets))

  init_chunks()

  # Setup arm variable selection, default reference arms, and default
  # comparison arms for encoding panel
  arm_ref_comp_observer(
    session, input,
    id_ref = "ref_arm",
    id_comp = "comp_arm",
    id_arm_var = extract_input("arm_var", parentname),
    datasets = datasets,
    dataname = parentname,
    arm_ref_comp = arm_ref_comp,
    module = "tm_t_tte",
    on_off = reactive(input$compare_arms)
  )

  anl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, paramcd, strata_var, aval_var),
    input_id = c("arm_var", "paramcd", "strata_var", "aval_var"),
    merge_function = "dplyr::inner_join"
  )

  adsl_merged <- data_merge_module(
    datasets = datasets,
    data_extract = list(arm_var, strata_var),
    input_id = c("arm_var", "strata_var"),
    anl_name = "ANL_ADSL"
  )

  observeEvent(
    c(input[[extract_input("aval_var", "ADRS")]],
      input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]]), {
        aval_var <- anl_merged()$columns_source$aval_var
        sel_param <- if (is.list(default_responses)) {
         default_responses[[input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]]]]
        } else default_responses
        common_rsp <- if (is.list(default_responses)) {
         if (is.list(sel_param)) {
           sel_param$rsp
         } else {
           sel_param
         }
        } else c("CR", "PR", "Y", "Complete Response (CR)", "Partial Response (PR)")
        responder_choices <- if (is_empty(aval_var)) {
         character(0)
        } else {
          if ("levels" %in% names(sel_param)) {
           if (length(intersect(unique(anl_merged()$data()[[aval_var]]), sel_param$levels)) > 1) {
             sel_param$levels
           }
          } else unique(anl_merged()$data()[[aval_var]])
        }
        updateSelectInput(
          session,
          "responders",
          choices = responder_choices,
          selected = intersect(responder_choices, common_rsp)
        )
    }, once = FALSE, ignoreInit = TRUE)

  # Because the AVALC values depends on the selected PARAMCD.
  observeEvent(anl_merged(), {
    aval_var <- anl_merged()$columns_source$aval_var
    responder_choices <- if (is_empty(aval_var)) {
      character(0)
    } else {
      unique(anl_merged()$data()[[aval_var]])
    }
    common_rsp <- c("CR", "PR", "Y", "Complete Response (CR)", "Partial Response (PR)")
    updateSelectInput(
      session, "responders",
      choices = responder_choices,
      selected = intersect(responder_choices, common_rsp)
    )
  }, once = TRUE, ignoreInit = TRUE)

  validate_check <- reactive({
    adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)

    anl_m <- anl_merged()
    input_arm_var <- as.vector(anl_m$columns_source$arm_var)
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    input_paramcd <- unlist(paramcd$filter)["vars_selected"]

    validate_args <- list(
      adsl = adsl_filtered,
      adslvars = c("USUBJID", "STUDYID", input_arm_var, input_strata_var),
      anl = anl_filtered,
      anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var),
      arm_var = input_arm_var
    )

    if (length(input_arm_var) > 0 && length(unique(adsl_filtered[[input_arm_var]])) == 1) {
      validate_args <- c(validate_args, list(min_n_levels_armvar = NULL))
    }
    if (input$compare_arms) {
      validate_args <- c(validate_args, list(ref_arm = input$ref_arm, comp_arm = input$comp_arm))
    }

    do.call(what = "validate_standard_inputs", validate_args)

    validate_one_row_per_id(anl_m$data(), key = c("USUBJID", "STUDYID", input_paramcd))

    validate(
      if (length(input_strata_var) >= 1L) {
        need(
          sum(
            vapply(
              anl_m$data()[input_strata_var],
              FUN = function(x) {
                length(unique(x)) > 1
              },
              logical(1)
            )
          ) > 0,
          "At least one strata variable must have more than one non-empty level after filtering."
        )
      }
    )

    validate(
      if (length(input_strata_var) >= 1L) {
        need(
          sum(summary(
            anl_merged()$data()$ARM[!anl_merged()$data()[[input_aval_var]] %in% input$responders]
            ) > 0) > 1L,
            "After filtering at least one combination of strata variable levels
            has too few observations to calculate the odds ratio.")
      }
    )

    validate(
      need(is_character_single(input_aval_var), "Analysis variable should be a single column."),
      need(input$responders, "`Responders` field is empty"))

    validate(
      need(all(unlist(lapply(default_responses, function(x) {
        lvls <- if (is.list(x)) x$levels else NULL
        if (is.null(lvls)) {
          all(unlist(x) %in% levels(unlist(anl_filtered[c(aval_var$select$choices)])))
        } else TRUE}))),
        "All selected default responses must be in AVAL")
    )

    validate(need(
      input$conf_level >= 0 && input$conf_level <= 1,
      "Please choose a confidence level between 0 and 1")
    )

    NULL
  })

  call_preparation <- reactive({
    validate_check()
    chunks_reset()

    anl_m <- anl_merged()
    input_aval_var <- as.vector(anl_m$columns_source$aval_var)
    req(input$responders %in% anl_m$data()[[input_aval_var]])

    chunks_push_data_merge(anl_m)
    chunks_push_new_line()

    anl_adsl <- adsl_merged()
    chunks_push_data_merge(anl_adsl)
    chunks_push_new_line()

    anl <- chunks_get_var("ANL") # nolint
    input_strata_var <- as.vector(anl_m$columns_source$strata_var)

    my_calls <- template_rsp(
      dataname = "ANL",
      parentname = "ANL_ADSL",
      arm_var = as.vector(anl_m$columns_source$arm_var),
      paramcd = unlist(anl_m$filter_info)["selected"],
      ref_arm = input$ref_arm,
      comp_arm = input$comp_arm,
      compare_arm = input$compare_arms,
      combine_comp_arms = input$combine_comp_arms,
      aval_var = input_aval_var,
      responder_val = input$responders,
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
          method_ci = input$s_diff_ci,
          method_test = input$s_diff_test,
          strat = if (length(input_strata_var) != 0) input_strata_var else NULL
        )
      ),
      add_total = input$add_total
    )
    mapply(expression = my_calls, chunks_push)
  })

  # Outputs to render.
  table <- reactive({
    call_preparation()
    chunks_safe_eval()
    chunks_get_var("result")
  })

  callModule(
    table_with_settings_srv,
    id = "table",
    table_r = table
  )

  # Render R code.
  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(
      list(arm_var, paramcd, aval_var, strata_var)
    ),
    modal_title = "Binary Outcome",
    code_header = label
  )
}
