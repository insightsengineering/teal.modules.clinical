#' Teal Module: Responders
#'
#' @name responders
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#'
NULL

#' Template for Responders
#'
#' Creates a valid expression for responder analysis.
#'
#' @inheritParams argument_convention
#' @param control (`list`)\cr list of settings for the analysis.
#' @param show_rsp_cat (`flag`)\cr display the multinomial response estimations.
#'
#' @examples
#'
#' # Preparation of the test case.
#' library(dplyr)
#' library(random.cdisc.data)
#' library(tern)
#' adsl <- radsl(cached = TRUE)
#' adrs <- radrs(cached = TRUE)
#'
#' # Generate an expression for the analysis of responders.
#' a <- template_rsp(
#'   dataname = "adrs",
#'   param = "INVET",
#'   arm_var = "ARMCD",
#'   ref_arm = "ARM A",
#'   compare_arm = TRUE,
#'   show_rsp_cat = TRUE
#' )
#'
#' styled_expr(a$data)
#' styled_expr(a$layout)
#' styled_expr(a$table)
#'
#' b <- mapply(expr = a, FUN = eval)
#' b$data
#' b$layout
#' b$table
#'
template_rsp <- function(dataname,
                         param,
                         arm_var,
                         ref_arm,
                         compare_arm = TRUE,
                         combine_arm = FALSE,
                         show_rsp_cat = TRUE,
                         control = list(
                           global = list(
                             method = "waldcc",
                             conf_level = 0.95
                           ),
                           unstrat = list(
                             method_ci = "waldcc",
                             method_test = "schouten",
                             odds = TRUE
                           ),
                           strat = list(
                             method_ci = "waldcc",
                             method_test = "cmh",
                             strat = NULL
                           )
                         )
) {
  y <- list()

  data_list <- list()
  data_list <- add_expr(
    data_list,
    substitute(
      expr = df %>%
        filter(PARAMCD == param) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(
          is_rsp = rsp_lab %in% c(
            "Complete Response (CR)", "Partial Response (PR)"
          )
        ),
      env = list(
        df = as.name(dataname),
        param = param
      )
    )
  )
  data_list <- add_expr(
    data_list,
    substitute_names(
      expr = mutate(arm_var = relevel(arm_var, ref = ref_arm)),
      names = list(arm_var = as.name(arm_var)),
      others = list(ref_arm = ref_arm)
    )
  )

  y$data <- substitute(
    expr = anl <- data_pipe,
    env = list(data_pipe = pipe_expr(data_list))
  )

  if (combine_arm) {
    y$combine_arm <- substitute(
      expr = groups <- combine_groups(fct = anl[[group]], ref = ref_arm),
      env = list(group = arm_var, ref_arm = ref_arm)
    )
  }

  layout_list <- list()

  layout_list <- add_expr(layout_list, substitute(basic_table()))

  # There are 4 possible column split patterns depending on
  # the 4 combination of boolean compare_arm and combine_arm.
  layout_list <- add_expr(
    layout_list,
    if (compare_arm & combine_arm) {
      substitute(
        expr = split_cols_by_groups(
          var = arm_var, groups_list = groups, ref_group = names(groups)[1]
        ),
        env = list(arm_var = arm_var)
      )
    } else if (compare_arm & !combine_arm) {
      substitute(
        expr = split_cols_by(var = arm_var, ref_group = ref_arm),
        env = list(arm_var = arm_var, ref_arm = ref_arm)
      )
    } else if (!compare_arm & combine_arm) {
      substitute(
        expr = split_cols_by_groups(var = arm_var, groups_list = groups),
        env = list(arm_var = arm_var)
      )
    } else if (!compare_arm & !combine_arm) {
      substitute(
        expr = split_cols_by(var = arm_var),
        env = list(arm_var = arm_var)
      )
    }
  )

  layout_list <- add_expr(
    layout_list,
    substitute(
      add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = conf_level,
          method = method
        ),
      env = list(
        conf_level = control$global$conf_level,
        method = control$global$method
      )
    )
  )

  if (compare_arm) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        expr = estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = conf_level,
          method = method_ci
        ) %>%
          test_proportion_diff(
            vars = "is_rsp",
            method = method_test
          ),
        env = list(
          conf_level = control$global$conf_level,
          method_ci = control$unstrat$method_ci,
          method_test = control$unstrat$method_test
        )
      )
    )

    if (control$unstrat$odds) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          expr = estimate_odds_ratio(vars = "is_rsp", conf_level = conf_level),
          env = list(conf_level = control$global$conf_level)
        )
      )
    }

    if (!is.null(control$strat$strat)) {
      layout_list <- add_expr(
        layout_list,
        substitute(
          expr = estimate_proportion_diff(
            vars = "is_rsp", show_labels = "visible",
            var_labels = "Stratified Analysis",
            variables = list(strata = strata),
            conf_level = conf_level,
            method = method_ci
          ) %>%
            test_proportion_diff(
              vars = "is_rsp",
              method = method_test,
              variables = list(strata = "SEX")
            ),
          env = list(
            conf_level = control$global$conf_level,
            method_ci = control$strat$method_ci,
            strata = control$strat$strat,
            method_test = control$strat$method_test
          )
        )
      )
    }
  }

  if (show_rsp_cat) {
    layout_list <- add_expr(
      layout_list,
      substitute(
        estimate_multinomial_response(
          var = "rsp_lab",
          conf_level = conf_level,
          method = method
        ),
        list(
          conf_level = control$global$conf_level,
          method = control$global$method
        )
      )
    )
  }

  y$layout <- substitute(
    expr = lyt <- layout_pipe,
    env = list(layout_pipe = pipe_expr(layout_list))
  )

  y$table <- substitute(
    expr = result <- build_table(lyt = lyt, df = anl)
  )
  y
}

#' @noRd
ui_rsp <- function(id,
                   datasets,
                   dataname,
                   ...) {
  args <- list(...)
  ns <- NS(id)
  standard_layout(
    output = white_small_well(uiOutput(ns("as_html"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(
        ns("param"),
        "PARAMCD",
        choices = args$param$choices,
        selected = args$param$selected,
        multiple = FALSE,
        fixed = args$param$fixed,
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
          value = 1,
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
          value = 1,
          size = "mini"
        )
      )
    ),
    forms = actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%"
    )
  )
}

#' @noRd
srv_rsp <- function(input,
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
    param <- input$param
    responder_choices <- unique(anl$AVALC[anl$PARAMCD == param])
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

    # Validate through assertions.
    assert_that(
      teal_enough_rows(data = adsl_filtered, min_nrow = 15),
      teal_enough_rows(data = anl_filtered, min_nrow = 15),
      teal_has_element(str = input$arm_var, label = "ARM")
    )

    # Send data where the analysis lives.
    e <- new.env()
    e$ADRS_FILTERED <- anl_filtered # nolint
    e$ADSL_FILTERED <- adsl_filtered # nolint
    e
  })

  # The R-code corresponding to the analysis.
  call_preparation <- reactive({
    chunks_reset(envir = prepared_env())
    my_calls <- template_rsp(
      dataname = "ADRS_FILTERED",
      param = input$param,
      arm_var = input$arm_var,
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

#' @describeIn responders Teal module for responders.
#'
#' @export
#' @examples
#' # Preparation of the test case.
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
#'     tm_t_rsp(
#'       label = "Binary responses",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARMCD"),
#'       param = choices_selected(levels(adrs$PARAMCD), "INVET"),
#'       arm_ref_comp = arm_ref_comp,
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
tm_t_rsp <- function(label,
                     dataname,
                     arm_var,
                     arm_ref_comp = NULL,
                     param,
                     strata_var) {
  args <- c(as.list(environment()))
  module(
    label = label,
    ui = ui_rsp,
    ui_args = args,
    server = srv_rsp,
    server_args = args,
    filters = dataname
  )
}
