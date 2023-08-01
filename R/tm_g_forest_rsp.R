#' Template: Response Forest Plot
#'
#' Creates a valid expression for response forest plot.
#'
#' @inheritParams template_arguments
#' @param obj_var_name (`character`)\cr additional text string append to output title.
#' @param responders (`character`)\cr values of `aval_var` that are considered to be responders.
#' @param col_symbol_size (`integer`)\cr column index to be used to determine relative size for
#'  estimator plot symbol. Typically, the symbol size is proportional to the sample size used
#'  to calculate the estimator. If `NULL`, the same symbol size is used for all subgroups.
#' @param strata_var (`character`)\cr
#'   names of the variables for stratified analysis.
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `ggplot2_args` object with `labs` list of following child elements:
#' `title`, `caption`.
#' No other elements would be taken into account. The argument is merged with option `teal.ggplot2_args` and
#' with default module arguments (hard coded in the module body).
#'
#' For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @seealso [tm_g_forest_rsp()]
#' @keywords internal
#'
template_forest_rsp <- function(dataname = "ANL",
                                parentname = "ADSL",
                                arm_var,
                                ref_arm = NULL,
                                comp_arm = NULL,
                                obj_var_name = "",
                                aval_var = "AVALC",
                                responders = c("CR", "PR"),
                                subgroup_var,
                                strata_var = NULL,
                                conf_level = 0.95,
                                col_symbol_size = NULL,
                                ggplot2_args = teal.widgets::ggplot2_args()) {
  assertthat::assert_that(
    assertthat::is.string(dataname),
    assertthat::is.string(parentname),
    assertthat::is.string(arm_var),
    assertthat::is.string(aval_var),
    assertthat::is.string(obj_var_name),
    is.null(subgroup_var) || is.character(subgroup_var)
  )

  y <- list()
  ref_arm_val <- paste(ref_arm, collapse = "/")

  # Data processing.
  data_list <- list()
  anl_list <- list()
  parent_list <- list()

  anl_list <- add_expr(
    anl_list,
    prepare_arm(
      dataname = dataname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  anl_list <- add_expr(
    anl_list,
    substitute(
      expr = dplyr::mutate(is_rsp = aval_var %in% responders),
      env = list(
        aval_var = as.name(aval_var),
        responders = responders
      )
    )
  )

  anl_list <- add_expr(
    anl_list,
    substitute_names(
      expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
      names = list(arm_var = as.name(arm_var)),
      others = list(comp_arm = comp_arm)
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      anl <- anl_list,
      env = list(
        anl = as.name(dataname),
        anl_list = pipe_expr(anl_list)
      )
    )
  )

  parent_list <- add_expr(
    parent_list,
    prepare_arm(
      dataname = parentname,
      arm_var = arm_var,
      ref_arm = ref_arm,
      comp_arm = comp_arm,
      ref_arm_val = ref_arm_val
    )
  )

  parent_list <- add_expr(
    parent_list,
    substitute_names(
      expr = dplyr::mutate(arm_var = combine_levels(arm_var, levels = comp_arm)),
      names = list(arm_var = as.name(arm_var)),
      others = list(comp_arm = comp_arm)
    )
  )

  data_list <- add_expr(
    data_list,
    substitute(
      parent <- parent_list,
      env = list(
        parent_list = pipe_expr(parent_list)
      )
    )
  )

  y$data <- bracket_expr(data_list)

  # Tabulate subgroup analysis of response.
  summary_list <- list()

  summary_list <- add_expr(
    summary_list,
    substitute(
      expr = df <- extract_rsp_subgroups(
        variables = list(
          rsp = "is_rsp", arm = arm_var, subgroups = subgroup_var, strat = strata_var
        ),
        data = anl,
        conf_level = conf_level
      ),
      env = list(
        anl = as.name(dataname),
        arm_var = arm_var,
        subgroup_var = subgroup_var,
        strata_var = strata_var,
        conf_level = conf_level
      )
    )
  )

  y$summary <- bracket_expr(summary_list)

  # Table output.
  y$table <- quote(
    result <- rtables::basic_table() %>%
      tabulate_rsp_subgroups(df, vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))
  )

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = teal.widgets::ggplot2_args(
      labs = list(title = paste0("Forest plot of best overall response for ", obj_var_name), caption = "")
    )
  )

  plot_call <- substitute(
    expr = g_forest(
      tbl = result,
      col_symbol_size = col_s_size
    ),
    env = list(col_s_size = col_symbol_size)
  )

  plot_call <- substitute(
    decorate_grob(p, titles = title, footnotes = caption, gp_footnotes = grid::gpar(fontsize = 12)),
    env = list(title = all_ggplot2_args$labs$title, caption = all_ggplot2_args$labs$caption, p = plot_call)
  )

  plot_call <- substitute(
    expr = {
      p <- plot_call
      grid::grid.newpage()
      grid::grid.draw(p)
    },
    env = list(plot_call = plot_call)
  )

  # Plot output.
  y$plot <- plot_call

  y
}

#' Teal Module: Forest Response Plot teal module
#'
#' This teal module produces a grid style Forest plot for response data with `ADaM` structure.
#'
#' @inheritParams module_arguments
#' @inheritParams tm_t_binary_outcome
#' @param fixed_symbol_size (`logical`)\cr
#' When (`TRUE`), the same symbol size is used for plotting each estimate.
#' Otherwise, the symbol size will be proportional to the sample size in each each subgroup.
#' @param ggplot2_args optional, (`ggplot2_args`)\cr
#' object created by [teal.widgets::ggplot2_args()] with settings for the module plot.
#' For this module, this argument will only accept `ggplot2_args` object with `labs` list of following child elements:
#' `title`, `caption`.
#' No other elements would be taken into account. The argument is merged with option `teal.ggplot2_args` and
#' with default module arguments (hard coded in the module body).
#'
#' For more details, see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @export
#'
#' @template author_song24
#'
#' @examples
#' library(nestcolor)
#'
#' adsl <- tmc_ex_adsl
#' adrs <- tmc_ex_adrs %>%
#'   dplyr::mutate(AVALC = tern::d_onco_rsp_label(AVALC) %>%
#'     formatters::with_label("Character Result/Finding")) %>%
#'   dplyr::filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
#'
#' arm_ref_comp <- list(
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   ),
#'   ARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", adsl),
#'     cdisc_dataset("ADRS", adrs)
#'   ),
#'   modules = modules(
#'     tm_g_forest_rsp(
#'       label = "Forest Response",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(
#'         variable_choices(adsl, c("ARM", "ARMCD")),
#'         "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(adrs, "PARAMCD", "PARAM"),
#'         "INVET"
#'       ),
#'       subgroup_var = choices_selected(
#'         variable_choices(adsl, names(adsl)),
#'         c("BMRKR2", "SEX")
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(adsl, c("STRATA1", "STRATA2")),
#'         "STRATA2"
#'       ),
#'       plot_height = c(600L, 200L, 2000L),
#'       default_responses = list(
#'         BESRSPI = list(
#'           rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
#'           levels = c(
#'             "Complete Response (CR)", "Partial Response (PR)", "Stable Disease (SD)",
#'             "Progressive Disease (PD)", "Not Evaluable (NE)"
#'           )
#'         ),
#'         INVET = list(
#'           rsp = c("Complete Response (CR)", "Partial Response (PR)"),
#'           levels = c(
#'             "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
#'             "Progressive Disease (PD)", "Stable Disease (SD)"
#'           )
#'         ),
#'         OVRINV = list(
#'           rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
#'           levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_forest_rsp <- function(label,
                            dataname,
                            parentname = ifelse(
                              inherits(arm_var, "data_extract_spec"),
                              teal.transform::datanames_input(arm_var),
                              "ADSL"
                            ),
                            arm_var,
                            arm_ref_comp = NULL,
                            paramcd,
                            aval_var = teal.transform::choices_selected(
                              teal.transform::variable_choices(dataname, "AVALC"), "AVALC",
                              fixed = TRUE
                            ),
                            subgroup_var,
                            strata_var,
                            fixed_symbol_size = TRUE,
                            conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                            default_responses = c("CR", "PR", "Y", "Complete Response (CR)", "Partial Response (PR)"),
                            plot_height = c(700L, 200L, 2000L),
                            plot_width = c(1200L, 800L, 3000L),
                            pre_output = NULL,
                            post_output = NULL,
                            ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_forest_rsp")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_flag(fixed_symbol_size)
  checkmate::assert_class(conf_level, "choices_selected")
  assertthat::assert_that(
    inherits(default_responses, c("list", "character", "numeric", "NULL")),
    msg = "`default_responses` must be a named list or an array."
  )
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = cs_to_des_select(arm_var, dataname = parentname),
    paramcd = cs_to_des_filter(paramcd, dataname = dataname),
    aval_var = cs_to_des_select(aval_var, dataname = dataname),
    subgroup_var = cs_to_des_select(subgroup_var, dataname = parentname, multiple = TRUE, ordered = TRUE),
    strata_var = cs_to_des_select(strata_var, dataname = parentname, multiple = TRUE)
  )

  module(
    label = label,
    ui = ui_g_forest_rsp,
    ui_args = c(data_extract_list, args),
    server = srv_g_forest_rsp,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        default_responses = default_responses,
        plot_height = plot_height,
        plot_width = plot_width,
        ggplot2_args = ggplot2_args
      )
    ),
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
}

#' @noRd
ui_g_forest_rsp <- function(id, ...) {
  a <- list(...) # module args
  is_single_dataset_value <- teal.transform::is_single_dataset(a$arm_var, a$paramcd, a$subgroup_var, a$strata_var)

  ns <- shiny::NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("myplot")),
    encoding = shiny::div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "aval_var", "subgroup_var", "strata_var")]),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::selectInput(
        ns("responders"),
        "Responders",
        choices = c("CR", "PR"),
        selected = c("CR", "PR"),
        multiple = TRUE
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::uiOutput(
        ns("arms_buckets"),
        title = paste(
          "Multiple reference groups are automatically combined into a single group when more than one",
          "value is selected."
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("subgroup_var"),
        label = "Subgroup Variables",
        data_extract_spec = a$subgroup_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("strata_var"),
        label = "Stratify by",
        data_extract_spec = a$strata_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          "Additional plot settings",
          teal.widgets::optionalSelectInput(
            inputId = ns("conf_level"),
            label = "Confidence Level",
            a$conf_level$choices,
            a$conf_level$selected,
            multiple = FALSE,
            fixed = a$conf_level$fixed
          ),
          shiny::checkboxInput(ns("fixed_symbol_size"), "Fixed symbol size", value = TRUE)
        )
      )
    ),
    forms = shiny::tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_forest_rsp <- function(id,
                             data,
                             reporter,
                             filter_panel_api,
                             dataname,
                             parentname,
                             arm_var,
                             arm_ref_comp,
                             paramcd,
                             aval_var,
                             subgroup_var,
                             strata_var,
                             plot_height,
                             plot_width,
                             label,
                             default_responses,
                             ggplot2_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {
    # Setup arm variable selection, default reference arms, and default
    # comparison arms for encoding panel
    iv_arm_ref <- arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = extract_input("arm_var", parentname),
      data = data[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_t_tte"
    )

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        arm_var = arm_var,
        subgroup_var = subgroup_var,
        strata_var = strata_var,
        paramcd = paramcd,
        aval_var = aval_var
      ),
      datasets = data,
      select_validation_rule = list(
        aval_var = shinyvalidate::sv_required("An analysis variable is required"),
        arm_var = shinyvalidate::sv_required("A treatment variable is required")
      ),
      filter_validation_rule = list(paramcd = shinyvalidate::sv_required(message = "Please select Endpoint filter."))
    )

    iv_r <- shiny::reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("conf_level", shinyvalidate::sv_required("Please choose a confidence level between 0 and 1"))
      iv$add_rule(
        "conf_level",
        shinyvalidate::sv_between(0, 1, message_fmt = "Please choose a confidence level between {left} and {right}")
      )
      iv$add_rule("responders", shinyvalidate::sv_required("`Responders` field is empty"))
      iv$add_validator(iv_arm_ref)
      teal.transform::compose_and_enable_validators(iv, selector_list, c("arm_var", "aval_var", "paramcd"))
    })

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::inner_join",
      join_keys = get_join_keys(data)
    )

    adsl_inputs <- teal.transform::merge_expression_module(
      datasets = data,
      data_extract = list(arm_var = arm_var, subgroup_var = subgroup_var, strata_var = strata_var),
      join_keys = get_join_keys(data),
      anl_name = "ANL_ADSL"
    )

    anl_q <- shiny::reactive({
      q <- teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data))
      qenv <- teal.code::eval_code(q, as.expression(anl_inputs()$expr))
      teal.code::eval_code(qenv, as.expression(adsl_inputs()$expr))
    })

    shiny::observeEvent(
      eventExpr = c(
        input[[extract_input("aval_var", "ADRS")]],
        input[[extract_input("paramcd", paramcd$filter[[1]]$dataname, filter = TRUE)]]
      ),
      handlerExpr = {
        shiny::req(anl_q())
        anl <- anl_q()[["ANL"]]
        aval_var <- anl_inputs()$columns_source$aval_var
        paramcd_level <- unlist(anl_inputs()$filter_info$paramcd[[1]]$selected)
        if (length(paramcd_level) == 0) {
          return(NULL)
        }

        sel_param <- if (is.list(default_responses)) {
          default_responses[[paramcd_level]]
        } else {
          default_responses
        }


        common_rsp <- if (is.list(sel_param)) {
          sel_param$rsp
        } else {
          sel_param
        }
        responder_choices <- if (length(aval_var) == 0) {
          character(0)
        } else {
          if ("levels" %in% names(sel_param)) {
            if (length(intersect(unique(anl[[aval_var]]), sel_param$levels)) > 1) {
              sel_param$levels
            } else {
              union(anl[[aval_var]], sel_param$levels)
            }
          } else {
            unique(anl[[aval_var]])
          }
        }
        shiny::updateSelectInput(
          session, "responders",
          choices = responder_choices,
          selected = intersect(responder_choices, common_rsp)
        )
      }
    )

    # Prepare the analysis environment (filter data, check data, populate envir).
    validate_checks <- shiny::reactive({
      teal::validate_inputs(iv_r())
      shiny::req(anl_q())
      qenv <- anl_q()
      adsl_filtered <- qenv[[parentname]]
      anl_filtered <- qenv[[dataname]]
      anl <- qenv[["ANL"]]

      anl_m <- anl_inputs()
      input_arm_var <- as.vector(anl_m$columns_source$arm_var)
      input_aval_var <- as.vector(anl_m$columns_source$aval_var)
      input_subgroup_var <- as.vector(anl_m$columns_source$subgroup_var)
      input_strata_var <- as.vector(anl_m$columns_source$strata_var)
      input_paramcd <- unlist(paramcd$filter)["vars_selected"]

      # validate inputs
      validate_args <- list(
        adsl = adsl_filtered,
        adslvars = c("USUBJID", "STUDYID", input_arm_var, input_subgroup_var, input_strata_var),
        anl = anl_filtered,
        anlvars = c("USUBJID", "STUDYID", input_paramcd, input_aval_var),
        arm_var = input_arm_var
      )
      validate_args <- append(
        validate_args,
        list(ref_arm = unlist(input$buckets$Ref), comp_arm = unlist(input$buckets$Comp))
      )

      do.call(what = "validate_standard_inputs", validate_args)

      teal::validate_one_row_per_id(qenv[["ANL"]], key = c("USUBJID", "STUDYID", input_paramcd))

      if (length(input_subgroup_var) > 0) {
        shiny::validate(
          shiny::need(
            all(vapply(adsl_filtered[, input_subgroup_var], is.factor, logical(1))),
            "Not all subgroup variables are factors."
          )
        )
      }
      if (length(input_strata_var) > 0) {
        shiny::validate(
          shiny::need(
            all(vapply(adsl_filtered[, input_strata_var], is.factor, logical(1))),
            "Not all stratification variables are factors."
          )
        )
      }

      if (!identical(default_responses, c("CR", "PR", "Y", "Complete Response (CR)", "Partial Response (PR)"))) {
        shiny::validate(
          shiny::need(
            all(unlist(lapply(default_responses, function(x) {
              if (is.list(x) & "levels" %in% names(x)) {
                lvls <- x$levels
                all(x$rsp %in% lvls)
              } else {
                lvls <- unique(anl[[input$`aval_var-dataset_ADRS_singleextract-select`]])
                if ("rsp" %in% names(x)) {
                  all(x$rsp %in% lvls)
                } else {
                  all(x %in% lvls)
                }
              }
            }))),
            "All selected default responses must be in the levels of AVAL."
          )
        )
      }

      if (is.list(default_responses)) {
        shiny::validate(
          shiny::need(
            all(
              grepl("\\.rsp|\\.levels", names(unlist(default_responses))) |
                names(unlist(default_responses)) %in% names(default_responses)
            ),
            "The lists given for each AVAL in default_responses must be named 'rsp' and 'levels'."
          )
        )
      }

      validate_has_data(qenv[["ANL"]], min_nrow = 1)
      NULL
    })

    # The R-code corresponding to the analysis.
    all_q <- shiny::reactive({
      validate_checks()
      qenv <- anl_q()
      anl_m <- anl_inputs()

      strata_var <- as.vector(anl_m$columns_source$strata_var)
      subgroup_var <- as.vector(anl_m$columns_source$subgroup_var)

      obj_var_name <- get_g_forest_obj_var_name(paramcd, input)

      my_calls <- template_forest_rsp(
        dataname = "ANL",
        parentname = "ANL_ADSL",
        arm_var = as.vector(anl_m$columns_source$arm_var),
        ref_arm = unlist(input$buckets$Ref),
        comp_arm = unlist(input$buckets$Comp),
        obj_var_name = obj_var_name,
        aval_var = as.vector(anl_m$columns_source$aval_var),
        responders = input$responders,
        subgroup_var = if (length(subgroup_var) != 0) subgroup_var else NULL,
        strata_var = if (length(strata_var) != 0) strata_var else NULL,
        conf_level = as.numeric(input$conf_level),
        col_symbol_size = `if`(input$fixed_symbol_size, NULL, 1),
        ggplot2_args = ggplot2_args
      )

      teal.code::eval_code(qenv, as.expression(my_calls))
    })

    plot_r <- shiny::reactive(all_q()[["p"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = shiny::reactive(teal.code::get_warnings(all_q())),
      title = "Warning",
      disabled = shiny::reactive(is.null(teal.code::get_warnings(all_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = shiny::reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Forest Response Plot")
        card$append_text("Forest Response Plot", "header2")
        if (with_filter) {
          card$append_fs(filter_panel_api$get_filter_state())
        }
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(all_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
