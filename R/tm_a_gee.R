#' Teal Module: Teal module for Generalized Estimating Equations (GEE) analysis
#'
#' @inheritParams module_arguments
#'
#' @export
#'
#' @examples
#'
#' library(scda)
#'
#' synthetic_cdisc_data_latest <- synthetic_cdisc_data("latest")
#' ADSL <- synthetic_cdisc_data_latest$adsl
#' ADQS <- synthetic_cdisc_data_latest$adqs %>%
#'   dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
#'   dplyr::mutate(
#'     AVISIT = as.factor(AVISIT),
#'     AVISITN = rank(AVISITN) %>%
#'       as.factor() %>%
#'       as.numeric() %>%
#'       as.factor(),
#'     AVALBIN = AVAL < 50 # Just as an example to get a binary endpoint.
#'   ) %>%
#'   droplevels()
#'
#' app <- teal::init(
#'   data = teal.data::cdisc_data(
#'     teal.data::cdisc_dataset("ADSL", ADSL),
#'     teal.data::cdisc_dataset("ADQS", ADQS)
#'   ),
#'   modules = teal::modules(
#'     tm_a_gee(
#'       label = "GEE",
#'       dataname = "ADQS",
#'       aval_var = choices_selected("AVALBIN", fixed = TRUE),
#'       id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       visit_var = choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
#'       paramcd = choices_selected(
#'         choices = value_choices(ADQS, "PARAMCD", "PARAM"),
#'         selected = "FKSI-FWB"
#'       ),
#'       cov_var = choices_selected(c("BASE", "AGE", "SEX"), NULL)
#'     )
#'   )
#' )
#' \dontrun{
#' shiny::shinyApp(app$ui, app$server)
#' }
tm_a_gee <- function(label,
                     dataname,
                     parentname = ifelse(
                       inherits(arm_var, "data_extract_spec"),
                       teal.transform::datanames_input(arm_var),
                       "ADSL"
                     ),
                     aval_var,
                     id_var,
                     arm_var,
                     visit_var,
                     cov_var,
                     arm_ref_comp = NULL,
                     paramcd,
                     conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
                     pre_output = NULL,
                     post_output = NULL,
                     basic_table_args = teal.widgets::basic_table_args()) {
  logger::log_info("Initializing tm_a_gee (prototype)")

  cov_var <- teal.transform::add_no_selected_choices(cov_var, multiple = TRUE)

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(parentname)
  checkmate::assert_class(conf_level, "choices_selected")
  checkmate::assert_class(pre_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(post_output, classes = "shiny.tag", null.ok = TRUE)
  checkmate::assert_class(basic_table_args, "basic_table_args")

  args <- as.list(environment())

  data_extract_list <- list(
    arm_var = teal.modules.clinical::cs_to_des_select(arm_var, dataname = parentname),
    paramcd = teal.modules.clinical::cs_to_des_filter(paramcd, dataname = dataname),
    id_var = teal.modules.clinical::cs_to_des_select(id_var, dataname = dataname),
    visit_var = teal.modules.clinical::cs_to_des_select(visit_var, dataname = dataname),
    cov_var = teal.modules.clinical::cs_to_des_select(cov_var, dataname = dataname, multiple = TRUE),
    split_covariates = teal.modules.clinical::cs_to_des_select(teal.modules.clinical::split_choices(cov_var), dataname = dataname, multiple = TRUE),
    aval_var = teal.modules.clinical::cs_to_des_select(aval_var, dataname = dataname)
  )

  teal::module(
    label = label,
    server = srv_gee,
    ui = ui_gee,
    ui_args = c(data_extract_list, args),
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        arm_ref_comp = arm_ref_comp,
        label = label,
        basic_table_args = basic_table_args
      )
    ),
    filters = teal.transform::get_extract_datanames(data_extract_list)
  )
}

ui_gee <- function(id, ...) {
  a <- list(...) # module args

  ns <- shiny::NS(id)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    a$arm_var,
    a$paramcd,
    a$id_var,
    a$visit_var,
    a$cov_var,
    a$aval_var
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      shiny::htmlOutput(ns("table"))
    ),
    # todo: in production use this if possible, couldn't get it to run quickly
    # output = teal.widgets::white_small_well(
    #   teal.widgets::table_with_settings_ui(ns("table"))
    # ),
    encoding = shiny::div(
      shiny::tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(a[c("arm_var", "paramcd", "id_var", "visit_var", "cov_var", "aval_var")]),
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Analysis Variable",
        data_extract_spec = a$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("paramcd"),
        label = "Select Endpoint",
        data_extract_spec = a$paramcd,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("visit_var"),
        label = "Visit Variable",
        data_extract_spec = a$visit_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("cov_var"),
        label = "Covariates",
        data_extract_spec = a$cov_var,
        is_single_dataset = is_single_dataset_value
      ),
      shinyjs::hidden(
        teal.transform::data_extract_ui(
          id = ns("split_covariates"),
          label = "Split Covariates",
          data_extract_spec = a$split_covariates,
          is_single_dataset = is_single_dataset_value
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("arm_var"),
        label = "Select Treatment Variable",
        data_extract_spec = a$arm_var,
        is_single_dataset = is_single_dataset_value
      ),
      shinyjs::hidden(shiny::uiOutput(ns("arms_buckets"))),
      shinyjs::hidden(
        shiny::helpText(
          id = ns("help_text"), "Multiple reference groups are automatically combined into a single group."
        )
      ),
      shinyjs::hidden(
        shiny::checkboxInput(
          ns("combine_comp_arms"),
          "Combine all comparison groups?",
          value = FALSE
        )
      ),
      teal.transform::data_extract_ui(
        id = ns("id_var"),
        label = "Subject Identifier",
        data_extract_spec = a$id_var,
        is_single_dataset = is_single_dataset_value
      ),
      shiny::selectInput(
        ns("cor_struct"),
        "Correlation Structure",
        choices = c(
          "unstructured",
          # "toeplitz", # needs the fix of https://github.com/insightsengineering/tern.gee/issues/3
          # "auto-regressive"
          "compound symmetry"
        ),
        selected = "unstructured",
        multiple = FALSE
      ),
      teal.widgets::optionalSelectInput(
        ns("conf_level"),
        "Confidence Level",
        a$conf_level$choices,
        a$conf_level$selected,
        multiple = FALSE,
        fixed = a$conf_level$fixed
      ),
      shiny::radioButtons(
        ns("output_table"),
        "Output Type",
        choices = c(
          "LS means" = "t_gee_lsmeans",
          "Covariance" = "t_gee_cov",
          "Coefficients" = "t_gee_coef"
        ),
        selected = "t_gee_lsmeans"
      ),
      pre_output = a$pre_output,
      post_output = a$post_output
    )
  )
}

srv_gee <- function(id,
                    datasets,
                    dataname,
                    parentname,
                    arm_var,
                    paramcd,
                    id_var,
                    visit_var,
                    cov_var,
                    split_covariates,
                    aval_var,
                    arm_ref_comp,
                    label,
                    plot_height,
                    plot_width,
                    basic_table_args) {
  shiny::moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()

    ## split_covariates ----
    shiny::observeEvent(input[[teal.modules.clinical::extract_input("cov_var", dataname)]],
      ignoreNULL = FALSE,
      {
        # update covariates as actual variables
        split_interactions_values <- teal.modules.clinical::split_interactions(
          input[[teal.modules.clinical::extract_input("cov_var", dataname)]]
        )
        arm_var_value <- input[[teal.modules.clinical::extract_input("arm_var", parentname)]]
        arm_in_cov <- length(intersect(split_interactions_values, arm_var_value)) >= 1L
        if (arm_in_cov) {
          split_covariates_selected <- setdiff(split_interactions_values, arm_var_value)
        } else {
          split_covariates_selected <- split_interactions_values
        }
        teal.widgets::updateOptionalSelectInput(
          session,
          inputId = teal.modules.clinical::extract_input("split_covariates", dataname),
          selected = split_covariates_selected
        )
      }
    )

    ## arm_ref_comp_observer ----
    arm_ref_comp_observer(
      session,
      input,
      output,
      id_arm_var = teal.modules.clinical::extract_input("arm_var", parentname),
      data = data[[parentname]],
      arm_ref_comp = arm_ref_comp,
      module = "tm_a_gee"
    )

    ## data_merge_modules ----
    anl_merged <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(
        arm_var = arm_var,
        paramcd = paramcd,
        id_var = id_var,
        visit_var = visit_var,
        split_covariates = split_covariates,
        aval_var = aval_var
      ),
      merge_function = "dplyr::inner_join"
    )
    adsl_merged <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(arm_var = arm_var),
      anl_name = "ANL_ADSL"
    )

    # To do in production: add validations.

    ## model_fit ----
    model_fit <- shiny::reactive({
      anl_merged <- anl_merged()
      data <- anl_merged$data()
      col_source <- anl_merged$columns_source
      cor_struct <- input$cor_struct

      tern.gee::fit_gee(
        vars = tern.gee:::vars_gee(
          response = as.vector(col_source$aval_var),
          covariates = as.vector(col_source$split_covariates),
          id = as.vector(col_source$id_var),
          arm = as.vector(col_source$arm_var),
          visit = as.vector(col_source$visit_var)
        ),
        data = data,
        regression = "logistic",
        cor_struct = cor_struct
      )
    })

    ## table_r ----
    table_r <- shiny::reactive({
      output_table <- input$output_table
      model_fit <- model_fit()
      conf_level <- as.numeric(input$conf_level)

      req(output_table)
      req(model_fit)

      if (output_table == "t_gee_cov") {
        tern.gee::as.rtable(model_fit, type = "cov")
      } else if (output_table == "t_gee_coef") {
        tern.gee::as.rtable(model_fit, type = "coef", conf_level = conf_level)
      } else if (output_table == "t_gee_lsmeans") {
        # To do: is this needed instead/as well?
        # adsl_filtered <- datasets$get_data(parentname, filtered = TRUE)
        anl_adsl <- adsl_merged()
        data <- anl_adsl$data()
        input_arm_var <- as.vector(anl_adsl$columns_source$arm_var)
        rtables::basic_table() %>%
          rtables::split_cols_by(input_arm_var, ref_group = model_fit$ref_level) %>%
          rtables::add_colcounts() %>%
          tern.gee::summarize_gee_logistic() %>%
          rtables::build_table(
            df = tern.gee::lsmeans(model_fit, conf_level = conf_level),
            alt_counts_df = data
          )
      }
    })

    ## table ----

    output$table <- shiny::renderUI({
      rtables::as_html(table_r())
    })

    # to do in production: use this kind of stuff.
    # teal.widgets::table_with_settings_srv(
    #   id = "table",
    #   table_r = table_r
    # )
  })
}
