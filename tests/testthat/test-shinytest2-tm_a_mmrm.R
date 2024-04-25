app_driver_tm_a_mmrm <- function() {
  arm_ref_comp <- list(
    ARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    )
  )

  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADQS <- tmc_ex_adqs %>%
      dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
      dplyr::filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")) %>%
      dplyr::mutate(
        AVISIT = as.factor(AVISIT),
        AVISITN = rank(AVISITN) %>%
          as.factor() %>%
          as.numeric() %>%
          as.factor() #' making consecutive numeric factor
      )
  })

  datanames <- c("ADSL", "ADQS")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  arm_var <- choices_selected(c("ARM", "ARMCD"), "ARM")

  init_teal_app_driver(
    data = data,
    modules = tm_a_mmrm(
      label = "MMRM",
      dataname = "ADQS",
      parentname = ifelse(inherits(arm_var, "data_extract_spec"),
        teal.transform::datanames_input(arm_var), "ADSL"
      ),
      aval_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
      id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
      arm_var = arm_var,
      visit_var = choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        choices = value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
        selected = "FKSI-FWB"
      ),
      cov_var = choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL),
      method = teal.transform::choices_selected(c(
        "Satterthwaite", "Kenward-Roger",
        "Kenward-Roger-Linear"
      ), "Satterthwaite", keep_order = TRUE),
      conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95,
        keep_order =
          TRUE
      ),
      plot_height = c(700L, 200L, 2000L),
      plot_width = NULL,
      total_label = default_total_label(),
      pre_output = NULL,
      post_output = NULL,
      basic_table_args = teal.widgets::basic_table_args(),
      ggplot2_args = teal.widgets::ggplot2_args()
    ),
    timeout = 30000
  )
}

testthat::test_that("e2e - tm_a_mmrm: Module initializes in teal without errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_mmrm()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  null_text <- app_driver$active_module_element_text("null_input_msg")

  testthat::expect_match(null_text, "Please first specify 'Model Settings' and press 'Fit Model'")

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_mmrm:
  Module initializes with specified label, x_var, y_var, ADQS filters, color, conf_level and stat", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_mmrm()

  testthat::expect_equal(app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"), "MMRM")

  testthat::expect_equal(app_driver$get_active_module_input("aval_var-dataset_ADQS_singleextract-select"), "AVAL")

  testthat::expect_equal(
    app_driver$get_active_module_input("paramcd-dataset_ADQS_singleextract-filter1-vals"),
    "FKSI-FWB"
  )

  testthat::expect_equal(app_driver$get_active_module_input("visit_var-dataset_ADQS_singleextract-select"), "AVISIT")

  testthat::expect_NULL(app_driver$get_active_module_input("cov_var-dataset_ADQS_singleextract-select"))

  testthat::expect_equal(app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"), "ARM")

  testthat::expect_equal(
    app_driver$get_active_module_input("buckets"),
    list(
      Ref = list("A: Drug X"),
      Comp = list("B: Placebo", "C: Combination")
    )
  )

  testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))

  testthat::expect_equal(app_driver$get_active_module_input("id_var-dataset_ADQS_singleextract-select"), "USUBJID")

  testthat::expect_equal(app_driver$get_active_module_input("weights_emmeans"), "proportional")

  testthat::expect_equal(app_driver$get_active_module_input("cor_struct"), "unstructured")

  testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")

  testthat::expect_equal(app_driver$get_active_module_input("method"), "Satterthwaite")

  testthat::expect_true(app_driver$get_active_module_input("parallel"))

  testthat::expect_equal(app_driver$get_active_module_input("output_function"), "t_mmrm_lsmeans")

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_mmrm: Click on fit model shows table for default selection", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_mmrm()
  app_driver$click(selector = app_driver$active_module_element("button_start"))
  app_driver$expect_no_validation_error()

  table <- app_driver$get_active_module_tws_output("mmrm_table")
  col_val <- app_driver$get_active_module_input("buckets")
  testthat::expect_true(all(unlist(col_val, use.names = FALSE) %in% colnames(table)))
  testthat::expect_gte(nrow(table), 25)

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_mmrm: Output type selection shows dynamic output settings", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_mmrm()

  app_driver$click(selector = app_driver$active_module_element("button_start"))
  app_driver$expect_no_validation_error()

  # Check and set different outputs and validate their effects
  output_functions <- c("g_mmrm_lsmeans", "t_mmrm_lsmeans", "t_mmrm_cov", "t_mmrm_fixed", "t_mmrm_diagnostic", "g_mmrm_diagnostic")
  for (func in output_functions) {
    #browser()
    set_input_and_validate(app_driver, "output_function", func)

    # Test default behavior or specific behavior based on the function
    switch(func,
           t_mmrm_lsmeans = {
             testthat::expect_equal(app_driver$get_active_module_input("t_mmrm_lsmeans_show_relative"), "reduction")
             set_input_and_validate(app_driver, "t_mmrm_lsmeans_show_relative", "increase")
           },
           g_mmrm_lsmeans = {
             plot_before <- app_driver$get_active_module_pws_output("mmrm_plot")
             testthat::expect_match(plot_before, "data:image/png;base64,")

             app_driver$set_active_module_input("g_mmrm_lsmeans_select", "estimates")
             app_driver$expect_no_validation_error()
             app_driver$set_active_module_input("g_mmrm_lsmeans_select", "contrasts")
             app_driver$expect_no_validation_error()

             app_driver$set_active_module_input(
               "g_mmrm_lsmeans_select",
               c("estimates", "contrasts")
             )
             app_driver$expect_no_validation_error()

             app_driver$set_active_module_input("g_mmrm_lsmeans_width", 0.9)
             app_driver$expect_no_validation_error()

             app_driver$set_active_module_input("g_mmrm_lsmeans_contrasts_show_pval", TRUE)
             app_driver$expect_no_validation_error()

             plot <- app_driver$get_active_module_pws_output("mmrm_plot")
             testthat::expect_match(plot, "data:image/png;base64,")

             testthat::expect_false(identical(plot_before, plot))
           },
           t_mmrm_cov = {
             table <- app_driver$get_active_module_tws_output("mmrm_table")
             testthat::expect_gt(nrow(table), 1)
           },
           t_mmrm_fixed = {
             table <- app_driver$get_active_module_tws_output("mmrm_table")
             testthat::expect_gt(nrow(table), 1)
           },
           t_mmrm_diagnostic = {
             table <- app_driver$get_active_module_tws_output("mmrm_table")
             testthat::expect_gt(nrow(table), 1)
           },
           g_mmrm_diagnostic = {
             plot_before <- app_driver$get_active_module_pws_output("mmrm_plot")
             testthat::expect_match(plot_before, "data:image/png;base64,")

             app_driver$set_active_module_input("g_mmrm_diagnostic_type", "q-q-residual")
             app_driver$expect_no_validation_error()

             plot <- app_driver$get_active_module_pws_output("mmrm_plot")
             testthat::expect_match(plot, "data:image/png;base64,")

             testthat::expect_false(identical(plot_before, plot))
           }
    )
  }

  app_driver$stop()
})
