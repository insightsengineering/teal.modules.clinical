app_driver_tm_t_logistic <- function() {
  data <- teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- teal.data::rADSL
    ADRS <- teal.data::rADRS %>%
      filter(PARAMCD %in% c("BESRSPI", "INVET"))
  })

  datanames <- c("ADSL", "ADRS")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  arm_ref_comp <- list(
    ACTARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    ),
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    )
  )

  init_teal_app_driver(
    data = data,
    modules = tm_t_logistic(
      label = "Logistic Regression",
      dataname = "ADRS",
      arm_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADRS"]], c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      arm_ref_comp = arm_ref_comp,
      paramcd = teal.transform::choices_selected(
        choices = teal.transform::value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
        selected = "BESRSPI"
      ),
      cov_var = teal.transform::choices_selected(
        choices = c("SEX", "AGE", "BMRKR1", "BMRKR2"),
        selected = "SEX"
      ),
      conf_level = teal.transform::choices_selected(c(2, 0.95, 0.9, 0.8), 0.95, keep_order = TRUE)
    )
  )
}

testthat::test_that("e2e - tm_t_logistic: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_logistic()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_logistic: Starts with specified label, paramcd, responders, arm_var, buckets,
  cov_var, interaction_var, conf_level, combine_comp_arms",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_logistic()
    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Logistic Regression"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals"),
      "BESRSPI"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("responders"),
      c("CR", "PR")
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(
        Ref = list("B: Placebo"),
        Comp = list("A: Drug X", "C: Combination")
      )
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("cov_var-dataset_ADRS_singleextract-select"),
      "SEX"
    )
    testthat::expect_null(app_driver$get_active_module_input("interaction_var"))
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    app_driver$stop()
  }
)
