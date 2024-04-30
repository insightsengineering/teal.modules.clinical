app_driver_tm_t_coxreg <- function() {
  # TODO: Check if data fabrication is needed for Cox regression
  data <- teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADTTE <- teal.data::rADTTE
  })
  datanames <- c("ADSL", "ADTTE")
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
    modules = tm_t_coxreg(
      label = "Cox Reg.",
      dataname = "ADTTE",
      arm_var = teal.transform::choices_selected(c("ARM", "ARMCD", "ACTARMCD"), "ARM"),
      arm_ref_comp = arm_ref_comp,
      paramcd = teal.transform::choices_selected(
        teal.transform::value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"), "OS"
      ),
      strata_var = teal.transform::choices_selected(
        c("COUNTRY", "STRATA1", "STRATA2"), "STRATA1"
      ),
      cov_var = teal.transform::choices_selected(
        c("AGE", "BMRKR1", "BMRKR2", "REGION1"), "AGE"
      ),
      multivariate = TRUE
    )
  )
}

testthat::test_that("e2e - tm_t_coxreg: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_coxreg()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_coxreg: Starts with specified label, type, paramcd, arm_var, buckets,
  cov_var, strata_var, pval_method, ties, conf_level, combine_comp_arms.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_coxreg()
    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Cox Reg."
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("type"),
      "Multivariate"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADTTE_singleextract-filter1-vals"),
      "OS"
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
      app_driver$get_active_module_input("cov_var-dataset_ADSL_singleextract-select"),
      "AGE"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("strata_var-dataset_ADSL_singleextract-select"),
      "STRATA1"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("pval_method"),
      "wald"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("ties"),
      "exact"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    app_driver$stop()
  }
)
