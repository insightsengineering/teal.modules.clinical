app_driver_tm_t_tte <- function() {
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
    modules = tm_t_tte(
      label = "Time To Event Table",
      dataname = "ADTTE",
      arm_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD", "ACTARMCD")),
        "ARM"
      ),
      arm_ref_comp = arm_ref_comp,
      paramcd = teal.transform::choices_selected(
        teal.transform::value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
        "OS"
      ),
      strata_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADSL"]], c("SEX", "BMRKR2")),
        "SEX"
      ),
      time_points = teal.transform::choices_selected(c(182, 243), 182),
      event_desc_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADTTE"]], "EVNTDESC"),
        "EVNTDESC",
        fixed = TRUE
      )
    )
  )
}

testthat::test_that("e2e - tm_t_tte: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_tte()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_tte: Starts with specified label, paramcd, event_desc_var, arm_var, buckets,
  strata_var, time_points, pval_method_coxph, ties_coxph, conf_level_coxph,
  conf_level_survfit, conf_type_survfit, probs_survfit, compare_arms, combine_comp_arms.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Time To Event Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADTTE_singleextract-filter1-vals"),
      "OS"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("event_desc_var-dataset_ADTTE_singleextract-select"),
      "EVNTDESC"
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
      app_driver$get_active_module_input("strata_var-dataset_ADSL_singleextract-select"),
      "SEX"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("time_points"),
      "182"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("pval_method_coxph"),
      "log-rank"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("ties_coxph"),
      "exact"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level_coxph"),
      "0.95"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level_survfit"),
      "0.95"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_type_survfit"),
      "plain"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("probs_survfit"),
      c(0.25, 0.75)
    )
    testthat::expect_true(app_driver$get_active_module_input("compare_arms"))
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    app_driver$stop()
  }
)
