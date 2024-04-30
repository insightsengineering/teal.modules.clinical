app_driver_tm_t_summary_by <- function() {
  data <- teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADLB <- teal.data::rADLB
  })

  datanames <- c("ADSL", "ADLB")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = tm_t_summary_by(
      label = "Summary by Row Groups Table",
      dataname = "ADLB",
      arm_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      add_total = TRUE,
      by_vars = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], c("PARAM", "AVISIT")),
        selected = c("AVISIT")
      ),
      summarize_vars = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], c("AVAL", "CHG")),
        selected = c("AVAL")
      ),
      useNA = "ifany",
      paramcd = teal.transform::choices_selected(
        choices = teal.transform::value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = "ALT"
      )
    )
  )
}

testthat::test_that("e2e - tm_t_summary_by: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary_by()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_summary_by: Starts with specified label, arm_var, paramcd, by_vars, summarize_vars,
  useNA, numeric_stats, add_total, parallel_vars, row_groups, drop_zero_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_summary_by()
    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Summary by Row Groups Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals"),
      "ALT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("by_vars-dataset_ADLB_singleextract-select"),
      "AVISIT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("summarize_vars-dataset_ADLB_singleextract-select"),
      "AVAL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("useNA"),
      "ifany"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("numeric_stats"),
      c("n", "mean_sd", "median", "range")
    )
    testthat::expect_true(app_driver$get_active_module_input("add_total"))
    testthat::expect_false(app_driver$get_active_module_input("parallel_vars"))
    testthat::expect_false(app_driver$get_active_module_input("row_groups"))
    testthat::expect_true(app_driver$get_active_module_input("drop_zero_levels"))
    app_driver$stop()
  }
)
