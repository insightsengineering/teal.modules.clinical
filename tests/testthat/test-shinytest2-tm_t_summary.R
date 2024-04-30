app_driver_tm_t_summary <- function() {
  data <- teal.data::teal_data() %>%
    within({
      ADSL <- teal.data::rADSL
      ADSL$EOSDY[1] <- NA_integer_
    })
  teal.data::datanames(data) <- "ADSL"
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys["ADSL"]

  init_teal_app_driver(
    data = data,
    modules = tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = teal.transform::choices_selected(c("ARM", "ARMCD"), "ARM"),
      add_total = TRUE,
      summarize_vars = teal.transform::choices_selected(
        c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE"),
        c("SEX", "RACE")
      ),
      useNA = "ifany"
    )
  )
}

testthat::test_that("e2e - tm_t_summary: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_t_summary: Starts with specified label, arm_var, summarize_vars, useNA, denominator.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary()
  testthat::expect_equal(
    app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Demographic Table"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
    "ARM"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("summarize_vars-dataset_ADSL_singleextract-select"),
    c("SEX", "RACE")
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("useNA"),
    "ifany"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("denominator"),
    "N"
  )
  app_driver$stop()
})
