app_driver_tm_t_events <- function() {
  data <- teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADAE <- teal.data::rADAE
  })
  datanames <- c("ADSL", "ADAE")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = tm_t_events(
      label = "Adverse Event Table",
      dataname = "ADAE",
      arm_var = teal.transform::choices_selected(c("ARM", "ARMCD"), "ARM"),
      llt = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADAE"]], c("AETERM", "AEDECOD")),
        selected = c("AEDECOD")
      ),
      hlt = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADAE"]], c("AEBODSYS", "AESOC")),
        selected = "AEBODSYS"
      ),
      add_total = TRUE,
      event_type = "adverse event"
    )
  )
}

testthat::test_that("e2e - tm_t_events: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events: Starts with specified label, arm_var, hlt, llt, sort_criteria,
  prune_freq, prune_diff, add_total, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events()

    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Adverse Event Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("hlt-dataset_ADAE_singleextract-select"),
      "AEBODSYS"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("llt-dataset_ADAE_singleextract-select"),
      "AEDECOD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("sort_criteria"),
      "freq_desc"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("prune_freq"),
      0
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("prune_diff"),
      0
    )
    testthat::expect_true(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    app_driver$stop()
  }
)
