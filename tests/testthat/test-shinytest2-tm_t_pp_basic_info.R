app_driver_tm_t_pp_basic_info <- function() {
  data <- teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
  })
  teal.data::datanames(data) <- "ADSL"
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys["ADSL"]

  init_teal_app_driver(
    data = data,
    modules = tm_t_pp_basic_info(
      label = "Basic Info",
      dataname = "ADSL",
      patient_col = "USUBJID",
      vars = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADSL"]]),
        selected = c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT")
      )
    )
  )
}

testthat::test_that("e2e - tm_t_pp_basic_info: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_basic_info()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("basic_info_table"))
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_t_pp_basic_info: Starts with specified label, patient_id, vars", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_basic_info()
  testthat::expect_equal(
    app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Basic Info"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("patient_id"),
    "AB12345-CHN-3-id-128"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("vars-dataset_ADSL_singleextract-select"),
    c("AGE", "SEX", "RACE", "COUNTRY", "ARM", "EOSSTT")
  )
  app_driver$stop()
})
