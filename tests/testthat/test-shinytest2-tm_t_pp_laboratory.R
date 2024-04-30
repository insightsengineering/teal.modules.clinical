app_driver_tm_t_pp_laboratory <- function() {
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
    modules = tm_t_pp_laboratory(
      label = "Vitals",
      dataname = "ADLB",
      patient_col = "USUBJID",
      paramcd = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], "PARAMCD"),
        selected = "PARAMCD"
      ),
      param = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], "PARAM"),
        selected = "PARAM"
      ),
      timepoints = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], "ADY"),
        selected = "ADY"
      ),
      anrind = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], "ANRIND"),
        selected = "ANRIND"
      ),
      aval_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], "AVAL"),
        selected = "AVAL"
      ),
      avalu_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], "AVALU"),
        selected = "AVALU"
      )
    )
  )
}

testthat::test_that("e2e - tm_t_pp_laboratory: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("lab_values_table"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Starts with specified label, patient_id, paramcd, param,
  timepoints, aval_var, avalu_var, anrind, round_value",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()

    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Vitals"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-3-id-128"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADLB_singleextract-select"),
      "PARAMCD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("param-dataset_ADLB_singleextract-select"),
      "PARAM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("timepoints-dataset_ADLB_singleextract-select"),
      "ADY"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADLB_singleextract-select"),
      "AVAL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("avalu_var-dataset_ADLB_singleextract-select"),
      "AVALU"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("anrind-dataset_ADLB_singleextract-select"),
      "ANRIND"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("round_value"),
      "4"
    )
    app_driver$stop()
  }
)
