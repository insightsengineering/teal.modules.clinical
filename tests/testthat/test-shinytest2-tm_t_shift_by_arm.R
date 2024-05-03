app_driver_tm_t_shift_by_arm <- function() {
  data <- teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADEG <- tmc_ex_adeg
  })

  datanames <- c("ADSL", "ADEG")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = tm_t_shift_by_arm(
      label = "Shift by Arm Table",
      dataname = "ADEG",
      arm_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADSL"]], subset = c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      paramcd = teal.transform::choices_selected(
        teal.transform::value_choices(data[["ADEG"]], "PARAMCD"),
        selected = "HR"
      ),
      visit_var = teal.transform::choices_selected(
        teal.transform::value_choices(data[["ADEG"]], "AVISIT"),
        selected = "POST-BASELINE MINIMUM"
      ),
      aval_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADEG"]], subset = "ANRIND"),
        selected = "ANRIND", fixed = TRUE
      ),
      baseline_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADEG"]], subset = "BNRIND"),
        selected = "BNRIND", fixed = TRUE
      ),
      useNA = "ifany"
    )
  )
}

testthat::test_that("e2e - tm_t_shift_by_arm: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm: Starts with specified label, arm_varparamcd, visit_var,
  useNA, treatment_flag_var, add_total.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm()
    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Shift by Arm Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADEG_singleextract-filter1-vals"),
      "HR"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("visit_var-dataset_ADEG_singleextract-filter1-vals"),
      "POST-BASELINE MINIMUM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("useNA"),
      "ifany"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("treatment_flag_var-dataset_ADEG_singleextract-select"),
      "ONTRTFL"
    )
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_shift_by_arm: Selecting arm_var-variable changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm()
    table_before <- app_driver$get_active_module_tws_output("table")
    app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", "ARMCD")
    testthat::expect_false(identical(table_before, app_driver$get_active_module_tws_output("table")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_shift_by_arm: Deselection of arm_var-variable throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_tws_output("table"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"),
    "A treatment variable is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm: Selecting paramcd-level changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm()
    table_before <- app_driver$get_active_module_tws_output("table")
    app_driver$set_active_module_input("paramcd-dataset_ADEG_singleextract-filter1-vals", "QT")
    testthat::expect_false(identical(table_before, app_driver$get_active_module_tws_output("table")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_shift_by_arm: Deselection of paramcd-level throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm()
  app_driver$set_active_module_input("paramcd-dataset_ADEG_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_tws_output("table"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text(
      "paramcd-dataset_ADEG_singleextract-filter1-vals_input .shiny-validation-message"
    ),
    "An endpoint is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm: Selecting visit_var-variable changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm()
    table_before <- app_driver$get_active_module_tws_output("table")
    app_driver$set_active_module_input("visit_var-dataset_ADEG_singleextract-filter1-vals", "SCREENING")
    testthat::expect_false(identical(table_before, app_driver$get_active_module_tws_output("table")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_shift_by_arm: Deselection of visit_var-variable throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm()
  app_driver$set_active_module_input("visit_var-dataset_ADEG_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_tws_output("table"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text(
      "visit_var-dataset_ADEG_singleextract-filter1-vals_input .shiny-validation-message"
    ),
    "A visit is required"
  )
  app_driver$stop()
})
