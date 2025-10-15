app_driver_tm_t_pp_medical_history <- function() { # nolint: object_length.
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADMH <- tmc_ex_admh
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_pp_medical_history(
        label = "Medical History",
        dataname = "ADMH",
        parentname = "ADSL",
        patient_col = "USUBJID",
        mhterm = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADMH"]], c("MHTERM", "STUDYID")),
          selected = "MHTERM"
        ),
        mhbodsys = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADMH"]], c("MHBODSYS", "EOSSTT")),
          selected = "MHBODSYS"
        ),
        mhdistat = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADMH"]], c("MHDISTAT", "STUDYID")),
          selected = "MHDISTAT"
        ),
        pre_output = NULL,
        post_output = NULL
      )
    )
  )
}

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Module initializes in teal without errors and produces table output.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    testthat::expect_true(
      app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Starts with specified label, patient_id, mhterm, mhbodsys, mhdistat.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()

    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "Medical History"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-1-id-1"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("mhterm-dataset_ADMH_singleextract-select"),
      "MHTERM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("mhbodsys-dataset_ADMH_singleextract-select"),
      "MHBODSYS"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("mhdistat-dataset_ADMH_singleextract-select"),
      "MHDISTAT"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Selecting patient_id changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("patient_id", "AB12345-USA-1-id-45")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_medical_history: Deselection of patient_id throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_medical_history()
  app_driver$set_active_module_input("patient_id", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text("patient_id_input .shiny-validation-message"),
    "Please select a patient"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Selecting mhterm changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("mhterm-dataset_ADMH_singleextract-select", "STUDYID")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_medical_history: Deselection of mhterm throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_medical_history()
  app_driver$set_active_module_input("mhterm-dataset_ADMH_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text("mhterm-dataset_ADMH_singleextract-select_input .shiny-validation-message"),
    "Please select MHTERM variable."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Selecting mhbodsys changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("mhbodsys-dataset_ADMH_singleextract-select", "EOSSTT")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_medical_history: Deselection of mhbodsys throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_medical_history()
  app_driver$set_active_module_input("mhbodsys-dataset_ADMH_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text("mhbodsys-dataset_ADMH_singleextract-select_input .shiny-validation-message"),
    "Please select MHBODSYS variable."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Selecting mhbodsys changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("mhdistat-dataset_ADMH_singleextract-select", "STUDYID")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_medical_history: Deselection of mhdistat throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_medical_history()
  app_driver$set_active_module_input("mhdistat-dataset_ADMH_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text("mhdistat-dataset_ADMH_singleextract-select_input .shiny-validation-message"),
    "Please select MHDISTAT variable."
  )
  app_driver$stop()
})
