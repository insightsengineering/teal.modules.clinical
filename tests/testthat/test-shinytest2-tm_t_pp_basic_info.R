app_driver_tm_t_pp_basic_info <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_pp_basic_info(
        label = "Basic Info",
        dataname = "ADSL",
        patient_col = "USUBJID",
        vars = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]]),
          selected = c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT")
        ),
        pre_output = NULL,
        post_output = NULL
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
    app_driver$is_visible(app_driver$namespaces(TRUE)$module("basic_info_table"))
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_t_pp_basic_info: Starts with specified label, patient_id, vars.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_basic_info()
  testthat::expect_equal(
    app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
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

testthat::test_that(
  "e2e - tm_t_pp_basic_info: Selecting patient_id changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_basic_info()
    table_before <- app_driver$get_active_module_table_output("basic_info_table")
    app_driver$set_active_module_input("patient_id", "AB12345-USA-1-id-261")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("basic_info_table")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_basic_info: Deselection of patient_id throws validation error and table is not visible.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_basic_info()
    app_driver$set_active_module_input("patient_id", NULL)
    testthat::expect_false(
      app_driver$is_visible(
        app_driver$namespaces(TRUE)$module("basic_info_table"),
        visibility_property = TRUE
      )
    )
    app_driver$expect_validation_error()
    testthat::expect_equal(
      app_driver$namespaces(TRUE)$module("patient_id_input .shiny-validation-message"),
      "Please select a patient"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_basic_info: Selecting cov_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_basic_info()
    table_before <- app_driver$get_active_module_table_output("basic_info_table")
    app_driver$set_active_module_input(
      "vars-dataset_ADSL_singleextract-select",
      c("AGE", "BMRKR1")
    )
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("basic_info_table")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_basic_info: Deselection of cov_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_basic_info()
  app_driver$set_active_module_input("vars-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_false(
    app_driver$is_visible(
      app_driver$namespaces(TRUE)$module("basic_info_table"),
      visibility_property = TRUE
    )
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module("vars-dataset_ADSL_singleextract-select_input .shiny-validation-message"),
    "Please select basic info variables"
  )
  app_driver$stop()
})
