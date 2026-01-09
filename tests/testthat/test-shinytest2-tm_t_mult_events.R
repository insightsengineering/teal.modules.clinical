skip("CI test")
app_driver_tm_t_mult_events <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADCM <- teal.data::rADCM
  })
  keys <- teal.data::default_cdisc_join_keys[names(data)]
  keys["ADCM", "ADCM"] <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
  teal.data::join_keys(data) <- keys

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_mult_events(
        label = "Concomitant Medications by Medication Class and Preferred Name",
        dataname = "ADCM",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(c("ARM", "ARMCD"), "ARM"),
        seq_var = teal.transform::choices_selected("CMSEQ", selected = "CMSEQ", fixed = TRUE),
        hlt = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("ATC1", "ATC2", "ATC3", "ATC4")),
          selected = c("ATC1", "ATC2", "ATC3", "ATC4")
        ),
        llt = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("CMDECOD")),
          selected = c("CMDECOD")
        ),
        add_total = TRUE,
        event_type = "treatment",
        title_text = "Concom. Meds",
        total_label = default_total_label(),
        na_level = default_na_str(),
        drop_arm_levels = TRUE,
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_mult_events: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_mult_events()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_mult_events: Starts with specified label, arm_var, hlt, llt, add_total, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_mult_events()
    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Concomitant Medications by Medication Class and Preferred Name"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("hlt-dataset_ADCM_singleextract-select"),
      c("ATC1", "ATC2", "ATC3", "ATC4")
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("llt-dataset_ADCM_singleextract-select"),
      "CMDECOD"
    )
    testthat::expect_true(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_mult_events: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_mult_events()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", "ARMCD")
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

testthat::test_that("e2e - tm_t_mult_events: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_mult_events()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message")
    ),
    "Please select a treatment variable"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_mult_events: Selecting hlt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_mult_events()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("hlt-dataset_ADCM_singleextract-select", c("ATC1", "ATC2"))
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

testthat::test_that(
  "e2e - tm_t_mult_events: Deselection of hlt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_mult_events()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("hlt-dataset_ADCM_singleextract-select", NULL)
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

testthat::test_that("e2e - tm_t_mult_events: Deselection of llt throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_mult_events()
  app_driver$set_active_module_input("llt-dataset_ADCM_singleextract-select", NULL)
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("llt-dataset_ADCM_singleextract-select_input .shiny-validation-message")
    ),
    "Please select a \"LOW LEVEL TERM\" variable"
  )
  app_driver$stop()
})
