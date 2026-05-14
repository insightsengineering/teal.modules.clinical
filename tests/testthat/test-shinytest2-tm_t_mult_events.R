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
        arm_var = teal.picks::variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
        seq_var = teal.picks::variables(choices = "CMSEQ", selected = "CMSEQ", fixed = TRUE),
        hlt = teal.picks::variables(
          choices = c("ATC1", "ATC2", "ATC3", "ATC4"),
          selected = c("ATC1", "ATC2", "ATC3", "ATC4")
        ),
        llt = teal.picks::variables(
          choices = c("CMDECOD"),
          selected = c("CMDECOD"),
          fixed = FALSE
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
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
})

testthat::test_that(
  "e2e - tm_t_mult_events: Starts with specified label, arm_var, hlt, llt, add_total, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_mult_events()
    withr::defer(app_driver$stop())
    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Concomitant Medications by Medication Class and Preferred Name"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-variables-selected"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("hlt-variables-selected"),
      c("ATC1", "ATC2", "ATC3", "ATC4")
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("llt-variables-selected"),
      "CMDECOD"
    )
    testthat::expect_true(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
  }
)

testthat::test_that(
  "e2e - tm_t_mult_events: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_mult_events()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "arm_var", "variables", "ARMCD")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_mult_events: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_mult_events()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "arm_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("table-table_out_main")
    ),
    "Please select a treatment variable."
  )
})

testthat::test_that(
  "e2e - tm_t_mult_events: Selecting hlt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_mult_events()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "hlt", "variables", c("ATC1", "ATC2"))
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_t_mult_events: Deselection of hlt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_mult_events()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "hlt", "variables", NULL)
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_mult_events: Deselection of llt throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_mult_events()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "llt", "variables", NULL)
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("table-table_out_main")
    ),
    "Please select a \"LOW LEVEL TERM\" variable"
  )
})
