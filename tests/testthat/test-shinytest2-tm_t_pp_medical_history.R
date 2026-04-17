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
        mhterm = teal.picks::variables(c("MHTERM", "STUDYID"), multiple = FALSE),
        mhbodsys = teal.picks::variables(c("MHBODSYS", "EOSSTT"), multiple = FALSE),
        mhdistat = teal.picks::variables(c("MHDISTAT", "STUDYID"), multiple = FALSE),
        pre_output = NULL,
        post_output = NULL
      )
    )
  )
}

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Module initializes in teal without errors and produces table output.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    withr::defer(app_driver$stop())
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  }
)

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Starts with specified label, patient_id, mhterm, mhbodsys, mhdistat.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Medical History"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-1-id-1"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(
      exported_values[["mhterm-picks_resolved"]]$variables$selected,
      "MHTERM"
    )
    testthat::expect_equal(
      exported_values[["mhbodsys-picks_resolved"]]$variables$selected,
      "MHBODSYS"
    )
    testthat::expect_equal(
      exported_values[["mhdistat-picks_resolved"]]$variables$selected,
      "MHDISTAT"
    )
  }
)

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Selecting patient_id changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("patient_id", "AB12345-USA-1-id-45")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_medical_history: Deselection of patient_id throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_medical_history()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("patient_id", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
    "Please select a patient"
  )
})

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Selecting mhterm changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "mhterm", "variables", "STUDYID")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_medical_history: Deselection of mhterm throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_medical_history()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "mhterm", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table-with-settings")),
    "Please select MHTERM variable.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Selecting mhbodsys changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "mhbodsys", "variables", "EOSSTT")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_medical_history: Deselection of mhbodsys throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_medical_history()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "mhbodsys", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table-with-settings")),
    "Please select MHBODSYS variable.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_t_pp_medical_history: Selecting mhdistat changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_medical_history()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "mhdistat", "variables", "STUDYID")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_medical_history: Deselection of mhdistat throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_medical_history()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "mhdistat", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table-with-settings")),
    "Please select MHDISTAT variable.",
    fixed = TRUE
  )
})
