app_driver_tm_t_pp_prior_medication <- function() { # nolint: object_length.
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADCM <- teal.data::rADCM
    ADSL <- teal.data::rADSL |>
      filter(USUBJID %in% ADCM$USUBJID)
    ADCM$CMASTDTM <- ADCM$ASTDTM
    ADCM$CMAENDTM <- ADCM$AENDTM
  })
  keys <- teal.data::default_cdisc_join_keys[names(data)]
  keys["ADCM", "ADCM"] <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
  teal.data::join_keys(data) <- keys

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_pp_prior_medication(
        label = "Prior Medication",
        dataname = "ADCM",
        parentname = "ADSL",
        patient_col = "USUBJID",
        atirel = teal.picks::variables(c("ATIREL", "SEX"), multiple = FALSE),
        cmdecod = teal.picks::variables(c("CMDECOD", "RACE"), multiple = FALSE),
        cmindc = teal.picks::variables(c("CMINDC", "SEX"), multiple = FALSE),
        cmstdy = teal.picks::variables(c("ASTDY", "AGE"), multiple = FALSE),
        pre_output = NULL,
        post_output = NULL
      )
    )
  )
}

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Module initializes in teal without errors and produces table output.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    withr::defer(app_driver$stop())
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("prior_medication_table"))
  }
)

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Starts with specified label, patient_id, cmdecod, atirel, cmindc, cmstdy.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Prior Medication"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-3-id-128"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(
      exported_values[["cmdecod-picks_resolved"]]$variables$selected,
      "CMDECOD"
    )
    testthat::expect_equal(
      exported_values[["atirel-picks_resolved"]]$variables$selected,
      "ATIREL"
    )
    testthat::expect_equal(
      exported_values[["cmindc-picks_resolved"]]$variables$selected,
      "CMINDC"
    )
    testthat::expect_equal(
      exported_values[["cmstdy-picks_resolved"]]$variables$selected,
      "ASTDY"
    )
  }
)

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting patient_id changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    app_driver$set_active_module_input("patient_id", "AB12345-USA-1-id-261")
    testthat::expect_false(
      identical(
        nrow(table_before),
        nrow(app_driver$get_active_module_table_output("prior_medication_table"))
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of patient_id throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("patient_id", NULL)
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(".teal-widgets.standard-layout .htmlwidgets-error"),
    "Please select patient id"
  )
})

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting cmdecod changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    set_teal_picks_slot(app_driver, "cmdecod", "variables", "RACE")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("prior_medication_table")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of cmdecod throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmdecod", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(".teal-widgets.standard-layout .htmlwidgets-error"),
    "A medication decoding variable is required",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting atirel changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    set_teal_picks_slot(app_driver, "atirel", "variables", "SEX")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("prior_medication_table")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of atirel throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "atirel", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(".teal-widgets.standard-layout .htmlwidgets-error"),
    "An ATIREL variable is required",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting cmindc changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    set_teal_picks_slot(app_driver, "cmindc", "variables", "SEX")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("prior_medication_table")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of cmindc throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmindc", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(".teal-widgets.standard-layout .htmlwidgets-error"),
    "A CMINDC variable is required",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting cmstdy changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    set_teal_picks_slot(app_driver, "cmstdy", "variables", "AGE")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("prior_medication_table")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of cmstdy throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmstdy", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(".teal-widgets.standard-layout .htmlwidgets-error"),
    "A CMSTDY variable is required",
    fixed = TRUE
  )
})
