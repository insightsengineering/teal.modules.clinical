app_driver_tm_t_pp_prior_medication <- function() { # nolint: object_length.
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADCM <- teal.data::rADCM
    ADSL <- teal.data::rADSL %>%
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
        atirel = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("ATIREL", "SEX")),
          selected = "ATIREL"
        ),
        cmdecod = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("CMDECOD", "RACE")),
          selected = "CMDECOD"
        ),
        cmindc = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("CMINDC", "SEX")),
          selected = "CMINDC"
        ),
        cmstdy = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("ASTDY", "AGE")),
          selected = "ASTDY"
        ),
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
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    testthat::expect_true(
      app_driver$is_visible(app_driver$namespaces(TRUE)$module("prior_medication_table"))
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Starts with specified label, patient_id, cmdecod, atirel, cmindc, cmstdy.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Prior Medication"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-3-id-128"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("cmdecod-dataset_ADCM_singleextract-select"),
      "CMDECOD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("atirel-dataset_ADCM_singleextract-select"),
      "ATIREL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("cmindc-dataset_ADCM_singleextract-select"),
      "CMINDC"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("cmstdy-dataset_ADCM_singleextract-select"),
      "ASTDY"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting patient_id changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    app_driver$set_active_module_input("patient_id", "AB12345-USA-1-id-261")
    testthat::expect_false(
      identical(
        nrow(table_before),
        nrow(app_driver$get_active_module_table_output("prior_medication_table"))
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of patient_id throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  app_driver$set_active_module_input("patient_id", NULL)
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("patient_id_input .shiny-validation-message")),
    "Please select patient id"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting cmdecod changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    app_driver$set_active_module_input("cmdecod-dataset_ADCM_singleextract-select", "RACE")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("prior_medication_table")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of cmdecod throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  app_driver$set_active_module_input("cmdecod-dataset_ADCM_singleextract-select", NULL)
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("cmdecod-dataset_ADCM_singleextract-select_input .shiny-validation-message")
    ),
    "A medication decoding variable is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting atirel changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    app_driver$set_active_module_input("atirel-dataset_ADCM_singleextract-select", "SEX")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("prior_medication_table")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of atirel throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  app_driver$set_active_module_input("atirel-dataset_ADCM_singleextract-select", NULL)
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("atirel-dataset_ADCM_singleextract-select_input .shiny-validation-message")
    ),
    "An ATIREL variable is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting cmindc changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    app_driver$set_active_module_input("cmindc-dataset_ADCM_singleextract-select", "SEX")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("prior_medication_table")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of cmindc throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  app_driver$set_active_module_input("cmindc-dataset_ADCM_singleextract-select", NULL)
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("cmindc-dataset_ADCM_singleextract-select_input .shiny-validation-message")
    ),
    "A CMINDC variable is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_prior_medication: Selecting cmstdy changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_prior_medication()
    table_before <- app_driver$get_active_module_table_output("prior_medication_table")
    app_driver$set_active_module_input("cmstdy-dataset_ADCM_singleextract-select", "AGE")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("prior_medication_table")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_prior_medication: Deselection of cmstdy throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_prior_medication()
  app_driver$set_active_module_input("cmstdy-dataset_ADCM_singleextract-select", NULL)
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("cmstdy-dataset_ADCM_singleextract-select_input .shiny-validation-message")
    ),
    "A CMSTDY variable is required"
  )
  app_driver$stop()
})
