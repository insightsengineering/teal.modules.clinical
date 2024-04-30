app_driver_tm_t_pp_prior_medication <- function() { # nolint: object_length
  data <- teal_data()
  data <- within(data, {
    library(dplyr)
    ADCM <- teal.data::rADCM
    ADSL <- teal.data::rADSL %>%
      filter(USUBJID %in% ADCM$USUBJID)
    ADCM$CMASTDTM <- ADCM$ASTDTM
    ADCM$CMAENDTM <- ADCM$AENDTM
  })

  datanames <- c("ADSL", "ADCM")
  teal.data::datanames(data) <- datanames
  keys <- teal.data::default_cdisc_join_keys[datanames]
  keys["ADCM", "ADCM"] <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
  teal.data::join_keys(data) <- keys

  init_teal_app_driver(
    data = data,
    modules = tm_t_pp_prior_medication(
      label = "Prior Medication",
      dataname = "ADCM",
      parentname = "ADSL",
      patient_col = "USUBJID",
      atirel = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADCM"]], "ATIREL"),
        selected = "ATIREL"
      ),
      cmdecod = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADCM"]], "CMDECOD"),
        selected = "CMDECOD"
      ),
      cmindc = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADCM"]], "CMINDC"),
        selected = "CMINDC"
      ),
      cmstdy = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADCM"]], "ASTDY"),
        selected = "ASTDY"
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
      app_driver$is_visible(app_driver$active_module_element("prior_medication_table"))
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
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
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
