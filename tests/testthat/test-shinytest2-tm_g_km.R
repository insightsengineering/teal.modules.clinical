app_driver_tm_g_km <- function() {
  data <- teal.data::teal_data() %>%
    within({
      library(dplyr)
      ADSL <- tmc_ex_adsl
      ADTTE <- tmc_ex_adtte %>%
        rename(
          VALUE_UNIT = AVALU,
          ANALYSIS_VAL = AVAL,
          CENSORING = CNSR
        )
    })

  datanames <- c("ADSL", "ADTTE")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  arm_ref_comp <- list(
    ACTARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    ),
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    )
  )

  init_teal_app_driver(
    data = data,
    modules = tm_g_km(
      label = "Kaplan-Meier Plot",
      dataname = "ADTTE",
      arm_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD", "ACTARMCD")),
        "ARM"
      ),
      paramcd = teal.transform::choices_selected(
        teal.transform::value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
        "OS"
      ),
      arm_ref_comp = arm_ref_comp,
      strata_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADSL"]], c("SEX", "BMRKR2")),
        "SEX"
      ),
      facet_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADSL"]], c("SEX", "BMRKR2")),
        NULL
      ),
      time_unit_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADTTE"]], "VALUE_UNIT"), "VALUE_UNIT", fixed = TRUE
      ),
      aval_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADTTE"]], "ANALYSIS_VAL"), "ANALYSIS_VAL", fixed = TRUE
      ),
      cnsr_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADTTE"]], "CENSORING"), "CENSORING", fixed = TRUE
      ),
      conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8, -1), 0.95, keep_order = TRUE)
    )
  )
}

testthat::test_that("e2e - tm_g_km: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_match(
    app_driver$get_active_module_pws_output("myplot"),
    "data:image/png;base64,"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_q_km: Starts with specified paramcd, aval_var, cnsr_var, facet_var, arm_var, compare_arms, strata_var.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_q_km()

  testthat::expect_equal(
    app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Kaplan-Meier Plot"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("paramcd-dataset_ADTTE_singleextract-filter1-vals"),
    "OS"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("aval_var-dataset_ADTTE_singleextract-select"),
    "ANALYSIS_VAL"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("cnsr_var-dataset_ADTTE_singleextract-select"),
    "CENSORING"
  )


  testthat::expect_equal(
    app_driver$get_active_module_input("facet_var-dataset_ADSL_singleextract-select"),
    NULL
  )


  testthat::expect_equal(
    app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
    "ARM"
  )

  testthat::expect_true(app_driver$get_active_module_input("compare_arms"))

  testthat::expect_equal(
    app_driver$get_active_module_input("strata_var-dataset_ADSL_singleextract-select"),
    "SEX"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_q_km: Starts with specified groups.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_q_km()

  app_driver$stop()
})

testthat::test_that("e2e - tm_q_km: Starts with specified comparison settings.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_q_km()

  app_driver$stop()
})

testthat::test_that("e2e - tm_q_km: Starts with specified additional plot settings.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_q_km()

  app_driver$stop()
})
