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
