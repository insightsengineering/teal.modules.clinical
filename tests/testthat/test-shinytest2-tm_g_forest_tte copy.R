app_driver_tm_g_ipp <- function() {
  data <- teal_data() %>%
    within({
      library(nestcolor)
      library(dplyr)
      ADSL <- tmc_ex_adsl %>%
        slice(1:20) %>%
        df_explicit_na()
      ADLB <- tmc_ex_adlb %>%
        filter(USUBJID %in% ADSL$USUBJID) %>%
        df_explicit_na() %>%
        filter(AVISIT != "SCREENING")
    })

  datanames <- c("ADSL", "ADLB")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = teal::modules(
      tm_g_ipp(
        label = "Individual Patient Plot",
        dataname = "ADLB",
        arm_var = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADLB"]], "ARMCD"),
          "ARM A"
        ),
        paramcd = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADLB"]], "PARAMCD"),
          "ALT"
        ),
        aval_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("AVAL", "CHG")),
          "AVAL"
        ),
        avalu_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("AVALU")),
          "AVALU",
          fixed = TRUE
        ),
        id_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("USUBJID")),
          "USUBJID",
          fixed = TRUE
        ),
        visit_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("AVISIT")),
          "AVISIT"
        ),
        baseline_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("BASE")),
          "BASE",
          fixed = TRUE
        ),
        add_baseline_hline = FALSE,
        separate_by_obs = FALSE
      )
    )
  )
}

testthat::test_that("e2e - tm_g_ipp: module initializes in teal without errors and produces plot output", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_match(
    app_driver$get_attr(
      app_driver$active_module_element("myplot-plot_main > img"),
      "src"
    ),
    "data:image/png;base64,"
  )
  app_driver$stop()
})
