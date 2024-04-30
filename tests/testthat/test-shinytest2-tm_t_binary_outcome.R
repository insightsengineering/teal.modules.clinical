app_driver_tm_t_binary_outcome <- function() {
  data <- teal.data::teal_data() %>%
    within({
      library(dplyr)
      ADSL <- teal.data::rADSL
      ADRS <- teal.data::rADRS %>%
        mutate(
          AVALC = d_onco_rsp_label(AVALC) %>%
            with_label("Character Result/Finding")
        ) %>%
        filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
    })

  datanames <- c("ADSL", "ADRS")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  arm_ref_comp <- list(
    ARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
    ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
  )
  init_teal_app_driver(
    data = data,
    modules = tm_t_binary_outcome(
      label = "Responders",
      dataname = "ADRS",
      paramcd = teal.transform::choices_selected(
        choices = teal.transform::value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
        selected = "BESRSPI"
      ),
      arm_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADRS"]], c("ARM", "ARMCD", "ACTARMCD")),
        selected = "ARM"
      ),
      arm_ref_comp = arm_ref_comp,
      strata_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADRS"]], c("SEX", "BMRKR2", "RACE")),
        selected = "RACE"
      ),
      conf_level = teal.transform::choices_selected(
        c(2, 0.95, 0.9, 0.8), 0.95,
        keep_order = TRUE
      ),
      default_responses = list(
        BESRSPI = list(
          rsp = c("Complete Response (CR)", "Partial Response (PR)"),
          levels = c(
            "Complete Response (CR)", "Partial Response (PR)",
            "Stable Disease (SD)", "Progressive Disease (PD)"
          )
        ),
        INVET = list(
          rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
          levels = c(
            "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
            "Progressive Disease (PD)", "Stable Disease (SD)"
          )
        ),
        OVRINV = list(
          rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
          levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
        )
      )
    )
  )
}

testthat::test_that("e2e - tm_t_binary_outcome: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_binary_outcome()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_binary_outcome: Starts with specified label, paramcd, responders, arm_var,
  buckets, u_diff_ci, u_diff_test, strata_var, s_diff_ci, prop_ci_method, conf_level,
  aval_var, compare_arms, combine_comp_arms, u_odds_ratio, show_rsp_cat.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_binary_outcome()

    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Responders"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals"),
      "BESRSPI"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("responders"),
      c("Complete Response (CR)", "Partial Response (PR)")
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(
        Ref = list("B: Placebo"),
        Comp = list("A: Drug X", "C: Combination")
      )
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("u_diff_ci"),
      "waldcc"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("u_diff_test"),
      "schouten"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("strata_var-dataset_ADSL_singleextract-select"),
      "RACE"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("s_diff_ci"),
      "cmh"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("prop_ci_method"),
      "waldcc"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADRS_singleextract-select"),
      "AVALC"
    )
    testthat::expect_true(app_driver$get_active_module_input("compare_arms"))
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    testthat::expect_true(app_driver$get_active_module_input("u_odds_ratio"))
    testthat::expect_false(app_driver$get_active_module_input("show_rsp_cat"))
    app_driver$stop()
  }
)
