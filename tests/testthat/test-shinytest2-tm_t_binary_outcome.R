app_driver_tm_t_binary_outcome <- function() {
  data <- within(teal.data::teal_data(), {
    library(dplyr)
    ADSL <- teal.data::rADSL
    ADRS <- teal.data::rADRS %>%
      mutate(
        AVALC = d_onco_rsp_label(AVALC) %>%
          with_label("Character Result/Finding")
      ) %>%
      filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  arm_ref_comp <- list(
    ARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
    ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
  )

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_binary_outcome(
        label = "Responders",
        dataname = "ADRS",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADRS"]], c("ARM", "ARMCD", "ACTARMCD")),
          selected = "ARM"
        ),
        arm_ref_comp = arm_ref_comp,
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
          selected = "BESRSPI"
        ),
        strata_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADRS"]], c("SEX", "BMRKR2", "RACE")),
          selected = "RACE"
        ),
        aval_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(
            data[["ADRS"]], c("AVALC", "SEX")
          ),
          selected = "AVALC",
          fixed = FALSE
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
        ),
        rsp_table = FALSE,
        control = list(global = list(
          method = "waldcc",
          conf_level = 0.95
        ), unstrat = list(
          method_ci = "waldcc",
          method_test = "schouten", odds = TRUE
        ), strat = list(
          method_ci = "cmh", method_test =
            "cmh"
        )),
        add_total = FALSE,
        total_label = default_total_label(),
        na_level = default_na_str(),
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_binary_outcome: Module initializes in teal without errors and produces table output.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_binary_outcome()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_binary_outcome: Starts with specified label, paramcd, responders, arm_var,
  buckets, u_diff_ci, u_diff_test, strata_var, s_diff_ci, prop_ci_method, conf_level,
  aval_var, compare_arms, combine_comp_arms, u_odds_ratio, show_rsp_cat.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_binary_outcome()

    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
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

testthat::test_that(
  "e2e - tm_t_binary_outcome: Selecting paramcd changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_binary_outcome()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals", "INVET")
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

testthat::test_that("e2e - tm_t_binary_outcome: Deselection of paramcd throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_binary_outcome()
  app_driver$set_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADRS_singleextract-filter1-vals_input .shiny-validation-message"
    ),
    "Please select a filter."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_binary_outcome: Selecting responders changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_binary_outcome()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("responders", c("Stable Disease (SD)", "Progressive Disease (PD)"))
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

testthat::test_that("e2e - tm_t_binary_outcome: Deselection of responders throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_binary_outcome()
  app_driver$set_active_module_input("responders", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text("#teal-teal_modules-responders .shiny-validation-message"),
    "`Responders` field is empty"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_binary_outcome: Selecting arm_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_binary_outcome()
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

testthat::test_that("e2e - tm_t_binary_outcome: Deselection of arm_var throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_binary_outcome()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"),
    "Treatment variable must be selected"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_binary_outcome: Selecting strata_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_binary_outcome()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("strata_var-dataset_ADSL_singleextract-select", "SEX")

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
  "e2e - tm_t_binary_outcome: Deselection of strata_var changes the table and does not throw validation errors.", # nolint line_length_linter
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_binary_outcome()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("strata_var-dataset_ADSL_singleextract-select", NULL)
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
