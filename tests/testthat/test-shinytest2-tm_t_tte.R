app_driver_tm_t_tte <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADTTE <- teal.data::rADTTE
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

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
    teal::init(
      data = data,
      modules = tm_t_tte(
        label = "Time To Event Table",
        dataname = "ADTTE",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD", "ACTARMCD")),
          "ARM"
        ),
        arm_ref_comp = arm_ref_comp,
        paramcd = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
          "OS"
        ),
        strata_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], c("SEX", "BMRKR2")),
          "SEX"
        ),
        time_points = teal.transform::choices_selected(c(182, 243), 182),
        event_desc_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADTTE"]], "EVNTDESC"),
          "EVNTDESC",
          fixed = TRUE
        ),
        aval_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADTTE"]], "AVAL"), "AVAL",
          fixed = TRUE
        ),
        cnsr_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADTTE"]], "CNSR"), "CNSR",
          fixed = TRUE
        ),
        conf_level_coxph = teal.transform::choices_selected(
          c(0.95, 0.9, 0.8), 0.95,
          keep_order = TRUE
        ),
        conf_level_survfit = teal.transform::choices_selected(
          c(0.95, 0.9, 0.8), 0.95,
          keep_order = TRUE
        ),
        time_unit_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADTTE"]], "AVALU"), "AVALU",
          fixed = TRUE
        ),
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

testthat::test_that("e2e - tm_t_tte: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_tte()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_tte: Starts with specified label, paramcd, event_desc_var, arm_var, buckets,
  strata_var, time_points, pval_method_coxph, ties_coxph, conf_level_coxph,
  conf_level_survfit, conf_type_survfit, probs_survfit, compare_arms, combine_comp_arms.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Time To Event Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADTTE_singleextract-filter1-vals"),
      "OS"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("event_desc_var-dataset_ADTTE_singleextract-select"),
      "EVNTDESC"
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
      app_driver$get_active_module_input("strata_var-dataset_ADSL_singleextract-select"),
      "SEX"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("time_points"),
      "182"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("pval_method_coxph"),
      "log-rank"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("ties_coxph"),
      "exact"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level_coxph"),
      "0.95"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level_survfit"),
      "0.95"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_type_survfit"),
      "plain"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("probs_survfit"),
      c(0.25, 0.75)
    )
    testthat::expect_true(app_driver$get_active_module_input("compare_arms"))
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_tte: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADTTE_singleextract-filter1-vals", "CRSD")
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

testthat::test_that("e2e - tm_t_tte: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_tte()
  app_driver$set_active_module_input("paramcd-dataset_ADTTE_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADTTE_singleextract-filter1-vals_input .shiny-validation-message"
    )),
    "An endpoint is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_tte: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
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

testthat::test_that("e2e - tm_t_tte: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_tte()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"
    )),
    "Treatment variable must be selected"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_tte: Selecting strata_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("strata_var-dataset_ADSL_singleextract-select", "BMRKR2")
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
  "e2e - tm_t_tte: Deselection of strata_var changes the table and throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
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
