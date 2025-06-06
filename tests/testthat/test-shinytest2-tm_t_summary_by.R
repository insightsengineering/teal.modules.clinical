app_driver_tm_t_summary_by <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADLB <- teal.data::rADLB
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    data = data,
    modules = tm_t_summary_by(
      label = "Summary by Row Groups Table",
      dataname = "ADLB",
      parentname = "ADSL",
      arm_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      add_total = TRUE,
      by_vars = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], c("PARAM", "AVISIT")),
        selected = c("AVISIT")
      ),
      summarize_vars = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADLB"]], c("AVAL", "CHG")),
        selected = c("AVAL")
      ),
      useNA = "ifany",
      paramcd = teal.transform::choices_selected(
        choices = teal.transform::value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = "ALT"
      ),
      id_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADLB"]], subset = "USUBJID"),
        selected = "USUBJID", fixed = TRUE
      ),
      total_label = default_total_label(),
      parallel_vars = FALSE,
      row_groups = FALSE,
      na_level = default_na_str(),
      numeric_stats = c("n", "mean_sd", "median", "range"),
      denominator = teal.transform::choices_selected(c("n", "N", "omit"), "omit",
        fixed = TRUE
      ),
      drop_arm_levels = TRUE,
      drop_zero_levels = TRUE,
      pre_output = NULL,
      post_output = NULL,
      basic_table_args = teal.widgets::basic_table_args()
    )
  )
}

testthat::test_that("e2e - tm_t_summary_by: Module initializes in teal without errors and produces table output.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary_by()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_summary_by: Starts with specified label, arm_var, paramcd, by_vars, summarize_vars,
  useNA, numeric_stats, add_total, parallel_vars, row_groups, drop_zero_levels.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_summary_by()
    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "Summary by Row Groups Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals"),
      "ALT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("by_vars-dataset_ADLB_singleextract-select"),
      "AVISIT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("summarize_vars-dataset_ADLB_singleextract-select"),
      "AVAL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("useNA"),
      "ifany"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("numeric_stats"),
      c("n", "mean_sd", "median", "range")
    )
    testthat::expect_true(app_driver$get_active_module_input("add_total"))
    testthat::expect_false(app_driver$get_active_module_input("parallel_vars"))
    testthat::expect_false(app_driver$get_active_module_input("row_groups"))
    testthat::expect_true(app_driver$get_active_module_input("drop_zero_levels"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_summary_by: Selecting arm_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_summary_by()
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

testthat::test_that("e2e - tm_t_summary_by: Deselection of arm_var throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary_by()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"),
    "Please select 1 or 2 column variables"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_summary_by: Selecting arm_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_summary_by()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals", "CRP")
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

testthat::test_that("e2e - tm_t_summary_by: Deselection of arm_var throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary_by()
  app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text(
      "paramcd-dataset_ADLB_singleextract-filter1-vals_input .shiny-validation-message"
    ),
    "Please select a filter."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_summary_by: Selecting arm_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_summary_by()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("by_vars-dataset_ADLB_singleextract-select", "PARAM")
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
  "e2e - tm_t_summary_by: Deselection of arm_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_summary_by()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("by_vars-dataset_ADLB_singleextract-select", NULL)
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
  "e2e - tm_t_summary_by: Selecting summarize_vars changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_summary_by()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("summarize_vars-dataset_ADLB_singleextract-select", "CHG")
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

testthat::test_that("e2e - tm_t_summary_by: Deselection of summarize_vars throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary_by()
  app_driver$set_active_module_input("summarize_vars-dataset_ADLB_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$active_module_element_text(
      "summarize_vars-dataset_ADLB_singleextract-select_input .shiny-validation-message"
    ),
    "Please select a summarize variable."
  )
  app_driver$stop()
})
