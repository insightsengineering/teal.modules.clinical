app_driver_tm_t_shift_by_arm_by_worst <- function() { # nolint: object_length.
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADEG <- tmc_ex_adeg
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_shift_by_arm_by_worst(
        label = "Shift by Arm Table",
        dataname = "ADEG",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], subset = c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        paramcd = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADEG"]], "PARAMCD"),
          selected = "ECGINTP"
        ),
        worst_flag_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADEG"]], c("WORS02FL", "WORS01FL")),
          selected = "WORS02FL"
        ),
        worst_flag = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADEG"]], "WORS02FL"),
          selected = "Y", fixed = TRUE
        ),
        aval_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADEG"]], c("REGION1", "AVALC")),
          selected = "REGION1"
        ),
        baseline_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADEG"]], c("AVISIT", "BASEC")),
          selected = "AVISIT"
        ),
        useNA = "ifany",
        treatment_flag = teal.transform::choices_selected("Y"),
        na_level = default_na_str(),
        add_total = FALSE,
        total_label = default_total_label(),
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Module initializes in teal without errors and produces table output.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    testthat::expect_true(
      app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Starts with specified label, arm_var, paramcd, worst_flag_var,
  aval_var, baseline_var, useNA, treatment_flag_var, add_total.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()

    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "Shift by Arm Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADEG_singleextract-filter1-vals"),
      "ECGINTP"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_flag_var-dataset_ADEG_singleextract-select"),
      "WORS02FL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADEG_singleextract-select"),
      "REGION1"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("baseline_var-dataset_ADEG_singleextract-select"),
      "AVISIT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("useNA"),
      "ifany"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("treatment_flag_var-dataset_ADEG_singleextract-select"),
      "ONTRTFL"
    )
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Selecting arm_var changes the table
  and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
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

testthat::test_that("e2e - tm_t_shift_by_arm_by_worst: Deselection of arm_var throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"),
    "A treatment variable is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Selecting paramcd changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADEG_singleextract-filter1-vals", "HR")
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

testthat::test_that("e2e - tm_t_shift_by_arm_by_worst: Deselection of paramcd throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
  app_driver$set_active_module_input("paramcd-dataset_ADEG_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADEG_singleextract-filter1-vals_input .shiny-validation-message"
    ),
    "An endpoint is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Selecting worst_flag changes the table
  and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("worst_flag_var-dataset_ADEG_singleextract-select", "WORS01FL")
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

testthat::test_that("e2e - tm_t_shift_by_arm_by_worst: Deselection of worst_flag throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
  app_driver$set_active_module_input("worst_flag_var-dataset_ADEG_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module(
      "worst_flag_var-dataset_ADEG_singleextract-select_input .shiny-validation-message"
    ),
    "A worst flag variable is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Selecting aval_var changes the table
  and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("aval_var-dataset_ADEG_singleextract-select", "AVALC")
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

testthat::test_that("e2e - tm_t_shift_by_arm_by_worst: Deselection of aval_var throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
  app_driver$set_active_module_input("aval_var-dataset_ADEG_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module("aval_var-dataset_ADEG_singleextract-select_input .shiny-validation-message"),
    "An analysis range indicator required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Selecting baseline_var changes the table
  and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("baseline_var-dataset_ADEG_singleextract-select", "BASEC")
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

testthat::test_that("e2e - tm_t_shift_by_arm_by_worst: Deselection of baseline_var throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
  app_driver$set_active_module_input("baseline_var-dataset_ADEG_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module(
      "baseline_var-dataset_ADEG_singleextract-select_input .shiny-validation-message"
    ),
    "A baseline reference range indicator is required"
  )
  app_driver$stop()
})
