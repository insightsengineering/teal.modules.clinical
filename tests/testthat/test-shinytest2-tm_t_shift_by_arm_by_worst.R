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
        arm_var = variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
        paramcd = variables(choices = "PARAMCD"),
        worst_flag_var = variables(
          choices = c("WORS02FL", "WORS01FL"),
          selected = "WORS02FL"
        ),
        worst_flag = teal.transform::choices_selected(c("Y", "N"), selected = "Y", fixed = TRUE),
        aval_var = variables(choices = c("AVALC", "ANRIND"), selected = "ANRIND"),
        baseline_var = variables(choices = c("BASEC", "BNRIND"), selected = "BNRIND"),
        useNA = "ifany",
        treatment_flag = teal.transform::choices_selected(c("Y", "N"), selected = "Y", fixed = TRUE),
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
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Starts with specified label, arm_var, worst_flag_var,
  aval_var, baseline_var, useNA, add_total.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Shift by Arm Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-variables-selected"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_flag_var-variables-selected"),
      "WORS02FL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-variables-selected"),
      "ANRIND"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("baseline_var-variables-selected"),
      "BNRIND"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("useNA"),
      "ifany"
    )
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Selecting arm_var changes the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("arm_var-variables-selected", "ARMCD")
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
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
  app_driver$set_active_module_input("arm_var-variables-selected", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-values-selected", "HR")
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
  "e2e - tm_t_shift_by_arm_by_worst: Selecting worst_flag_var changes the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("worst_flag_var-variables-selected", "WORS01FL")
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

testthat::test_that("e2e - tm_t_shift_by_arm_by_worst: Deselection of worst_flag_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
  app_driver$set_active_module_input("worst_flag_var-variables-selected", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Selecting aval_var changes the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("aval_var-variables-selected", "AVALC")
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
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
  app_driver$set_active_module_input("aval_var-variables-selected", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Selecting baseline_var changes the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("baseline_var-variables-selected", "BASEC")
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
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
  app_driver$set_active_module_input("baseline_var-variables-selected", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})
