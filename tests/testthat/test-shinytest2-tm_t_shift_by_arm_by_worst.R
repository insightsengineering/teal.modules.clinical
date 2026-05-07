app_driver_tm_t_shift_by_arm_by_worst <- function() { # nolint: object_length.
  paramcd_levels <- unique(as.character(teal.modules.clinical::tmc_ex_adeg$PARAMCD))
  paramcd_default <- if ("HR" %in% paramcd_levels) "HR" else paramcd_levels[[1]]
  paramcd <- teal.picks::picks(
    teal.picks::variables(choices = "PARAMCD", selected = "PARAMCD"),
    teal.picks::values(
      choices = paramcd_levels,
      selected = paramcd_default,
      multiple = FALSE
    ),
    check_dataset = FALSE
  )

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
        arm_var = teal.picks::variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
        paramcd = paramcd,
        worst_flag_var = teal.picks::variables(
          choices = c("WORS02FL", "WORS01FL"),
          selected = "WORS02FL"
        ),
        worst_flag = teal.transform::choices_selected(c("Y", "N"), selected = "Y", fixed = TRUE),
        aval_var = teal.picks::variables(choices = c("AVALC", "ANRIND"), selected = "ANRIND"),
        baseline_var = teal.picks::variables(choices = c("BASEC", "BNRIND"), selected = "BNRIND"),
        useNA = "ifany",
        treatment_flag = teal.picks::values(c("Y", "N", ""), "Y", multiple = FALSE),
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
  "e2e - tm_t_shift_by_arm_by_worst: Starts with specified label, useNA, add_total.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()

    testthat::expect_equal(
      app_driver$get_text(".teal-modules-tree a.module-button.active"),
      "Shift by Arm Table"
    )
    # Initial pick values are not readable via get_active_module_input until the badge is opened
    # (teal.picks badge-dropdown); defaults are covered by the table output tests below.
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
    set_teal_picks_slot(app_driver, "arm_var", "variables", "ARMCD")
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
  set_teal_picks_slot(app_driver, "arm_var", "variables", NULL)
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
    set_teal_picks_slot(app_driver, "paramcd", "values", "QT")
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
    set_teal_picks_slot(app_driver, "worst_flag_var", "variables", "WORS01FL")
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
  set_teal_picks_slot(app_driver, "worst_flag_var", "variables", NULL)
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
    # One endpoint keeps AVALC cardinality below the module limit (< 50 levels).
    set_teal_picks_slot(app_driver, "paramcd", "values", "ECGINTP")
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "aval_var", "variables", "AVALC")
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
  set_teal_picks_slot(app_driver, "aval_var", "variables", NULL)
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
    # One endpoint keeps BASEC cardinality below the module limit (< 50 levels).
    set_teal_picks_slot(app_driver, "paramcd", "values", "ECGINTP")
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "baseline_var", "variables", "BASEC")
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
  set_teal_picks_slot(app_driver, "baseline_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})
