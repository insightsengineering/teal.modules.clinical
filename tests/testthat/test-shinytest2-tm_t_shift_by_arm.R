app_driver_tm_t_shift_by_arm <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADEG <- tmc_ex_adeg
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_shift_by_arm(
        label = "Shift by Arm Table",
        dataname = "ADEG",
        parentname = "ADSL",
        arm_var = teal.picks::variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
        paramcd = teal.picks::variables(choices = "PARAMCD"),
        visit_var = teal.picks::variables(
          choices = c("AVISIT", "AVISITN"),
          selected = "AVISIT"
        ),
        aval_var = teal.picks::variables(choices = "ANRIND"),
        baseline_var = teal.picks::variables(choices = "BNRIND"),
        useNA = "ifany",
        treatment_flag_var = teal.picks::variables(choices = "ONTRTFL"),
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

testthat::test_that("e2e - tm_t_shift_by_arm: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm: Starts with specified label, arm_var, useNA, add_total.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm()
    testthat::expect_equal(
      app_driver$get_text(".teal-modules-tree a.module-button.active"),
      "Shift by Arm Table"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "arm_var", "variables"),
      "ARM"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "aval_var", "variables"),
      "ANRIND"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "baseline_var", "variables"),
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
  "e2e - tm_t_shift_by_arm: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm()
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

testthat::test_that("e2e - tm_t_shift_by_arm: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_arm()
  set_teal_picks_slot(app_driver, "arm_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_arm: Selecting paramcd values changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "paramcd", "values", "HR")
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
  "e2e - tm_t_shift_by_arm: Selecting visit_var values changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "visit_var", "values", "SCREENING")
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
