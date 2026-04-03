app_driver_tm_t_summary <- function() {
  data <- within(teal.data::teal_data(), {
    ADSL <- teal.data::rADSL
    ADSL$EOSDY[1] <- NA_integer_
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_summary(
        label = "Demographic Table",
        dataname = "ADSL",
        arm_var = variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
        add_total = TRUE,
        summarize_vars = variables(
          choices = c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE"),
          selected = c("SEX", "RACE")
        ),
        useNA = "ifany",
        parentname = "ADSL",
        total_label = default_total_label(),
        na_level = default_na_str(),
        numeric_stats = c(
          "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles",
          "range", "geom_mean"
        ),
        denominator = c("N", "n", "omit"),
        drop_arm_levels = TRUE,
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_summary: Module initializes in teal without errors and produces table output.", {

  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_t_summary: Starts with specified label, arm_var, summarize_vars, useNA, denominator.", {

  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary()
  testthat::expect_equal(
    app_driver$get_text(".teal-modules-tree a.module-button.active"),
    "Demographic Table"
  )
  testthat::expect_identical(
    as.vector(get_teal_picks_slot(app_driver, "arm_var", "variables")),
    "ARM"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("useNA"),
    "ifany"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("denominator"),
    "N"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_summary: Selecting arm_var changes the table and does not throw validation errors.",
  {

    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_summary()
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

testthat::test_that("e2e - tm_t_summary: Deselection of arm_var throws validation error.", {

  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary()
  set_teal_picks_slot(app_driver, "arm_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_summary: Selecting summarize_vars changes the table and does not throw validation errors.",
  {

    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_summary()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "summarize_vars", "variables", c("SEX", "AGE"))
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

testthat::test_that("e2e - tm_t_summary: Deselection of summarize_vars throws validation error.", {

  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_summary()
  set_teal_picks_slot(app_driver, "summarize_vars", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})
