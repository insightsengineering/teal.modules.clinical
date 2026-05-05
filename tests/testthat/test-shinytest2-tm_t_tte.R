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
        arm_var = teal.picks::variables(choices = c("ARM", "ARMCD", "ACTARMCD"), selected = "ARM"),
        arm_ref_comp = arm_ref_comp,
        paramcd = teal.picks::variables(c("PARAMCD", "PARAM")),
        strata_var = teal.picks::variables(choices = c("SEX", "BMRKR2"), selected = "SEX"),
        time_points = teal.transform::choices_selected(c(182, 243), 182),
        event_desc_var = teal.picks::variables(choices = "EVNTDESC"),
        aval_var = teal.picks::variables(choices = "AVAL"),
        cnsr_var = teal.picks::variables(choices = "CNSR"),
        conf_level_coxph = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
        conf_level_survfit = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
        time_unit_var = teal.picks::variables(choices = "AVALU"),
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
  "e2e - tm_t_tte: Starts with specified label, paramcd, event_desc_var, arm_var,
  strata_var, time_points, conf_level_coxph, conf_level_survfit, compare_arms, combine_comp_arms.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
    testthat::expect_equal(
      app_driver$get_text(".teal-modules-tree a.module-button.active"),
      "Time To Event Table"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "arm_var", "variables"),
      "ARM"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "strata_var", "variables"),
      "SEX"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "event_desc_var", "variables"),
      "EVNTDESC"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("time_points"),
      "182"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level_coxph"),
      "0.95"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level_survfit"),
      "0.95"
    )
    testthat::expect_true(app_driver$get_active_module_input("compare_arms"))
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_tte: Selecting paramcd values changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    initial_param <- as.character(unlist(get_teal_picks_slot(app_driver, "paramcd", "values")))
    other_param <- setdiff(c("OS", "PFS", "CRSD", "EFS", "TNE"), initial_param)[[1]]
    set_teal_picks_slot(app_driver, "paramcd", "values", other_param)
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

testthat::test_that("e2e - tm_t_tte: Deselection of paramcd values throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_tte()
  set_teal_picks_slot(app_driver, "paramcd", "values", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_tte: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
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

testthat::test_that("e2e - tm_t_tte: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_tte()
  set_teal_picks_slot(app_driver, "arm_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_tte: Selecting strata_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "strata_var", "variables", "BMRKR2")
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
  "e2e - tm_t_tte: Deselection of strata_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_tte()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "strata_var", "variables", NULL)
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
