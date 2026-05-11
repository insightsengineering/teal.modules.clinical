app_driver_tm_t_events <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADAE <- teal.data::rADAE
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_events(
        label = "Adverse Event Table",
        dataname = "ADAE",
        parentname = "ADSL",
        arm_var = teal.picks::variables(
          choices = c("ARM", "ARMCD"),
          selected = "ARM"
        ),
        llt = teal.picks::variables(
          choices = c("AETERM", "AEDECOD"),
          selected = "AEDECOD"
        ),
        hlt = teal.picks::variables(
          choices = c("AEBODSYS", "AESOC"),
          selected = "AEBODSYS"
        ),
        add_total = TRUE,
        event_type = "adverse event",
        total_label = default_total_label(),
        na_level = default_na_str(),
        sort_freq_col = default_total_label(),
        prune_freq = 0,
        prune_diff = 0,
        drop_arm_levels = TRUE,
        incl_overall_sum = TRUE,
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_events: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events: Starts with specified label, arm_var, hlt, llt, sort_criteria,
  prune_freq, prune_diff, add_total, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events()

    testthat::expect_equal(
      app_driver$get_text(".teal-modules-tree a.module-button.active"),
      "Adverse Event Table"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "arm_var", "variables"),
      "ARM"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "hlt", "variables"),
      "AEBODSYS"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "llt", "variables"),
      "AEDECOD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("sort_criteria"),
      "freq_desc"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("prune_freq"),
      0
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("prune_diff"),
      0
    )
    testthat::expect_true(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_events: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events()
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

testthat::test_that("e2e - tm_t_events: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events()
  set_teal_picks_slot(app_driver, "arm_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events: Selecting hlt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "hlt", "variables", "AESOC")
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
  "e2e - tm_t_events: Deselection of hlt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "hlt", "variables", NULL)
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
  "e2e - tm_t_events: Selecting llt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "llt", "variables", "AETERM")
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
  "e2e - tm_t_events: Deselection of llt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "llt", "variables", NULL)
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
