app_driver_tm_t_events_patyear <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- tmc_ex_adsl
    ADAETTE <- tmc_ex_adaette %>%
      filter(PARAMCD %in% c("AETTE1", "AETTE2", "AETTE3")) %>%
      mutate(is_event = CNSR == 0) %>%
      mutate(n_events = as.integer(is_event))
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_events_patyear(
        label = "AE Rate Adjusted for Patient-Years At Risk Table",
        dataname = "ADAETTE",
        parentname = "ADSL",
        arm_var = teal.picks::variables(
          choices = c("ARM", "ARMCD", "SEX"),
          selected = "ARMCD"
        ),
        add_total = TRUE,
        events_var = teal.picks::variables(choices = "n_events"),
        paramcd = teal.picks::variables(choices = "PARAMCD"),
        conf_level = teal.transform::choices_selected(
          c(0.95, 0.9, 0.8), 0.95,
          keep_order = TRUE
        ),
        aval_var = teal.picks::variables(choices = "AVAL"),
        avalu_var = teal.picks::variables(choices = "AVALU"),
        total_label = default_total_label(),
        na_level = default_na_str(),
        drop_arm_levels = TRUE,
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_events_patyear: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events_patyear()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("patyear_table-table-with-settings"))
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events_patyear: Starts with specified label, arm_var, paramcd, conf_level,
  conf_method, num_pt_year, input_time_unit, add_total, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_patyear()
    testthat::expect_equal(
      app_driver$get_text(".teal-modules-tree a.module-button.active"),
      "AE Rate Adjusted for Patient-Years At Risk Table"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "arm_var", "variables"),
      "ARMCD"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "paramcd", "variables"),
      "PARAMCD"
    )
    testthat::expect_identical(
      sort(get_teal_picks_slot(app_driver, "paramcd", "values")),
      sort(unique(as.character(tmc_ex_adaette[["PARAMCD"]])))
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_method"),
      "Normal (rate)"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("num_pt_year"),
      "100"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("input_time_unit"),
      "year"
    )
    testthat::expect_true(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_events_patyear: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_patyear()
    table_before <- app_driver$get_active_module_table_output("patyear_table-table-with-settings")
    set_teal_picks_slot(app_driver, "paramcd", "values", "AETTE2")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("patyear_table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_events_patyear: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events_patyear()
  set_teal_picks_slot(app_driver, "paramcd", "values", NULL)
  testthat::expect_identical(
    app_driver$get_active_module_table_output("patyear_table-table-with-settings"),
    data.frame()
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events_patyear: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_patyear()
    table_before <- app_driver$get_active_module_table_output("patyear_table-table-with-settings")
    set_teal_picks_slot(app_driver, "arm_var", "variables", "ARM")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("patyear_table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_events_patyear: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events_patyear()
  set_teal_picks_slot(app_driver, "arm_var", "variables", NULL)
  testthat::expect_identical(
    app_driver$get_active_module_table_output("patyear_table-table-with-settings"),
    data.frame()
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events_patyear: Selecting 2 variables as arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_patyear()
    table_before <- app_driver$get_active_module_table_output("patyear_table-table-with-settings")
    set_teal_picks_slot(app_driver, "arm_var", "variables", c("ARM", "SEX"))
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("patyear_table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)
