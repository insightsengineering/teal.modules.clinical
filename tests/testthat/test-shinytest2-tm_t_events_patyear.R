app_driver_tm_t_events_patyear <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- tmc_ex_adsl
    ADAETTE <- tmc_ex_adaette %>% # nolint object_name
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
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD", "SEX")),
          selected = "ARMCD"
        ),
        add_total = TRUE,
        events_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAETTE"]], "n_events"),
          selected = "n_events",
          fixed = TRUE
        ),
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADAETTE"]], "PARAMCD", "PARAM"),
          selected = "AETTE1"
        ),
        conf_level = teal.transform::choices_selected(
          c(2, 0.95, 0.9, 0.8), 0.95,
          keep_order = TRUE
        ),
        aval_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAETTE"]], "AVAL"),
          selected = "AVAL", fixed = TRUE
        ),
        avalu_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAETTE"]], "AVALU"),
          selected = "AVALU", fixed = TRUE
        ),
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
  testthat::expect_true(
    app_driver$is_visible(app_driver$namespaces(TRUE)$module("patyear_table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events_patyear: Starts with specified label, arm_var, paramcd, conf_level,
  conf_method, num_pt_year, input_time_unit, add_total, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_patyear()
    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "AE Rate Adjusted for Patient-Years At Risk Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARMCD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADAETTE_singleextract-filter1-vals"),
      "AETTE1"
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
    app_driver$set_active_module_input("paramcd-dataset_ADAETTE_singleextract-filter1-vals", "AETTE2")
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
  app_driver$set_active_module_input("paramcd-dataset_ADAETTE_singleextract-filter1-vals", NULL)
  testthat::expect_identical(
    app_driver$get_active_module_table_output("patyear_table-table-with-settings"),
    data.frame()
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADAETTE_singleextract-filter1-vals_input .shiny-validation-message"
    )),
    "A Event Type Parameter is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events_patyear: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_patyear()
    table_before <- app_driver$get_active_module_table_output("patyear_table-table-with-settings")
    app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", "ARM")
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
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(
    app_driver$get_active_module_table_output("patyear_table-table-with-settings"),
    data.frame()
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message")),
    "Please select exactly 1 or 2 treatment variables"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events_patyear: Selecting 2 variables as arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_patyear()
    table_before <- app_driver$get_active_module_table_output("patyear_table-table-with-settings")
    app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", c("ARM", "SEX"))
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
