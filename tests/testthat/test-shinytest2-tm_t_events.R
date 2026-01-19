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
        arm_var = teal.transform::choices_selected(c("ARM", "ARMCD"), "ARM"),
        llt = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], c("AETERM", "AEDECOD")),
          selected = c("AEDECOD")
        ),
        hlt = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], c("AEBODSYS", "AESOC")),
          selected = "AEBODSYS"
        ),
        add_total = TRUE,
        event_type = "adverse event",
        total_label = default_total_label(),
        na_level = default_na_str(),
        sort_criteria = c("freq_desc", "alpha"),
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
      app_driver$get_text("a.nav-link.active"),
      "Adverse Event Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("hlt-dataset_ADAE_singleextract-select"),
      "AEBODSYS"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("llt-dataset_ADAE_singleextract-select"),
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

testthat::test_that("e2e - tm_t_events: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  selector <- "arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(selector)),
    "Please select 1 or 2 treatment variable values"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events: Selecting hlt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("hlt-dataset_ADAE_singleextract-select", "AESOC")
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
    app_driver$set_active_module_input("hlt-dataset_ADAE_singleextract-select", NULL)
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
    app_driver$set_active_module_input("llt-dataset_ADAE_singleextract-select", "AETERM")
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
    app_driver$set_active_module_input("llt-dataset_ADAE_singleextract-select", NULL)
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
