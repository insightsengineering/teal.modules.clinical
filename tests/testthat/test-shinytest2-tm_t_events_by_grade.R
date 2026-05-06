app_driver_tm_t_events_by_grade <- function() { # nolint: object_length.
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)

    ADSL <- teal.data::rADSL
    .lbls_adae <- col_labels(teal.data::rADAE)
    ADAE <- teal.data::rADAE %>%
      mutate_if(is.character, as.factor)
    col_labels(ADAE) <- .lbls_adae
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_events_by_grade(
        label = "Adverse Events by Grade Table",
        dataname = "ADAE",
        parentname = "ADSL",
        arm_var = teal.picks::variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
        llt = teal.picks::variables(
          choices = c("AETERM", "AEDECOD"),
          selected = "AEDECOD"
        ),
        hlt = teal.picks::variables(
          choices = c("AEBODSYS", "AESOC"),
          selected = "AEBODSYS"
        ),
        grade = teal.picks::variables(
          choices = c("AETOXGR", "AESEV"),
          selected = "AETOXGR"
        ),
        grading_groups = list(
          `Any Grade (%)` = c("1", "2", "3", "4", "5"), `Grade 1-2 (%)` =
            c("1", "2"), `Grade 3-4 (%)` = c("3", "4"), `Grade 5 (%)` = "5"
        ),
        col_by_grade = FALSE,
        prune_freq = 0,
        prune_diff = 0,
        add_total = TRUE,
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

testthat::test_that(
  "e2e - tm_t_events_by_grade: Module initializes in teal without errors and produces table output.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_by_grade()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_events_by_grade: Starts with specified label, arm_var, hlt, llt,
  grade, prune_freq, prune_diff, add_total, col_by_grade, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_by_grade()
    testthat::expect_equal(
      app_driver$get_text(".teal-modules-tree a.module-button.active"),
      "Adverse Events by Grade Table"
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
      get_teal_picks_slot(app_driver, "grade", "variables"),
      "AETOXGR"
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
    testthat::expect_false(app_driver$get_active_module_input("col_by_grade"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_events_by_grade: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_by_grade()
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

testthat::test_that("e2e - tm_t_events_by_grade: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events_by_grade()
  set_teal_picks_slot(app_driver, "arm_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_events_by_grade: Selecting hlt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_by_grade()
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
  "e2e - tm_t_events_by_grade: Deselection of hlt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_by_grade()
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
  "e2e - tm_t_events_by_grade: Selecting llt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_by_grade()
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
  "e2e - tm_t_events_by_grade: Deselection of llt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_by_grade()
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

testthat::test_that(
  "e2e - tm_t_events_by_grade: Selecting grade changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_events_by_grade()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "grade", "variables", "AESEV")
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

testthat::test_that("e2e - tm_t_events_by_grade: Deselection of grade throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events_by_grade()
  set_teal_picks_slot(app_driver, "grade", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})
