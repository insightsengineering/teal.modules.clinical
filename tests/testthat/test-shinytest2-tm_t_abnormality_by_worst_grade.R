app_driver_tm_t_abnormality_by_worst_grade <- function() { # nolint: object_length.
  data <- within(teal.data::teal_data(), {
    library(dplyr)

    ADSL <- teal.data::rADSL
    ADLB <- teal.data::rADLB %>%
      filter(!AVISIT %in% c("SCREENING", "BASELINE"))
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_abnormality_by_worst_grade(
        label = "Laboratory Test Results with Highest Grade Post-Baseline",
        dataname = "ADLB",
        parentname = "ADSL",
        arm_var = variables(choices = any_of(c("ARM", "ARMCD")), selected = "ARM"),
        id_var = variables(choices = any_of("USUBJID"), selected = "USUBJID", fixed = TRUE),
        paramcd = variables(choices = "PARAMCD", selected = "PARAMCD"),
        add_total = FALSE,
        atoxgr_var = variables(choices = any_of("ATOXGR"), selected = "ATOXGR", fixed = TRUE),
        worst_high_flag_var = variables(choices = any_of("WGRHIFL"), selected = "WGRHIFL", fixed = TRUE),
        worst_low_flag_var = variables(choices = any_of("WGRLOFL"), selected = "WGRLOFL", fixed = TRUE),
        worst_flag_indicator = teal.transform::choices_selected("Y"),
        total_label = default_total_label(),
        drop_arm_levels = TRUE,
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      ),
      filter = teal::teal_slices(
        teal_slice("ADSL", "SAFFL", selected = "Y"),
        teal_slice("ADLB", "ONTRTFL", selected = "Y")
      )
    )
  )
}

testthat::test_that(
  "e2e - tm_t_abnormality_by_worst_grade: Module initializes in teal without errors and produces table output.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()

    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_abnormality_by_worst_grade: Starts with specified label, arm_var, paramcd, id_var, atoxgr_var,
  worst_high_flag_var, worst_low_flag_var, worst_flag_indicator, add_total, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality_by_worst_grade()

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Laboratory Test Results with Highest Grade Post-Baseline"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-variables-selected"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-values-selected"),
      c("ALT", "CRP", "IGA")
    )
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    testthat::expect_equal(
      app_driver$get_active_module_input("id_var-variables-selected"),
      "USUBJID"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("atoxgr_var-variables-selected"),
      "ATOXGR"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_high_flag_var-variables-selected"),
      "WGRHIFL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_low_flag_var-variables-selected"),
      "WGRLOFL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_flag_indicator"),
      "Y"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_abnormality_by_worst_grade: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
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

testthat::test_that(
  "e2e - tm_t_abnormality_by_worst_grade: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-values-selected", c("ALT", "CRP"))
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

testthat::test_that("e2e - tm_t_abnormality_by_worst_grade: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
  app_driver$set_active_module_input("arm_var-variables-selected", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_t_abnormality_by_worst_grade: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
  app_driver$set_active_module_input("paramcd-values-selected", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_abnormality_by_worst_grade: Changing add_total changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("add_total", TRUE)
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
  "e2e - tm_t_abnormality_by_worst_grade: Changing drop_arm_levels does not change the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("drop_arm_levels", FALSE)
    testthat::expect_identical(table_before, app_driver$get_active_module_table_output("table-table-with-settings"))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)
