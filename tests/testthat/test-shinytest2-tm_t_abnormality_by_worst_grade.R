skip("CI test")
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
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], subset = c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        id_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "USUBJID"),
          selected = "USUBJID", fixed = TRUE
        ),
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
          selected = c("ALT", "CRP", "IGA")
        ),
        add_total = FALSE,
        atoxgr_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "ATOXGR"),
          selected = "ATOXGR", fixed = TRUE
        ),
        worst_high_flag_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "WGRHIFL"),
          selected = "WGRHIFL", fixed = TRUE
        ),
        worst_low_flag_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "WGRLOFL"),
          selected = "WGRLOFL", fixed = TRUE
        ),
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
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals"),
      c("ALT", "CRP", "IGA")
    )
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    testthat::expect_equal(
      app_driver$get_active_module_input("id_var-dataset_ADLB_singleextract-select"),
      "USUBJID"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("atoxgr_var-dataset_ADLB_singleextract-select"),
      "ATOXGR"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_high_flag_var-dataset_ADLB_singleextract-select"),
      "WGRHIFL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_low_flag_var-dataset_ADLB_singleextract-select"),
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

testthat::test_that(
  "e2e - tm_t_abnormality_by_worst_grade: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals", c("ALT", "CRP"))
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
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message")
    ),
    "Please select a treatment variable."
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_t_abnormality_by_worst_grade: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
  app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADLB_singleextract-filter1-vals_input .shiny-validation-message"
    )),
    "Please select at least one Laboratory parameter."
  )
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
