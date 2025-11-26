app_driver_tm_t_abnormality <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)

    ADSL <- teal.data::rADSL
    ADLB <- teal.data::rADLB %>%
      mutate(
        ONTRTFL = case_when(
          AVISIT %in% c("SCREENING", "BASELINE") ~ "",
          TRUE ~ "Y"
        ) %>% with_label("On Treatment Record Flag")
      )
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_abnormality(
        label = "Abnormality Table",
        dataname = "ADLB",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], subset = c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        add_total = FALSE,
        by_vars = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], subset = c("LBCAT", "PARAM", "AVISIT")),
          selected = c("LBCAT", "PARAM"),
          keep_order = TRUE
        ),
        baseline_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "BNRIND"),
          selected = "BNRIND", fixed = TRUE
        ),
        grade = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], subset = "ANRIND"),
          selected = "ANRIND",
          fixed = TRUE
        ),
        abnormal = list(low = "LOW", high = "HIGH"),
        id_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "USUBJID"),
          selected = "USUBJID", fixed = TRUE
        ),
        exclude_base_abn = FALSE,
        treatment_flag_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "ONTRTFL"),
          selected = "ONTRTFL", fixed = TRUE
        ),
        treatment_flag = teal.transform::choices_selected("Y"),
        total_label = default_total_label(),
        drop_arm_levels = TRUE,
        pre_output = NULL,
        post_output = NULL,
        na_level = default_na_str(),
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_abnormality: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_abnormality()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_true(
    app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_abnormality: Starts with specified label, arm_var, by_vars,
  add_total, exclude_base_abn, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality()

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Abnormality Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("by_vars-dataset_ADLB_singleextract-select"),
      c("LBCAT", "PARAM")
    )
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    testthat::expect_false(app_driver$get_active_module_input("exclude_base_abn"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    testthat::expect_equal(
      app_driver$get_active_module_input("baseline_var-dataset_ADLB_singleextract-select"),
      "BNRIND"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("grade-dataset_ADLB_singleextract-select"),
      "ANRIND"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_abnormality: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality()
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

testthat::test_that("e2e - arm_var: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_abnormality()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message")),
    "Please select a treatment variable."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_abnormality: Selecting by_vars changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("by_vars-dataset_ADLB_singleextract-select", "AVISIT")
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

testthat::test_that("e2e - tm_t_abnormality: Deselection of by_vars throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_abnormality()
  app_driver$set_active_module_input("by_vars-dataset_ADLB_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("by_vars-dataset_ADLB_singleextract-select_input .shiny-validation-message")),
    "Please select a Row By Variable."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_abnormality: Changing add_total changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality()
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
  "e2e - tm_t_abnormality: Changing exclude_base_abn changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("exclude_base_abn", TRUE)
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
  "e2e - tm_t_abnormality: Changing drop_arm_levels does not change the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("drop_arm_levels", FALSE)
    testthat::expect_true(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)
