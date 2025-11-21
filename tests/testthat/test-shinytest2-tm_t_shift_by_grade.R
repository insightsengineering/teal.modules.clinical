app_driver_tm_t_shift_by_grade <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADLB <- teal.data::rADLB
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_shift_by_grade(
        label = "Grade Laboratory Abnormality Table",
        dataname = "ADLB",
        parentname = "ADSL",
        visit_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "AVISIT"),
          selected = "AVISIT", fixed = TRUE
        ),
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], subset = c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
          selected = "ALT"
        ),
        worst_flag_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(
            data[["ADLB"]],
            subset = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL")
          ),
          selected = c("WGRLOVFL")
        ),
        worst_flag_indicator = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADLB"]], "WGRLOVFL"),
          selected = "Y", fixed = TRUE
        ),
        anl_toxgrade_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], subset = c("ATOXGR")),
          selected = c("ATOXGR"),
          fixed = TRUE
        ),
        base_toxgrade_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], subset = c("BTOXGR")),
          selected = c("BTOXGR"),
          fixed = TRUE
        ),
        id_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "USUBJID"),
          selected = "USUBJID", fixed = TRUE
        ),
        add_total = FALSE,
        total_label = default_total_label(),
        drop_arm_levels = TRUE,
        pre_output = NULL,
        post_output = NULL,
        na_level = default_na_str(),
        code_missing_baseline = FALSE,
        basic_table_args = teal.widgets::basic_table_args()
      ),
      filter = teal::teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y"))
    )
  )
}

testthat::test_that(
  "e2e - tm_t_shift_by_grade: Module initializes in teal without errors and produces table output.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_grade()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    testthat::expect_true(
      app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_shift_by_grade: Starts with specified label, arm_var, paramcd, worst_flag_var, anl_toxgrade_var,
  base_toxgrade_var, worst_flag_indicator, add_total, drop_arm_levels, code_missing_baseline.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_grade()
    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "Grade Laboratory Abnormality Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals"),
      "ALT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_flag_var-dataset_ADLB_singleextract-select"),
      "WGRLOVFL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("anl_toxgrade_var-dataset_ADLB_singleextract-select"),
      "ATOXGR"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("base_toxgrade_var-dataset_ADLB_singleextract-select"),
      "BTOXGR"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_flag_indicator"),
      "Y"
    )
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    testthat::expect_false(app_driver$get_active_module_input("code_missing_baseline"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_shift_by_grade: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_grade()
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

testthat::test_that("e2e - tm_t_shift_by_grade: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_grade()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"),
    "A treatment variable is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_grade: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_grade()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals", "CRP")
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

testthat::test_that("e2e - tm_t_shift_by_grade: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_grade()
  app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADLB_singleextract-filter1-vals_input .shiny-validation-message"
    ),
    "A laboratory parameter is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_grade: Selecting worst_flag changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_grade()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("worst_flag_var-dataset_ADLB_singleextract-select", "WGRLOFL")
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

testthat::test_that("e2e - tm_t_shift_by_grade: Deselection of worst_flag throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_grade()
  app_driver$set_active_module_input("worst_flag_var-dataset_ADLB_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module(
      "worst_flag_var-dataset_ADLB_singleextract-select_input .shiny-validation-message"
    ),
    "A worst treatment flag is required"
  )
  app_driver$stop()
})

app_driver_tm_t_shift_by_grade_invalid_data <- function() { # nolint: object_length, object_name.
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADLB <- tmc_ex_adlb
    ADLB$WGRLOFL <- "NA"
  })
  join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]
  #'
  ADSL <- data[["ADSL"]]
  ADLB <- data[["ADLB"]]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = modules(
        tm_t_shift_by_grade(
          label = "Grade Laboratory Abnormality Table",
          dataname = "ADLB",
          arm_var = teal.transform::choices_selected(
            choices = teal.transform::variable_choices(ADSL, subset = c("ARM", "ARMCD")),
            selected = "ARM"
          ),
          paramcd = teal.transform::choices_selected(
            choices = teal.transform::value_choices(ADLB, "PARAMCD", "PARAM"),
            selected = "ALT"
          ),
          worst_flag_var = teal.transform::choices_selected(
            choices = teal.transform::variable_choices(ADLB, subset = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL")),
            selected = "WGRLOFL"
          ),
          worst_flag_indicator = teal.transform::choices_selected(
            teal.transform::value_choices(ADLB, "WGRLOVFL"),
            selected = "Y", fixed = TRUE
          ),
          anl_toxgrade_var = teal.transform::choices_selected(
            choices = teal.transform::variable_choices(ADLB, subset = c("ATOXGR")),
            selected = c("ATOXGR"),
            fixed = TRUE
          ),
          base_toxgrade_var = teal.transform::choices_selected(
            choices = teal.transform::variable_choices(ADLB, subset = c("BTOXGR")),
            selected = c("BTOXGR"),
            fixed = TRUE
          ),
          add_total = FALSE
        )
      ),
      filter = teal::teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y"))
    )
  )
}

testthat::test_that(
  "e2e - tm_t_shift_by_grade: Invalid worst flag indicator shows validation error instead of hanging",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_grade_invalid_data()
    app_driver$expect_validation_error()
    testthat::expect_true(
      nrow(app_driver$get_active_module_table_output("table-table-with-settings")) == 0
    )
    app_driver$stop()
  }
)
