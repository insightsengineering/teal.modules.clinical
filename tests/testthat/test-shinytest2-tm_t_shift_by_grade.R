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
        visit_var = teal.picks::variables(choices = "AVISIT"),
        arm_var = teal.picks::variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
        paramcd = teal.picks::variables(choices = "PARAMCD"),
        worst_flag_var = teal.picks::variables(
          choices = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"),
          selected = "WGRLOVFL"
        ),
        worst_flag_indicator = teal.picks::values(c("Y", "N"), "Y", fixed = TRUE),
        anl_toxgrade_var = teal.picks::variables(choices = "ATOXGR"),
        base_toxgrade_var = teal.picks::variables(choices = "BTOXGR"),
        id_var = teal.picks::variables(choices = "USUBJID"),
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
      app_driver$get_text(".teal-modules-tree a.module-button.active"),
      "Grade Laboratory Abnormality Table"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "arm_var", "variables"),
      "ARM"
    )
    testthat::expect_identical(
      sort(get_teal_picks_slot(app_driver, "paramcd", "values")),
      sort(unique(as.character(teal.data::rADLB[["PARAMCD"]])))
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "worst_flag_var", "variables"),
      "WGRLOVFL"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "anl_toxgrade_var", "variables"),
      "ATOXGR"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "base_toxgrade_var", "variables"),
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

testthat::test_that("e2e - tm_t_shift_by_grade: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_shift_by_grade()
  set_teal_picks_slot(app_driver, "arm_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_shift_by_grade: Selecting paramcd values changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_grade()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "paramcd", "values", "CRP")
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
  "e2e - tm_t_shift_by_grade: Selecting worst_flag changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_grade()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "worst_flag_var", "variables", "WGRLOFL")
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
  set_teal_picks_slot(app_driver, "worst_flag_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
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

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = modules(
        tm_t_shift_by_grade(
          label = "Grade Laboratory Abnormality Table",
          dataname = "ADLB",
          arm_var = teal.picks::variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
          paramcd = teal.picks::variables(choices = "PARAMCD"),
          worst_flag_var = teal.picks::variables(
            choices = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"),
            selected = "WGRLOFL"
          ),
          worst_flag_indicator = teal.picks::values(c("Y", "N"), "Y", fixed = TRUE),
          anl_toxgrade_var = teal.picks::variables(choices = "ATOXGR"),
          base_toxgrade_var = teal.picks::variables(choices = "BTOXGR"),
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
