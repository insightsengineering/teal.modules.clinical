app_driver_tm_t_shift_by_grade <- function() {
  data <- teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADLB <- teal.data::rADLB
  })

  datanames <- c("ADSL", "ADLB")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = tm_t_shift_by_grade(
      label = "Grade Laboratory Abnormality Table",
      dataname = "ADLB",
      arm_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADSL"]], subset = c("ARM", "ARMCD")),
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
      add_total = FALSE
    ),
    filter = teal::teal_slices(teal.slice::teal_slice("ADSL", "SAFFL", selected = "Y"))
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
      app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
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
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
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
