app_driver_tm_t_shift_by_arm_by_worst <- function() { # nolint: object_length
  data <- teal_data()
  data <- within(data, {
    ADSL <- tmc_ex_adsl
    ADEG <- tmc_ex_adeg
  })

  datanames <- c("ADSL", "ADEG")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = tm_t_shift_by_arm_by_worst(
      label = "Shift by Arm Table",
      dataname = "ADEG",
      arm_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADSL"]], subset = c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      paramcd = teal.transform::choices_selected(
        teal.transform::value_choices(data[["ADEG"]], "PARAMCD"),
        selected = "ECGINTP"
      ),
      worst_flag_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADEG"]], c("WORS02FL", "WORS01FL")),
        selected = "WORS02FL"
      ),
      worst_flag = teal.transform::choices_selected(
        teal.transform::value_choices(data[["ADEG"]], "WORS02FL"),
        selected = "Y", fixed = TRUE
      ),
      aval_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADEG"]], c("AVALC", "ANRIND")),
        selected = "AVALC"
      ),
      baseline_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADEG"]], c("BASEC", "BNRIND")),
        selected = "BASEC"
      ),
      useNA = "ifany"
    )
  )
}

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Module initializes in teal without errors and produces table output.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    testthat::expect_true(
      app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_shift_by_arm_by_worst: Starts with specified label, arm_var, paramcd, worst_flag_var,
  aval_var, baseline_var, useNA, treatment_flag_var, add_total.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_shift_by_arm_by_worst()

    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Shift by Arm Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADEG_singleextract-filter1-vals"),
      "ECGINTP"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("worst_flag_var-dataset_ADEG_singleextract-select"),
      "WORS02FL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADEG_singleextract-select"),
      "AVALC"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("baseline_var-dataset_ADEG_singleextract-select"),
      "BASEC"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("useNA"),
      "ifany"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("treatment_flag_var-dataset_ADEG_singleextract-select"),
      "ONTRTFL"
    )
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    app_driver$stop()
  }
)
