app_driver_tm_t_ancova <- function() {
  data <- teal.data::teal_data() %>%
    within({
      ADSL <- tmc_ex_adsl
      ADQS <- tmc_ex_adqs
    })

  datanames <- c("ADSL", "ADQS")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  arm_ref_comp <- list(
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    ),
    ACTARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    )
  )

  init_teal_app_driver(
    data = data,
    modules = tm_t_ancova(
      label = "ANCOVA Table",
      dataname = "ADQS",
      avisit = teal.transform::choices_selected(
        choices = teal.transform::value_choices(data[["ADQS"]], "AVISIT"),
        selected = "WEEK 1 DAY 8"
      ),
      arm_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ACTARMCD", "ARMCD")),
        selected = "ARMCD"
      ),
      arm_ref_comp = arm_ref_comp,
      aval_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADQS"]], c("CHG", "AVAL")),
        selected = "CHG"
      ),
      cov_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADQS"]], c("BASE", "STRATA1", "SEX")),
        selected = "STRATA1"
      ),
      paramcd = teal.transform::choices_selected(
        choices = teal.transform::value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
        selected = "FKSI-FWB"
      ),
      interact_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADQS"]], c("BASE", "STRATA1", "SEX")),
        selected = "STRATA1"
      ),
      conf_level = teal.transform::choices_selected(c(2, 0.95, 0.9, 0.8), 0.95, keep_order = TRUE)
    )
  )
}

testthat::test_that("e2e - tm_t_ancova: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_ancova: Starts with specified label, avisit, paramcd, aval_var, aval_var,
  arm_var, buckets, combine_comp_arms, interact_var, cov_var, conf_level, include_interact.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()

    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "ANCOVA Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("avisit-dataset_ADQS_singleextract-filter1-vals"),
      "WEEK 1 DAY 8"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADQS_singleextract-filter1-vals"),
      "FKSI-FWB"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADQS_singleextract-select"),
      "CHG"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARMCD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(
        Ref = list("ARM A"),
        Comp = list("ARM B", "ARM C")
      )
    )
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    testthat::expect_equal(
      app_driver$get_active_module_input("interact_var-dataset_ADQS_singleextract-select"),
      "STRATA1"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("cov_var-dataset_ADQS_singleextract-select"),
      "STRATA1"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_false(app_driver$get_active_module_input("include_interact"))
    app_driver$stop()
  }
)
