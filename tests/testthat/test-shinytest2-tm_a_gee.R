app_driver_tm_a_gee <- function() {

  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- tmc_ex_adsl
    ADQS <- tmc_ex_adqs %>%
      filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
      mutate(
        AVISIT = as.factor(AVISIT),
        AVISITN = rank(AVISITN) %>%
          as.factor() %>%
          as.numeric() %>%
          as.factor(),
        AVALBIN = AVAL < 50 # Just as an example to get a binary endpoint.
      ) %>%
      droplevels()
  })
  datanames <- c("ADSL", "ADQS")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = tm_a_gee(
      label = "GEE",
      dataname = "ADQS",
      aval_var = teal.transform::choices_selected("AVALBIN", fixed = TRUE),
      id_var = teal.transform::choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
      arm_var = teal.transform::choices_selected(c("ARM", "ARMCD"), "ARM"),
      visit_var = teal.transform::choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
      paramcd = teal.transform::choices_selected(
        choices = teal.transform::value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
        selected = "FKSI-FWB"
      ),
      cov_var = teal.transform::choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL)
    )
  )
}

testthat::test_that("e2e - tm_a_gee: example gee module initializes in teal without errors and produces table output", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_gee()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(app_driver$is_visible(app_driver$active_module_element("table-table-with-settings")))
})

testthat::test_that("e2e - tm_a_gee: starts with specified label, id_var, arm_var, visit_var, paramcd and cov_var", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()

  testthat::expect_equal(
    app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "GEE"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("aval_var-dataset_ADQS_singleextract-select"),
    "AVALBIN"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("id_var-dataset_ADQS_singleextract-select"),
    "USUBJID"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
    "ARM"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("visit_var-dataset_ADQS_singleextract-select"),
    "AVISIT"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("paramcd-dataset_ADQS_singleextract-filter1-vals"),
    "FKSI-FWB"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("cov_var-dataset_ADQS_singleextract-select"),
    NULL
  )
})
