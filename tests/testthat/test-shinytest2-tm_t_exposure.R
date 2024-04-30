app_driver_tm_t_exposure <- function() {
  data <- teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- teal.data::rADSL
    ADEX <- teal.data::rADEX

    set.seed(1, kind = "Mersenne-Twister")
    labels <- col_labels(ADEX, fill = FALSE)
    ADEX <- ADEX %>%
      distinct(USUBJID, .keep_all = TRUE) %>%
      mutate(
        PARAMCD = "TDURD",
        PARAM = "Overall duration (days)",
        AVAL = sample(x = seq(1, 200), size = n(), replace = TRUE),
        AVALU = "Days"
      ) %>%
      bind_rows(ADEX)
    col_labels(ADEX) <- labels
  })

  datanames <- c("ADSL", "ADEX")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = tm_t_exposure(
      label = "Duration of Exposure Table",
      dataname = "ADEX",
      paramcd = teal.transform::choices_selected(
        choices = teal.transform::value_choices(data[["ADEX"]], "PARAMCD", "PARAM"),
        selected = "TDURD"
      ),
      col_by_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADEX"]], subset = c("SEX", "ARM")),
        selected = "SEX"
      ),
      row_by_var = teal.transform::choices_selected(
        choices = teal.transform::variable_choices(data[["ADEX"]], subset = c("RACE", "REGION1", "STRATA1", "SEX")),
        selected = "RACE"
      ),
      parcat = teal.transform::choices_selected(
        choices = teal.transform::value_choices(data[["ADEX"]], "PARCAT2"),
        selected = "Drug A"
      ),
      add_total = FALSE
    ),
    filter = teal::teal_slices(teal.slice::teal_slice("ADSL", "SAFFL", selected = "Y"))
  )
}

testthat::test_that("e2e - tm_t_exposure: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_exposure()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_exposure: Starts with specified label, paramcd, parcat,
  col_by_var, row_by_var, add_total_row, add_total",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Duration of Exposure Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADEX_singleextract-filter1-vals"),
      "TDURD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("parcat-dataset_ADEX_singleextract-filter1-vals"),
      "Drug A"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("col_by_var-dataset_ADSL_singleextract-select"),
      "SEX"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("row_by_var-dataset_ADEX_singleextract-select"),
      "RACE"
    )
    testthat::expect_true(app_driver$get_active_module_input("add_total_row"))
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    app_driver$stop()
  }
)
