app_driver_tm_t_exposure <- function() {
  data <- teal.data::teal_data()
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
      parentname = "ADSL",
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
      add_total = FALSE,
      paramcd_label = "PARAM",
      id_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADEX"]], subset = "USUBJID"),
        selected = "USUBJID", fixed = TRUE
      ),
      aval_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADEX"]], subset = "AVAL"),
        selected = "AVAL", fixed = TRUE
      ),
      avalu_var = teal.transform::choices_selected(
        teal.transform::variable_choices(data[["ADEX"]], subset = "AVALU"),
        selected = "AVALU", fixed = TRUE
      ),
      total_label = default_total_label(),
      add_total_row = TRUE,
      total_row_label = "Total number of patients and patient time*",
      na_level = default_na_str(),
      pre_output = NULL,
      post_output = NULL,
      basic_table_args = teal.widgets::basic_table_args()
    ),
    filter = teal::teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y")),
  )
}

testthat::test_that(
  "e2e - tm_t_exposure: Deselection of col_by_var-variable changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    table_before <- app_driver$get_active_module_tws_output("table")
    print(app_driver$get_logs())
    print(app_driver$get_values()$input)
    print(app_driver$get_active_module_input("col_by_var-dataset_ADSL_singleextract-select"))
    app_driver$set_input(
      sprintf("%s-%s", app_driver$active_module_ns(), "col_by_var-dataset_ADSL_singleextract-select"),
      NULL
    )
    print(app_driver$get_active_module_input("col_by_var-dataset_ADSL_singleextract-select"))
    print("Trying to wait after setting the value.")
    app_driver$wait_for_idle()
    testthat::expect_false(identical(table_before, app_driver$get_active_module_tws_output("table")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)
