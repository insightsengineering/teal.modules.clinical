skip("CI test")
app_driver_tm_t_exposure <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- teal.data::rADSL
    ADEX <- teal.data::rADEX

    set.seed(1, kind = "Mersenne-Twister")
    .labels <- col_labels(ADEX, fill = FALSE)
    ADEX <- ADEX %>%
      distinct(USUBJID, .keep_all = TRUE) %>%
      mutate(
        PARAMCD = "TDURD",
        PARAM = "Overall duration (days)",
        AVAL = sample(x = seq(1, 200), size = n(), replace = TRUE),
        AVALU = "Days"
      ) %>%
      bind_rows(ADEX)
    col_labels(ADEX) <- .labels
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
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
  )
}

testthat::test_that("e2e - tm_t_exposure: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_exposure()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_exposure: Starts with specified label, paramcd, parcat,
  col_by_var, row_by_var, add_total_row, add_total",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
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

testthat::test_that(
  "e2e - tm_t_exposure: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADEX_singleextract-filter1-vals", "DOSE")
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

testthat::test_that("e2e - tm_t_exposure: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_exposure()
  app_driver$set_active_module_input("paramcd-dataset_ADEX_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADEX_singleextract-filter1-vals_input .shiny-validation-message"
    )),
    "Please select a parameter value."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_exposure: Selecting parcat changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("parcat-dataset_ADEX_singleextract-filter1-vals", "Drug B")
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

testthat::test_that("e2e - tm_t_exposure: Deselection of parcat throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_exposure()
  app_driver$set_active_module_input("parcat-dataset_ADEX_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "parcat-dataset_ADEX_singleextract-filter1-vals_input .shiny-validation-message"
    )),
    "Please select a parameter category value."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_exposure: Selecting col_by_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("col_by_var-dataset_ADSL_singleextract-select", "ARM")
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
  "e2e - tm_t_exposure: Deselection of col_by_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("col_by_var-dataset_ADSL_singleextract-select", character(0), wait_ = FALSE)
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
  "e2e - tm_t_exposure: Selecting row_by_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("row_by_var-dataset_ADEX_singleextract-select", "REGION1")
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

testthat::test_that("e2e - tm_t_exposure: Deselection of row_by_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_exposure()
  app_driver$set_active_module_input("row_by_var-dataset_ADEX_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "row_by_var-dataset_ADEX_singleextract-select_input .shiny-validation-message"
    )),
    "Please select a row by variable."
  )
  app_driver$stop()
})
