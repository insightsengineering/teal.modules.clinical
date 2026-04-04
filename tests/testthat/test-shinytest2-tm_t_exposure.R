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

  app_driver <- init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_exposure(
        label = "Duration of Exposure Table",
        dataname = "ADEX",
        parentname = "ADSL",
        paramcd = variables(choices = "PARAMCD"),
        col_by_var = variables(
          choices = c("SEX", "ARM"),
          selected = "SEX"
        ),
        row_by_var = variables(
          choices = c("RACE", "REGION1", "STRATA1", "SEX"),
          selected = "RACE"
        ),
        parcat = variables(choices = "PARCAT2"),
        add_total = FALSE,
        paramcd_label = "PARAM",
        id_var = variables(choices = "USUBJID"),
        aval_var = variables(choices = "AVAL"),
        avalu_var = variables(choices = "AVALU"),
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
  # Default picks select all param / parcat values; merged ADEX then has multiple AVALU
  # (Days vs mg) and the module requires exactly one unit. Match a single endpoint + category.
  set_teal_picks_slot(app_driver, "paramcd", "values", "TDURD")
  set_teal_picks_slot(app_driver, "parcat", "values", "Drug A")
  app_driver
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
      app_driver$get_text(".teal-modules-tree a.module-button.active"),
      "Duration of Exposure Table"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "paramcd", "values"),
      "TDURD"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "parcat", "values"),
      "Drug A"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "col_by_var", "variables"),
      "SEX"
    )
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "row_by_var", "variables"),
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
    set_teal_picks_slot(app_driver, "paramcd", "values", "DOSE")
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
  set_teal_picks_slot(app_driver, "paramcd", "values", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_exposure: Selecting parcat changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "parcat", "values", "Drug B")
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
  set_teal_picks_slot(app_driver, "parcat", "values", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_exposure: Selecting col_by_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_exposure()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "col_by_var", "variables", "ARM")
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
    set_teal_picks_slot(app_driver, "col_by_var", "variables", NULL, wait = FALSE)
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
    set_teal_picks_slot(app_driver, "row_by_var", "variables", "REGION1")
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
  set_teal_picks_slot(app_driver, "row_by_var", "variables", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})
