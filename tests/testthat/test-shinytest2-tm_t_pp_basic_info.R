app_driver_tm_t_pp_basic_info <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_pp_basic_info(
        label = "Basic Info",
        dataname = "ADSL",
        patient_col = "USUBJID",
        vars = teal.picks::variables(
          choices = colnames(data[["ADSL"]]),
          selected = c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT")
        ),
        pre_output = NULL,
        post_output = NULL
      )
    )
  )
}

testthat::test_that("e2e - tm_t_pp_basic_info: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_basic_info()
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("basic_info_table"))
})

testthat::test_that("e2e - tm_t_pp_basic_info: Starts with specified label, patient_id, vars.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_basic_info()
  withr::defer(app_driver$stop())
  testthat::expect_equal(
    app_driver$get_text("a.nav-link.active"),
    "Basic Info"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("patient_id"),
    "AB12345-CHN-3-id-128"
  )

  exported_values <- app_driver$get_values()$export
  names(exported_values) <- gsub(
    sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
    fixed = TRUE
  )

  testthat::expect_equal(
    exported_values[["vars-picks_resolved"]]$variables$selected,
    c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT")
  )
})

testthat::test_that(
  "e2e - tm_t_pp_basic_info: Selecting patient_id changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_basic_info()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("basic_info_table")
    app_driver$set_active_module_input("patient_id", "AB12345-USA-1-id-261")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("basic_info_table")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_basic_info: Deselection of patient_id throws validation error and table is not visible.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_basic_info()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("patient_id", NULL)
    app_driver$expect_hidden(
      app_driver$namespaces(TRUE)$module("basic_info_table"),
      visibility_property = TRUE
    )
    app_driver$expect_validation_error()
    testthat::expect_equal(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("title")),
      "Please select a patient"
    )
  }
)

testthat::test_that(
  "e2e - tm_t_pp_basic_info: Selecting cov_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_basic_info()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("basic_info_table")
    set_teal_picks_slot(app_driver, "vars", "variables", c("AGE", "BMRKR1"))
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("basic_info_table")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_basic_info: Deselection of cov_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_basic_info()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "vars", "variables", character(0L))
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("basic_info_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("title")),
    "Please select basic info variables",
    fixed = TRUE
  )
})
