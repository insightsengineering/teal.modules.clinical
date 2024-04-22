app_driver_tm_g_ci <- function() {
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb

  init_teal_app_driver(
    data = teal.data::cdisc_data(
      ADSL = ADSL,
      ADLB = ADLB,
      code = "
        ADSL <- tmc_ex_adsl
        ADLB <- tmc_ex_adlb
      "
    ),
    modules = tm_g_ci(
      label = "Confidence Interval Plot",
      x_var = teal.transform::data_extract_spec(
        dataname = "ADSL",
        select = teal.transform::select_spec(
          choices = c("ARMCD", "BMRKR2"),
          selected = c("ARMCD"),
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      y_var = teal.transform::data_extract_spec(
        dataname = "ADLB",
        filter = list(
          teal.transform::filter_spec(
            vars = "PARAMCD",
            choices = levels(ADLB$PARAMCD),
            selected = levels(ADLB$PARAMCD)[1],
            multiple = FALSE,
            label = "Select lab:"
          ),
          teal.transform::filter_spec(
            vars = "AVISIT",
            choices = levels(ADLB$AVISIT),
            selected = levels(ADLB$AVISIT)[1],
            multiple = FALSE,
            label = "Select visit:"
          )
        ),
        select = teal.transform::select_spec(
          label = "Analyzed Value",
          choices = c("AVAL", "CHG"),
          selected = "AVAL",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      color = teal.transform::data_extract_spec(
        dataname = "ADSL",
        select = teal.transform::select_spec(
          label = "Color by variable",
          choices = c("SEX", "STRATA1", "STRATA2"),
          selected = c("STRATA1"),
          multiple = FALSE,
          fixed = FALSE
        )
      )
    )
  )
}

# returns base 64 encoded image
plot_output <- function(app_driver) {
  app_driver$get_attr(
    app_driver$active_module_element("myplot-plot_main > img"),
    "src"
  )
}

testthat::test_that("e2e - tm_g_ci: example ci module initializes in teal without errors and produces plot output", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_match(plot_output(app_driver), "data:image/png;base64,")
})

testthat::test_that("e2e - tm_g_ci: Module initializes with specified label, x_var, y_var, ADLB filters and color", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()

  testthat::expect_equal(
    app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Confidence Interval Plot"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("x_var-dataset_ADSL_singleextract-select"),
    "ARMCD"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("y_var-dataset_ADLB_singleextract-select"),
    "AVAL"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("y_var-dataset_ADLB_singleextract-filter1-col"),
    "PARAMCD"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("y_var-dataset_ADLB_singleextract-filter1-vals"),
    "ALT"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("y_var-dataset_ADLB_singleextract-filter2-col"),
    "AVISIT"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("y_var-dataset_ADLB_singleextract-filter2-vals"),
    "SCREENING"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("color-dataset_ADSL_singleextract-select"),
    "STRATA1"
  )
})

testthat::test_that("e2e - tm_g_ci: Selecting x_var column changes plot and doesn't throw validation errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- plot_output(app_driver)
  app_driver$set_active_module_input("x_var-dataset_ADSL_singleextract-select", "BMRKR2")
  testthat::expect_false(
    identical(
      plot_before,
      plot_output(app_driver)
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Deselecting x_var column throws validation error", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$set_active_module_input("x_var-dataset_ADSL_singleextract-select", character(0))
  testthat::expect_identical(plot_output(app_driver), character(0))
  app_driver$expect_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Selecting y_var column changes plot and doesn't throw validation errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- plot_output(app_driver)
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-select", "CHG")
  testthat::expect_false(
    identical(
      plot_before,
      plot_output(app_driver)
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Deselecting y_var column throws validation error", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-select", character(0))
  testthat::expect_identical(plot_output(app_driver), character(0))
  app_driver$expect_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Selecting PARAMCD filter value changes plot and doesn't throw validation errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- plot_output(app_driver)
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-filter1-vals", "CRP")
  testthat::expect_false(
    identical(
      plot_before,
      plot_output(app_driver)
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Deselecting PARAMCD filter value throws validation error", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-filter1-vals", character(0))
  testthat::expect_identical(plot_output(app_driver), character(0))
  app_driver$expect_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Selecting AVISIT filter value doesn't throw validation errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- plot_output(app_driver)
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-filter2-vals", "VISIT1")
  testthat::expect_false(identical(plot_before, plot_output(app_driver)))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Deselecting AVISIT filter value throws validation error", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-filter2-vals", character(0))
  testthat::expect_identical(plot_output(app_driver), character(0))
  app_driver$expect_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Selecting color column changes plot output and doesn't throw validation errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- plot_output(app_driver)
  app_driver$set_active_module_input("color-dataset_ADSL_singleextract-select", "SEX")
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Deselecting color column throws validation error", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$set_active_module_input("color-dataset_ADSL_singleextract-select", character(0))
  testthat::expect_identical(plot_output(app_driver), character(0))
  app_driver$expect_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Selecting confidence interval value changes plot and doesn't throw any errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- plot_output(app_driver)
  app_driver$set_active_module_input("conf_level", 0.90)
  testthat::expect_false(identical(plot_before, plot_output(app_driver)))
  app_driver$expect_no_validation_error()
})


testthat::test_that("e2e - tm_g_ci: Selecting statistic to use changes a plot and doesn't throw any errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- plot_output(app_driver)
  app_driver$set_active_module_input("stat", "median")
  testthat::expect_false(identical(plot_before, plot_output(app_driver)))
  app_driver$expect_no_validation_error()
})
