app_driver_tm_g_ci <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADLB <- teal.data::rADLB
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
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
              choices = c("ALT", "CRP", "IGA"),
              selected = "ALT",
              multiple = FALSE,
              label = "Select lab:"
            ),
            teal.transform::filter_spec(
              vars = "AVISIT",
              choices = c(
                "SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15",
                "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"
              ),
              selected = "SCREENING",
              multiple = FALSE,
              label = "Select visit:"
            )
          ),
          select = teal.transform::select_spec(
            label = "Analyzed Value",
            choices = c("AVAL", "CHG", "CHG2"),
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
        ),
        stat = c("mean", "median"),
        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95,
          keep_order = TRUE
        ),
        plot_height = c(700L, 200L, 2000L),
        plot_width = NULL,
        pre_output = NULL,
        post_output = NULL,
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_g_ci: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_match(app_driver$get_active_module_plot_output("myplot"), "data:image/png;base64,")
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_ci: Module initializes with specified label, x_var, y_var, ADLB filters, color, conf_level and stat.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_ci()
    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
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

    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("stat"),
      "mean"
    )

    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_ci: Selecting x_var column changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("x_var-dataset_ADSL_singleextract-select", "BMRKR2")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ci: Deselecting x_var column throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$set_active_module_input("x_var-dataset_ADSL_singleextract-select", character(0))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("x_var-dataset_ADSL_singleextract-select_input > div > span")),
    "Select a treatment (x axis)"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ci: Selecting y_var column changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-select", "CHG2")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ci: Deselecting y_var column throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-select", character(0))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("y_var-dataset_ADLB_singleextract-select_input > div > span")),
    "Select an analysis value (y axis)"
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_ci: Selecting PARAMCD filter value changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_ci()
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-filter1-vals", "CRP")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("myplot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_ci: Deselecting PARAMCD filter value throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-filter1-vals", character(0))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("y_var-dataset_ADLB_singleextract-filter1-vals_input > div > span")),
    "Please select the filters."
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ci: Selecting AVISIT filter value doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-filter2-vals", "BASELINE")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ci: Deselecting AVISIT filter value throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  app_driver$set_active_module_input("y_var-dataset_ADLB_singleextract-filter2-vals", character(0))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("y_var-dataset_ADLB_singleextract-filter2-vals_input > div > span")),
    "Please select the filters."
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ci: Selecting color column changes plot output and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("color-dataset_ADSL_singleextract-select", "SEX")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ci: Deselecting color column changes plot output and doesn't throw validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("color-dataset_ADSL_singleextract-select", character(0))
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ci: Selecting confidence interval value changes plot and doesn't throw any errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("conf_level", 0.90)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})


testthat::test_that("e2e - tm_g_ci: Selecting statistic to use changes a plot and doesn't throw any errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("stat", "median")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})
