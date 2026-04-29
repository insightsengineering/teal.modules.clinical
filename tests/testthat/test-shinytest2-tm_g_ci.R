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
        x_var = teal.picks::variables(
          choices = c("ARMCD", "BMRKR2"),
          selected = "ARMCD",
          multiple = FALSE
        ),
        y_var = teal.picks::variables(
          choices = c("AVAL", "CHG", "CHG2"),
          selected = "AVAL",
          multiple = FALSE
        ),
        color = teal.picks::variables(
          choices = c("SEX", "STRATA1", "STRATA2"),
          selected = "STRATA1",
          multiple = FALSE
        ),
        x_dataname = "ADSL",
        y_dataname = "ADLB",
        paramcd_value = teal.picks::values(
          choices = c("ALT", "CRP", "IGA"),
          selected = "ALT",
          multiple = FALSE
        ),
        avisit_value = teal.picks::values(
          choices = c(
            "SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15",
            "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"
          ),
          selected = "SCREENING",
          multiple = FALSE
        ),
        stat = c("mean", "median"),
        conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), selected = "0.95", keep_order = TRUE),
        plot_height = c(700L, 200L, 2000L),
        plot_width = NULL,
        pre_output = NULL,
        post_output = NULL,
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_g_ci: Module initializes and produces plot output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_match(app_driver$get_active_module_plot_output("myplot"), "data:image/png;base64,")
})

testthat::test_that(
  "e2e - tm_g_ci: Module matches label, picks exports, conf_level and stat.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_ci()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Confidence Interval Plot"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(
      exported_values[["x_var_picks-picks_resolved"]]$variables$selected,
      "ARMCD"
    )

    testthat::expect_equal(
      exported_values[["y_var_picks-picks_resolved"]]$variables$selected,
      "AVAL"
    )

    testthat::expect_equal(
      exported_values[["paramcd_picks-picks_resolved"]]$values$selected,
      "ALT"
    )

    testthat::expect_equal(
      exported_values[["avisit_picks-picks_resolved"]]$values$selected,
      "SCREENING"
    )

    testthat::expect_equal(
      exported_values[["color_picks-picks_resolved"]]$variables$selected,
      "STRATA1"
    )

    testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")

    testthat::expect_equal(app_driver$get_active_module_input("stat"), "mean")
  }
)

testthat::test_that("e2e - tm_g_ci: Selecting x_var column updates plot.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "x_var_picks", "variables", "BMRKR2")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Deselecting x_var column shows validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "x_var_picks", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please select a treatment variable"
  )
})

testthat::test_that("e2e - tm_g_ci: Selecting y_var column updates plot.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "y_var_picks", "variables", "CHG2")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Deselecting y_var column shows validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "y_var_picks", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please select an analysis value variable"
  )
})

testthat::test_that(
  "e2e - tm_g_ci: Selecting PARAMCD filter value updates plot.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_ci()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    set_teal_picks_slot(app_driver, "paramcd_picks", "values", "CRP")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("myplot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_g_ci: Deselecting PARAMCD filter shows validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "paramcd_picks", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please select a lab parameter"
  )
})

testthat::test_that("e2e - tm_g_ci: Selecting AVISIT filter value updates plot.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "avisit_picks", "values", "BASELINE")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Deselecting AVISIT filter shows validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "avisit_picks", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please select a visit"
  )
})

testthat::test_that("e2e - tm_g_ci: Selecting color column updates plot.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "color_picks", "variables", "SEX")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Deselecting color column updates plot without validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "color_picks", "variables", character(0L))
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Changing confidence level updates plot.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("conf_level", "0.9")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ci: Changing statistic updates plot.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ci()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("stat", "median")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})
