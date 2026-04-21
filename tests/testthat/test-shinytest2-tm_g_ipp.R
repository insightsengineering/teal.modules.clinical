app_driver_tm_g_ipp <- function() {
  data <- within(teal.data::teal_data(), {
    library(dplyr)
    library(tern)
    ADSL <- teal.data::rADSL |>
      slice(1:20) |>
      df_explicit_na()
    ADLB <- teal.data::rADLB |>
      filter(USUBJID %in% ADSL$USUBJID) |>
      df_explicit_na() |>
      filter(AVISIT != "SCREENING")
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  testthat::expect_warning(
    paramcd_value <- teal.picks::values(selected = "ALT", multiple = FALSE),
    "doesn't guarantee that `selected` is a subset of `choices`.",
    fixed = TRUE
  )

  testthat::expect_warning(
    arm_var_value <- teal.picks::values(selected = "ARM A", multiple = FALSE),
    "doesn't guarantee that `selected` is a subset of `choices`.",
    fixed = TRUE
  )

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_ipp(
        label = "Individual Patient Plot",
        dataname = "ADLB",
        parentname = "ADSL",
        arm_var = teal.picks::picks(
          teal.picks::datasets("ADSL"),
          teal.picks::variables("ARMCD", fixed = TRUE),
          arm_var_value
        ),
        paramcd_var = teal.picks::variables("PARAMCD", fixed = TRUE),
        paramcd_value = paramcd_value,
        aval_var = teal.picks::variables(c("AVAL", "CHG"), multiple = FALSE),
        avalu_var = teal.picks::variables("AVALU", fixed = TRUE),
        id_var = teal.picks::variables("USUBJID", fixed = TRUE),
        visit_var = teal.picks::variables(c("AVISIT", "ATOXGR"), multiple = FALSE),
        baseline_var = teal.picks::variables("BASE", fixed = TRUE),
        add_baseline_hline = FALSE,
        separate_by_obs = FALSE,
        suppress_legend = FALSE,
        add_avalu = TRUE,
        plot_height = c(1200L, 400L, 5000L),
        plot_width = NULL,
        pre_output = NULL,
        post_output = NULL,
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_g_ipp: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_match(
    app_driver$get_active_module_plot_output("myplot"),
    "data:image/png;base64,"
  )
})

testthat::test_that(
  "e2e - tm_g_ipp: Starts with specified
  label, parentname, arm_var, paramcd, id_var, visit_var, aval_var, avalu_var, baseline_var
  add_baseline_hline, separate_by_obs, suppress_legend, add_avalu.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_ipp()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Individual Patient Plot"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(
      exported_values[["arm_var-picks_resolved"]]$variables$selected,
      "ARMCD"
    )
    testthat::expect_equal(
      exported_values[["arm_var-picks_resolved"]]$values$selected,
      "ARM A"
    )
    testthat::expect_equal(
      exported_values[["paramcd-picks_resolved"]]$variables$selected,
      "PARAMCD"
    )
    testthat::expect_equal(
      exported_values[["paramcd-picks_resolved"]]$values$selected,
      "ALT"
    )
    testthat::expect_equal(
      exported_values[["visit_var-picks_resolved"]]$variables$selected,
      "AVISIT"
    )
    testthat::expect_equal(
      exported_values[["aval_var-picks_resolved"]]$variables$selected,
      "AVAL"
    )
    testthat::expect_equal(
      exported_values[["id_var-picks_resolved"]]$variables$selected,
      "USUBJID"
    )
    testthat::expect_equal(
      exported_values[["avalu_var-picks_resolved"]]$variables$selected,
      "AVALU"
    )
    testthat::expect_equal(
      exported_values[["baseline_var-picks_resolved"]]$variables$selected,
      "BASE"
    )
    testthat::expect_false(app_driver$get_active_module_input("add_baseline_hline"))
    testthat::expect_false(app_driver$get_active_module_input("separate_by_obs"))
    testthat::expect_false(app_driver$get_active_module_input("suppress_legend"))
    testthat::expect_true(app_driver$get_active_module_input("add_avalu"))
  }
)

testthat::test_that("e2e - tm_g_ipp: Selecting arm_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "arm_var", "values", "ARM B")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ipp: Deselecting arm_var column throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "arm_var", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "Arm variable is empty.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_ipp: Selecting paramcd changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "paramcd", "values", "CRP")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ipp: Deselecting paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "paramcd", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "`Select Parameter` field is empty",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_ipp: Selecting visit_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "visit_var", "variables", "ATOXGR")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ipp: Deselecting visit_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "visit_var", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "A Timepoint Variable must be selected",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_ipp: Selecting aval_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "aval_var", "variables", "CHG")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ipp: Deselecting aval_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "aval_var", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "A Parameter values over Time must be selected",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_ipp: Changing add_baseline_hline changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("add_baseline_hline", TRUE)
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ipp: Changing separate_by_obs changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("separate_by_obs", TRUE)
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ipp: Changing suppress_legend changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("suppress_legend", TRUE)
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_ipp: Changing add_avalu changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("add_avalu", FALSE)
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
})
