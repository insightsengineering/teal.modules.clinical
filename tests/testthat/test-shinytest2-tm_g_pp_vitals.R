app_driver_tm_g_pp_vitals <- function() {
  data <- within(teal.data::teal_data(), {
    ADSL <- teal.data::rADSL
    ADVS <- teal.data::rADVS
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_pp_vitals(
        label = "Vitals",
        dataname = "ADVS",
        parentname = "ADSL",
        patient_col = "USUBJID",
        plot_height = c(600L, 200L, 2000L),
        paramcd = teal.picks::variables(c("PARAMCD", "PARAM"), multiple = FALSE),
        xaxis = teal.picks::variables(c("ADY", "BMRKR1"), multiple = FALSE),
        aval_var = teal.picks::variables(c("AVAL", "BASE2"), multiple = FALSE),
        plot_width = NULL,
        pre_output = NULL,
        post_output = NULL,
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_g_pp_vitals: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_match(
    app_driver$get_attr(
      app_driver$namespaces(TRUE)$module("vitals_plot-plot_main > img"),
      "src"
    ),
    "data:image/png;base64,"
  )
})

testthat::test_that(
  "e2e - tm_g_pp_vitals: Starts with specified label, patient_id, paramcd, xaxis, aval_var, font_size,
  parentname, patient_col.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_vitals()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Vitals"
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
      exported_values[["paramcd-picks_resolved"]]$variables$selected,
      "PARAMCD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd_levels_vals"),
      c("DIABP", "PULSE", "RESP", "SYSBP", "TEMP", "WEIGHT")
    )
    testthat::expect_equal(
      exported_values[["xaxis-picks_resolved"]]$variables$selected,
      "ADY"
    )
    testthat::expect_equal(
      exported_values[["aval_var-picks_resolved"]]$variables$selected,
      "AVAL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("font_size"),
      12
    )
  }
)

testthat::test_that("e2e - tm_g_pp_vitals: Selecting patient_id changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("vitals_plot")
  app_driver$set_active_module_input("patient_id", "AB12345-CHN-15-id-262")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("vitals_plot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_pp_vitals: Deselecting patient_id column throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("patient_id", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("vitals_plot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("vitals_plot-plot-out-main")),
    "Please select a patient.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_g_pp_vitals: Selecting valid paramcd and paramcd_levels_vals changes plot
  and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_vitals()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("vitals_plot")

    # Changing the PARAMCD variable
    set_teal_picks_slot(app_driver, "paramcd", "variables", "PARAM")

    # Expecting validation error on empty PARAMCD levels input
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("vitals_plot-plot-out-main")),
      "Please select PARAMCD variable levels.",
      fixed = TRUE
    )

    # Updating the dependant PARAMCD levels
    app_driver$set_active_module_input(
      "paramcd_levels_vals",
      c(
        "Diastolic Blood Pressure", "Pulse Rate", "Respiratory Rate",
        "Systolic Blood Pressure", "Temperature", "Weight"
      )
    )

    # Expecting the plot to update without errors
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("vitals_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_g_pp_vitals: Deselecting paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "paramcd", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("vitals_plot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("vitals_plot-plot-out-main")),
    "Please select PARAMCD variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_vitals: Selecting xaxis changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("vitals_plot")
  set_teal_picks_slot(app_driver, "xaxis", "variables", "BMRKR1")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("vitals_plot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_pp_vitals: Deselecting xaxis column throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "xaxis", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("vitals_plot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("vitals_plot-plot-out-main")),
    "Please select Vitals x-axis variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_vitals: Selecting aval_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("vitals_plot")
  set_teal_picks_slot(app_driver, "aval_var", "variables", "BASE2")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("vitals_plot")
    )
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_pp_vitals: Deselecting aval_var column throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "aval_var", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("vitals_plot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("vitals_plot-plot-out-main")),
    "Please select AVAL variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_vitals: Changing font_size changes plot and doesn't throw validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("vitals_plot")
  app_driver$set_active_module_input("font_size", 20)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("vitals_plot")))
  app_driver$expect_no_validation_error()
})
