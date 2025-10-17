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
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADVS"]], c("PARAMCD", "PARAM")),
          selected = "PARAMCD"
        ),
        xaxis = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADVS"]], c("ADY", "BMRKR1")),
          selected = "ADY"
        ),
        aval_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADVS"]], c("AVAL", "BASE2")),
          selected = "AVAL"
        ),
        plot_width = NULL,
        pre_output = NULL,
        post_output = NULL,
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_g_pp_vitals: Module initializes in teal without errors and produces plot output.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_match(
    app_driver$get_attr(
      app_driver$namespaces(TRUE)$module("vitals_plot-plot_main > img"),
      "src"
    ),
    "data:image/png;base64,"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_pp_vitals: Starts with specified label, patient_id, paramcd, xaxis, aval_var, font_size,
  parentname, patient_col.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_vitals()

    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "Vitals"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-3-id-128"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADVS_singleextract-select"),
      "PARAMCD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd_levels_vals"),
      c("DIABP", "PULSE", "RESP", "SYSBP", "TEMP", "WEIGHT")
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("xaxis-dataset_ADVS_singleextract-select"),
      "ADY"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADVS_singleextract-select"),
      "AVAL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("font_size"),
      12
    )
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_pp_vitals: Selecting patient_id changes plot and doesn't throw validation errors.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  plot_before <- app_driver$get_active_module_plot_output("vitals_plot")
  app_driver$set_active_module_input("patient_id", "AB12345-CHN-15-id-262")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("vitals_plot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_pp_vitals: Deselecting patient_id column throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  app_driver$set_active_module_input("patient_id", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("vitals_plot"), character(0))
  testthat::expect_identical(
    app_driver$namespaces(TRUE)$module("patient_id_input > div > span"),
    "Please select a patient."
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_pp_vitals: Selecting valid paramcd and paramcd_levels_vals changes plot
  and doesn't throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_vitals()
    plot_before <- app_driver$get_active_module_plot_output("vitals_plot")

    # Changing the PARAMCD variable
    app_driver$set_active_module_input("paramcd-dataset_ADVS_singleextract-select", "PARAM")

    # Expecting validation error on empty PARAMCD levels input
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$namespaces(TRUE)$module("paramcd_levels > div > span"),
      "Please select PARAMCD variable levels."
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
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_pp_vitals: Deselecting paramcd throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  app_driver$set_active_module_input("paramcd-dataset_ADVS_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("vitals_plot"), character(0))
  testthat::expect_identical(
    app_driver$namespaces(TRUE)$module("paramcd-dataset_ADVS_singleextract-select_input > div > span"),
    "Please select PARAMCD variable."
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_pp_vitals: Selecting xaxis changes plot and doesn't throw validation errors.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  plot_before <- app_driver$get_active_module_plot_output("vitals_plot")
  app_driver$set_active_module_input("xaxis-dataset_ADVS_singleextract-select", "BMRKR1")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("vitals_plot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_pp_vitals: Deselecting xaxis column throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  app_driver$set_active_module_input("xaxis-dataset_ADVS_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("vitals_plot"), character(0))
  testthat::expect_identical(
    app_driver$namespaces(TRUE)$module("xaxis-dataset_ADVS_singleextract-select_input > div > span"),
    "Please select Vitals x-axis variable."
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_pp_vitals: Selecting aval_var changes plot and doesn't throw validation errors.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  plot_before <- app_driver$get_active_module_plot_output("vitals_plot")
  app_driver$set_active_module_input("aval_var-dataset_ADVS_singleextract-select", "BASE2")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("vitals_plot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_pp_vitals: Deselecting aval_var column throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  app_driver$set_active_module_input("aval_var-dataset_ADVS_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("vitals_plot"), character(0))
  testthat::expect_identical(
    app_driver$namespaces(TRUE)$module("aval_var-dataset_ADVS_singleextract-select_input > div > span"),
    "Please select AVAL variable."
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_pp_vitals: Changing font_size changes plot and doesn't throw validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_vitals()
  plot_before <- app_driver$get_active_module_plot_output("vitals_plot")
  app_driver$set_active_module_input("font_size", 20)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("vitals_plot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})
