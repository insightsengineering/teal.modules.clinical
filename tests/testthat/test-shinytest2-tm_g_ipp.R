app_driver_tm_g_ipp <- function() {
  data <- within(teal.data::teal_data(), {
    library(dplyr)
    library(tern)
    ADSL <- teal.data::rADSL %>%
      slice(1:20) %>%
      df_explicit_na()
    ADLB <- teal.data::rADLB %>%
      filter(USUBJID %in% ADSL$USUBJID) %>%
      df_explicit_na() %>%
      filter(AVISIT != "SCREENING")
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_ipp(
        label = "Individual Patient Plot",
        dataname = "ADLB",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADLB"]], "ARMCD"),
          "ARM A"
        ),
        paramcd = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADLB"]], "PARAMCD"),
          "ALT"
        ),
        aval_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("AVAL", "CHG")),
          "AVAL"
        ),
        avalu_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("AVALU")),
          "AVALU",
          fixed = TRUE
        ),
        id_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("USUBJID")),
          "USUBJID",
          fixed = TRUE
        ),
        visit_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("AVISIT", "ATOXGR")),
          "AVISIT"
        ),
        baseline_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], c("BASE")),
          "BASE",
          fixed = TRUE
        ),
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
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_match(
    app_driver$get_active_module_plot_output("myplot"),
    "data:image/png;base64,"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_ipp: Starts with specified
  label, parentname, arm_var, paramcd, id_var, visit_var, aval_var, avalu_var, baseline_var
  add_baseline_hline, separate_by_obs, suppress_legend, add_avalu.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_ipp()
    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "Individual Patient Plot"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARMCD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-filter1-vals"),
      "ARM A"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-col"),
      "PARAMCD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals"),
      "ALT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("visit_var-dataset_ADLB_singleextract-select"),
      "AVISIT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADLB_singleextract-select"),
      "AVAL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("id_var-dataset_ADLB_singleextract-select"),
      "USUBJID"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("avalu_var-dataset_ADLB_singleextract-select"),
      "AVALU"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("baseline_var-dataset_ADLB_singleextract-select"),
      "BASE"
    )
    testthat::expect_false(app_driver$get_active_module_input("add_baseline_hline"))
    testthat::expect_false(app_driver$get_active_module_input("separate_by_obs"))
    testthat::expect_false(app_driver$get_active_module_input("suppress_legend"))
    testthat::expect_true(app_driver$get_active_module_input("add_avalu"))
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_ipp: Selecting arm_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-filter1-vals", "ARM B")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Deselecting arm_var column throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-filter1-vals_input > div > span")),
    "Please select Arm filter."
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Selecting paramcd changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals", "CRP")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Deselecting paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("paramcd-dataset_ADLB_singleextract-filter1-vals_input > div > span")),
    "Please select Parameter filter."
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Selecting visit_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("visit_var-dataset_ADLB_singleextract-select", "ATOXGR")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Deselecting visit_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  app_driver$set_active_module_input("visit_var-dataset_ADLB_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("visit_var-dataset_ADLB_singleextract-select_input > div > span")),
    "A Timepoint Variable must be selected"
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Selecting aval_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("aval_var-dataset_ADLB_singleextract-select", "CHG")
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Deselecting aval_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  app_driver$set_active_module_input("aval_var-dataset_ADLB_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("aval_var-dataset_ADLB_singleextract-select_input > div > span")),
    "A Parameter values over Time must be selected"
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Changing add_baseline_hline changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("add_baseline_hline", TRUE)
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})
testthat::test_that("e2e - tm_g_ipp: Changing separate_by_obs changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("separate_by_obs", TRUE)
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Changing suppress_legend changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("suppress_legend", TRUE)
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_ipp: Changing add_avalu changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_ipp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("add_avalu", FALSE)
  testthat::expect_false(
    identical(
      plot_before,
      app_driver$get_active_module_plot_output("myplot")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})
