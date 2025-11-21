app_driver_tm_g_forest_rsp <- function() {
  data <- within(teal.data::teal_data(), {
    library(dplyr)
    library(tern)
    ADSL <- teal.data::rADSL
    ADRS <- teal.data::rADRS %>%
      mutate(AVALC = d_onco_rsp_label(AVALC)) %>%
      with_label("Character Result/Finding") %>%
      filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  arm_ref_comp <- list(
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    ),
    ARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    )
  )

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = modules(
        tm_g_forest_rsp(
          label = "Forest Response",
          dataname = "ADRS",
          parentname = "ADSL",
          arm_var = teal.transform::choices_selected(
            teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD")),
            "ARMCD"
          ),
          arm_ref_comp = arm_ref_comp,
          paramcd = teal.transform::choices_selected(
            teal.transform::value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
            "INVET"
          ),
          aval_var = teal.transform::choices_selected(
            teal.transform::variable_choices(data[["ADRS"]], "AVALC"),
            "AVALC",
            fixed = TRUE
          ),
          subgroup_var = teal.transform::choices_selected(
            teal.transform::variable_choices(data[["ADSL"]], names(data[["ADSL"]])),
            c("BMRKR2", "SEX")
          ),
          strata_var = teal.transform::choices_selected(
            teal.transform::variable_choices(data[["ADSL"]], c("STRATA1", "STRATA2")),
            "STRATA2"
          ),
          fixed_symbol_size = TRUE,
          conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8, 2), 0.95, keep_order = TRUE),
          plot_height = c(600L, 200L, 2000L),
          default_responses = list(
            BESRSPI = list(
              rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
              levels = c(
                "Complete Response (CR)", "Partial Response (PR)", "Stable Disease (SD)",
                "Progressive Disease (PD)", "Not Evaluable (NE)"
              )
            ),
            INVET = list(
              rsp = c("Complete Response (CR)", "Partial Response (PR)"),
              levels = c(
                "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
                "Progressive Disease (PD)", "Stable Disease (SD)"
              )
            ),
            OVRINV = list(
              rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
              levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
            )
          ),
          plot_width = c(1500L, 800L, 3000L),
          rel_width_forest = c(25L, 0L, 100L),
          font_size = c(15L, 1L, 30L),
          pre_output = NULL,
          post_output = NULL,
          ggplot2_args = teal.widgets::ggplot2_args()
        )
      )
    )
  )
}

testthat::test_that("e2e - tm_g_forest_rsp: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_forest_rsp()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(app_driver$is_visible(app_driver$namespaces(TRUE)$module("myplot-plot_main")))

  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_forest_rsp: Module initializes with specified
  label, arm_var, paramcd, aval_var, responders, subgroup_var, strata_var,
  conf_level, fixed_symbol_size, rel_width_forest, font_size.",
  {
    skip_if_too_deep(5)

    app_driver <- app_driver_tm_g_forest_rsp()

    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "Forest Response"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARMCD"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals"),
      "INVET"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADRS_singleextract-select"),
      "AVALC"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("responders"),
      c("Complete Response (CR)", "Partial Response (PR)")
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("subgroup_var-dataset_ADSL_singleextract-select"),
      c("SEX", "BMRKR2")
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("strata_var-dataset_ADSL_singleextract-select"),
      "STRATA2"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_true(app_driver$get_active_module_input("fixed_symbol_size"))
    testthat::expect_equal(
      app_driver$get_active_module_input("rel_width_forest"),
      25
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("font_size"),
      15
    )

    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_forest_rsp: Selecting arm_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", "ARM")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Treatment variable must be selected"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting paramcd changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals", "OVRINV")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  app_driver$set_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals", NULL)
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please select Endpoint filter"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting responders changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("responders", "Complete Response (CR)")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting responders throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  app_driver$set_active_module_input("responders", NULL)
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "`Responders` field is empty"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting subgroup_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("subgroup_var-dataset_ADSL_singleextract-select", c("SEX", "BMRKR2", "AGEU"))
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting a non-factors column in subgroup_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  app_driver$set_active_module_input("subgroup_var-dataset_ADSL_singleextract-select", c("SEX", "AGE"))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Not all subgroup variables are factors"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting subgroup_var changes plot and doesn't throw validation errors.", { # nolint: line_length
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("subgroup_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting strata_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("strata_var-dataset_ADSL_singleextract-select", "STRATA1")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting strata_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("strata_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting conf_level changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("conf_level", "0.9")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting conf_level or selecting outside the range of 0-1 throws validation error.", { # nolint: line_length
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  app_driver$set_active_module_input("conf_level", NULL)
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please choose a confidence level between 0 and 1"
  )
  app_driver$set_active_module_input("conf_level", 2)
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please choose a confidence level between 0 and 1"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Unsetting fixed_symbol_size changes plot and doesn't throw validation errors.", { # nolint: line_length
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("fixed_symbol_size", FALSE)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Changing rel_width_forest changes plot and doesn't throw validation errors.", { # nolint: line_length
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("rel_width_forest", 30)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: Changing font_size changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("font_size", 25)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})
