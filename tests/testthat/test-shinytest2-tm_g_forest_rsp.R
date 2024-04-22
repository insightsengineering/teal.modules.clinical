app_driver_tm_g_forest_rsp <- function() {
  data <- teal.data::teal_data() %>%
    within({
      library(dplyr)
      library(tern)
      library(formatters)
      ADSL <- tmc_ex_adsl
      ADRS <- tmc_ex_adrs %>%
        mutate(AVALC = d_onco_rsp_label(AVALC)) %>%
        with_label("Character Result/Finding") %>%
        filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
    })

  datanames <- c("ADSL", "ADRS")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

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
    data = data,
    modules = modules(
      tm_g_forest_rsp(
        label = "Forest Response",
        dataname = "ADRS",
        arm_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD")),
          "ARMCD"
        ),
        arm_ref_comp = arm_ref_comp,
        paramcd = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
          "INVET"
        ),
        subgroup_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], names(data[["ADSL"]])),
          c("BMRKR2", "SEX")
        ),
        strata_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], c("STRATA1", "STRATA2")),
          "STRATA2"
        ),
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
        )
      )
    )
  )
}

testthat::test_that("e2e - tm_g_forest_rsp: module initializes without errors and produces plot output", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_forest_rsp()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(app_driver$is_visible(app_driver$active_module_element("myplot-plot_main")))

  app_driver$stop()
})

testthat::test_that("e2e - tm_g_forest_rsp: module is initialized with expected defaults in encoding panel", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_forest_rsp()

  testthat::expect_equal(
    app_driver$get_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals"),
    "INVET"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("responders"),
    c("Complete Response (CR)", "Partial Response (PR)")
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
    "ARMCD"
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
})

testthat::test_that("e2e - tm_g_forest_rsp: module initializes without errors and produces plot output", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_forest_rsp()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(app_driver$is_visible(app_driver$active_module_element("myplot-plot_main")))

  app_driver$stop()
})
