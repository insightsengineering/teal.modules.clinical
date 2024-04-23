app_driver_tm_g_forest_tte <- function() {
  data <- teal.data::teal_data() %>%
    within({
      library(nestcolor)
      library(formatters)

      ADSL <- tmc_ex_adsl
      ADTTE <- tmc_ex_adtte
      ADSL$RACE <- droplevels(ADSL$RACE) %>%
        with_label("Race")
    })
  datanames <- c("ADSL", "ADTTE")
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
    modules = teal::modules(
      tm_g_forest_tte(
        label = "Forest Survival",
        dataname = "ADTTE",
        arm_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD")),
          "ARMCD"
        ),
        arm_ref_comp = arm_ref_comp,
        paramcd = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
          "OS"
        ),
        subgroup_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], names(data[["ADSL"]])),
          c("BMRKR2", "SEX")
        ),
        strata_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], c("STRATA1", "STRATA2")),
          "STRATA2"
        )
      )
    )
  )
}

testthat::test_that("e2e - tm_g_forest_tte: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_tte()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_match(
    app_driver$get_attr(
      app_driver$active_module_element("myplot-plot_main > img"),
      "src"
    ),
    "data:image/png;base64,"
  )
  app_driver$stop()
})
