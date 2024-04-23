app_driver_tm_t_ancova <- function() {
  data <- teal.data::teal_data() %>%
    within({
      ADSL <- tmc_ex_adsl
      ADQS <- tmc_ex_adqs
    })

  datanames <- c("ADSL", "ADQS")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  arm_ref_comp <- list(
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    ),
    ACTARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    )
  )

  init_teal_app_driver(
    data = data,
    modules = teal::modules(
      tm_t_ancova(
        label = "ANCOVA Table",
        dataname = "ADQS",
        avisit = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADQS"]], "AVISIT"),
          selected = "WEEK 1 DAY 8"
        ),
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ACTARMCD", "ARMCD")),
          selected = "ARMCD"
        ),
        arm_ref_comp = arm_ref_comp,
        aval_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADQS"]], c("CHG", "AVAL")),
          selected = "CHG"
        ),
        cov_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADQS"]], c("BASE", "STRATA1", "SEX")),
          selected = "STRATA1"
        ),
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
          selected = "FKSI-FWB"
        ),
        interact_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADQS"]], c("BASE", "STRATA1", "SEX")),
          selected = "STRATA1"
        )
      )
    )
  )
}

testthat::test_that("e2e - tm_t_ancova: module initializes in teal without errors and produces table output", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_true(
      app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
    )
  app_driver$stop()
})
