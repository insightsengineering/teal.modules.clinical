app_driver_tm_g_lineplot <- function() {
  data <- within(teal.data::teal_data(), {
    require(nestcolor)
    ADSL <- teal.modules.clinical::tmc_ex_adsl

    ADLB <- dplyr::mutate(
      teal.modules.clinical::tmc_ex_adlb,
      AVISIT == forcats::fct_reorder(AVISIT, AVISITN, min)
    )
  })

  datanames <- c("ADSL", "ADLB")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = tm_g_lineplot(
      label = "Line Plot",
      dataname = "ADLB",
      strata = teal.transform::choices_selected(
        teal.transform::variable_choices("ADSL", c("ARM", "ARMCD", "ACTARMCD")),
        "ARM"
      ),
      x = teal.transform::choices_selected(teal.transform::variable_choices(
        "ADLB",
        "AVISIT"
      ), "AVISIT", fixed = TRUE),
      y = teal.transform::choices_selected(
        teal.transform::variable_choices("ADLB", c("AVAL", "BASE", "CHG", "PCHG")),
        "AVAL"
      ),
      y_unit = teal.transform::choices_selected(teal.transform::variable_choices(
        "ADLB",
        "AVALU"
      ), "AVALU", fixed = TRUE),
      paramcd = teal.transform::choices_selected(teal.transform::variable_choices(
        "ADLB",
        "PARAMCD"
      ), "PARAMCD", fixed = TRUE),
      param = teal.transform::choices_selected(
        teal.transform::value_choices("ADLB", "PARAMCD", "PARAM"),
        "ALT"
      ),
      conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95,
                                                    keep_order =
                                                      TRUE
      ),
      interval = "mean_ci",
      mid = "mean",
      whiskers = c("mean_ci_lwr", "mean_ci_upr"),
      table = c("n", "mean_sd", "median", "range"),
      mid_type = "pl",
      mid_point_size = c(2, 1, 5),
      table_font_size = c(4, 2, 6),
      plot_height = c(1000L, 200L, 4000L),
      plot_width = NULL,
      pre_output = NULL,
      post_output = NULL,
      ggplot2_args = teal.widgets::ggplot2_args()
    ),
    timeout = 30000
  )
}

testthat::test_that("e2e - tm_g_lineplot: Module initializes in teal without errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_lineplot()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  app_driver$stop()
})

