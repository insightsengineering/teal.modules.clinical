app_driver_tm_t_abnormality <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)

    ADSL <- tmc_ex_adsl
    ADLB <- tmc_ex_adlb %>%
      mutate(
        ONTRTFL = case_when(
          AVISIT %in% c("SCREENING", "BASELINE") ~ "",
          TRUE ~ "Y"
        ) %>% with_label("On Treatment Record Flag")
      )
  })
  datanames <- c("ADSL", "ADLB")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = teal::modules(
      tm_t_abnormality(
        label = "Abnormality Table",
        dataname = "ADLB",
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], subset = c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        add_total = FALSE,
        by_vars = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], subset = c("LBCAT", "PARAM", "AVISIT")),
          selected = c("LBCAT", "PARAM"),
          keep_order = TRUE
        ),
        baseline_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADLB"]], subset = "BNRIND"),
          selected = "BNRIND", fixed = TRUE
        ),
        grade = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], subset = "ANRIND"),
          selected = "ANRIND",
          fixed = TRUE
        ),
        abnormal = list(low = "LOW", high = "HIGH"),
        exclude_base_abn = FALSE
      )
    )
  )
}

testthat::test_that("e2e - tm_t_abnormality: module initializes in teal without errors and produces table output", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_abnormality()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
  )
  app_driver$stop()
})
