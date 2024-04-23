app_driver_tm_t_abnormality_by_worst_grade <- function() { # nolint: object_length
  data <- teal.data::teal_data() %>%
    within({
      library(dplyr)

      ADSL <- tmc_ex_adsl
      ADLB <- tmc_ex_adlb %>%
        filter(!AVISIT %in% c("SCREENING", "BASELINE"))
    })

  datanames <- c("ADSL", "ADLB")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = teal::modules(
      tm_t_abnormality_by_worst_grade(
        label = "Laboratory Test Results with Highest Grade Post-Baseline",
        dataname = "ADLB",
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], subset = c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
          selected = c("ALT", "CRP", "IGA")
        ),
        add_total = FALSE
      )
    ),
    filter = teal::teal_slices(
      teal.slice::teal_slice("ADSL", "SAFFL", selected = "Y"),
      teal.slice::teal_slice("ADLB", "ONTRTFL", selected = "Y")
    )
  )
}

testthat::test_that(
  "e2e - tm_t_abnormality_by_worst_grade: Module initializes in teal without errors and produces table output.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_abnormality_by_worst_grade()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()

    testthat::expect_true(
      app_driver$is_visible(app_driver$active_module_element("table-table-with-settings"))
    )
    app_driver$stop()
  }
)
