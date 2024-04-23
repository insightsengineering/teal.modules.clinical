app_driver_tm_g_pp_adverse_events <- function() { # nolint: object_length
  data <- teal.data::teal_data() %>%
    within({
      library(dplyr)
      ADAE <- tmc_ex_adae
      ADSL <- tmc_ex_adsl %>%
        filter(USUBJID %in% ADAE$USUBJID)
    })

  datanames <- c("ADSL", "ADAE")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = teal::modules(
      tm_g_pp_adverse_events(
        label = "Adverse Events",
        dataname = "ADAE",
        parentname = "ADSL",
        patient_col = "USUBJID",
        plot_height = c(600L, 200L, 2000L),
        aeterm = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], "AETERM"),
          selected = "AETERM"
        ),
        tox_grade = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], "AETOXGR"),
          selected = "AETOXGR"
        ),
        causality = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], "AEREL"),
          selected = "AEREL"
        ),
        outcome = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], "AEOUT"),
          selected = "AEOUT"
        ),
        action = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], "AEACN"),
          selected = "AEACN"
        ),
        time = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], "ASTDY"),
          selected = "ASTDY"
        ),
        decod = NULL
      )
    )
  )
}

testthat::test_that(
  "e2e - tm_g_pp_adverse_events: module initializes in teal without errors and produces plot output",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
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
  }
)
