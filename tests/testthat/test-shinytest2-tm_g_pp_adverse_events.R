app_driver_tm_g_pp_adverse_events <- function() {
  data <- teal.data::teal_data() |> within({
    library(nestcolor)
    library(dplyr)

    ADAE <- teal.data::rADAE
    ADSL <- teal.data::rADSL %>% filter(USUBJID %in% ADAE$USUBJID)
  })

  teal.data::datanames(data) <- c("ADAE", "ADSL")
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[c("ADAE", "ADSL")]


  init_teal_app_driver(
    data = data,
    modules = modules(
      tm_g_pp_adverse_events(
        label = "Adverse Events",
        dataname = "ADAE",
        parentname = "ADSL",
        patient_col = "USUBJID",
        plot_height = c(600L, 200L, 2000L),
        aeterm = choices_selected(
          choices = variable_choices(data[["ADAE"]], "AETERM"),
          selected = "AETERM"
        ),
        tox_grade = choices_selected(
          choices = variable_choices(data[["ADAE"]], "AETOXGR"),
          selected = "AETOXGR"
        ),
        causality = choices_selected(
          choices = variable_choices(data[["ADAE"]], "AEREL"),
          selected = "AEREL"
        ),
        outcome = choices_selected(
          choices = variable_choices(data[["ADAE"]], "AEOUT"),
          selected = "AEOUT"
        ),
        action = choices_selected(
          choices = variable_choices(data[["ADAE"]], "AEACN"),
          selected = "AEACN"
        ),
        time = choices_selected(
          choices = variable_choices(data[["ADAE"]], "ASTDY"),
          selected = "ASTDY"
        ),
        decod = NULL
      )
    )
  )
}

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Module initializes in teal without any errors and produces a plot.",
  {
    app_driver <- app_driver_tm_g_pp_adverse_events()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    testthat::expect_match(app_driver$get_active_module_pws_output("chart"), "data:image/png;base64,")
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Module initializes with specific label, patient_id, aeterm, tox_grade, causality, outcome, action, time, decod.",
  {
    app_driver <- app_driver_tm_g_pp_adverse_events()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting patient_id doesn't throw errors and changes a plot",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting patient_id throw validation error",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting aeterm column doesn't throw errors and changes a plot",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting aeterm column throw validation error",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting tox_grade column doesn't throw errors and changes a plot",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting tox_grade column throw validation error",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting causality column doesn't throw errors and changes a plot",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting causality column throw validation error",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting outcome column doesn't throw errors and changes a plot",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting outcome column throw validation error",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting action column doesn't throw errors and changes a plot",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting action column throw validation error",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting time column doesn't throw errors and changes a plot",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting time column throw validation error",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting decod column doesn't throw errors and changes a plot",
  {

  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting decod column throw validation error",
  {

  }
)
