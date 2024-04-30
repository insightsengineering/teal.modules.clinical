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
    # skip_if_too_deep(5)
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
    # skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    testthat::expect_equal(
      app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
      "Adverse Events"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-3-id-128"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("aeterm-dataset_ADAE_singleextract-select"),
      "AETERM"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("tox_grade-dataset_ADAE_singleextract-select"),
      "AETOXGR"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("causality-dataset_ADAE_singleextract-select"),
      "AEREL"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("outcome-dataset_ADAE_singleextract-select"),
      "AEOUT"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("action-dataset_ADAE_singleextract-select"),
      "AEACN"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("time-dataset_ADAE_singleextract-select"),
      "ASTDY"
    )

    testthat::expect_null(
      app_driver$get_active_module_input("decod-dataset_ADAE_singleextract-select")
    )

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting patient_id doesn't throw errors and changes a plot",
  {
    # skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    plot_before <- app_driver$get_active_module_pws_output("chart")
    app_driver$set_active_module_input("patient_id", "AB12345-CHN-15-id-262")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_pws_output("chart")
      )
    )
    app_driver$expect_no_shiny_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting patient_id throw validation error",
  {
    # skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "patient_id"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input > div > span", input_id)),
      "Please select a patient"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting aeterm column doesn't throw errors and changes a plot",
  {
    # skip_if_too_deep(5)
    # not possible for given app due to single choice
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting aeterm column throw validation error",
  {
    # skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "aeterm-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input > div > span", input_id)),
      "Please select AETERM variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting tox_grade column doesn't throw errors and changes a plot",
  {
    # skip_if_too_deep(5)
    # not possible for given app due to single choice
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting tox_grade column throw validation error",
  {
    # skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "tox_grade-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input > div > span", input_id)),
      "Please select AETOXGR variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting causality column doesn't throw errors and changes a plot",
  {
    # skip_if_too_deep(5)
    # not possible for given app due to single choice
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting causality column throw validation error",
  {
    # skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "causality-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input > div > span", input_id)),
      "Please select AEREL variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting outcome column doesn't throw errors and changes a plot",
  {
    # skip_if_too_deep(5)
    # not possible for given app due to single choice
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting outcome column throw validation error",
  {
    # skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "outcome-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input > div > span", input_id)),
      "Please select AEOUT variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting action column doesn't throw errors and changes a plot",
  {
    # skip_if_too_deep(5)
    # not possible for given app due to single choice
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting action column throw validation error",
  {
    # skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "action-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input > div > span", input_id)),
      "Please select AEACN variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting time column doesn't throw errors and changes a plot",
  {
    # skip_if_too_deep(5)
    # not possible for given app due to single choice
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting time column throw validation error",
  {
    # skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "time-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input > div > span", input_id)),
      "Please select ASTDY variable."
    )
    app_driver$stop()
  }
)
