app_driver_tm_g_pp_adverse_events <- function() { # nolint: object_length.
  data <- within(teal.data::teal_data(), {
    library(nestcolor)
    library(dplyr)

    ADAE <- teal.data::rADAE
    ADSL <- teal.data::rADSL %>% filter(USUBJID %in% ADAE$USUBJID)
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_pp_adverse_events(
        label = "Adverse Events",
        dataname = "ADAE",
        parentname = "ADSL",
        patient_col = "USUBJID",
        plot_height = c(600L, 200L, 2000L),
        aeterm = choices_selected(
          choices = variable_choices(data[["ADAE"]], c("AETERM", "AGEU")),
          selected = "AETERM"
        ),
        tox_grade = choices_selected(
          choices = variable_choices(data[["ADAE"]], c("AETOXGR", "COUNTRY")),
          selected = "AETOXGR"
        ),
        causality = choices_selected(
          choices = variable_choices(data[["ADAE"]], c("AEREL", "ACTARM")),
          selected = "AEREL"
        ),
        outcome = choices_selected(
          choices = variable_choices(data[["ADAE"]], c("AEOUT", "SITEID")),
          selected = "AEOUT"
        ),
        action = choices_selected(
          choices = variable_choices(data[["ADAE"]], c("AEACN", "SMQ01NAM")),
          selected = "AEACN"
        ),
        time = choices_selected(
          choices = variable_choices(data[["ADAE"]], c("ASTDY", "AGE")),
          selected = "ASTDY"
        ),
        decod = NULL,
        font_size = c(12L, 12L, 25L),
        plot_width = NULL,
        pre_output = NULL,
        post_output = NULL,
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    )
  )
}

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Module initializes in teal without any errors and produces the plot and table.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    testthat::expect_match(app_driver$get_active_module_plot_output("chart"), "data:image/png;base64,")
    testthat::expect_true(
      app_driver$is_visible(app_driver$active_module_element("table"))
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Module initializes with specific label, patient_id, aeterm,
  tox_grade, causality, outcome, action, time, decod.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
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
  "e2e tm_g_pp_adverse_events - Selecting patient_id doesn't throw errors and changes the plot and table.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    app_driver$set_active_module_input("patient_id", "AB12345-CHN-15-id-262")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("chart")
      )
    )
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table")
      )
    )
    app_driver$expect_no_shiny_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting patient_id throw validation error",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "patient_id"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input .shiny-validation-message", input_id)),
      "Please select a patient"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting aeterm column doesn't throw errors and changes the plot and table.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    app_driver$set_active_module_input("aeterm-dataset_ADAE_singleextract-select", "AGEU")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("chart")
      )
    )

    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table")
      )
    )
    app_driver$expect_no_shiny_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting aeterm column throw validation error",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "aeterm-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input .shiny-validation-message", input_id)),
      "Please select AETERM variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting tox_grade column doesn't throw errors and changes the plot and table.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    app_driver$set_active_module_input("tox_grade-dataset_ADAE_singleextract-select", "COUNTRY")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("chart")
      )
    )

    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table")
      )
    )
    app_driver$expect_no_shiny_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting tox_grade column throw validation error",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "tox_grade-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input .shiny-validation-message", input_id)),
      "Please select AETOXGR variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting causality column doesn't throw errors and changes the plot and table.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    app_driver$set_active_module_input("causality-dataset_ADAE_singleextract-select", "ACTARM")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("chart")
      )
    )

    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table")
      )
    )
    app_driver$expect_no_shiny_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting causality column throw validation error",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "causality-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input .shiny-validation-message", input_id)),
      "Please select AEREL variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting outcome column doesn't throw errors and changes the plot and table.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    app_driver$set_active_module_input("outcome-dataset_ADAE_singleextract-select", "SITEID")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("chart")
      )
    )

    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table")
      )
    )
    app_driver$expect_no_shiny_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting outcome column throw validation error",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "outcome-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input .shiny-validation-message", input_id)),
      "Please select AEOUT variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting action column doesn't throw errors and changes the plot and table.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    app_driver$set_active_module_input("action-dataset_ADAE_singleextract-select", "SMQ01NAM")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("chart")
      )
    )

    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table")
      )
    )
    app_driver$expect_no_shiny_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting action column throw validation error",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "action-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input .shiny-validation-message", input_id)),
      "Please select AEACN variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting time column doesn't throw errors and changes the plot and table.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    app_driver$set_active_module_input("time-dataset_ADAE_singleextract-select", "AGE")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("chart")
      )
    )

    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table")
      )
    )
    app_driver$expect_no_shiny_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting time column throw validation error",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    input_id <- "time-dataset_ADAE_singleextract-select"
    app_driver$set_active_module_input(input_id, "")
    app_driver$expect_validation_error()
    testthat::expect_identical(
      app_driver$active_module_element_text(sprintf("%s_input .shiny-validation-message", input_id)),
      "Please select ASTDY variable."
    )
    app_driver$stop()
  }
)
