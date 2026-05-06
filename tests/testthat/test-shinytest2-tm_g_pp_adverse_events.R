app_driver_tm_g_pp_adverse_events <- function() { # nolint: object_length.
  data <- within(teal.data::teal_data(), {
    library(nestcolor)
    library(dplyr)

    ADAE <- teal.data::rADAE
    ADSL <- teal.data::rADSL |> filter(USUBJID %in% ADAE$USUBJID)
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
        aeterm = teal.picks::variables(c("AETERM", "AGEU"), multiple = FALSE),
        tox_grade = teal.picks::variables(c("AETOXGR", "COUNTRY"), multiple = FALSE),
        causality = teal.picks::variables(c("AEREL", "ACTARM"), multiple = FALSE),
        outcome = teal.picks::variables(c("AEOUT", "SITEID"), multiple = FALSE),
        action = teal.picks::variables(c("AEACN", "SMQ01NAM"), multiple = FALSE),
        time = teal.picks::variables(c("ASTDY", "AGE"), multiple = FALSE),
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
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()
    testthat::expect_match(app_driver$get_active_module_plot_output("chart"), "data:image/png;base64,")
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table"))
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Module initializes with specific label, patient_id, aeterm,
  tox_grade, causality, outcome, action, time, decod.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Adverse Events"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-3-id-128"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(
      exported_values[["aeterm-picks_resolved"]]$variables$selected,
      "AETERM"
    )
    testthat::expect_equal(
      exported_values[["tox_grade-picks_resolved"]]$variables$selected,
      "AETOXGR"
    )
    testthat::expect_equal(
      exported_values[["causality-picks_resolved"]]$variables$selected,
      "AEREL"
    )
    testthat::expect_equal(
      exported_values[["outcome-picks_resolved"]]$variables$selected,
      "AEOUT"
    )
    testthat::expect_equal(
      exported_values[["action-picks_resolved"]]$variables$selected,
      "AEACN"
    )
    testthat::expect_equal(
      exported_values[["time-picks_resolved"]]$variables$selected,
      "ASTDY"
    )
    testthat::expect_null(
      exported_values[["decod-picks_resolved"]]
    )
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting patient_id doesn't throw errors and changes the plot and table.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
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
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting patient_id throw validation error",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("patient_id", NULL)
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("title")),
      "Please select a patient.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting aeterm column doesn't throw errors and changes the plot and table.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    set_teal_picks_slot(app_driver, "aeterm", "variables", "AGEU")
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
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting aeterm column throw validation error",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    set_teal_picks_slot(app_driver, "aeterm", "variables", character(0L))
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("title")),
      "Please select AETERM variable.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting tox_grade column doesn't throw errors and changes the plot and table.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    set_teal_picks_slot(app_driver, "tox_grade", "variables", "COUNTRY")
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
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting tox_grade column throw validation error",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    set_teal_picks_slot(app_driver, "tox_grade", "variables", character(0L))
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("title")),
      "Please select AETOXGR variable.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting causality column doesn't throw errors and changes the plot and table.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    set_teal_picks_slot(app_driver, "causality", "variables", "ACTARM")
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
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting causality column throw validation error",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    set_teal_picks_slot(app_driver, "causality", "variables", character(0L))
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("title")),
      "Please select AEREL variable.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting outcome column doesn't throw errors and changes the plot and table.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    set_teal_picks_slot(app_driver, "outcome", "variables", "SITEID")
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
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting outcome column throw validation error",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    set_teal_picks_slot(app_driver, "outcome", "variables", character(0L))
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("title")),
      "Please select AEOUT variable.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting action column doesn't throw errors and changes the plot and table.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    set_teal_picks_slot(app_driver, "action", "variables", "SMQ01NAM")
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
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting action column throw validation error",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    set_teal_picks_slot(app_driver, "action", "variables", character(0L))
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("title")),
      "Please select AEACN variable.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Selecting time column doesn't throw errors and changes the plot and table.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("chart")
    table_before <- app_driver$get_active_module_table_output("table")
    set_teal_picks_slot(app_driver, "time", "variables", "AGE")
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
  }
)

testthat::test_that(
  "e2e tm_g_pp_adverse_events - Deselecting time column throw validation error",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_adverse_events()
    withr::defer(app_driver$stop())
    set_teal_picks_slot(app_driver, "time", "variables", character(0L))
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("title")),
      "Please select ASTDY variable.",
      fixed = TRUE
    )
  }
)
