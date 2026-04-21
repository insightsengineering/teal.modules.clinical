app_driver_tm_g_pp_patient_timeline <- function() { # nolint object_length.
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADAE <- tmc_ex_adae
    ADSL <- tmc_ex_adsl |> filter(USUBJID %in% ADAE$USUBJID)
    ADCM <- tmc_ex_adcm |> mutate(
      CMSTDY = case_when(
        CMCAT == "medcl B" ~ 20,
        CMCAT == "medcl C" ~ 150,
        TRUE ~ 1
      ) |> with_label("Study Day of Start of Medication"),
      CMENDY = case_when(
        CMCAT == "medcl B" ~ 700,
        CMCAT == "medcl C" ~ 1000,
        TRUE ~ 500
      ) |> with_label("Study Day of End of Medication"),
      CMASTDTM = ASTDTM,
      CMAENDTM = AENDTM
    )
  })

  adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")

  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]
  teal.data::join_keys(data)["ADCM", "ADCM"] <- adcm_keys
  teal.data::join_keys(data)["ADAE", "ADCM"] <- c("STUDYID", "USUBJID")

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_pp_patient_timeline(
        label = "Patient Timeline",
        dataname_adae = "ADAE",
        dataname_adcm = "ADCM",
        parentname = "ADSL",
        patient_col = "USUBJID",
        plot_height = c(600L, 200L, 2000L),
        cmdecod = teal.picks::variables(c("CMDECOD", "CMCAT"), multiple = FALSE),
        aeterm = teal.picks::variables(c("AETERM", "AESOC"), multiple = FALSE),
        aetime_start = teal.picks::variables(c("ASTDTM", "TRTSDTM"), multiple = FALSE),
        aetime_end = teal.picks::variables(c("AENDTM", "EOSDT"), multiple = FALSE),
        dstime_start = teal.picks::variables(c("CMASTDTM", "TRTEDTM"), multiple = FALSE),
        dstime_end = teal.picks::variables(c("CMAENDTM", "TRTEDTM"), multiple = FALSE),
        aerelday_start = teal.picks::variables(c("ASTDY", "AENDY"), multiple = FALSE),
        aerelday_end = teal.picks::variables(c("AENDY", "ASTDY"), multiple = FALSE),
        dsrelday_start = teal.picks::variables(c("ASTDY", "AENDY"), multiple = FALSE),
        dsrelday_end = teal.picks::variables(c("AENDY", "ASTDY"), multiple = FALSE),
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
  "e2e - tm_g_pp_patient_timeline: Module initializes in teal without errors and produces plot output.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()

    testthat::expect_match(
      app_driver$get_active_module_plot_output("patient_timeline_plot"),
      "data:image/png;base64,"
    )
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Starts with specified label, patient_id, cmdecod, aeterm, aetime_start,
  aetime_end, dstime_start, dstime_end, aerelday_start, aerelday_end, dsrelday_start, dsrelday_end.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Patient Timeline"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-11-id-2"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(
      exported_values[["cmdecod-picks_resolved"]]$variables$selected,
      "CMDECOD"
    )
    testthat::expect_equal(
      exported_values[["aeterm-picks_resolved"]]$variables$selected,
      "AETERM"
    )
    testthat::expect_equal(
      exported_values[["aetime_start-picks_resolved"]]$variables$selected,
      "ASTDTM"
    )
    testthat::expect_equal(
      exported_values[["aetime_end-picks_resolved"]]$variables$selected,
      "AENDTM"
    )
    testthat::expect_equal(
      exported_values[["dstime_start-picks_resolved"]]$variables$selected,
      "CMASTDTM"
    )
    testthat::expect_equal(
      exported_values[["dstime_end-picks_resolved"]]$variables$selected,
      "CMAENDTM"
    )
    testthat::expect_equal(
      exported_values[["aerelday_start-picks_resolved"]]$variables$selected,
      "ASTDY"
    )
    testthat::expect_equal(
      exported_values[["aerelday_end-picks_resolved"]]$variables$selected,
      "AENDY"
    )
    testthat::expect_equal(
      exported_values[["dsrelday_start-picks_resolved"]]$variables$selected,
      "ASTDY"
    )
    testthat::expect_equal(
      exported_values[["dsrelday_end-picks_resolved"]]$variables$selected,
      "AENDY"
    )
    testthat::expect_true(app_driver$get_active_module_input("relday_x_axis"))
    testthat::expect_equal(
      app_driver$get_active_module_input("font_size"),
      12
    )
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Encodings aerelday_start, aerelday_end, dsrelday_start, dsrelday_end
  are shown only when relday_x_axis is checked.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())

    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("aerelday_start-variables-selected"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("aerelday_end-variables-selected"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("dsrelday_start-variables-selected"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("dsrelday_end-variables-selected"))

    app_driver$set_active_module_input("relday_x_axis", FALSE)

    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("aerelday_start-variables-selected"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("aerelday_end-variables-selected"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("dsrelday_start-variables-selected"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("dsrelday_end-variables-selected"))
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Encodings aetime_start, aetime_end, dstime_start, dstime_end
  are shown only when relday_x_axis is unchecked.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())

    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("aetime_start-variables-selected"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("aetime_end-variables-selected"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("dstime_start-variables-selected"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("dstime_end-variables-selected"))

    app_driver$set_active_module_input("relday_x_axis", FALSE)

    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("aetime_start-variables-selected"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("aetime_end-variables-selected"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("dstime_start-variables-selected"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("dstime_end-variables-selected"))
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting patient_id changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("patient_id", "AB12345-USA-2-id-3")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_g_pp_patient_timeline: Deselecting patient_id column throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_patient_timeline()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("patient_id", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("patient_timeline_plot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("patient_timeline_plot-plot_main")),
    "Please select a patient.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting cmdecod changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "cmdecod", "variables", "CMCAT")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting cmdecod changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "cmdecod", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aeterm changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aeterm", "variables", "AESOC")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aeterm changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aeterm", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aetime_start changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aetime_start", "variables", "TRTSDTM")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aetime_start changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aetime_start", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aetime_end changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aetime_end", "variables", "EOSDT")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aetime_end changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aetime_end", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aerelday_start changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aerelday_start", "variables", "AENDY")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aerelday_start changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aerelday_start", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aerelday_end changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aerelday_end", "variables", "ASTDY")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aerelday_end changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "aerelday_end", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting dstime_start changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "dstime_start", "variables", "TRTEDTM")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting dstime_start changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "dstime_start", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting dstime_end changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "dstime_end", "variables", "TRTEDTM")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting dstime_end changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "dstime_end", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting dsrelday_start changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "dsrelday_start", "variables", "AENDY")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting dsrelday_start changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "dsrelday_start", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting dsrelday_end changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "dsrelday_end", "variables", "ASTDY")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting dsrelday_end changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    set_teal_picks_slot(app_driver, "dsrelday_end", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
  }
)
