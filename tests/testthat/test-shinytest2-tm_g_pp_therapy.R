app_driver_tm_g_pp_therapy <- function() {
  data <- within(teal.data::teal_data(), {
    ADCM <- teal.modules.clinical::tmc_ex_adcm
    ADSL <- dplyr::filter(
      teal.modules.clinical::tmc_ex_adsl,
      USUBJID %in% ADCM$USUBJID
    )
    ADCM$CMASTDTM <- ADCM$ASTDTM
    ADCM$CMAENDTM <- ADCM$AENDTM

    # Manual duplicate choices to test changes in the encodings
    set.seed(123)
    ADCM$ATIREL2 <- gsub("PRIOR", "PRIOR_CONCOMITANT", ADCM$ATIREL)
    ADCM$CMDECOD2 <- sample(ADCM$CMDECOD, size = length(ADCM$CMDECOD))
    ADCM$CMINDC2 <- sample(ADCM$CMINDC, size = length(ADCM$CMINDC))
    ADCM$CMDOSE2 <- sample(ADCM$CMDOSE, size = length(ADCM$CMDOSE))
    ADCM$CMDOSU2 <- sample(ADCM$CMDOSU, size = length(ADCM$CMDOSU))
    ADCM$CMROUTE2 <- sample(ADCM$CMROUTE, size = length(ADCM$CMROUTE))
    ADCM$CMDOSFRQ2 <- sample(ADCM$CMDOSFRQ, size = length(ADCM$CMDOSFRQ))
    ADCM$ASTDY2 <- sample(ADCM$ASTDY, size = length(ADCM$ASTDY))
    ADCM$AENDY2 <- sample(ADCM$AENDY, size = length(ADCM$AENDY))
    ADCM$CMTRT2 <- sample(ADCM$CMTRT, size = length(ADCM$CMTRT))
  })

  adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")

  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]
  teal.data::join_keys(data)["ADCM", "ADCM"] <- adcm_keys

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_pp_therapy(
        label = "Therapy (e-2-e)",
        dataname = "ADCM",
        parentname = "ADSL",
        patient_col = "USUBJID",
        atirel = teal.picks::variables(c("ATIREL", "ATIREL2"), selected = "ATIREL2", multiple = FALSE),
        cmdecod = teal.picks::variables(c("CMDECOD", "CMDECOD2"), selected = "CMDECOD2", multiple = FALSE),
        cmindc = teal.picks::variables(c("CMINDC", "CMINDC2"), selected = "CMINDC2", multiple = FALSE),
        cmdose = teal.picks::variables(c("CMDOSE", "CMDOSE2"), selected = "CMDOSE2", multiple = FALSE),
        cmtrt = teal.picks::variables(c("CMTRT", "CMTRT2"), selected = "CMTRT", multiple = FALSE),
        cmdosu = teal.picks::variables(c("CMDOSU", "CMDOSU2"), selected = "CMDOSU2", multiple = FALSE),
        cmroute = teal.picks::variables(c("CMROUTE", "CMROUTE2"), selected = "CMROUTE2", multiple = FALSE),
        cmdosfrq = teal.picks::variables(c("CMDOSFRQ", "CMDOSFRQ2"), selected = "CMDOSFRQ2", multiple = FALSE),
        cmstdy = teal.picks::variables(c("ASTDY", "ASTDY2"), selected = "ASTDY2", multiple = FALSE),
        cmendy = teal.picks::variables(c("AENDY", "AENDY2"), selected = "AENDY2", multiple = FALSE),
        font_size = c(12L, 1L, 30L),
        plot_height = c(500L, 300L, 2000L),
        plot_width = c(1000L, 700L, 2000L),
        pre_output = NULL,
        post_output = NULL,
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    )
  )
}

# Initialization --------------------------------------------------------------

testthat::test_that("e2e - tm_g_pp_therapy: Module initializes in teal without errors and produces output.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main"))
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("therapy_table"))
})

testthat::test_that(
  "e2e - tm_g_pp_therapy: Starts with specified label, patient_id, atirel, cmdecod, cmindc,
  cmdose, cmtrt, cmdosu, cmroute, cmdosfrq, cmstdy, cmendy and plot settings.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      trimws(app_driver$get_text("a.nav-link.active")),
      "Therapy (e-2-e)"
    )

    testthat::expect_equal(app_driver$get_active_module_input("patient_id"), "AB12345-CHN-11-id-2")

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(exported_values[["atirel-picks_resolved"]]$variables$selected, "ATIREL2")
    testthat::expect_equal(exported_values[["cmdecod-picks_resolved"]]$variables$selected, "CMDECOD2")
    testthat::expect_equal(exported_values[["cmindc-picks_resolved"]]$variables$selected, "CMINDC2")
    testthat::expect_equal(exported_values[["cmdose-picks_resolved"]]$variables$selected, "CMDOSE2")
    testthat::expect_equal(exported_values[["cmtrt-picks_resolved"]]$variables$selected, "CMTRT")
    testthat::expect_equal(exported_values[["cmdosu-picks_resolved"]]$variables$selected, "CMDOSU2")
    testthat::expect_equal(exported_values[["cmroute-picks_resolved"]]$variables$selected, "CMROUTE2")
    testthat::expect_equal(exported_values[["cmdosfrq-picks_resolved"]]$variables$selected, "CMDOSFRQ2")
    testthat::expect_equal(exported_values[["cmstdy-picks_resolved"]]$variables$selected, "ASTDY2")
    testthat::expect_equal(exported_values[["cmendy-picks_resolved"]]$variables$selected, "AENDY2")

    # Plot settings -----------------------------------------------------------
    testthat::expect_equal(app_driver$get_active_module_input("font_size"), 12)
  }
)

# Test changing selection ------------------------------------

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of patient_id changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    app_driver$set_active_module_input("patient_id", "AB12345-RUS-1-id-4")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of cmdecod changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    set_teal_picks_slot(app_driver, "cmdecod", "variables", "CMDECOD")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of atirel changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    set_teal_picks_slot(app_driver, "atirel", "variables", "ATIREL")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of cmindc changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    set_teal_picks_slot(app_driver, "cmindc", "variables", "CMINDC")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of cmdose changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    set_teal_picks_slot(app_driver, "cmdose", "variables", "CMDOSE")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of cmdosu changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    set_teal_picks_slot(app_driver, "cmdosu", "variables", "CMDOSU")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of cmroute changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    set_teal_picks_slot(app_driver, "cmroute", "variables", "CMROUTE")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of cmdosfrq changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    set_teal_picks_slot(app_driver, "cmdosfrq", "variables", "CMDOSFRQ")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of cmstdy changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    set_teal_picks_slot(app_driver, "cmstdy", "variables", "ASTDY")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Selection of cmendy changes the plot and table without any validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- list(
      app_driver$get_active_module_plot_output("therapy_plot"),
      app_driver$get_active_module_table_output("therapy_table")
    )
    set_teal_picks_slot(app_driver, "cmendy", "variables", "AENDY")
    testthat::expect_false(
      identical(
        plot_before,
        list(
          app_driver$get_active_module_plot_output("therapy_plot"),
          app_driver$get_active_module_table_output("therapy_table")
        )
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_therapy: Changing font_size changes the plot and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("therapy_plot")
    app_driver$set_active_module_input("font_size", 15)
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("therapy_plot")))
    app_driver$expect_no_validation_error()
  }
)

# Test de-selecting inputs ------------------------------------

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of patient_id throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("patient_id", NULL)
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select a patient.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of cmdecod throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmdecod", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select medication decoding variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of atirel throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "atirel", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select ATIREL variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of cmindc throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmindc", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select CMINDC variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of cmdose throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmdose", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select CMDOSE variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of cmdosu throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmdosu", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select CMDOSU variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of cmroute throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmroute", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select CMROUTE variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of cmdosfrq throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmdosfrq", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select CMDOSFRQ variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of cmstdy throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmstdy", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select CMSTDY variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_pp_therapy: Deselection of cmendy throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cmendy", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("therapy_plot-plot_main")),
    "Please select CMENDY variable.",
    fixed = TRUE
  )
})
