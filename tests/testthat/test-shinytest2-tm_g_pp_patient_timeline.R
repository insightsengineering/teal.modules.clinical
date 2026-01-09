skip("CI test")
app_driver_tm_g_pp_patient_timeline <- function() { # nolint object_length.
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADAE <- tmc_ex_adae
    ADSL <- tmc_ex_adsl %>% filter(USUBJID %in% ADAE$USUBJID)
    ADCM <- tmc_ex_adcm %>% mutate(
      CMSTDY = case_when(
        CMCAT == "medcl B" ~ 20,
        CMCAT == "medcl C" ~ 150,
        TRUE ~ 1
      ) %>% with_label("Study Day of Start of Medication"),
      CMENDY = case_when(
        CMCAT == "medcl B" ~ 700,
        CMCAT == "medcl C" ~ 1000,
        TRUE ~ 500
      ) %>% with_label("Study Day of End of Medication"),
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
        cmdecod = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("CMDECOD", "CMCAT")),
          selected = "CMDECOD"
        ),
        aeterm = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], c("AETERM", "AESOC")),
          selected = "AETERM"
        ),
        aetime_start = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], c("ASTDTM", "TRTSDTM")),
          selected = "ASTDTM"
        ),
        aetime_end = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], c("AENDTM", "EOSDT")),
          selected = "AENDTM"
        ),
        dstime_start = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("CMASTDTM", "TRTEDTM")),
          selected = "CMASTDTM"
        ),
        dstime_end = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("CMAENDTM", "TRTEDTM")),
          selected = "CMAENDTM"
        ),
        aerelday_start = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], c("ASTDY", "AENDY")),
          selected = "ASTDY"
        ),
        aerelday_end = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], c("AENDY", "ASTDY")),
          selected = "AENDY"
        ),
        dsrelday_start = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("ASTDY", "AENDY")),
          selected = "ASTDY"
        ),
        dsrelday_end = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], c("AENDY", "ASTDY")),
          selected = "AENDY"
        ),
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
    app_driver$expect_no_shiny_error()
    app_driver$expect_no_validation_error()

    testthat::expect_match(
      app_driver$get_active_module_plot_output("patient_timeline_plot"),
      "data:image/png;base64,"
    )

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Starts with specified label, patient_id, cmdecod, aeterm, aetime_start,
  aetime_end, dstime_start, dstime_end, aerelday_start, aerelday_end, dsrelday_start, dsrelday_en.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Patient Timeline"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-11-id-2"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("cmdecod-dataset_ADCM_singleextract-select"),
      "CMDECOD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aeterm-dataset_ADAE_singleextract-select"),
      "AETERM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aetime_start-dataset_ADAE_singleextract-select"),
      "ASTDTM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aetime_end-dataset_ADAE_singleextract-select"),
      "AENDTM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("dstime_start-dataset_ADCM_singleextract-select"),
      "CMASTDTM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("dstime_end-dataset_ADCM_singleextract-select"),
      "CMAENDTM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aerelday_start-dataset_ADAE_singleextract-select"),
      "ASTDY"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aerelday_end-dataset_ADAE_singleextract-select"),
      "AENDY"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("dsrelday_start-dataset_ADCM_singleextract-select"),
      "ASTDY"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("dsrelday_end-dataset_ADCM_singleextract-select"),
      "AENDY"
    )
    testthat::expect_true(app_driver$get_active_module_input("relday_x_axis"))
    testthat::expect_equal(
      app_driver$get_active_module_input("font_size"),
      12
    )

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Encodings aerelday_start, aerelday_end, dsrelday_start, dsrelday_end
  are shown only when relday_x_axis is checked.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("aerelday_start-dataset_ADAE_singleextract-select"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("aerelday_end-dataset_ADAE_singleextract-select"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("dsrelday_start-dataset_ADCM_singleextract-select"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("dsrelday_end-dataset_ADCM_singleextract-select"))

    app_driver$set_active_module_input("relday_x_axis", FALSE)

    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("aerelday_start-dataset_ADAE_singleextract-select"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("aerelday_end-dataset_ADAE_singleextract-select"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("dsrelday_start-dataset_ADCM_singleextract-select"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("dsrelday_end-dataset_ADCM_singleextract-select"))

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Encodings aetime_start, aetime_end, dstime_start, dstime_end
  are shown only when relday_x_axis is unchecked.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()

    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("aetime_start-dataset_ADAE_singleextract-select"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("aetime_end-dataset_ADAE_singleextract-select"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("dstime_start-dataset_ADCM_singleextract-select"))
    app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("dstime_end-dataset_ADCM_singleextract-select"))

    app_driver$set_active_module_input("relday_x_axis", FALSE)

    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("aetime_start-dataset_ADAE_singleextract-select"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("aetime_end-dataset_ADAE_singleextract-select"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("dstime_start-dataset_ADCM_singleextract-select"))
    app_driver$expect_visible(app_driver$namespaces(TRUE)$module("dstime_end-dataset_ADCM_singleextract-select"))

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting patient_id changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("patient_id", "AB12345-USA-2-id-3")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_pp_patient_timeline: Deselecting patient_id column throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_patient_timeline()
  app_driver$set_active_module_input("patient_id", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("patient_timeline_plot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("patient_id_input > div > span")),
    "Please select a patient"
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting cmdecod changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("cmdecod-dataset_ADCM_singleextract-select", "CMCAT")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting cmdecod changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("cmdecod-dataset_ADCM_singleextract-select", NULL)
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aeterm changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("aeterm-dataset_ADAE_singleextract-select", "AESOC")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aeterm changes plot and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("aeterm-dataset_ADAE_singleextract-select", NULL)
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("patient_timeline_plot")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aetime_start changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("aetime_start-dataset_ADAE_singleextract-select", "TRTSDTM")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aetime_start throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    app_driver$set_active_module_input("aetime_start-dataset_ADAE_singleextract-select", NULL)
    testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
    testthat::expect_identical(
      app_driver$get_text(
        app_driver$namespaces(TRUE)$module("aetime_start-dataset_ADAE_singleextract-select_input > div > span")
      ),
      "Please add AE start date."
    )
    app_driver$expect_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aetime_end changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("aetime_end-dataset_ADAE_singleextract-select", "EOSDT")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aetime_end throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    app_driver$set_active_module_input("aetime_end-dataset_ADAE_singleextract-select", NULL)
    testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
    testthat::expect_identical(
      app_driver$get_text(
        app_driver$namespaces(TRUE)$module("aetime_end-dataset_ADAE_singleextract-select_input > div > span")
      ),
      "Please add AE end date."
    )
    app_driver$expect_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aerelday_start changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("aerelday_start-dataset_ADAE_singleextract-select", "AENDY")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aerelday_start throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("aerelday_start-dataset_ADAE_singleextract-select", NULL)
    testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
    testthat::expect_identical(
      app_driver$get_text(
        app_driver$namespaces(TRUE)$module("aerelday_start-dataset_ADAE_singleextract-select_input > div > span")
      ),
      "Please add AE start date."
    )
    app_driver$expect_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting aerelday_end changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("aerelday_end-dataset_ADAE_singleextract-select", "ASTDY")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting aerelday_end throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("aerelday_end-dataset_ADAE_singleextract-select", NULL)
    testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
    testthat::expect_identical(
      app_driver$get_text(
        app_driver$namespaces(TRUE)$module("aerelday_end-dataset_ADAE_singleextract-select_input > div > span")
      ),
      "Please add AE end date."
    )
    app_driver$expect_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting dstime_start changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("dstime_start-dataset_ADCM_singleextract-select", "TRTEDTM")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting dstime_start throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    app_driver$set_active_module_input("dstime_start-dataset_ADCM_singleextract-select", NULL)
    testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
    testthat::expect_identical(
      app_driver$get_text(
        app_driver$namespaces(TRUE)$module("dstime_start-dataset_ADCM_singleextract-select_input > div > span")
      ),
      "Please add Medication start date."
    )
    app_driver$expect_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting dstime_end changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("dstime_end-dataset_ADCM_singleextract-select", "TRTEDTM")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting dstime_end throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("relday_x_axis", FALSE)
    app_driver$set_active_module_input("dstime_end-dataset_ADCM_singleextract-select", NULL)
    testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
    testthat::expect_identical(
      app_driver$get_text(
        app_driver$namespaces(TRUE)$module("dstime_end-dataset_ADCM_singleextract-select_input > div > span")
      ),
      "Please add Medication end date."
    )
    app_driver$expect_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting dsrelday_start changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("dsrelday_start-dataset_ADCM_singleextract-select", "AENDY")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting dsrelday_start throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("dsrelday_start-dataset_ADCM_singleextract-select", NULL)
    testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
    testthat::expect_identical(
      app_driver$get_text(
        app_driver$namespaces(TRUE)$module("dsrelday_start-dataset_ADCM_singleextract-select_input > div > span")
      ),
      "Please add Medication start date."
    )
    app_driver$expect_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Selecting dsrelday_end changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    plot_before <- app_driver$get_active_module_plot_output("patient_timeline_plot")
    app_driver$set_active_module_input("dsrelday_end-dataset_ADCM_singleextract-select", "ASTDY")
    testthat::expect_false(
      identical(
        plot_before,
        app_driver$get_active_module_plot_output("patient_timeline_plot")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_pp_patient_timeline: Deselecting dsrelday_end throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_patient_timeline()
    app_driver$set_active_module_input("dsrelday_end-dataset_ADCM_singleextract-select", NULL)
    testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
    testthat::expect_identical(
      app_driver$get_text(
        app_driver$namespaces(TRUE)$module("dsrelday_end-dataset_ADCM_singleextract-select_input > div > span")
      ),
      "Please add Medication end date."
    )
    app_driver$expect_validation_error()
    app_driver$stop()
  }
)
