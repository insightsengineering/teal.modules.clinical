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
        label = "Therapy (e2e)",
        dataname = "ADCM",
        parentname = "ADSL",
        patient_col = "USUBJID",
        atirel = choices_selected(
          choices = variable_choices("ADCM", c("ATIREL", "ATIREL2")),
          selected = "ATIREL2"
        ),
        cmdecod = choices_selected(
          choices = variable_choices("ADCM", c("CMDECOD", "CMDECOD2")),
          selected = "CMDECOD2"
        ),
        cmindc = choices_selected(
          choices = variable_choices("ADCM", c("CMINDC", "CMINDC2")),
          selected = "CMINDC2"
        ),
        cmdose = choices_selected(
          choices = variable_choices("ADCM", c("CMDOSE", "CMDOSE2")),
          selected = "CMDOSE2"
        ),
        cmtrt = choices_selected(
          choices = variable_choices("ADCM", c("CMTRT", "CMTRT2")),
          selected = "CMTRT"
        ),
        cmdosu = choices_selected(
          choices = variable_choices("ADCM", c("CMDOSU", "CMDOSU2")),
          selected = "CMDOSU2"
        ),
        cmroute = choices_selected(
          choices = variable_choices("ADCM", c("CMROUTE", "CMROUTE2")),
          selected = "CMROUTE2"
        ),
        cmdosfrq = choices_selected(
          choices = variable_choices("ADCM", c("CMDOSFRQ", "CMDOSFRQ2")),
          selected = "CMDOSFRQ2"
        ),
        cmstdy = choices_selected(
          choices = variable_choices("ADCM", c("ASTDY", "ASTDY2")),
          selected = "ASTDY2"
        ),
        cmendy = choices_selected(
          choices = variable_choices("ADCM", c("AENDY", "AENDY2")),
          selected = "AENDY2"
        ),
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
  testthat::skip("chromium")
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_pp_therapy()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("therapy_plot-plot_out_main"))
  )
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("therapy_table"))
  )

  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_pp_therapy: Starts with specified label, paramcd, arm_var, buckets,
  paramcd, subgroup_var, strata_var and plot settings.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()

    testthat::expect_equal(
      trimws(app_driver$get_text("#teal-teal_modules-active_tab .active")),
      "Therapy (e2e)"
    )

    testthat::expect_equal(app_driver$get_active_module_input("patient_id"), "AB12345-CHN-11-id-2")

    select_inputs <- c(
      "atirel" = "ATIREL2", "cmdecod" = "CMDECOD2", "cmindc" = "CMINDC2",
      "cmdose" = "CMDOSE2", "cmtrt" = "CMTRT", "cmdosu" = "CMDOSU2",
      "cmroute" = "CMROUTE2", "cmdosfrq" = "CMDOSFRQ2", "cmstdy" = "ASTDY2",
      "cmendy" = "AENDY2"
    )

    for (el_name in names(select_inputs)) {
      testthat::expect_equal(
        app_driver$get_active_module_input(ns_des_input(el_name, "ADCM", "select")),
        select_inputs[[el_name]]
      )
    }

    # Plot settings -----------------------------------------------------------
    # only tests the options that are customizable
    testthat::expect_equal(app_driver$get_active_module_input("font_size"), 12)

    app_driver$stop()
  }
)

# Test changing selection ------------------------------------

# Check if a new selection of input changes the plot and table without any validation errors.
test_different_selection <- function(input_name, input_id, new_value) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_pp_therapy: Selection of %s changes the plot and table without any validation errors.",
      input_name
    ),
    {
      testthat::skip("chromium")
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_pp_therapy()
      plot_before <- list(
        app_driver$get_active_module_plot_output("therapy_plot"),
        app_driver$active_module_element_text("therapy_table")
      )
      app_driver$set_active_module_input(input_id, new_value)
      testthat::expect_false(
        identical(
          plot_before,
          list(
            app_driver$get_active_module_plot_output("therapy_plot"),
            app_driver$active_module_element_text("therapy_table")
          )
        )
      )
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )
}

test_different_selection("patient_id", "patient_id", "AB12345-RUS-1-id-4")
test_different_selection("cmdecod", ns_des_input("cmdecod", "ADCM", "select"), "CMDECOD")
test_different_selection("atirel", ns_des_input("atirel", "ADCM", "select"), "ATIREL")
test_different_selection("cmindc", ns_des_input("cmindc", "ADCM", "select"), "CMINDC")
test_different_selection("cmdose", ns_des_input("cmdose", "ADCM", "select"), "CMDOSE")
test_different_selection("cmdosu", ns_des_input("cmdosu", "ADCM", "select"), "CMDOSU")
test_different_selection("cmroute", ns_des_input("cmroute", "ADCM", "select"), "CMROUTE")
test_different_selection("cmdosfrq", ns_des_input("cmdosfrq", "ADCM", "select"), "CMDOSFRQ")
test_different_selection("cmstdy", ns_des_input("cmstdy", "ADCM", "select"), "ASTDY")
test_different_selection("cmendy", ns_des_input("cmendy", "ADCM", "select"), "AENDY")

testthat::test_that(
  "e2e - tm_g_pp_therapy: Changing font_size changes the plot and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    plot_before <- app_driver$get_active_module_plot_output("therapy_plot")
    app_driver$set_active_module_input("font_size", 15)
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("therapy_plot")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

# Test de-selecting inputs ------------------------------------

# Check if the delection throws the expected validation error
# When `deselect_message` is not provided, the test will check for a standard message "Please select %s variable."
test_delection_validation <- function(input_name, input_id, deselect_message) {
  if (missing(deselect_message)) {
    deselect_message <- sprintf("Please select %s variable.", toupper(input_name))
  }
  testthat::test_that(sprintf("e2e - tm_g_pp_therapy: Deselection of %s throws validation error.", input_name), {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    app_driver$set_active_module_input(input_id, NULL)
    app_driver$expect_validation_error()
    testthat::expect_equal(
      app_driver$active_module_element_text(
        sprintf(
          "%s_input .shiny-validation-message",
          input_id
        )
      ),
      deselect_message
    )
    app_driver$stop()
  })
}

test_delection_validation("patient_id", "patient_id", "Please select a patient.")
test_delection_validation("cmdecod", ns_des_input("cmdecod", "ADCM", "select"), "Please select medication decoding variable.") # nolint line_length_linter
test_delection_validation("atirel", ns_des_input("atirel", "ADCM", "select"))
test_delection_validation("cmindc", ns_des_input("cmindc", "ADCM", "select"))
test_delection_validation("cmdose", ns_des_input("cmdose", "ADCM", "select"))
test_delection_validation("cmdosu", ns_des_input("cmdosu", "ADCM", "select"))
test_delection_validation("cmroute", ns_des_input("cmroute", "ADCM", "select"))
test_delection_validation("cmdosfrq", ns_des_input("cmdosfrq", "ADCM", "select"))
test_delection_validation("cmstdy", ns_des_input("cmstdy", "ADCM", "select"))
test_delection_validation("cmendy", ns_des_input("cmendy", "ADCM", "select"))
