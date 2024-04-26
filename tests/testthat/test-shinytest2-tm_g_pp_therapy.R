app_driver_tm_g_pp_therapy <- function() { # nolint: object_length.
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
  })

  adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
  datanames <- c("ADSL", "ADCM")

  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]
  teal.data::join_keys(data)["ADCM", "ADCM"] <- adcm_keys

  init_teal_app_driver(
    data = data,
    modules = tm_g_pp_therapy(
      label = "Therapy (e2e)",
      dataname = "ADCM",
      parentname = "ADSL",
      patient_col = "USUBJID",
      atirel = choices_selected(
        choices = variable_choices("ADCM", c("ATIREL", "ATIREL2")),
        selected = c("ATIREL2")
      ),
      cmdecod = choices_selected(
        choices = variable_choices("ADCM", "CMDECOD"), # Bug with module doesn't allow to change name
        selected = "CMDECOD"
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
        selected = "CMTRT2"
      ),
      cmdosu = choices_selected(
        choices = variable_choices("ADCM", c("CMDOSU", "CMDOSU2")),
        selected = c("CMDOSU2")
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
}

# Initialization --------------------------------------------------------------

testthat::test_that("e2e - tm_g_pp_therapy: Module initializes in teal without errors and produces output.", {
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
  paste0(
    "e2e - tm_g_pp_therapy: Starts with specified ",
    "label, paramcd, arm_var, buckets, paramcd, subgroup_var, strata_var and plot settings"
  ),
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()

    testthat::expect_equal(
      trimws(app_driver$get_text("#teal-main_ui-root-active_tab > li.active")),
      "Therapy (e2e)"
    )

    testthat::expect_equal(app_driver$get_active_module_input("patient_id"), "AB12345-CHN-11-id-2")

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_des_input("cmdecod", "ADCM", "select")),
      "CMDECOD"
    )

    select_inputs <- c(
      "atirel" = "ATIREL2", "cmdecod" = "CMDECOD", "cmindc" = "CMINDC2",
      "cmdose" = "CMDOSE2", "cmtrt" = "CMTRT2", "cmdosu" = "CMDOSU2",
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

# Test changing selection and de-selecting ------------------------------------

test_that_change_inputs <- function(name, input_id, new_value, deselect_message) {
  if (!missing(new_value)) {
    testthat::test_that(
      sprintf(
        "e2e - tm_g_pp_therapy: Selection of {%s} changes the element and does not throw validation errors",
        name
      ),
      {
        skip_if_too_deep(5)
        app_driver <- app_driver_tm_g_pp_therapy()
        plot_before <- list(
          app_driver$get_active_module_pws_output("therapy_plot"),
          app_driver$active_module_element_text("therapy_table")
        )
        app_driver$set_active_module_input(input_id, new_value)
        testthat::expect_false(
          identical(
            plot_before,
            list(
              app_driver$get_active_module_pws_output("therapy_plot"),
              app_driver$active_module_element_text("therapy_table")
            )
          )
        )
        app_driver$expect_no_validation_error()
        app_driver$stop()
      }
    )
  }

  if (missing(deselect_message)) {
    deselect_message <- sprintf("Please select %s variable.", toupper(name))
  }

  testthat::test_that(sprintf("e2e - tm_g_pp_therapy: Deselection of '%s' throws validation error.", input_id), {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_pp_therapy()
    app_driver$set_active_module_input(input_id, character(0L))
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

test_that_change_inputs("patient_id", "patient_id", "AB12345-RUS-1-id-4", "Please select a patient.")

# Could not change this variable and observe a difference in output
test_that_change_inputs("cmtrt", ns_des_input("cmtrt", "ADCM", "select"))
# Variable selected needs to be CMDECOD as it's hardcoded in module
test_that_change_inputs("cmdecod", ns_des_input("cmdecod", "ADCM", "select"))

test_that_change_inputs("atirel", ns_des_input("atirel", "ADCM", "select"), "ATIREL")
test_that_change_inputs("cmindc", ns_des_input("cmindc", "ADCM", "select"), "CMINDC")
test_that_change_inputs("cmdose", ns_des_input("cmdose", "ADCM", "select"), "CMDOSE")
test_that_change_inputs("cmdosu", ns_des_input("cmdosu", "ADCM", "select"), "CMDOSU")
test_that_change_inputs("cmroute", ns_des_input("cmroute", "ADCM", "select"), "CMROUTE")
test_that_change_inputs("cmdosfrq", ns_des_input("cmdosfrq", "ADCM", "select"), "CMDOSFRQ")
test_that_change_inputs("cmstdy", ns_des_input("cmstdy", "ADCM", "select"), "ASTDY")
test_that_change_inputs("cmendy", ns_des_input("cmendy", "ADCM", "select"), "AENDY")

# Plot settings ---------------------------------------------------------------

test_that_input_changes_pws <- function(input_id, new_value) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_pp_therapy: Changing '%s' changes the plot and does not throw validation errors.",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_pp_therapy()
      plot_before <- app_driver$get_active_module_pws_output("therapy_plot")
      app_driver$set_active_module_input(input_id, new_value)
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("therapy_plot")))
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )
}

test_that_input_changes_pws("font_size", 15)
