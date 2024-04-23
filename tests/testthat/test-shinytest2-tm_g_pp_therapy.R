# Error
app_driver_tm_g_pp_therapy <- function() {
  data <- teal.data::teal_data() %>%
    within({
      library(dplyr)
      ADCM <- tmc_ex_adcm
      ADSL <- tmc_ex_adsl %>%
        filter(USUBJID %in% ADCM$USUBJID)
      ADCM$CMASTDTM <- ADCM$ASTDTM
      ADCM$CMAENDTM <- ADCM$AENDTM
      adcm_keys <- c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "ATC1", "ATC2", "ATC3", "ATC4")
    })

  datanames <- c("ADSL", "ADCM")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = teal::modules(
      tm_g_pp_therapy(
        label = "Therapy",
        dataname = "ADCM",
        parentname = "ADSL",
        patient_col = "USUBJID",
        plot_height = c(600L, 200L, 2000L),
        atirel = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "ATIREL"),
          selected = c("ATIREL")
        ),
        cmdecod = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "CMDECOD"),
          selected = "CMDECOD"
        ),
        cmindc = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "CMINDC"),
          selected = "CMINDC"
        ),
        cmdose = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "CMDOSE"),
          selected = "CMDOSE"
        ),
        cmtrt = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "CMTRT"),
          selected = "CMTRT"
        ),
        cmdosu = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "CMDOSU"),
          selected = c("CMDOSU")
        ),
        cmroute = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "CMROUTE"),
          selected = "CMROUTE"
        ),
        cmdosfrq = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "CMDOSFRQ"),
          selected = "CMDOSFRQ"
        ),
        cmstdy = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "ASTDY"),
          selected = "ASTDY"
        ),
        cmendy = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADCM"]], "AENDY"),
          selected = "AENDY"
        )
      )
    )
  )
}

testthat::test_that("e2e - tm_g_pp_therapy: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_pp_therapy()
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
})
