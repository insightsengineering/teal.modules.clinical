testthat::skip("Slow CI machine prevents this test from succeding")

# Setup timeout options for shinytest2 if none are set in options nor on environment variables
withr::local_options(
  list(
    shinytest2.timeout = getOption(
      "shinytest2.timeout",
      default = Sys.getenv("SHINYTEST2_TIMEOUT", unset = 30 * 1000)
    ),
    shinytest2.load_timeout = getOption(
      "shinytest2.load_timeout",
      default = Sys.getenv("SHINYTEST2_LOAD_TIMEOUT", unset = 60 * 1000)
    ),
    shinytest2.duration = getOption(
      "shinytest2.duration",
      default = Sys.getenv("SHINYTEST2_DURATION", unset = 1.5 * 1000)
    )
  ),
  .local_envir = testthat::test_env()
)

app_driver_tm_t_pp_laboratory <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADLB <- teal.data::rADLB
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_pp_laboratory(
        label = "Vitals",
        dataname = "ADLB",
        parentname = "ADSL",
        patient_col = "USUBJID",
        paramcd = teal.picks::variables(c("PARAMCD", "STUDYID"), multiple = FALSE),
        param = teal.picks::variables(c("PARAM", "SEX"), multiple = FALSE),
        timepoints = teal.picks::variables(c("ADY", "AGE"), multiple = FALSE),
        anrind = teal.picks::variables(c("ANRIND", "AGEU"), multiple = FALSE),
        aval_var = teal.picks::variables(c("AVAL", "AGE"), multiple = FALSE),
        avalu_var = teal.picks::variables(c("AVALU", "SEX"), multiple = FALSE),
        pre_output = NULL,
        post_output = NULL
      )
    )
  )
}

testthat::test_that("e2e - tm_t_pp_laboratory: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  app_driver$wait_for_idle()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("lab_values_table"))
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Starts with specified label, patient_id, paramcd, param,
  timepoints, aval_var, avalu_var, anrind, round_value.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Vitals"
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
      exported_values[["paramcd-picks_resolved"]]$variables$selected,
      "PARAMCD"
    )
    testthat::expect_equal(
      exported_values[["param-picks_resolved"]]$variables$selected,
      "PARAM"
    )
    testthat::expect_equal(
      exported_values[["timepoints-picks_resolved"]]$variables$selected,
      "ADY"
    )
    testthat::expect_equal(
      exported_values[["aval_var-picks_resolved"]]$variables$selected,
      "AVAL"
    )
    testthat::expect_equal(
      exported_values[["avalu_var-picks_resolved"]]$variables$selected,
      "AVALU"
    )
    testthat::expect_equal(
      exported_values[["anrind-picks_resolved"]]$variables$selected,
      "ANRIND"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("round_value"),
      "4"
    )
  }
)

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting patient_id changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    app_driver$set_active_module_input("patient_id", "AB12345-USA-1-id-261")
    app_driver$wait_for_idle()
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of patient_id throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("patient_id", NULL)
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("patient_id_input .shiny-validation-message")),
    "Please select a patient"
  )
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    set_teal_picks_slot(app_driver, "paramcd", "variables", "STUDYID")
    app_driver$wait_for_idle()
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "paramcd", "variables", character(0L))
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("lab_values_table")),
    "Please select PARAMCD variable.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting param changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    set_teal_picks_slot(app_driver, "param", "variables", "SEX")
    app_driver$wait_for_idle()
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Deselection of param throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    withr::defer(app_driver$stop())
    set_teal_picks_slot(app_driver, "param", "variables", character(0L))
    app_driver$expect_hidden(
      app_driver$namespaces(TRUE)$module("lab_values_table"),
      visibility_property = TRUE
    )
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module("lab_values_table")),
      "Please select PARAM variable.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting timepoints changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    set_teal_picks_slot(app_driver, "timepoints", "variables", "AGE")
    app_driver$wait_for_idle()
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of timepoints throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "timepoints", "variables", character(0L))
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("lab_values_table")),
    "Please select timepoints variable.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting avalu_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    set_teal_picks_slot(app_driver, "avalu_var", "variables", "SEX")
    app_driver$wait_for_idle()
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of avalu_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "avalu_var", "variables", character(0L))
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("lab_values_table")),
    "Please select AVALU variable.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting aval_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    set_teal_picks_slot(app_driver, "aval_var", "variables", "AGE")
    app_driver$wait_for_idle()
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of aval_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "aval_var", "variables", character(0L))
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("lab_values_table")),
    "Please select AVAL variable.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting anrind changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    set_teal_picks_slot(app_driver, "anrind", "variables", "AGEU")
    app_driver$wait_for_idle()
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of anrind throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "anrind", "variables", character(0L))
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("lab_values_table")),
    "Please select ANRIND variable.",
    fixed = TRUE
  )
})
