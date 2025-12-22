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
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], c("PARAMCD", "STUDYID")),
          selected = "PARAMCD"
        ),
        param = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], c("PARAM", "SEX")),
          selected = "PARAM"
        ),
        timepoints = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], c("ADY", "AGE")),
          selected = "ADY"
        ),
        anrind = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], c("ANRIND", "AGEU")),
          selected = "ANRIND"
        ),
        aval_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], c("AVAL", "AGE")),
          selected = "AVAL"
        ),
        avalu_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADLB"]], c("AVALU", "SEX")),
          selected = "AVALU"
        ),
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
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Starts with specified label, patient_id, paramcd, param,
  timepoints, aval_var, avalu_var, anrind, round_value.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Vitals"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("patient_id"),
      "AB12345-CHN-3-id-128"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADLB_singleextract-select"),
      "PARAMCD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("param-dataset_ADLB_singleextract-select"),
      "PARAM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("timepoints-dataset_ADLB_singleextract-select"),
      "ADY"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADLB_singleextract-select"),
      "AVAL"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("avalu_var-dataset_ADLB_singleextract-select"),
      "AVALU"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("anrind-dataset_ADLB_singleextract-select"),
      "ANRIND"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("round_value"),
      "4"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting patient_id changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    app_driver$set_active_module_input("patient_id", "AB12345-USA-1-id-261")
    app_driver$wait_for_idle()
    testthat::expect_snapshot_failure(
      testthat::expect_identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of patient_id throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
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
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting paramcd changes the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    app_driver$wait_for_idle()
    app_driver$get_screenshot("~/remove_me_before_debugging.png")
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-select", "STUDYID")
    app_driver$wait_for_idle()
    app_driver$get_screenshot("~/remove_me_after_debugging.png")
    testthat::expect_snapshot_failure(
      testthat::expect_identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  app_driver$set_active_module_input("paramcd-dataset_ADLB_singleextract-select", NULL)
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("paramcd-dataset_ADLB_singleextract-select_input .shiny-validation-message")
    ),
    "Please select PARAMCD variable."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting param changes the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    app_driver$set_active_module_input("param-dataset_ADLB_singleextract-select", "SEX")
    app_driver$wait_for_idle()
    testthat::expect_snapshot_failure(
      testthat::expect_identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Deselection of param throws validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    app_driver$set_active_module_input("param-dataset_ADLB_singleextract-select", NULL)
    app_driver$expect_hidden(
      app_driver$namespaces(TRUE)$module("lab_values_table"),
      visibility_property = TRUE
    )
    app_driver$expect_validation_error()
    testthat::expect_equal(
      app_driver$get_text(
        app_driver$namespaces(TRUE)$module("param-dataset_ADLB_singleextract-select_input .shiny-validation-message")
      ),
      "Please select PARAM variable."
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting timepoints changes the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    app_driver$set_active_module_input("timepoints-dataset_ADLB_singleextract-select", "AGE")
    app_driver$wait_for_idle()
    testthat::expect_snapshot_failure(
      testthat::expect_identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of timepoints throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  app_driver$set_active_module_input("timepoints-dataset_ADLB_singleextract-select", NULL)
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "timepoints-dataset_ADLB_singleextract-select_input .shiny-validation-message"
    )),
    "Please select timepoints variable."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting avalu changes the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    app_driver$set_active_module_input("avalu_var-dataset_ADLB_singleextract-select", "SEX")
    app_driver$wait_for_idle()
    testthat::expect_snapshot_failure(
      testthat::expect_identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of avalu throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  app_driver$set_active_module_input("avalu_var-dataset_ADLB_singleextract-select", NULL)
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "avalu_var-dataset_ADLB_singleextract-select_input .shiny-validation-message"
    )),
    "Please select AVALU variable."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting aval_var changes the table
  and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    app_driver$set_active_module_input("aval_var-dataset_ADLB_singleextract-select", "AGE")
    app_driver$wait_for_idle()
    testthat::expect_snapshot_failure(
      testthat::expect_identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of aval_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  app_driver$set_active_module_input("aval_var-dataset_ADLB_singleextract-select", NULL)
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("aval_var-dataset_ADLB_singleextract-select_input .shiny-validation-message")
    ),
    "Please select AVAL variable."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_pp_laboratory: Selecting arind changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_pp_laboratory()
    app_driver$wait_for_idle()
    table_before <- app_driver$get_active_module_table_output("lab_values_table", which = 2)
    app_driver$set_active_module_input("anrind-dataset_ADLB_singleextract-select", "AGEU")
    app_driver$wait_for_idle()
    testthat::expect_snapshot_failure(
      testthat::expect_identical(
        table_before,
        app_driver$get_active_module_table_output("lab_values_table", which = 2)
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_pp_laboratory: Deselection of arind throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_pp_laboratory()
  app_driver$set_active_module_input("anrind-dataset_ADLB_singleextract-select", NULL)
  app_driver$expect_hidden(
    app_driver$namespaces(TRUE)$module("lab_values_table"),
    visibility_property = TRUE
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("anrind-dataset_ADLB_singleextract-select_input .shiny-validation-message")
    ),
    "Please select ANRIND variable."
  )
  app_driver$stop()
})
