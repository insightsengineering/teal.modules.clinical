app_driver_tm_t_smq <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADAE <- teal.data::rADAE

    .names_baskets <- grep("^(SMQ|CQ).*NAM$", names(ADAE), value = TRUE)
    .names_scopes <- grep("^SMQ.*SC$", names(ADAE), value = TRUE)

    .cs_baskets <- choices_selected(
      choices = teal.transform::variable_choices(ADAE, subset = .names_baskets),
      selected = .names_baskets
    )

    .cs_scopes <- choices_selected(
      choices = teal.transform::variable_choices(ADAE, subset = .names_scopes),
      selected = .names_scopes,
      fixed = TRUE
    )
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_smq(
        label = "Adverse Events by SMQ Table",
        dataname = "ADAE",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], subset = c("ARM", "SEX")),
          selected = "ARM"
        ),
        id_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], subset = "USUBJID"),
          selected = "USUBJID", fixed = TRUE
        ),
        add_total = FALSE,
        total_label = default_total_label(),
        sort_criteria = c("freq_desc", "alpha"),
        drop_arm_levels = TRUE,
        na_level = default_na_str(),
        smq_varlabel = "Standardized MedDRA Query",
        baskets = data[[".cs_baskets"]],
        scopes = data[[".cs_scopes"]],
        llt = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADAE"]], subset = c("AEDECOD", "SEX")),
          selected = "AEDECOD"
        ),
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_smq: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_smq()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_smq: Starts with specified label, arm_var, llt, baskets, sort_criteria, add_total, drop_arm_levels.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_smq()
    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Adverse Events by SMQ Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("llt-dataset_ADAE_singleextract-select"),
      "AEDECOD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("baskets-dataset_ADAE_singleextract-select"),
      c("SMQ01NAM", "SMQ02NAM", "CQ01NAM")
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("sort_criteria"),
      "freq_desc"
    )
    testthat::expect_false(app_driver$get_active_module_input("add_total"))
    testthat::expect_true(app_driver$get_active_module_input("drop_arm_levels"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_smq: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_smq()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", "SEX")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_smq: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_smq()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module(
        "arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"
      )
    ),
    "At least one treatment variable is required"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_t_smq: Selecting paramcd changes the table and does not throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_smq()
  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  app_driver$set_active_module_input("llt-dataset_ADAE_singleextract-select", "SEX")
  testthat::expect_false(
    identical(
      table_before,
      app_driver$get_active_module_table_output("table-table-with-settings")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_t_smq: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_smq()
  app_driver$set_active_module_input("llt-dataset_ADAE_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module(
        "llt-dataset_ADAE_singleextract-select_input .shiny-validation-message"
      )
    ),
    "A low level term variable is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_smq: Selecting worst_flag changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_smq()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("baskets-dataset_ADAE_singleextract-select", "CQ01NAM")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_t_smq: Deselection of worst_flag throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_smq()
  app_driver$set_active_module_input("baskets-dataset_ADAE_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module(
        "baskets-dataset_ADAE_singleextract-select_input .shiny-validation-message"
      )
    ),
    "At least one basket is required"
  )
  app_driver$stop()
})
