app_driver_tm_t_smq <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADAE <- teal.data::rADAE
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_smq(
        label = "Adverse Events by SMQ Table",
        dataname = "ADAE",
        parentname = "ADSL",
        arm_var = variables(choices = c("ARM", "SEX"), selected = "ARM"),
        id_var = variables(choices = "USUBJID"),
        add_total = FALSE,
        total_label = default_total_label(),
        sort_criteria = c("freq_desc", "alpha"),
        drop_arm_levels = TRUE,
        na_level = default_na_str(),
        smq_varlabel = "Standardized MedDRA Query",
        baskets = variables(choices = starts_with("SMQ") | starts_with("CQ")),
        scopes = variables(choices = ends_with("SC")),
        llt = variables(choices = c("AEDECOD", "SEX"), selected = "AEDECOD"),
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
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "Adverse Events by SMQ Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-variables-selected"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("llt-variables-selected"),
      "AEDECOD"
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
    app_driver$set_active_module_input("arm_var-variables-selected", "SEX")
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
  app_driver$set_active_module_input("arm_var-variables-selected", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_smq: Selecting llt changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_smq()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("llt-variables-selected", "SEX")
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

testthat::test_that("e2e - tm_t_smq: Deselection of llt throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_smq()
  app_driver$set_active_module_input("llt-variables-selected", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_smq: Selecting baskets changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_smq()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("baskets-variables-selected", "CQ01NAM")
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

testthat::test_that("e2e - tm_t_smq: Deselection of baskets throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_smq()
  app_driver$set_active_module_input("baskets-variables-selected", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  app_driver$stop()
})
