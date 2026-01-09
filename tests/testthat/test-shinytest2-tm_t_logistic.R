app_driver_tm_t_logistic <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- teal.data::rADSL
    ADRS <- teal.data::rADRS %>%
      filter(PARAMCD %in% c("BESRSPI", "INVET"))
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  arm_ref_comp <- list(
    ACTARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    ),
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    )
  )

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_logistic(
        label = "Logistic Regression",
        parentname = "ADSL",
        dataname = "ADRS",
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADRS"]], c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        arm_ref_comp = arm_ref_comp,
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
          selected = "BESRSPI"
        ),
        cov_var = teal.transform::choices_selected(
          choices = c("SEX", "AGE", "BMRKR1", "BMRKR2"),
          selected = "SEX"
        ),
        conf_level = teal.transform::choices_selected(c(2, 0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
        avalc_var = teal.transform::choices_selected(teal.transform::variable_choices(
          data[["ADRS"]],
          "AVALC"
        ), "AVALC", fixed = TRUE),
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_logistic: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_logistic()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_logistic: Starts with specified label, paramcd, responders, arm_var, buckets,
  cov_var, interaction_var, conf_level, combine_comp_arms.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_logistic()
    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Logistic Regression"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals"),
      "BESRSPI"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("responders"),
      c("CR", "PR")
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARM"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(
        Ref = list("B: Placebo"),
        Comp = list("A: Drug X", "C: Combination")
      )
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("cov_var-dataset_ADRS_singleextract-select"),
      "SEX"
    )
    testthat::expect_null(app_driver$get_active_module_input("interaction_var"))
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_logistic: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_logistic()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals", "INVET")
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

testthat::test_that("e2e - tm_t_logistic: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_logistic()
  app_driver$set_active_module_input("paramcd-dataset_ADRS_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADRS_singleextract-filter1-vals_input .shiny-validation-message"
    )),
    "`Select Endpoint` field is empty"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_logistic: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_logistic()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", "ARMCD")
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

testthat::test_that("e2e - tm_t_logistic: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_logistic()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message")
    ),
    "Treatment variable must be selected"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_logistic: Selecting cov_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_logistic()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("cov_var-dataset_ADRS_singleextract-select", c("AGE", "BMRKR1"))
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

testthat::test_that("e2e - tm_t_logistic: Deselection of cov_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_logistic()
  app_driver$set_active_module_input("cov_var-dataset_ADRS_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("cov_var-dataset_ADRS_singleextract-select_input .shiny-validation-message")
    ),
    "`Covariates` field is empty"
  )
  app_driver$stop()
})
