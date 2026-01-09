skip("CI test")
app_driver_tm_t_coxreg <- function() {
  # TODO: Check if data fabrication is needed for Cox regression
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADTTE <- teal.data::rADTTE
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
      modules = tm_t_coxreg(
        label = "Cox Reg.",
        dataname = "ADTTE",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(c("ARM", "ARMCD", "ACTARMCD"), "ARM"),
        arm_ref_comp = arm_ref_comp,
        paramcd = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"), "OS"
        ),
        strata_var = teal.transform::choices_selected(
          c("COUNTRY", "STRATA1", "STRATA2"), "STRATA1"
        ),
        cov_var = teal.transform::choices_selected(
          c("AGE", "BMRKR1", "BMRKR2", "REGION1"), "AGE"
        ),
        multivariate = TRUE,
        aval_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADTTE"]], "AVAL"), "AVAL",
          fixed = TRUE
        ),
        cnsr_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADTTE"]], "CNSR"), "CNSR",
          fixed = TRUE
        ),
        na_level = default_na_str(),
        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95,
          keep_order =
            TRUE
        ),
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_coxreg: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_coxreg()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_coxreg: Starts with specified label, type, paramcd, arm_var, buckets,
  cov_var, strata_var, pval_method, ties, conf_level, combine_comp_arms.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_coxreg()
    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Cox Reg."
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("type"),
      "Multivariate"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADTTE_singleextract-filter1-vals"),
      "OS"
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
      app_driver$get_active_module_input("cov_var-dataset_ADSL_singleextract-select"),
      "AGE"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("strata_var-dataset_ADSL_singleextract-select"),
      "STRATA1"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("pval_method"),
      "wald"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("ties"),
      "exact"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_coxreg: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_coxreg()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADTTE_singleextract-filter1-vals", "CRSD")
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

testthat::test_that("e2e - tm_t_coxreg: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_coxreg()
  app_driver$set_active_module_input("paramcd-dataset_ADTTE_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADTTE_singleextract-filter1-vals_input .shiny-validation-message"
    )),
    "An endpoint is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_coxreg: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_coxreg()
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

testthat::test_that("e2e - tm_t_coxreg: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_coxreg()
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
  "e2e - tm_t_coxreg: Selecting cov_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_coxreg()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("cov_var-dataset_ADSL_singleextract-select", c("BMRKR1", "BMRKR2"))
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

testthat::test_that(
  "e2e - tm_t_coxreg: Deselection of cov_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_coxreg()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("cov_var-dataset_ADSL_singleextract-select", NULL)
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

testthat::test_that(
  "e2e - tm_t_coxreg: Selecting strata_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_coxreg()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("strata_var-dataset_ADSL_singleextract-select", c("STRATA2", "COUNTRY"))
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

testthat::test_that(
  "e2e - tm_t_coxreg: Deselection of strata_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_coxreg()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("strata_var-dataset_ADSL_singleextract-select", NULL)
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
