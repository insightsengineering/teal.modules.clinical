app_driver_tm_t_ancova <- function() {
  data <- within(teal.data::teal_data(), {
    ADSL <- tmc_ex_adsl
    ADQS <- tmc_ex_adqs
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  arm_ref_comp <- list(
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    ),
    ACTARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    )
  )

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_ancova(
        label = "ANCOVA Table",
        dataname = "ADQS",
        parentname = "ADSL",
        avisit = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADQS"]], "AVISIT"),
          selected = "WEEK 1 DAY 8"
        ),
        arm_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ACTARMCD", "ARMCD")),
          selected = "ARMCD"
        ),
        arm_ref_comp = arm_ref_comp,
        aval_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADQS"]], c("CHG", "AVAL")),
          selected = "CHG"
        ),
        cov_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADQS"]], c("BASE", "STRATA1", "SEX")),
          selected = "STRATA1"
        ),
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
          selected = "FKSI-FWB"
        ),
        interact_var = teal.transform::choices_selected(
          choices = teal.transform::variable_choices(data[["ADQS"]], c("BASE", "STRATA1", "SEX")),
          selected = "STRATA1"
        ),
        conf_level = teal.transform::choices_selected(c(2, 0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
        include_interact = FALSE,
        interact_y = FALSE,
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_t_ancova: Module initializes in teal without errors and produces table output.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_ancova: Starts with specified label, avisit, paramcd, aval_var, aval_var,
  arm_var, buckets, combine_comp_arms, interact_var, cov_var, conf_level, include_interact.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()

    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "ANCOVA Table"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("avisit-dataset_ADQS_singleextract-filter1-vals"),
      "WEEK 1 DAY 8"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("avisit-dataset_ADQS_singleextract-select"),
      "AVISIT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("paramcd-dataset_ADQS_singleextract-filter1-vals"),
      "FKSI-FWB"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("aval_var-dataset_ADQS_singleextract-select"),
      "CHG"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("arm_var-dataset_ADSL_singleextract-select"),
      "ARMCD"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(
        Ref = list("ARM A"),
        Comp = list("ARM B", "ARM C")
      )
    )
    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
    testthat::expect_equal(
      app_driver$get_active_module_input("interact_var-dataset_ADQS_singleextract-select"),
      "STRATA1"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("cov_var-dataset_ADQS_singleextract-select"),
      "STRATA1"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_false(app_driver$get_active_module_input("include_interact"))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_t_ancova: Selecting avisit changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input(
      "avisit-dataset_ADQS_singleextract-filter1-vals",
      c("WEEK 1 DAY 8", "WEEK 2 DAY 15")
    )
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

testthat::test_that("e2e - tm_t_ancova: Deselection of avisit throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  app_driver$set_active_module_input("avisit-dataset_ADQS_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module(
      "avisit-dataset_ADQS_singleextract-filter1-vals_input .shiny-validation-message"
    ),
    "`Analysis Visit` field cannot be empty."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_ancova: Selecting paramcd changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("paramcd-dataset_ADQS_singleextract-filter1-vals", c("BFIALL", "FATIGI"))
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

testthat::test_that("e2e - tm_t_ancova: Deselection of paramcd throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  app_driver$set_active_module_input("paramcd-dataset_ADQS_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module(
      "paramcd-dataset_ADQS_singleextract-filter1-vals_input .shiny-validation-message"
    ),
    "`Select Endpoint` is not selected."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_ancova: Selecting aval_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("aval_var-dataset_ADQS_singleextract-select", "AVAL")
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

testthat::test_that("e2e - tm_t_ancova: Deselection of aval_var throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  app_driver$set_active_module_input("aval_var-dataset_ADQS_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module("aval_var-dataset_ADQS_singleextract-select_input .shiny-validation-message"),
    "Analysis variable cannot be empty."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_ancova: Selecting arm_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", "ARM")
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

testthat::test_that("e2e - tm_t_ancova: Deselection of arm_var throws validation error.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  app_driver$set_active_module_input("arm_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input .shiny-validation-message"),
    "Treatment variable must be selected"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_t_ancova: Selecting cov_var changes the table and does not throw validation errors.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("cov_var-dataset_ADQS_singleextract-select", "BASE")
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
  "e2e - tm_t_ancova: Deselection of cov_var changes table and doesn't throw validation error.",
  {
    testthat::skip("chromium")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input("cov_var-dataset_ADQS_singleextract-select", NULL)
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
