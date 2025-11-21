app_driver_tm_a_gee <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- tmc_ex_adsl
    ADQS <- tmc_ex_adqs %>%
      filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
      mutate(
        AVISIT = as.factor(AVISIT),
        AVISITN = rank(AVISITN) %>%
          as.factor() %>%
          as.numeric() %>%
          as.factor(),
        AVALBIN = AVAL < 50 # Just as an example to get a binary endpoint.
      ) %>%
      droplevels()
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_a_gee(
        label = "GEE",
        dataname = "ADQS",
        parentname = "ADSL",
        aval_var = teal.transform::choices_selected("AVALBIN", fixed = TRUE),
        id_var = teal.transform::choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
        arm_var = teal.transform::choices_selected(c("ARM", "ARMCD"), "ARM"),
        visit_var = teal.transform::choices_selected(c("AVISIT", "AVISITN"), "AVISIT"),
        paramcd = teal.transform::choices_selected(
          choices = teal.transform::value_choices(data[["ADQS"]], "PARAMCD", "PARAM"),
          selected = "FKSI-FWB"
        ),
        cov_var = teal.transform::choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL),
        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8, -1), 0.95, keep_order = TRUE),
        arm_ref_comp = NULL,
        pre_output = NULL,
        post_output = NULL,
        basic_table_args = teal.widgets::basic_table_args()
      )
    )
  )
}

testthat::test_that("e2e - tm_a_gee: Module initializes in teal without errors and produces table output.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_gee()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(app_driver$is_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings")))
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_a_gee: Starts with specified label, id_var, arm_var, visit_var, paramcd, cov_var,
  conf_level and conf_struct.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_gee()

    testthat::expect_equal(
      app_driver$get_text("#teal-teal_modules-active_tab .active > a"),
      "GEE"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_des_input("aval_var", "ADQS", "select")),
      "AVALBIN"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_des_input("id_var", "ADQS", "select")),
      "USUBJID"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_des_input("arm_var", "ADSL", "select")),
      "ARM"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_des_input("visit_var", "ADQS", "select")),
      "AVISIT"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input(ns_des_input("paramcd", "ADQS", "filter1-vals")),
      "FKSI-FWB"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("cov_var-dataset_ADQS_singleextract-select"),
      NULL
    )

    testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")

    testthat::expect_equal(app_driver$get_active_module_input("cor_struct"), "unstructured")

    radio_buttons <- app_driver$get_text(app_driver$namespaces(TRUE)$module("output_table"))
    testthat::expect_match(
      radio_buttons,
      "Output Type.*LS means.*Covariance.*Coefficients",
      fixed = FALSE
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_a_gee: Selection of id_var does not change the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_gee()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input(ns_des_input("id_var", "ADQS", "select"), "SUBJID")
    testthat::expect_true(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_a_gee: Deselection of id_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  app_driver$set_active_module_input(ns_des_input("id_var", "ADQS", "select"), character(0))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("id_var-dataset_ADQS_singleextract-select_input > div > span")),
    "A Subject identifier is required"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Change in arm_var changes the table and does not throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()

  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  app_driver$set_active_module_input(ns_des_input("arm_var", "ADSL", "select"), "ARMCD")
  testthat::expect_false(
    identical(
      table_before,
      app_driver$get_active_module_table_output("table-table-with-settings")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  app_driver$set_active_module_input(ns_des_input("arm_var", "ADSL", "select"), character(0))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("arm_var-dataset_ADSL_singleextract-select_input > div > span")),
    "A treatment variable is required"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_a_gee: Selection of visit_var does not change the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_gee()
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    app_driver$set_active_module_input(ns_des_input("visit_var", "ADQS", "select"), "AVISITN")
    testthat::expect_true(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_a_gee: Deselection of visit_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  app_driver$set_active_module_input(ns_des_input("visit_var", "ADQS", "select"), character(0))
  app_driver$wait_for_idle()
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("visit_var-dataset_ADQS_singleextract-select_input > div > span")),
    "A visit variable is required"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Selection of paramcd changes the table and does not throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  app_driver$set_active_module_input(ns_des_input("paramcd", "ADQS", "filter1-vals"), "BFIALL")
  testthat::expect_false(
    identical(
      table_before,
      app_driver$get_active_module_table_output("table-table-with-settings")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  app_driver$set_active_module_input(ns_des_input("paramcd", "ADQS", "filter1-vals"), character(0))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("paramcd-dataset_ADQS_singleextract-filter1-vals_input > div > span")),
    "An endpoint is required"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Selection of cov_var changes the table and does not throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
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
})

testthat::test_that("e2e - tm_a_gee: Selection of conf_level changes the table and does not throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  app_driver$set_active_module_input("conf_level", 0.90)
  testthat::expect_false(
    identical(
      table_before,
      app_driver$get_active_module_table_output("table-table-with-settings")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Selection of conf_level out of [0,1] range throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  app_driver$set_active_module_input("conf_level", -1)
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("conf_level_input > div > span")),
    "Confidence level must be between 0 and 1"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Deselection of conf_level throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  app_driver$set_active_module_input("conf_level", character(0))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("conf_level_input > div > span")),
    "Please choose a confidence level"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Selection of cor_struct changes the table and does not throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  app_driver$set_active_module_input("cor_struct", "auto-regressive")
  testthat::expect_false(
    identical(
      table_before,
      app_driver$get_active_module_table_output("table-table-with-settings")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Deselection of cor_struct does not throw validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  app_driver$set_active_module_input("cor_struct", character(0), wait_ = FALSE) # not waiting because of a warning
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Selection of output_table changes the table and doesn't throw validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  app_driver$set_active_module_input("output_table", "t_gee_cov")
  testthat::expect_false(
    identical(
      table_before,
      app_driver$get_active_module_table_output("table-table-with-settings")
    )
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})
