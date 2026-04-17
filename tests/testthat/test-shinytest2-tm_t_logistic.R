app_driver_tm_t_logistic <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- teal.data::rADSL
    ADRS <- teal.data::rADRS |>
      dplyr::filter(PARAMCD %in% c("BESRSPI", "INVET"))
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
        arm_var = teal.picks::variables(c("ARM", "ARMCD")),
        arm_ref_comp = arm_ref_comp,
        paramcd_var = teal.picks::variables("PARAMCD", "PARAMCD"),
        paramcd_values = suppressWarnings(teal.picks::values(selected = "BESRSPI", multiple = FALSE)),
        cov_var = teal.picks::variables(c("SEX", "AGE", "BMRKR1", "BMRKR2"), selected = "SEX"),
        conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95", keep_order = TRUE),
        avalc_var = teal.picks::variables("AVALC", fixed = TRUE),
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
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
})

testthat::test_that(
  "e2e - tm_t_logistic: Starts with specified label, paramcd, responders, arm_var, buckets,
  cov_var, interaction_var, conf_level, combine_comp_arms.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_logistic()
    withr::defer(app_driver$stop())

    testthat::expect_equal(app_driver$get_text("a.nav-link.active"), "Logistic Regression")

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(exported_values[["paramcd-picks_resolved"]]$values$selected, "BESRSPI")

    testthat::expect_equal(
      app_driver$get_active_module_input("responders"),
      c("CR", "PR")
    )

    testthat::expect_equal(exported_values[["arm_var-picks_resolved"]]$variables$selected, "ARM")

    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(
        Ref = list("B: Placebo"),
        Comp = list("A: Drug X", "C: Combination")
      )
    )

    testthat::expect_equal(exported_values[["cov_var-picks_resolved"]]$variables$selected, "SEX")

    testthat::expect_null(app_driver$get_active_module_input("interaction_var"))

    testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")

    testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))
  }
)

testthat::test_that(
  "e2e - tm_t_logistic: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_logistic()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "paramcd", "values", "INVET")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_logistic: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_logistic()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "paramcd", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
    "`Select Endpoint` field is empty"
  )
})

testthat::test_that(
  "e2e - tm_t_logistic: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_logistic()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "arm_var", "variables", "ARMCD")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_logistic: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_logistic()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "arm_var", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
    "Treatment Variable is empty."
  )
})

testthat::test_that(
  "e2e - tm_t_logistic: Selecting cov_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_logistic()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "cov_var", "variables", c("AGE", "BMRKR1"))
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_logistic: Deselection of cov_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_logistic()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "cov_var", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
    "`Covariates` field is empty"
  )
})
