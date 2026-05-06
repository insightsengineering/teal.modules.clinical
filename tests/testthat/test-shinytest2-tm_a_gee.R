app_driver_tm_a_gee <- function() {
  data <- teal.data::teal_data()
  data <- within(data, {
    library(dplyr)
    ADSL <- tmc_ex_adsl
    ADQS <- tmc_ex_adqs |>
      filter(ABLFL != "Y" & ABLFL2 != "Y") |>
      mutate(
        AVISIT = as.factor(AVISIT),
        AVISITN = rank(AVISITN) |>
          as.factor() |>
          as.numeric() |>
          as.factor(),
        AVALBIN = AVAL < 50 # Just as an example to get a binary endpoint.
      ) |>
      droplevels()
  })

  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  all_values <- function(x) unique(x)
  class(all_values) <- append(class(all_values), "des-delayed")

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_a_gee(
        label = "GEE",
        dataname = "ADQS",
        parentname = "ADSL",
        aval_var = teal.picks::variables(choices = "AVALBIN", fixed = TRUE),
        id_var = teal.picks::variables(choices = c("USUBJID", "SUBJID"), selected = "USUBJID"),
        arm_var = teal.picks::variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
        visit_var = teal.picks::variables(choices = c("AVISIT", "AVISITN"), selected = "AVISIT"),
        paramcd = picks(
           variables(choices = c("PARAMCD", "PARAM")),
           values(all_values, "FKSI-FWB"),
           check_dataset = FALSE
        ),
        cov_var = teal.picks::variables(choices = c("BASE", "AGE", "SEX", "BASE:AVISIT"), selected = NULL),
        conf_level = teal.picks::values(c(0.95, 0.9, 0.8, -1), 0.95, multiple = FALSE),
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
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_a_gee: Starts with specified label, id_var, arm_var, visit_var, paramcd, cov_var,
  conf_level and conf_struct.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_a_gee()
    app_driver$wait_for_idle()

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "GEE"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(
      exported_values[["aval_var-picks_resolved"]]$variables$selected,
      "AVALBIN"
    )

    testthat::expect_equal(
      exported_values[["id_var-picks_resolved"]]$variables$selected,
      "USUBJID"
    )

    testthat::expect_equal(
      exported_values[["arm_var-picks_resolved"]]$variables$selected,
      "ARM"
    )

    testthat::expect_equal(
      exported_values[["visit_var-picks_resolved"]]$variables$selected,
      "AVISIT"
    )
    testthat::expect_equal(
      exported_values[["paramcd-picks_resolved"]]$variables$selected,
      "PARAMCD"
    )

    testthat::expect_equal(
      exported_values[["cov_var_resolved"]]$variables$selected,
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
    set_teal_picks_slot(app_driver, "id_var", "variables", "SUBJID")
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
  set_teal_picks_slot(app_driver, "id_var", "variables", character(0))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module(
        "table-table_out_main"
      )
    ),
    "A subject identifier is required"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Change in arm_var changes the table and does not throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()

  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  set_teal_picks_slot(app_driver, "arm_var", "variables", "ARMCD")
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
  set_teal_picks_slot(app_driver, "arm_var", "variables", character(0))
  testthat::expect_identical(
    app_driver$get_active_module_table_output("table-table-with-settings"), data.frame()
  )
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("table-table_out_main")
    ),
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
    set_teal_picks_slot(app_driver, "visit_var", "variables", "AVISITN")
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
  set_teal_picks_slot(app_driver, "visit_var", "variables", character(0))
  app_driver$wait_for_idle()
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("table-table_out_main")
    ),
    "A visit variable is required"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Selection of paramcd changes the table and does not throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  set_teal_picks_slot(app_driver, "paramcd", "values", "FATIGI")
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
  set_teal_picks_slot(app_driver, "paramcd", "variables", character(0))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_equal(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("table-table_out_main")
    ),
    "An endpoint is required"
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_a_gee: Selection of cov_var changes the table and does not throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_a_gee()
  table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
  set_teal_picks_slot(app_driver, "cov_var", "variables", "AGE")
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
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
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
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
    "Please choose a confidence level\nConfidence level must be between 0 and 1"
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
