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

  avisit_values <- suppressWarnings(
    teal.picks::values(selected = "WEEK 1 DAY 8", multiple = TRUE),
    classes = "picks_delayed"
  )

  paramcd_values <- suppressWarnings(
    teal.picks::values(selected = "FKSI-FWB", multiple = TRUE),
    classes = "picks_delayed"
  )

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_t_ancova(
        label = "ANCOVA Table",
        dataname = "ADQS",
        parentname = "ADSL",
        avisit = teal.picks::picks(
          teal.picks::variables("AVISIT", "AVISIT"),
          avisit_values,
          check_dataset = FALSE
        ),
        arm_var = teal.picks::variables(c("ARM", "ACTARMCD", "ARMCD"), selected = "ARMCD"),
        arm_ref_comp = arm_ref_comp,
        aval_var = teal.picks::variables(c("CHG", "AVAL"), selected = "CHG", multiple = FALSE),
        cov_var = teal.picks::variables(c("BASE", "STRATA1", "SEX"), selected = "STRATA1"),
        paramcd = teal.picks::picks(
          teal.picks::variables("PARAMCD", "PARAMCD"),
          paramcd_values,
          check_dataset = FALSE
        ),
        interact_var = teal.picks::variables(
          c("BASE", "STRATA1", "SEX"),
          selected = "STRATA1",
          multiple = FALSE
        ),
        conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95", keep_order = TRUE),
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
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table-table-with-settings"))
})

testthat::test_that(
  "e2e - tm_t_ancova: Starts with specified label, avisit, paramcd, aval_var, aval_var,
  arm_var, buckets, combine_comp_arms, interact_var, cov_var, conf_level, include_interact.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "ANCOVA Table"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(
      exported_values[["avisit-picks_resolved"]]$values$selected,
      "WEEK 1 DAY 8"
    )
    testthat::expect_equal(
      exported_values[["avisit-picks_resolved"]]$variables$selected,
      "AVISIT"
    )
    testthat::expect_equal(
      exported_values[["paramcd-picks_resolved"]]$values$selected,
      "FKSI-FWB"
    )
    testthat::expect_equal(
      exported_values[["aval_var-picks_resolved"]]$variables$selected,
      "CHG"
    )
    testthat::expect_equal(
      exported_values[["arm_var-picks_resolved"]]$variables$selected,
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
      exported_values[["interact_var-picks_resolved"]]$variables$selected,
      "STRATA1"
    )
    testthat::expect_equal(
      exported_values[["cov_var-picks_resolved"]]$variables$selected,
      "STRATA1"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_false(app_driver$get_active_module_input("include_interact"))
  }
)

testthat::test_that(
  "e2e - tm_t_ancova: Selecting avisit changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "avisit", "values", c("WEEK 1 DAY 8", "WEEK 2 DAY 15"))
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_ancova: Deselection of avisit throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "avisit", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
    "`Analysis Visit` field cannot be empty."
  )
})

testthat::test_that(
  "e2e - tm_t_ancova: Selecting paramcd changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "paramcd", "values", c("BFIALL", "FATIGI"))
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_ancova: Deselection of paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "paramcd", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
    "`Select Endpoint` is not selected."
  )
})

testthat::test_that(
  "e2e - tm_t_ancova: Selecting aval_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "aval_var", "variables", "AVAL")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_ancova: Deselection of aval_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "aval_var", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
    "Analysis variable cannot be empty."
  )
})

testthat::test_that(
  "e2e - tm_t_ancova: Selecting arm_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "arm_var", "variables", "ARM")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_t_ancova: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_ancova()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "arm_var", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_table_output("table-table-with-settings"), data.frame())
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("table-table_out_main")),
    "Arm variable cannot be empty."
  )
})

testthat::test_that(
  "e2e - tm_t_ancova: Selecting cov_var changes the table and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "cov_var", "variables", "BASE")
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_t_ancova: Deselection of cov_var changes table and doesn't throw validation error.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_t_ancova()
    withr::defer(app_driver$stop())
    table_before <- app_driver$get_active_module_table_output("table-table-with-settings")
    set_teal_picks_slot(app_driver, "cov_var", "variables", character(0L))
    testthat::expect_false(
      identical(
        table_before,
        app_driver$get_active_module_table_output("table-table-with-settings")
      )
    )
    app_driver$expect_no_validation_error()
  }
)
