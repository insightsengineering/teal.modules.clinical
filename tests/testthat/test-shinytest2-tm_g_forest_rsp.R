app_driver_tm_g_forest_rsp <- function() {
  data <- within(teal.data::teal_data(), {
    library(dplyr)
    library(tern)
    ADSL <- teal.data::rADSL
    ADRS <- teal.data::rADRS |>
      mutate(AVALC = d_onco_rsp_label(AVALC)) |>
      with_label("Character Result/Finding") |>
      filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  arm_ref_comp <- list(
    ARM = list(
      ref = "B: Placebo",
      comp = c("A: Drug X", "C: Combination")
    ),
    ARMCD = list(
      ref = "ARM B",
      comp = c("ARM A", "ARM C")
    )
  )

  testthat::expect_warning(
    paramcd_value <- teal.picks::values(selected = "INVET", multiple = FALSE),
    "doesn't guarantee that `selected` is a subset of `choices`.",
    fixed = TRUE
  )

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = modules(
        tm_g_forest_rsp(
          label = "Forest Response",
          dataname = "ADRS",
          parentname = "ADSL",
          arm_var = teal.picks::variables(c("ARM", "ARMCD"), selected = "ARMCD", multiple = FALSE),
          arm_ref_comp = arm_ref_comp,
          paramcd = teal.picks::picks(teal.picks::variables("PARAMCD"), paramcd_value, check_dataset = FALSE),
          aval_var = teal.picks::variables("AVALC", fixed = TRUE),
          subgroup_var = teal.picks::variables(
            c("BMRKR2", "SEX"),
            selected = c("BMRKR2", "SEX"),
            multiple = TRUE
          ),
          strata_var = teal.picks::variables(
            c("STRATA1", "STRATA2"),
            selected = "STRATA2",
            multiple = TRUE
          ),
          fixed_symbol_size = TRUE,
          conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95", keep_order = TRUE),
          plot_height = c(600L, 200L, 2000L),
          default_responses = list(
            BESRSPI = list(
              rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
              levels = c(
                "Complete Response (CR)", "Partial Response (PR)", "Stable Disease (SD)",
                "Progressive Disease (PD)", "Not Evaluable (NE)"
              )
            ),
            INVET = list(
              rsp = c("Complete Response (CR)", "Partial Response (PR)"),
              levels = c(
                "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
                "Progressive Disease (PD)", "Stable Disease (SD)"
              )
            ),
            OVRINV = list(
              rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
              levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
            )
          ),
          plot_width = c(1500L, 800L, 3000L),
          rel_width_forest = c(25L, 0L, 100L),
          font_size = c(15L, 1L, 30L),
          pre_output = NULL,
          post_output = NULL,
          ggplot2_args = teal.widgets::ggplot2_args()
        )
      )
    )
  )
}

testthat::test_that("e2e - tm_g_forest_rsp: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("myplot-plot_main"))
})

testthat::test_that(
  "e2e - tm_g_forest_rsp: Module initializes with specified
  label, arm_var, paramcd, aval_var, responders, subgroup_var, strata_var,
  conf_level, fixed_symbol_size, rel_width_forest, font_size.",
  {
    skip_if_too_deep(5)

    app_driver <- app_driver_tm_g_forest_rsp()
    withr::defer(app_driver$stop())

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Forest Response"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(
      exported_values[["arm_var-picks_resolved"]]$variables$selected,
      "ARMCD"
    )
    testthat::expect_equal(
      exported_values[["paramcd-picks_resolved"]]$values$selected,
      "INVET"
    )
    testthat::expect_equal(
      exported_values[["aval_var-picks_resolved"]]$variables$selected,
      "AVALC"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("responders"),
      c("Complete Response (CR)", "Partial Response (PR)")
    )
    testthat::expect_equal(
      sort(exported_values[["subgroup_var-picks_resolved"]]$variables$selected),
      sort(c("SEX", "BMRKR2"))
    )
    testthat::expect_equal(
      exported_values[["strata_var-picks_resolved"]]$variables$selected,
      "STRATA2"
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("conf_level"),
      "0.95"
    )
    testthat::expect_true(app_driver$get_active_module_input("fixed_symbol_size"))
    testthat::expect_equal(
      app_driver$get_active_module_input("rel_width_forest"),
      25
    )
    testthat::expect_equal(
      app_driver$get_active_module_input("font_size"),
      15
    )
  }
)

testthat::test_that("e2e - tm_g_forest_rsp: Selecting arm_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "arm_var", "variables", "ARM")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "arm_var", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "A treatment variable is required.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting paramcd changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "paramcd", "values", "OVRINV")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting paramcd throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "paramcd", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please select an endpoint (PARAMCD).",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting responders changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("responders", "Complete Response (CR)")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting responders throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("responders", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "`Responders` field is empty.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting subgroup_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "subgroup_var", "variables", c("SEX", "BMRKR2", "AGEU"))
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting a non-factors column in subgroup_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "subgroup_var", "variables", c("SEX", "AGE"))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Not all subgroup variables are factors.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting subgroup_var changes plot and doesn't throw validation errors.", { # nolint: line_length
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "subgroup_var", "variables", character(0L))
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting strata_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "strata_var", "variables", "STRATA1")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting strata_var changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "strata_var", "variables", character(0L))
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting conf_level changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("conf_level", "0.9")
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Deselecting conf_level throws validation error.", { # nolint: line_length
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("conf_level", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please choose a confidence level.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_forest_rsp: Selecting conf_level outside range 0-1 throws validation error.", { # nolint: line_length
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("conf_level", "2")
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Confidence level must be between 0 and 1.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_forest_rsp: Unsetting fixed_symbol_size changes plot and doesn't throw validation errors.", { # nolint: line_length
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("fixed_symbol_size", FALSE)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Changing rel_width_forest changes plot and doesn't throw validation errors.", { # nolint: line_length
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("rel_width_forest", 30)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_forest_rsp: Changing font_size changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_rsp()
  withr::defer(app_driver$stop())
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("font_size", 25)
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$expect_no_validation_error()
})
