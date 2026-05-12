app_driver_tm_g_forest_tte <- function() {
  data <- within(teal.data::teal_data(), {
    ADSL <- teal.modules.clinical::tmc_ex_adsl
    ADSL$RACE <- formatters::with_label(droplevels(ADSL$RACE), "Race")
    ADTTE <- teal.modules.clinical::tmc_ex_adtte
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

  paramcd_value <- suppressWarnings(
    teal.picks::values(selected = "OS", multiple = FALSE),
    classes = "picks_delayed"
  )

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_forest_tte(
        label = "Forest Survival (e-2-e)",
        dataname = "ADTTE",
        parentname = "ADSL",
        arm_var = teal.picks::variables(c("ARM", "ARMCD"), selected = "ARMCD", multiple = FALSE),
        arm_ref_comp = arm_ref_comp,
        paramcd = teal.picks::picks(teal.picks::variables("PARAMCD"), paramcd_value, check_dataset = FALSE),
        subgroup_var = suppressWarnings(teal.picks::variables(
          selected = c("BMRKR2", "SEX"),
          multiple = TRUE
        )),
        strata_var = teal.picks::variables(
          c("STRATA1", "STRATA2"),
          selected = "STRATA2",
          multiple = TRUE
        ),
        aval_var = teal.picks::variables("AVAL", fixed = TRUE),
        cnsr_var = teal.picks::variables("CNSR", fixed = TRUE),
        conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95", keep_order = TRUE),
        time_unit_var = teal.picks::variables("AVALU", fixed = TRUE),
        fixed_symbol_size = FALSE,
        plot_height = c(500L, 300L, 2000L),
        plot_width = c(1000L, 700L, 2000L),
        rel_width_forest = c(25L, 0L, 100L),
        font_size = c(12L, 1L, 30L),
        pre_output = NULL,
        post_output = NULL,
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    )
  )
}

# Initialization --------------------------------------------------------------

testthat::test_that("e2e - tm_g_forest_tte: Module initializes in teal without errors and produces output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_tte()
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$wait_for_idle()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("myplot-plot_main"))
})

testthat::test_that(
  "e2e - tm_g_forest_tte: Starts with specified label, paramcd, arm_var, buckets,
    paramcd, subgroup_var, strata_var and plot settings.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    withr::defer(app_driver$stop())

    testthat::expect_identical(
      app_driver$get_text("a.nav-link.active"),
      "Forest Survival (e-2-e)"
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
      "OS"
    )

    testthat::expect_equal(
      exported_values[["aval_var-picks_resolved"]]$variables$selected,
      "AVAL"
    )

    testthat::expect_equal(
      exported_values[["cnsr_var-picks_resolved"]]$variables$selected,
      "CNSR"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(Ref = list("ARM B"), Comp = list("ARM A", "ARM C"))
    )

    testthat::expect_setequal(
      exported_values[["subgroup_var-picks_resolved"]]$variables$selected,
      c("SEX", "BMRKR2")
    )

    testthat::expect_equal(
      exported_values[["strata_var-picks_resolved"]]$variables$selected,
      "STRATA2"
    )

    # Plot settings -----------------------------------------------------------
    # only tests the options that are customizable

    testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")
    testthat::expect_false(app_driver$get_active_module_input("fixed_symbol_size"))
    testthat::expect_equal(app_driver$get_active_module_input("rel_width_forest"), 25)
    testthat::expect_equal(app_driver$get_active_module_input("font_size"), 12)
  }
)

testthat::test_that(
  "e2e - tm_g_forest_tte: Selection of 'paramcd' changes the element and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    set_teal_picks_slot(app_driver, "paramcd", "values", "CRSD")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_g_forest_tte: Deselection of paramcd filter throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_tte()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "paramcd", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please select Endpoint filter.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_g_forest_tte: Selection of 'arm_var' changes the element and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    set_teal_picks_slot(app_driver, "arm_var", "variables", "ARM")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_g_forest_tte: Deselection of arm_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_tte()
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

testthat::test_that(
  "e2e - tm_g_forest_tte: Selecting conf_level does not throw validation errors and changes a plot.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    app_driver$set_active_module_input("conf_level", "0.9")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_g_forest_tte: Deselection of conf_level throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_tte()
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

testthat::test_that("e2e - tm_g_forest_tte: Selecting conf_level outside range 0-1 throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_tte()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("conf_level", 2)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Please choose a confidence level.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_g_forest_tte: Selection of subgroup_var changes the element and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    set_teal_picks_slot(app_driver, "subgroup_var", "variables", c("SEX", "BMRKR2", "AGEU"))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_g_forest_tte: Selecting a non-factor column in subgroup_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_tte()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "subgroup_var", "variables", c("SEX", "AGE"))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_out_main")),
    "Not all subgroup variables are factors.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_g_forest_tte: Deselecting subgroup_var changes plot and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    set_teal_picks_slot(app_driver, "subgroup_var", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_forest_tte: Selection of strata_var changes the element and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    set_teal_picks_slot(app_driver, "strata_var", "variables", "STRATA1")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that(
  "e2e - tm_g_forest_tte: Deselecting strata_var changes plot and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    withr::defer(app_driver$stop())
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    set_teal_picks_slot(app_driver, "strata_var", "variables", character(0L))
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
  }
)
