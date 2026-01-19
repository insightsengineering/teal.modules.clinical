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

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_forest_tte(
        label = "Forest Survival (e-2-e)",
        dataname = "ADTTE",
        parentname = "ADSL",
        arm_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], c("ARM", "ARMCD")),
          "ARMCD"
        ),
        arm_ref_comp = arm_ref_comp,
        paramcd = teal.transform::choices_selected(
          teal.transform::value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
          "OS"
        ),
        subgroup_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], names(data[["ADSL"]])),
          c("BMRKR2", "SEX")
        ),
        strata_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADSL"]], c("STRATA1", "STRATA2")),
          "STRATA2"
        ),
        aval_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADTTE"]], "AVAL"),
          "AVAL",
          fixed = TRUE
        ),
        cnsr_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADTTE"]], "CNSR"),
          "CNSR",
          fixed = TRUE
        ),
        conf_level = teal.transform::choices_selected(
          c(0.95, 0.9, 0.8), 0.95,
          keep_order = TRUE
        ),
        time_unit_var = teal.transform::choices_selected(
          teal.transform::variable_choices(data[["ADTTE"]], "AVALU"),
          "AVALU",
          fixed = TRUE
        ),
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
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  app_driver$wait_for_idle()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("myplot-plot_main"))

  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_forest_tte: Starts with specified label, paramcd, arm_var, buckets,
    paramcd, subgroup_var, strata_var and plot settings.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()

    testthat::expect_identical(
      app_driver$get_text("a.nav-link.active"),
      "Forest Survival (e-2-e)"
    )

    testthat::expect_identical(
      app_driver$get_active_module_input(ns_des_input("arm_var", "ADSL", "select")),
      "ARMCD"
    )

    testthat::expect_identical(
      app_driver$get_active_module_input(ns_des_input("paramcd", "ADTTE", "filter1-vals")),
      "OS"
    )

    testthat::expect_identical(
      app_driver$get_active_module_input(ns_des_input("aval_var", "ADTTE", "select")),
      "AVAL"
    )

    testthat::expect_identical(
      app_driver$get_active_module_input(ns_des_input("cnsr_var", "ADTTE", "select")),
      "CNSR"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(Ref = list("ARM B"), Comp = list("ARM A", "ARM C"))
    )

    testthat::expect_setequal(
      app_driver$get_active_module_input(ns_des_input("subgroup_var", "ADSL", "select")),
      c("SEX", "BMRKR2")
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_des_input("strata_var", "ADSL", "select")),
      "STRATA2"
    )

    # Plot settings -----------------------------------------------------------
    # only tests the options that are customizable

    testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")
    testthat::expect_true(app_driver$get_active_module_input("fixed_symbol_size"))
    testthat::expect_equal(app_driver$get_active_module_input("rel_width_forest"), 25)
    testthat::expect_equal(app_driver$get_active_module_input("font_size"), 12)

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_forest_tte: Selection of 'paramcd' changes the element and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    app_driver$set_active_module_input(ns_des_input("paramcd", "ADTTE", "filter1-vals"), "CRSD")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_forest_tte: Deselection of paramcd filter throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_tte()
  input_id <- ns_des_input("paramcd", "ADTTE", "filter1-vals")
  app_driver$set_active_module_input(input_id, character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      sprintf(
        "%s_input .shiny-validation-message",
        input_id
      )
    )),
    "Please select Endpoint filter."
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_forest_tte: Selection of 'arm_var' changes the element and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    app_driver$set_active_module_input(ns_des_input("arm_var", "ADSL", "select"), "ARM")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_forest_tte: Deselection of paramcd var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_forest_tte()
  input_id <- ns_des_input("arm_var", "ADSL", "select")
  app_driver$set_active_module_input(input_id, character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      sprintf(
        "%s_input .shiny-validation-message",
        input_id
      )
    )),
    "Treatment variable must be selected"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_forest_tte: Selecting conf_level does not throw validation errors and changes a plot.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    input_id <- "conf_level"
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    app_driver$set_active_module_input(input_id, "0.99")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$get_text(app_driver$namespaces(TRUE)$module(
        sprintf("%s_input .shiny-validation-message", input_id)
      )),
      "Please choose a confidence level"
    )
    app_driver$stop()
  }
)
