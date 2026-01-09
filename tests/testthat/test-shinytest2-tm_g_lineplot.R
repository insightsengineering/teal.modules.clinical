app_driver_tm_g_lineplot <- function() {
  data <- within(teal.data::teal_data(), {
    require(nestcolor)
    require(dplyr)
    ADSL <- teal.modules.clinical::tmc_ex_adsl

    ADLB <- dplyr::mutate(
      teal.modules.clinical::tmc_ex_adlb,
      AVISIT == forcats::fct_reorder(AVISIT, AVISITN, min)
    )
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_lineplot(
        label = "Line Plot",
        dataname = "ADLB",
        parentname = "ADSL",
        group_var = teal.transform::choices_selected(
          teal.transform::variable_choices("ADSL", c("ARM", "ARMCD", "ACTARMCD")),
          "ARM"
        ),
        x = teal.transform::choices_selected(teal.transform::variable_choices(
          "ADLB",
          "AVISIT"
        ), "AVISIT", fixed = TRUE),
        y = teal.transform::choices_selected(
          teal.transform::variable_choices("ADLB", c("AVAL", "BASE", "CHG", "PCHG")),
          "AVAL"
        ),
        y_unit = teal.transform::choices_selected(teal.transform::variable_choices(
          "ADLB",
          "AVALU"
        ), "AVALU", fixed = TRUE),
        paramcd = teal.transform::choices_selected(teal.transform::variable_choices(
          "ADLB",
          "PARAMCD"
        ), "PARAMCD", fixed = TRUE),
        param = teal.transform::choices_selected(
          teal.transform::value_choices("ADLB", "PARAMCD", "PARAM"),
          "ALT"
        ),
        conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95,
          keep_order =
            TRUE
        ),
        interval = "mean_ci",
        mid = "mean",
        whiskers = c("mean_ci_lwr", "mean_ci_upr"),
        table = c("n", "mean_sd", "median", "range"),
        mid_type = "pl",
        mid_point_size = c(2, 1, 5),
        table_font_size = c(4, 2, 6),
        plot_height = c(1000L, 200L, 4000L),
        plot_width = NULL,
        pre_output = NULL,
        post_output = NULL,
        ggplot2_args = teal.widgets::ggplot2_args()
      )
    ),
    timeout = 30000
  )
}

testthat::test_that("e2e - tm_g_lineplot: Module initializes in teal without errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_lineplot()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("myplot-plot_main"))

  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_lineplot: Starts with specified label, param, group_var, y, x, mid, interval, incl_screen,
    plot_settings and table_settings.",
  {
    skip_if_too_deep(5)

    app_driver <- app_driver_tm_g_lineplot()

    testthat::expect_equal(trimws(app_driver$get_text("a.nav-link.active")), "Line Plot")
    testthat::expect_equal(app_driver$get_active_module_input("param-dataset_ADLB_singleextract-filter1-vals"), "ALT")
    testthat::expect_equal(app_driver$get_active_module_input("group_var-dataset_ADSL_singleextract-select"), "ARM")
    testthat::expect_equal(app_driver$get_active_module_input("y-dataset_ADLB_singleextract-select"), "AVAL")
    testthat::expect_equal(app_driver$get_active_module_input("x-dataset_ADLB_singleextract-select"), "AVISIT")
    testthat::expect_equal(app_driver$get_active_module_input("mid"), "mean")
    testthat::expect_equal(app_driver$get_active_module_input("interval"), "mean_ci")
    testthat::expect_true(app_driver$get_active_module_input("incl_screen"))

    # addtional plot settings
    testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")
    testthat::expect_equal(app_driver$get_active_module_input("mid_point_size"), 2)
    testthat::expect_equal(app_driver$get_active_module_input("whiskers"), c("Upper", "Lower"))
    testthat::expect_equal(app_driver$get_active_module_input("mid_type"), "pl")
    testthat::expect_equal(app_driver$get_active_module_input("y_unit-dataset_ADLB_singleextract-select"), "AVALU")
    testthat::expect_equal(app_driver$get_active_module_input("paramcd-dataset_ADLB_singleextract-select"), "PARAMCD")

    # addtional table settings
    testthat::expect_equal(app_driver$get_active_module_input("table_font_size"), 4)
    testthat::expect_equal(app_driver$get_active_module_input("table"), c("n", "mean_sd", "median", "range"))

    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_lineplot: Selecting param changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("param-dataset_ADLB_singleextract-filter1-vals", "CRP")
  app_driver$wait_for_idle()
  plot_after <- app_driver$get_active_module_plot_output("myplot")
  testthat::expect_false(
    identical(plot_before, plot_after)
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_lineplot: Deselecting param throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  app_driver$set_active_module_input("param-dataset_ADLB_singleextract-filter1-vals", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("param-dataset_ADLB_singleextract-filter1-vals_input > div > span")
    ),
    "Please select Biomarker filter."
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_lineplot: Selecting group_var changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_lineplot()
    app_driver$wait_for_idle()
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    app_driver$set_active_module_input("group_var-dataset_ADSL_singleextract-select", "ARMCD")
    app_driver$wait_for_idle()
    plot_after <- app_driver$get_active_module_plot_output("myplot")
    testthat::expect_false(
      identical(plot_before, plot_after)
    )
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_lineplot: Deselecting group_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  app_driver$set_active_module_input("group_var-dataset_ADSL_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "group_var-dataset_ADSL_singleextract-select_input > div > span"
    )),
    "Please select a treatment variable"
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_lineplot: Selecting y changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("y-dataset_ADLB_singleextract-select", "BASE")
  app_driver$wait_for_idle()
  plot_after <- app_driver$get_active_module_plot_output("myplot")
  testthat::expect_false(
    identical(plot_before, plot_after)
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_lineplot: Deselecting y throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  app_driver$set_active_module_input("y-dataset_ADLB_singleextract-select", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module(
      "y-dataset_ADLB_singleextract-select_input > div > span"
    )),
    "Please select an analysis variable"
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_lineplot: Selecting conf_level changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("conf_level", "0.8")
  app_driver$wait_for_idle()
  plot_after <- app_driver$get_active_module_plot_output("myplot")
  testthat::expect_false(
    identical(plot_before, plot_after)
  )
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_lineplot: Deselecting conf_level validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  app_driver$set_active_module_input("conf_level", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  testthat::expect_identical(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("conf_level_input > div > span")),
    "Please choose a confidence level"
  )
  app_driver$expect_validation_error()
  app_driver$stop()
})
