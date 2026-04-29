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
        group_var = teal.picks::variables(
          choices = c("ARM", "ARMCD", "ACTARMCD"),
          selected = "ARM",
          multiple = FALSE
        ),
        x = teal.picks::variables("AVISIT", fixed = TRUE),
        y = teal.picks::variables(
          choices = c("AVAL", "BASE", "CHG", "PCHG"),
          selected = "AVAL",
          multiple = FALSE
        ),
        y_unit = teal.picks::variables("AVALU", fixed = TRUE),
        param = teal.picks::picks(
          teal.picks::datasets("ADLB"),
          teal.picks::variables("PARAMCD", fixed = TRUE),
          teal.picks::values(
            choices = levels(data[["ADLB"]]$PARAMCD),
            selected = "ALT",
            multiple = FALSE
          )
        ),
        conf_level = teal.picks::values(c("0.95", "0.9", "0.8"), "0.95", keep_order = TRUE),
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
  withr::defer(app_driver$stop())
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("myplot-plot_main"))
})

testthat::test_that(
  "e2e - tm_g_lineplot: Starts with specified label, param, group_var, y, x, mid, interval, incl_screen,
    plot_settings and table_settings.",
  {
    skip_if_too_deep(5)

    app_driver <- app_driver_tm_g_lineplot()
    withr::defer(app_driver$stop())

    testthat::expect_equal(trimws(app_driver$get_text("a.nav-link.active")), "Line Plot")

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(exported_values[["param-picks_resolved"]]$values$selected, "ALT")
    testthat::expect_equal(exported_values[["group_var-picks_resolved"]]$variables$selected, "ARM")
    testthat::expect_equal(exported_values[["y-picks_resolved"]]$variables$selected, "AVAL")
    testthat::expect_equal(exported_values[["x-picks_resolved"]]$variables$selected, "AVISIT")

    testthat::expect_equal(app_driver$get_active_module_input("mid"), "mean")
    testthat::expect_equal(app_driver$get_active_module_input("interval"), "mean_ci")
    testthat::expect_true(app_driver$get_active_module_input("incl_screen"))

    # additional plot settings
    testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")
    testthat::expect_equal(app_driver$get_active_module_input("mid_point_size"), 2)
    testthat::expect_equal(app_driver$get_active_module_input("whiskers"), c("Upper", "Lower"))
    testthat::expect_equal(app_driver$get_active_module_input("mid_type"), "pl")
    testthat::expect_equal(exported_values[["y_unit-picks_resolved"]]$variables$selected, "AVALU")

    # additional table settings
    testthat::expect_equal(app_driver$get_active_module_input("table_font_size"), 4)
    testthat::expect_equal(app_driver$get_active_module_input("table"), c("n", "mean_sd", "median", "range"))
  }
)

testthat::test_that("e2e - tm_g_lineplot: Selecting param changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "param", "values", "CRP")
  app_driver$wait_for_idle()
  testthat::expect_false(
    identical(plot_before, app_driver$get_active_module_plot_output("myplot"))
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_lineplot: Deselecting param throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "param", "values", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "Please select a Biomarker filter.",
    fixed = TRUE
  )
})

testthat::test_that(
  "e2e - tm_g_lineplot: Selecting group_var changes plot and doesn't throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_lineplot()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    set_teal_picks_slot(app_driver, "group_var", "variables", "ARMCD")
    app_driver$wait_for_idle()
    testthat::expect_false(
      identical(plot_before, app_driver$get_active_module_plot_output("myplot"))
    )
    app_driver$expect_no_validation_error()
  }
)

testthat::test_that("e2e - tm_g_lineplot: Deselecting group_var throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "group_var", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "Please select a treatment variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_lineplot: Selecting y changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "y", "variables", "BASE")
  app_driver$wait_for_idle()
  testthat::expect_false(
    identical(plot_before, app_driver$get_active_module_plot_output("myplot"))
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_lineplot: Deselecting y throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  withr::defer(app_driver$stop())
  set_teal_picks_slot(app_driver, "y", "variables", character(0L))
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "Please select an analysis variable.",
    fixed = TRUE
  )
})

testthat::test_that("e2e - tm_g_lineplot: Selecting conf_level changes plot and doesn't throw validation errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("conf_level", "0.8")
  app_driver$wait_for_idle()
  testthat::expect_false(
    identical(plot_before, app_driver$get_active_module_plot_output("myplot"))
  )
  app_driver$expect_no_validation_error()
})

testthat::test_that("e2e - tm_g_lineplot: Deselecting conf_level validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_lineplot()
  withr::defer(app_driver$stop())
  app_driver$set_active_module_input("conf_level", NULL)
  testthat::expect_identical(app_driver$get_active_module_plot_output("myplot"), character(0))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "Please choose a confidence level.",
    fixed = TRUE
  )
})
