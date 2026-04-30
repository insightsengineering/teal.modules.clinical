app_driver_tm_g_km <- function() {
  data <- within(teal.data::teal_data(), {
    library(dplyr)
    ADSL <- tmc_ex_adsl
    ADTTE <- tmc_ex_adtte %>%
      rename(
        VALUE_UNIT = AVALU,
        ANALYSIS_VAL = AVAL,
        CENSORING = CNSR
      )
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
      modules = tm_g_km(
        label = "Kaplan-Meier Plot",
        dataname = "ADTTE",
        parentname = "ADSL",
        arm_var = teal.picks::variables(
          choices = c("ARM", "ARMCD", "ACTARMCD"),
          selected = "ARM",
          multiple = FALSE
        ),
        paramcd = teal.picks::picks(
          teal.picks::datasets("ADTTE"),
          teal.picks::variables("PARAMCD", fixed = TRUE),
          teal.picks::values(
            choices = levels(data[["ADTTE"]]$PARAMCD),
            selected = "OS",
            multiple = FALSE
          )
        ),
        arm_ref_comp = arm_ref_comp,
        strata_var = teal.picks::variables(
          choices = c("SEX", "BMRKR2"),
          selected = "SEX",
          multiple = TRUE
        ),
        time_unit_var = teal.picks::variables("VALUE_UNIT", fixed = TRUE),
        aval_var = teal.picks::variables("ANALYSIS_VAL", fixed = TRUE),
        cnsr_var = teal.picks::variables("CENSORING", fixed = TRUE),
        conf_level = teal.picks::values(
          c("0.95", "0.9", "0.8", "-1"),
          selected = "0.95",
          keep_order = TRUE,
          multiple = FALSE
        ),
        conf_type = teal.picks::values(
          c("plain", "log", "log-log"),
          selected = "log",
          keep_order = TRUE,
          multiple = FALSE
        ),
        font_size = c(11L, 1L, 30),
        control_annot_surv_med = control_surv_med_annot(),
        control_annot_coxph = control_coxph_annot(x = 0.27, y = 0.35, w = 0.3),
        legend_pos = c(0.9, 0.5),
        rel_height_plot = c(80L, 0L, 100L),
        plot_height = c(800L, 400L, 5000L),
        plot_width = NULL,
        pre_output = NULL,
        post_output = NULL
      )
    ),
    timeout = 30000
  )
}

testthat::test_that("e2e - tm_g_km: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_match(
    app_driver$get_active_module_plot_output("myplot"),
    "data:image/png;base64,"
  )
  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_km: Starts with specified paramcd, aval_var, cnsr_var, facet_var, arm_var, compare_arms, strata_var.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_km()
    app_driver$wait_for_idle()

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Kaplan-Meier Plot"
    )

    exported_values <- app_driver$get_values()$export
    names(exported_values) <- gsub(
      sprintf("%s-", app_driver$namespaces()$module(NULL)), "", names(exported_values),
      fixed = TRUE
    )

    testthat::expect_equal(exported_values[["paramcd-picks_resolved"]]$values$selected, "OS")
    testthat::expect_equal(exported_values[["aval_var-picks_resolved"]]$variables$selected, "ANALYSIS_VAL")
    testthat::expect_equal(exported_values[["cnsr_var-picks_resolved"]]$variables$selected, "CENSORING")
    testthat::expect_equal(
      get_teal_picks_slot(app_driver, "facet_var", "variables"),
      "-- no selection --"
    )
    testthat::expect_equal(exported_values[["arm_var-picks_resolved"]]$variables$selected, "ARM")
    testthat::expect_true(app_driver$get_active_module_input("compare_arms"))
    testthat::expect_equal(exported_values[["strata_var-picks_resolved"]]$variables$selected, "SEX")

    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_km: Changing {paramcd} changes the plot without errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "paramcd", "values", "EFS")
  app_driver$expect_no_validation_error()
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Changing {facet_var} changes the plot without errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "facet_var", "variables", "SEX")
  app_driver$expect_no_validation_error()
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Changing {arm_var} changes the plot without errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "arm_var", "variables", "ACTARMCD")
  app_driver$expect_no_validation_error()
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Changing {compare_arms} changes the plot without errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("compare_arms", FALSE)
  app_driver$expect_no_validation_error()
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Changing {strata_var} changes the plot without errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  set_teal_picks_slot(app_driver, "strata_var", "variables", "BMRKR2")
  app_driver$expect_no_validation_error()
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Deselecting {paramcd} throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  set_teal_picks_slot(app_driver, "paramcd", "values", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "Please select an endpoint.",
    fixed = TRUE
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Deselecting {arm_var} throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  set_teal_picks_slot(app_driver, "arm_var", "variables", character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "Please select a treatment variable.",
    fixed = TRUE
  )
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Deselecting {compare_arms} sets it to FALSE.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$set_active_module_input("compare_arms", NULL)
  app_driver$expect_no_validation_error()
  testthat::expect_false(app_driver$get_active_module_input("compare_arms"))
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Deselecting {strata_var} does not throw errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  set_teal_picks_slot(app_driver, "strata_var", "variables", character(0L))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

# groups ----------------------------------------------------------------------------------------------------------

testthat::test_that("e2e - tm_g_km: Starts with specified groups.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()

  testthat::expect_equal(
    app_driver$get_active_module_input("buckets"),
    list(
      Ref = list("B: Placebo"),
      Comp = list("A: Drug X", "C: Combination")
    )
  )

  testthat::expect_false(app_driver$get_active_module_input("combine_comp_arms"))

  app_driver$stop()
})

# comparison settings ---------------------------------------------------------------------------------------------

testthat::test_that("e2e - tm_g_km: Starts with specified collapsed comparison settings.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()

  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("pval_method_coxph"))
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("ties_coxph"))

  testthat::expect_equal(app_driver$get_active_module_input("pval_method_coxph"), "log-rank")
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("pval_method_coxph-label")),
    "p-value method for Coxph (Hazard Ratio)"
  )
  testthat::expect_equal(app_driver$get_active_module_input("ties_coxph"), "exact")
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("ties_coxph-label")),
    "Ties for Coxph (Hazard Ratio)"
  )

  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Changing {pval_method_coxph} changes the plot without errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("pval_method_coxph", "wald")
  app_driver$expect_no_validation_error()
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Changing {ties_coxph} changes the plot without errors.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  plot_before <- app_driver$get_active_module_plot_output("myplot")
  app_driver$set_active_module_input("ties_coxph", "breslow")
  app_driver$expect_no_validation_error()
  testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Deselecting {pval_method_coxph} gives no validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$set_active_module_input("pval_method_coxph", character(0))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

testthat::test_that("e2e - tm_g_km: Deselecting {ties_coxph} gives no validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$set_active_module_input("ties_coxph", character(0))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})

# plot settings ---------------------------------------------------------------------------------------------------

testthat::test_that("e2e - tm_g_km: Starts with collapsed additional plot settings.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()

  app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("xticks"))
  app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("yval"))
  app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("font_size"))
  app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("rel_height_plot"))
  app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("show_ci_ribbon"))
  app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("show_km_table"))
  app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("conf_level"))
  app_driver$expect_hidden(app_driver$namespaces(TRUE)$module("xlab"))

  testthat::expect_equal(app_driver$get_active_module_input("xticks"), "")
  testthat::expect_equal(app_driver$get_active_module_input("yval"), "Survival probability")
  testthat::expect_equal(app_driver$get_active_module_input("font_size"), 11)
  testthat::expect_equal(app_driver$get_active_module_input("rel_height_plot"), 80)
  testthat::expect_false(app_driver$get_active_module_input("show_ci_ribbon"))
  testthat::expect_true(app_driver$get_active_module_input("show_km_table"))
  testthat::expect_equal(app_driver$get_active_module_input("conf_level"), "0.95")
  testthat::expect_equal(app_driver$get_active_module_input("conf_type"), "log")
  testthat::expect_equal(app_driver$get_active_module_input("xlab"), "Time")

  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("xticks-label")),
    "Specify break intervals for x-axis e.g. 0 ; 500"
  )
  testthat::expect_match(
    app_driver$get_text(
      app_driver$namespaces(TRUE)$module("yval-label")
    ),
    "Value on y-axis",
    fixed = FALSE
  )
  testthat::expect_equal(app_driver$get_text(app_driver$namespaces(TRUE)$module("font_size-label")), "Table Font Size")
  testthat::expect_equal(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("rel_height_plot-label")), "Relative Height of Plot (%)"
  )
  testthat::expect_equal(app_driver$get_text(app_driver$namespaces(TRUE)$module("xlab-label")), "X-axis label")

  app_driver$stop()
})

test_that_plot_settings <- function(input_id, new_value) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_km: Changing '%s' changes the plot and does not throw validation errors.",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_km()
      app_driver$wait_for_idle()
      plot_before <- app_driver$get_active_module_plot_output("myplot")
      app_driver$set_active_module_input(input_id, new_value)
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )
}

test_that_plot_settings("xticks", 50)
test_that_plot_settings("yval", "Failure probability")
test_that_plot_settings("font_size", 12)
test_that_plot_settings("rel_height_plot", 70)
test_that_plot_settings("show_ci_ribbon", TRUE)
test_that_plot_settings("show_km_table", FALSE)
test_that_plot_settings("conf_level", "0.8")
test_that_plot_settings("xlab", "Time2")

testthat::test_that("e2e - tm_g_km: Deselecting {conf_level} throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$wait_for_idle()
  app_driver$set_active_module_input("conf_level", "-1")
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$get_text(app_driver$namespaces(TRUE)$module("myplot-plot_main")),
    "Confidence level must be between 0 and 1."
  )
  app_driver$stop()
})
