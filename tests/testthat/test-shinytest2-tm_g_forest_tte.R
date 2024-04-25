app_driver_tm_g_forest_tte <- function() { # nolint: object_length.
  data <- within(teal.data::teal_data(), {
    ADSL <- teal.modules.clinical::tmc_ex_adsl
    ADSL$RACE <- with_label(droplevels(ADSL$RACE), "Race")
    ADTTE <- teal.modules.clinical::tmc_ex_adtte
  })

  datanames <- c("ADSL", "ADTTE")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

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
    data = data,
    modules = tm_g_forest_tte(
      label = "Forest Survival (e2e)",
      dataname = "ADTTE",
      arm_var = choices_selected(
        variable_choices(data[["ADSL"]], c("ARM", "ARMCD")),
        "ARMCD"
      ),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
        "OS"
      ),
      subgroup_var = choices_selected(
        variable_choices(data[["ADSL"]], names(data[["ADSL"]])),
        c("BMRKR2", "SEX")
      ),
      strata_var = choices_selected(
        variable_choices(data[["ADSL"]], c("STRATA1", "STRATA2")),
        "STRATA2"
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
}

ns_dataset <- function(prefix, suffix, dataset, extract = "singleextract") {
  sprintf("%s-dataset_%s_%s-%s", prefix, dataset, extract, suffix)
}

# Initialization --------------------------------------------------------------

testthat::test_that("e2e - tm_g_forest_tte: Module initializes in teal without errors and produces output.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_forest_tte()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("myplot-plot_out_main"))
  )

  app_driver$stop()
})

testthat::test_that(
  paste0(
    "e2e - tm_g_forest_tte: Starts with specified ",
    "label, id_var, arm_var, visit_var, paramcd, cov_var, conf_level and conf_struct."
  ),
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()

    testthat::expect_equal(
      trimws(app_driver$get_text("#teal-main_ui-root-active_tab > li.active")),
      "Forest Survival (e2e)"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_dataset("paramcd", "filter1-vals", "ADTTE")),
      "OS"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_dataset("arm_var", "select", "ADSL")),
      "ARMCD"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("buckets"),
      list(Ref = list("ARM B"), Comp = list("ARM A", "ARM C"))
    )

    testthat::expect_setequal(
      app_driver$get_active_module_input(ns_dataset("subgroup_var", "select", "ADSL")),
      c("SEX", "BMRKR2")
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_dataset("strata_var", "select", "ADSL")),
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

# paramcd and arm_var ---------------------------------------------------------

testthat::test_that(
  "e2e - tm_g_forest_tte: Selection of 'paramcd' changes the element and does not throw validation errors",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    plot_before <- app_driver$get_active_module_pws_output("myplot")
    app_driver$set_active_module_input(ns_dataset("paramcd", "filter1-vals", "ADTTE"), "CRSD")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("myplot")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e - tm_g_forest_tte: Selection of 'arm_var' changes the element and does not throw validation errors",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    plot_before <- app_driver$get_active_module_pws_output("myplot")
    app_driver$set_active_module_input(ns_dataset("arm_var", "select", "ADSL"), "ARM")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("myplot")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

for (input_id in c(ns_dataset("paramcd", "filter1-vals", "ADTTE"), ns_dataset("arm_var", "select", "ADSL"))) {
  testthat::test_that(sprintf("e2e - tm_g_forest_tte: Deselection of '%s' throws validation error.", input_id), {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_forest_tte()
    app_driver$set_active_module_input(input_id, character(0L))
    app_driver$expect_validation_error()
    testthat::expect_match(
      app_driver$active_module_element_text(
        sprintf(
          "%s_input .shiny-validation-message",
          input_id
        )
      ),
      "(^Please select.*$)|(.* must be selected$)"
    )
    app_driver$stop()
  })
}


# Test changing selection and de-selecting ------------------------------------

test_dataset_selection <- function(input_id, dataset, new_value) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_forest_tte: Selection of '%s' dataset changes the element and does not throw validation errors",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_forest_tte()
      plot_before <- app_driver$get_active_module_pws_output("myplot")
      app_driver$set_active_module_input(ns_dataset(input_id, "select", dataset), new_value)
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("myplot")))
      testthat::expect_identical(
        app_driver$get_active_module_input(ns_dataset(input_id, "select", dataset)),
        new_value
      )
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )

  testthat::test_that(
    sprintf(
      "%s: De-selection of '%s' dataset changes the element and does not throw validation errors",
      "e2e - tm_g_forest_tte",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_forest_tte()
      plot_before <- app_driver$get_active_module_pws_output("myplot")
      app_driver$set_active_module_input(ns_dataset(input_id, "select", dataset), character(0L))
      testthat::expect_null(app_driver$get_active_module_input(ns_dataset(input_id, "select", dataset)))
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("myplot")))
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )
}

test_dataset_selection("subgroup_var", "ADSL", c("COUNTRY", "RACE"))
test_dataset_selection("strata_var", "ADSL", "STRATA1")

# Plot settings ---------------------------------------------------------------

test_that_plot_settings <- function(input_id, new_value) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_forest_tte: Changing '%s' changes the plot and does not throw validation errors.",
      id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_forest_tte()
      setup_fun(app_driver)
      plot_before <- app_driver$get_active_module_pws_output("myplot")
      app_driver$set_active_module_input(input_id, new_value)
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("myplot")))
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )
}

test_that_plot_settings("conf_level", "0.9")
test_that_plot_settings("font_size", 15)
test_that_plot_settings("fixed_symbol_size", FALSE)
