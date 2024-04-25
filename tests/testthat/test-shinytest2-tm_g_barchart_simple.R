app_driver_tm_g_barchart_simple <- function() { # nolint: object_length.
  data <- within(teal.data::teal_data(), {
    ADSL <- dplyr::mutate(
      teal.modules.clinical::tmc_ex_adsl,
      ITTFL = with_label(factor("Y"), "Intent-To-Treat Population Flag")
    )

    ADAE <- dplyr::filter(
      teal.modules.clinical::tmc_ex_adae,
      !((AETOXGR == 1) & (AESEV == "MILD") & (ARM == "A: Drug X"))
    )
  })

  datanames <- c("ADSL", "ADAE")
  teal.data::datanames(data) <- datanames
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datanames]

  init_teal_app_driver(
    data = data,
    modules = tm_g_barchart_simple(
      label = "ADAE Analysis (e2e)",
      x = teal.transform::data_extract_spec(
        dataname = "ADSL",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            "ADSL", c("ARM", "ACTARM", "SEX", "RACE", "ITTFL", "SAFFL", "STRATA2")
          ),
          selected = "ACTARM",
          multiple = FALSE
        )
      ),
      fill = list(
        teal.transform::data_extract_spec(
          dataname = "ADSL",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              "ADSL", c("ARM", "ACTARM", "SEX", "RACE", "ITTFL", "SAFFL", "STRATA2")
            ),
            selected = "SEX",
            multiple = FALSE
          )
        ),
        teal.transform::data_extract_spec(
          dataname = "ADAE",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices("ADAE", c("AETOXGR", "AESEV", "AESER")),
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      x_facet = list(
        teal.transform::data_extract_spec(
          dataname = "ADAE",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices("ADAE", c("AETOXGR", "AESEV", "AESER")),
            selected = "AETOXGR",
            multiple = FALSE
          )
        ),
        teal.transform::data_extract_spec(
          dataname = "ADSL",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              "ADSL",
              c("ARM", "ACTARM", "SEX", "RACE", "ITTFL", "SAFFL", "STRATA2")
            ),
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      y_facet = list(
        data_extract_spec(
          dataname = "ADAE",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              "ADAE",
              c("AETOXGR", "AESEV", "AESER")
            ),
            selected = "AESEV",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADSL",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              "ADSL",
              c("ARM", "ACTARM", "SEX", "RACE", "ITTFL", "SAFFL", "STRATA2")
            ),
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      plot_options = list(
        stacked = TRUE,
        label_bars = FALSE,
        rotate_bar_labels = TRUE,
        rotate_x_label = TRUE,
        rotate_y_label = TRUE,
        flip_axis = TRUE,
        show_n = FALSE
      ),
      plot_height = c(600L, 200L, 2000L),
      plot_width = NULL,
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

testthat::test_that("e2e - tm_g_barchart_simple: Module initializes in teal without errors and produces output.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_barchart_simple()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_true(
    app_driver$is_visible(app_driver$active_module_element("myplot-plot_out_main"))
  )

  testthat::expect_true(app_driver$is_visible(app_driver$active_module_element("table")))

  app_driver$stop()
})

testthat::test_that(
  paste0(
    "e2e - tm_g_barchart_simple: Starts with specified ",
    "label, id_var, arm_var, visit_var, paramcd, cov_var, conf_level and conf_struct."
  ),
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_barchart_simple()

    testthat::expect_equal(
      trimws(app_driver$get_text("#teal-main_ui-root-active_tab > li.active")),
      "ADAE Analysis (e2e)"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_dataset("x", "select", "ADSL")),
      "ACTARM"
    )

    testthat::expect_equal(app_driver$get_active_module_input("fill-dataset"), "ADSL")

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_dataset("fill", "select", "ADSL")),
      "SEX"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("x_facet-dataset"),
      "ADAE"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_dataset("x_facet", "select", "ADAE")),
      "AETOXGR"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input("y_facet-dataset"),
      "ADAE"
    )

    testthat::expect_equal(
      app_driver$get_active_module_input(ns_dataset("y_facet", "select", "ADAE")),
      "AESEV"
    )

    # Plot settings -----------------------------------------------------------
    # only tests the options that are customizable

    testthat::expect_equal(app_driver$get_active_module_input("barlayout"), "stacked")
    testthat::expect_false(app_driver$get_active_module_input("label_bars"))
    testthat::expect_true(app_driver$get_active_module_input("rotate_bar_labels"))
    testthat::expect_true(app_driver$get_active_module_input("rotate_x_label"))
    testthat::expect_true(app_driver$get_active_module_input("rotate_y_label"))
    testthat::expect_true(app_driver$get_active_module_input("flip_axis"))
    testthat::expect_false(app_driver$get_active_module_input("show_n"))

    app_driver$stop()
  }
)

# X-variable ------------------------------------------------------------------

testthat::test_that(
  "e2e - tm_g_barchart_simple: Selection of 'x' changes the element and does not throw validation errors",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_barchart_simple()
    plot_before <- app_driver$get_active_module_pws_output("myplot")
    app_driver$set_active_module_input(ns_dataset("x", "select", "ADSL"), "RACE")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("myplot")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_barchart_simple: Deselection of 'x' throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_barchart_simple()
  app_driver$set_active_module_input(ns_dataset("x", "select", "ADSL"), character(0L))
  app_driver$expect_validation_error()
  testthat::expect_match(
    app_driver$active_module_element_text(
      sprintf(
        "%s .shiny-validation-message",
        ns_dataset("x", "select_input", "ADSL")
      )
    ),
    "^Please select an x-variable$"
  )
  app_driver$stop()
})

# Test pairs of dataset selection ---------------------------------------------

test_dataset_selection <- function(input_id, new_dataset, new_value) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_barchart_simple: Selection of '%s' dataset changes the element and does not throw validation errors",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_barchart_simple()
      plot_before <- app_driver$get_active_module_pws_output("myplot")
      app_driver$set_active_module_input(sprintf("%s-dataset", input_id), new_dataset)
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("myplot")))
      testthat::expect_null(app_driver$get_active_module_input(ns_dataset(input_id, "select", new_dataset)))
      app_driver$set_active_module_input(ns_dataset(input_id, "select", new_dataset), new_value)
      testthat::expect_identical(
        app_driver$get_active_module_input(ns_dataset(input_id, "select", new_dataset)),
        new_value
      )
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )

  testthat::test_that(
    sprintf(
      "%s: De-selection of '%s' dataset changes the element and does not throw validation errors",
      "e2e - tm_g_barchart_simple",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_barchart_simple()
      plot_before <- app_driver$get_active_module_pws_output("myplot")
      app_driver$set_active_module_input(sprintf("%s-dataset", input_id), character(0L))
      testthat::expect_null(app_driver$get_active_module_input(input_id))
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("myplot")))
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )
}

test_dataset_selection("fill", "ADAE", "AESER")
test_dataset_selection("x_facet", "ADSL", "RACE")
test_dataset_selection("y_facet", "ADSL", "ARM")

# Duplicate variables cannot be selected --------------------------------------

for (input_id in c("fill", "x_facet", "y_facet")) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_barchart_simple: Duplicate between 'x' and '%s' selection throws validation error",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_barchart_simple()
      app_driver$set_active_module_input(ns_dataset("x", "select", "ADSL"), "ACTARM", wait_ = FALSE)
      app_driver$set_active_module_input(sprintf("%s-dataset", input_id), "ADSL", wait_ = FALSE)
      app_driver$set_active_module_input(ns_dataset(input_id, "select", "ADSL"), "ACTARM")

      app_driver$expect_validation_error()

      testthat::expect_match(
        app_driver$active_module_element_text(
          sprintf(
            "%s .shiny-validation-message",
            ns_dataset("x", "select_input", "ADSL")
          )
        ),
        "^Duplicated value: ACTARM$"
      )

      testthat::expect_match(
        app_driver$active_module_element_text(
          sprintf(
            "%s .shiny-validation-message",
            ns_dataset(input_id, "select_input", "ADSL")
          )
        ),
        "^Duplicated value: ACTARM$"
      )
    }
  )
}

# Plot settings ---------------------------------------------------------------

test_that_plot_settings <- function(input_id, new_value, setup_fun = function(app_driver) NULL) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_barchart_simple: Changing '%s' changes the plot and does not throw validation errors.",
      id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_barchart_simple()
      setup_fun(app_driver)
      plot_before <- app_driver$get_active_module_pws_output("myplot")
      app_driver$set_active_module_input(input_id, new_value)
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_pws_output("myplot")))
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )
}

test_that_plot_settings("barlayout", "side_by_side")
test_that_plot_settings("expand_y_range", 0.9)
test_that_plot_settings("facet_scale_x", FALSE)
test_that_plot_settings("facet_scale_y", FALSE)
test_that_plot_settings("label_bars", TRUE)
test_that_plot_settings("rotate_x_label", FALSE)
test_that_plot_settings("rotate_y_label", FALSE)
test_that_plot_settings("flip_axis", FALSE)
test_that_plot_settings("show_n", TRUE)

# needs extra setup
test_that_plot_settings(
  "rotate_bar_labels",
  FALSE,
  setup_fun = function(app_driver) app_driver$set_active_module_input("label_bars", TRUE)
)
