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
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]

  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_barchart_simple(
        label = "ADAE Analysis (e-2-e)",
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
  )
}

# Initialization --------------------------------------------------------------

testthat::test_that("e2e - tm_g_barchart_simple: Module initializes in teal without errors and produces output.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_barchart_simple()
  app_driver$wait_for_idle()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  testthat::expect_match(
    app_driver$get_active_module_plot_output("myplot"),
    "data:image/png;base64,"
  )

  # Table is rendered asynchronously, wait a bit more
  app_driver$wait_for_idle()
  app_driver$expect_visible(app_driver$namespaces(TRUE)$module("table > table"))

  app_driver$stop()
})

testthat::test_that(
  "e2e - tm_g_barchart_simple: Starts with specified label, id_var, arm_var, visit_var,
  paramcd, cov_var, conf_level and conf_struct.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_barchart_simple()
    app_driver$wait_for_idle()

    testthat::expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "ADAE Analysis (e-2-e)"
    )

    testthat::expect_equal(get_teal_picks_slot(app_driver, "x", "datasets"), "ADSL")
    testthat::expect_equal(
      teal_picks_strip_ds_prefix_vec(get_teal_picks_slot(app_driver, "x", "variables")),
      "ACTARM"
    )

    testthat::expect_equal(get_teal_picks_slot(app_driver, "fill", "datasets"), "ADSL")
    testthat::expect_equal(
      teal_picks_strip_ds_prefix_vec(get_teal_picks_slot(app_driver, "fill", "variables")),
      "SEX"
    )

    testthat::expect_equal(get_teal_picks_slot(app_driver, "x_facet", "datasets"), "ADAE")
    testthat::expect_equal(
      teal_picks_strip_ds_prefix_vec(get_teal_picks_slot(app_driver, "x_facet", "variables")),
      "AETOXGR"
    )

    testthat::expect_equal(get_teal_picks_slot(app_driver, "y_facet", "datasets"), "ADAE")
    testthat::expect_equal(
      teal_picks_strip_ds_prefix_vec(get_teal_picks_slot(app_driver, "y_facet", "variables")),
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
  "e2e - tm_g_barchart_simple: Selection of 'x' changes the element and does not throw validation errors.",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_barchart_simple()
    app_driver$wait_for_idle()
    plot_before <- app_driver$get_active_module_plot_output("myplot")
    set_teal_picks_slot(app_driver, "x", "variables", "RACE")
    testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
    app_driver$expect_no_validation_error()
    app_driver$stop()
  }
)

testthat::test_that("e2e - tm_g_barchart_simple: Deselection of 'x' throws validation error.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_barchart_simple()
  app_driver$wait_for_idle()
  set_teal_picks_slot(app_driver, "x", "variables", character(0L))
  app_driver$expect_validation_error()
  app_driver$stop()
})

# Test pairs of dataset selection ---------------------------------------------

test_dataset_selection <- function(input_id, new_dataset, new_value) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_barchart_simple: Selection of '%s' dataset changes the element and does not throw validation errors.",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_barchart_simple()
      app_driver$wait_for_idle()
      plot_before <- app_driver$get_active_module_plot_output("myplot")
      set_teal_picks_slot(app_driver, input_id, "datasets", new_dataset)
      set_teal_picks_slot(app_driver, input_id, "variables", new_value)
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
      testthat::expect_equal(get_teal_picks_slot(app_driver, input_id, "datasets"), new_dataset)
      testthat::expect_equal(
        teal_picks_strip_ds_prefix_vec(get_teal_picks_slot(app_driver, input_id, "variables")),
        new_value
      )
      app_driver$expect_no_validation_error()
      app_driver$stop()
    }
  )

  testthat::test_that(
    sprintf(
      "%s: Deselection of '%s' dataset changes the element and does not throw validation errors.",
      "e2e - tm_g_barchart_simple",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_barchart_simple()
      app_driver$wait_for_idle()
      plot_before <- app_driver$get_active_module_plot_output("myplot")
      set_teal_picks_slot(app_driver, input_id, "datasets", character(0L))
      app_driver$wait_for_idle()
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
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
      "e2e - tm_g_barchart_simple: Duplicate between 'x' and '%s' selection throws validation error.",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_barchart_simple()
      app_driver$wait_for_idle()
      # Align x with ADSL + ACTARM, then pick the same column on another encoding (also on ADSL).
      set_teal_picks_slot(app_driver, "x", "datasets", "ADSL", wait = FALSE)
      set_teal_picks_slot(app_driver, "x", "variables", "ACTARM", wait = TRUE)
      set_teal_picks_slot(app_driver, input_id, "datasets", "ADSL", wait = FALSE)
      set_teal_picks_slot(app_driver, input_id, "variables", "ACTARM", wait = TRUE)

      app_driver$expect_validation_error()
      app_driver$stop()
    }
  )
}

# Plot settings ---------------------------------------------------------------

test_that_plot_settings <- function(input_id, new_value, setup_fun = function(app_driver) NULL) {
  testthat::test_that(
    sprintf(
      "e2e - tm_g_barchart_simple: Changing '%s' changes the plot and does not throw validation errors.",
      input_id
    ),
    {
      skip_if_too_deep(5)
      app_driver <- app_driver_tm_g_barchart_simple()
      app_driver$wait_for_idle()
      setup_fun(app_driver)
      app_driver$wait_for_idle()
      plot_before <- app_driver$get_active_module_plot_output("myplot")
      app_driver$set_active_module_input(input_id, new_value)
      app_driver$wait_for_idle()
      testthat::expect_false(identical(plot_before, app_driver$get_active_module_plot_output("myplot")))
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
  setup_fun = function(app_driver) {
    app_driver$set_active_module_input("label_bars", TRUE)
    NULL
  }
)
