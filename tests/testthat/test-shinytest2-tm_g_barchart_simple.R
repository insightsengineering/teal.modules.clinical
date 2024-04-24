app_driver_tm_g_barchart_simple <- function() {
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
      x = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices("ADSL", c("ARM", "ACTARM", "SEX", "RACE", "ITTFL", "SAFFL", "STRATA2")),
          selected = "ACTARM",
          multiple = FALSE
        )
      ),
      fill = list(
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices("ADSL", c("ARM", "ACTARM", "SEX", "RACE", "ITTFL", "SAFFL", "STRATA2")),
            selected = "SEX",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADAE",
          select = select_spec(
            choices = variable_choices("ADAE", c("AETOXGR", "AESEV", "AESER")),
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      x_facet = list(
        data_extract_spec(
          dataname = "ADAE",
          select = select_spec(
            choices = variable_choices("ADAE", c("AETOXGR", "AESEV", "AESER")),
            selected = "AETOXGR",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices("ADSL", c("ARM", "ACTARM", "SEX", "RACE", "ITTFL", "SAFFL", "STRATA2")),
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      y_facet = list(
        data_extract_spec(
          dataname = "ADAE",
          select = select_spec(
            choices = variable_choices("ADAE", c("AETOXGR", "AESEV", "AESER")),
            selected = "AESEV",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices("ADSL", c("ARM", "ACTARM", "SEX", "RACE", "ITTFL", "SAFFL", "STRATA2")),
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
      )
    )
  )
}

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

    ns_dataset <- function(prefix, suffix, dataset, extract = "singleextract") {
      sprintf("%s-dataset_%s_%s-%s", prefix, dataset, extract, suffix)
    }

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
    testthat::expect_equal(app_driver$get_active_module_input("label_bars"), FALSE)
    testthat::expect_equal(app_driver$get_active_module_input("rotate_bar_labels"), TRUE)
    testthat::expect_equal(app_driver$get_active_module_input("rotate_x_label"), TRUE)
    testthat::expect_equal(app_driver$get_active_module_input("rotate_y_label"), TRUE)
    testthat::expect_equal(app_driver$get_active_module_input("flip_axis"), TRUE)
    testthat::expect_equal(app_driver$get_active_module_input("show_n"), FALSE)

    app_driver$stop()
  }
)
