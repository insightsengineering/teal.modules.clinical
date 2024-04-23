app_driver_tm_g_km <- function() {

}

testthat::test_that("e2e - tm_g_km: module initializes in teal without errors and produces plot output", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_km()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  testthat::expect_match(
    app_driver$get_attr(
      app_driver$active_module_element("myplot-plot_main > img"),
      "src"
    ),
    "data:image/png;base64,"
  )
  app_driver$stop()
})
