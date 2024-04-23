app_driver_tm_t_events_summary <- function() {

}

testthat::test_that("e2e - tm_t_events_summary: Module initializes in teal without errors and produces plot output.", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_t_events_summary()
  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()

  app_driver$stop()
})
