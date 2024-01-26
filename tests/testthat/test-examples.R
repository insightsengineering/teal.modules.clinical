files <- devtools:::rd_files()

# this is temporary
# every app tested with shinytest2 throws a warning because of https://github.com/insightsengineering/teal.code/issues/194
# silent certain warnings
suppress_warnings <- function(expr, pattern, ...) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl(pattern, conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

for (i in files) {
  with_mocked_bindings(
    test_that(
      paste0("example-", basename(i)),{
        skip_on_cran()
        expect_no_error(
          suppress_warnings( # temporary (see above)
            pkgload::run_example(i, run_donttest = TRUE, run_dontrun = FALSE, quiet = TRUE),
            "may not be available when loading"
          )
        )
      }
    ),
    runApp = \(appDir, ...) {
      cat("hello from mocked runApp\n")
      app <- shinytest2::AppDriver$new(appDir)
      app$stop()
    },
    .package = "shiny"
  )
}