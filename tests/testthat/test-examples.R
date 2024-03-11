# this test requires a `man` directory in the `tests/testthat` directory
# (presumably symlinked to the package root `man` directory to avoid duplication)
# this also requires `devtools::document()` to be run before running the tests

rd_files <- function() {
  list.files(testthat::test_path("man"), pattern = "\\.[Rr]d$", full.names = TRUE)
}

suppress_warnings <- function(expr, pattern = "*", ...) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl(pattern, conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

for (i in rd_files()) {
  testthat::with_mocked_bindings(
    testthat::test_that(
      paste0("example-", basename(i)),
      {
        testthat::skip_on_cran()
        testthat::expect_no_error(
          # surpress warnings coming from saving qenv https://github.com/insightsengineering/teal.code/issues/194
          suppress_warnings(
            pkgload::run_example(i, run_donttest = TRUE, run_dontrun = FALSE, quiet = TRUE),
            "may not be available when loading"
          )
        )
      }
    ),
    runApp = function(appDir, ...) { # nolint object_name_linter.
      app <- shinytest2::AppDriver$new(appDir)
      app$stop()
    },
    .package = "shiny"
  )
}
