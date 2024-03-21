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
          capture.output(
            # surpress warnings coming from saving qenv https://github.com/insightsengineering/teal.code/issues/194
            suppress_warnings(
              # quiet argument must be FALSE - otherwise the shiny apps are not invoked
              pkgload::run_example(i, run_donttest = TRUE, run_dontrun = FALSE, quiet = FALSE),
              "may not be available when loading"
            )
          )
        )
      }
    ),
    runApp = function(appDir, ...) { # nolint object_name_linter.
      app_driver <- shinytest2::AppDriver$new(appDir)
      on.exit(app_driver$stop)
      # shinytest2 will capture app crash but actually teal continues on error inside the module
      # we need to use a different way to check if there are errors
      if (!is.null(app_driver$get_html(".shiny-output-error:not(.shiny-output-error-validation)"))) {
        stop("module error is observed")
      }
      # validation errors from shinyvalidate - added by default to assure the examples are "clean"
      if (!is.null(app_driver$get_html(".shiny-input-container.has-error"))) {
        stop("shinyvalidate error is observed")
      }
    },
    .package = "shiny"
  )
}
