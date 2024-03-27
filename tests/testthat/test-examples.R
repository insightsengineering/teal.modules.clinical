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

with_mocked_app_bindings <- function(code) {
  shiny__shinyApp <- shiny::shinyApp # nolint object_name_linter.
  mocked_shinyApp <- function(...) { # nolint object_name_linter.
    print(shiny__shinyApp(...))
  }

  mocked_runApp <- function(x, ...) { # nolint object_name_linter.
    # suppress warnings coming from saving qenv https://github.com/insightsengineering/teal.code/issues/194
    suppress_warnings(
      app_driver <- shinytest2::AppDriver$new(
        x,
        shiny_args = list(...),
        check_names = FALSE, # explicit check below
        options = options() # https://github.com/rstudio/shinytest2/issues/377
      ),
      "may not be available when loading"
    )
    on.exit(app_driver$stop(), add = TRUE)
    app_driver$wait_for_idle(timeout = 20000)

    # Simple testing
    ## warning in the app does not invoke a warning in the test
    ## https://github.com/rstudio/shinytest2/issues/378
    app_logs <- subset(app_driver$get_logs(), location == "shiny")[["message"]]
    if (any(grepl("Warning in.*", app_logs))) {
      warning(
        sprintf(
          "Detected following warning(s) (a message might be incomplete):\n%s",
          paste0("* ", grep("Warning in.*", app_logs, value = TRUE), collapse = "\n")
        )
      )
    }

    ## Throw an error instead of a warning (default `AppDriver$new(..., check_names = TRUE)` throws a warning)
    app_driver$expect_unique_names()

    ## shinytest2 captures app crash but teal continues on error inside the module
    ## we need to use a different way to check if there are errors
    if (!is.null(err_el <- app_driver$get_html(".shiny-output-error"))) {
      stop(sprintf("Module error is observed:\n%s", err_el))
    }

    ## validation errors from shinyvalidate - added by default to assure the examples are "clean"
    if (!is.null(err_el <- app_driver$get_html(".shiny-input-container.has-error:not(.shiny-output-error-validation)"))) { # nolint line_length_linter.
      stop(sprintf("shinyvalidate error is observed:\n%s", err_el))
    }
  }

  # mock both local and package bindings to cover both `shinyApp(...)` and `shiny::shinyApp(...)` calls
  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      code,
      shinyApp = shiny::shinyApp,
      runApp = shiny::runApp,
      interactive = function() TRUE
    ),
    shinyApp = mocked_shinyApp,
    runApp = mocked_runApp,
    .package = "shiny"
  )
}

strict_exceptions <- c(
  # https://github.com/r-lib/gtable/pull/94
  "tm_g_barchart_simple.Rd",
  "tm_g_ci.Rd",
  "tm_g_ipp.Rd",
  "tm_g_pp_adverse_events.Rd",
  "tm_g_pp_vitals.Rd"
)

for (i in rd_files()) {
  with_mocked_app_bindings(
    testthat::test_that(
      paste0("example-", basename(i)),
      {
        testthat::skip_on_cran()
        if (basename(i) %in% strict_exceptions) {
          withr::with_options(
            opts_partial_match_old,
            testthat::expect_no_error(
              pkgload::run_example(i, run_donttest = TRUE, run_dontrun = FALSE, quiet = TRUE)
            )
          )
        } else {
          testthat::expect_no_error(
            pkgload::run_example(i, run_donttest = TRUE, run_dontrun = FALSE, quiet = TRUE)
          )
        }
      }
    )
  )
}
