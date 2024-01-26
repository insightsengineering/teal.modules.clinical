files <- devtools:::rd_files()

for (i in files) {
  with_mocked_bindings(
    test_that(
        paste0("example-", i),{
            skip_on_cran()
            expect_no_error(
                pkgload::run_example(i, run_donttest = TRUE, run_dontrun = FALSE)
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