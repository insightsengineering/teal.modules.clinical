# Compare `Shiny.setInputValue` via `run_js` vs `AppDriver$set_inputs(..., priority_ = "event")`
# for the teal.picks commit pulse (selected + selected_open TRUE/FALSE).
#
# The first `test_that()` checks R/JS literal alignment without a browser. The second needs
# Chromote/Chrome; it is skipped unless `Sys.setenv(TEAL_PICKS_TEST_COMMIT_EQUIVALENCE = "true")`
# (then run `testthat::test_local()` / `devtools::test()`).

if (!exists(".teal_picks_shiny_set_picker_and_commit", inherits = TRUE)) {
  source(file.path(testthat::test_path(), "helper-TealAppDriver.R"), local = TRUE)
}

normalize_pick_test_input <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x) && length(x) == 0L) {
    return(character(0))
  }
  if (is.logical(x) && length(x) == 1L) {
    return(as.logical(x))
  }
  if (is.character(x)) {
    return(as.character(unname(x)))
  }
  x
}

snapshot_commit_inputs <- function(val, commit_with) {
  checkmate::assert_choice(commit_with, c("run_js", "set_inputs"))

  dir <- tempfile("teal_picks_commit_test")
  dir.create(dir)
  writeLines(
    c(
      "library(shiny)",
      "ui <- fluidPage()",
      "server <- function(input, output, session) {}",
      "shinyApp(ui, server)"
    ),
    file.path(dir, "app.R")
  )

  app <- shinytest2::AppDriver$new(dir, load_timeout = 120 * 1000, timeout = 120 * 1000)
  on.exit(app$stop(), add = TRUE)

  sel_id <- "teal_picks_test_sel"
  open_id <- "teal_picks_test_open"

  .teal_picks_shiny_set_picker_and_commit(
    app,
    sel_id = sel_id,
    open_id = open_id,
    val = val,
    commit_with = commit_with
  )

  inputs <- app$get_values()$input
  list(
    sel = normalize_pick_test_input(inputs[[sel_id]]),
    open = normalize_pick_test_input(inputs[[open_id]])
  )
}

testthat::test_that(".teal_picks_shiny_selected_value_for_set_inputs matches setInput JS literal cases", {
  v0 <- character(0)
  testthat::expect_equal(
    .teal_picks_shiny_selected_value_for_set_inputs(v0),
    v0
  )
  testthat::expect_equal(
    .teal_picks_shiny_setinput_value_literal(v0),
    "[]"
  )

  testthat::expect_equal(
    .teal_picks_shiny_selected_value_for_set_inputs("ARM"),
    "ARM"
  )
  testthat::expect_equal(
    .teal_picks_shiny_setinput_value_literal("ARM"),
    "\"ARM\""
  )

  v2 <- c("ARM", "AGE")
  testthat::expect_equal(
    .teal_picks_shiny_selected_value_for_set_inputs(v2),
    v2
  )
  testthat::expect_equal(
    .teal_picks_shiny_setinput_value_literal(v2),
    "[\"ARM\",\"AGE\"]"
  )
})

testthat::test_that("teal.picks commit: run_js and set_inputs yield the same Shiny input snapshot", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()
  if (!identical(Sys.getenv("TEAL_PICKS_TEST_COMMIT_EQUIVALENCE", "false"), "true")) {
    testthat::skip("Set TEAL_PICKS_TEST_COMMIT_EQUIVALENCE=true to run (requires Chromote/Chrome).")
  }

  cases <- list(
    empty = character(0),
    one = "ARM",
    multi = c("ARM", "AGE")
  )

  for (nm in names(cases)) {
    val <- cases[[nm]]
    snap_js <- snapshot_commit_inputs(val, "run_js")
    snap_si <- snapshot_commit_inputs(val, "set_inputs")
    testthat::expect_equal(snap_js, snap_si, label = nm)
  }
})
