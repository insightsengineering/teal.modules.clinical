# Tests for teal.picks commit via `AppDriver$set_inputs(..., priority_ = "event")`.
#
# The first `test_that()` checks selected-value shaping without a browser. The second needs
# Chromote/Chrome; it follows the same `skip_if_too_deep()` pattern as other shinytest2 tests.

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

snapshot_commit_inputs <- function(val) {
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
    val = val
  )

  inputs <- app$get_values()$input
  list(
    sel = normalize_pick_test_input(inputs[[sel_id]]),
    open = normalize_pick_test_input(inputs[[open_id]])
  )
}

testthat::test_that(".teal_picks_shiny_selected_value_for_set_inputs maps empty / scalar / vector", {
  v0 <- character(0)
  testthat::expect_equal(
    .teal_picks_shiny_selected_value_for_set_inputs(v0),
    v0
  )

  testthat::expect_equal(
    .teal_picks_shiny_selected_value_for_set_inputs("ARM"),
    "ARM"
  )

  v2 <- c("ARM", "AGE")
  testthat::expect_equal(
    .teal_picks_shiny_selected_value_for_set_inputs(v2),
    v2
  )
})

testthat::test_that("teal.picks commit via set_inputs sets Shiny inputs as expected", {
  testthat::skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)

  cases <- list(
    empty = list(
      val = character(0),
      expected = list(sel = NULL, open = FALSE)
    ),
    one = list(
      val = "ARM",
      expected = list(sel = "ARM", open = FALSE)
    ),
    multi = list(
      val = c("ARM", "AGE"),
      expected = list(sel = c("ARM", "AGE"), open = FALSE)
    )
  )

  for (nm in names(cases)) {
    snap <- snapshot_commit_inputs(cases[[nm]]$val)
    testthat::expect_equal(snap, cases[[nm]]$expected, label = nm)
  }
})
