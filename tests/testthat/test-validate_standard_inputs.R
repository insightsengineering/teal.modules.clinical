skip("CI test")
testthat::test_that("validate_arm throws shiny error if arm_vec is not a factor", {
  testthat::expect_error(
    validate_arm(1:10),
    "Treatment variable is not a factor",
    class = "shiny.silent.error"
  )

  testthat::expect_error(
    validate_arm(c("A", "B", "C")),
    "Treatment variable is not a factor",
    class = "shiny.silent.error"
  )
})

testthat::test_that("validate_arm throws specific shiny error if arm_vec has just whitespace strings", {
  testthat::expect_error(
    validate_arm(as.factor(c("A", "   "))),
    "Treatment values cannot contain empty strings",
    class = "shiny.silent.error"
  )

  testthat::expect_error(
    validate_arm(as.factor(c("A", ""))),
    "Treatment values cannot contain empty strings",
    class = "shiny.silent.error"
  )
})

testthat::test_that("validate_arm throws no error if arm_vec is a factor", {
  testthat::expect_no_error(
    validate_arm(factor(c("A", "B"), levels = c("A", "B")))
  )

  testthat::expect_no_error(
    validate_arm(factor(c("A", "B"), levels = c("A", "B", "C")))
  )
})
