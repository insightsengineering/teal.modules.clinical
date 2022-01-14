test_that("glue_with_null works", {
  expect_equal(glue_with_null("Keep null: {NULL}"), "Keep null: NULL")
  expect_equal(glue_with_null("Keep null: {NULL}"), glue::glue("Keep null: {replace_null(NULL)}"))
  expect_equal(glue_with_null("Keep null: {NULL}, 1, {NULL}"), "Keep null: NULL, 1, NULL")

  expect_equal(glue_with_null("This is null: {NULL}", null_replacement = "NULL quoted"), "This is null: NULL quoted")
})


test_that(
  "`as_num` as a wide interpretation of input to extract numerics.",
  code = {
    dta <- list(
      character = c("text10,20.5letter30.!", "!-.40$$-50e5[", NA),
      factor    = factor(c("]+60e-6, 7.7%%8L", "%90sep.100\"1L", NA_character_)),
      numeric   = c(1, -5e+2, NA),
      logical   = c(TRUE, FALSE, NA)
    )

    expected <- list(
      character = list(c(10, 20.5, 30), c(-.4, -50e5), NA),
      factor    = list(c(60e-6, 7.7, 8), c(90, .1, 1), NA),
      numeric   = c(1, -5e+2, NA),
      logical   = c(1, 0, NA)
    )

    test <- lapply(dta, as_num)
    test <- lapply(
      X = list(output = test, reference = expected),
      FUN = function(x) {
        y <- unlist(x)
        y <- y[!is.na(y)]
        return(y)
      }
    )

    testthat::expect_equal(test$output, test$reference)
  }
)
