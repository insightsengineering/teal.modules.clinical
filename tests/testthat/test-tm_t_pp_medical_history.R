skip("CI test")
testthat::test_that("template_medical_history outputs a list with one named element", {
  testthat::expect_silent(res <- template_medical_history("ANL", "mhterm", "mhbodsys", "mhdistat"))
  testthat::expect_true(is.list(res))
  testthat::expect_length(res, 1)
  testthat::expect_false(is.null(names(res)))
})

testthat::test_that("template_medical_history's output has element table, which is of class call", {
  testthat::expect_silent(res <- template_medical_history("ANL", "mhterm", "mhbodsys", "mhdistat"))
  testthat::expect_true(is.call(res$table))
})

testthat::test_that("template_medical_history - non-default parameters", {
  testthat::expect_silent(res <- template_medical_history("anl", "mhterm", "mhbodsys", "mhdistat"))

  res <- testthat::expect_silent(res$table)
  testthat::expect_snapshot(res)
})
