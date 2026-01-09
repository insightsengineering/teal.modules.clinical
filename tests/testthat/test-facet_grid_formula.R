skip("CI test")
testthat::test_that("facet_grid_formula works", {
  testthat::expect_equal(paste0(facet_grid_formula("SEX", NULL)), c("~", ".", "SEX"))
  testthat::expect_equal(paste0(facet_grid_formula("SEX", character(0))), c("~", ".", "SEX"))
  testthat::expect_equal(paste0(facet_grid_formula(NULL, "SEX")), c("~", "SEX", "."))
  testthat::expect_equal(paste0(facet_grid_formula("ACTARM", "SEX")), c("~", "SEX", "ACTARM"))
})
