skip("CI test")
testthat::test_that("template_g_km works as expected with default arguments", {
  result <- template_g_km(comp_arm = c("ARM A", "ARM B"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_g_km gives correct data expression when we want to compare versus reference arms", {
  result <- template_g_km(
    comp_arm = c("ARM A", "ARM B"),
    ref_arm = c("ARM C", "ARM D"),
    compare_arm = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_g_km gives correct data expression when we want to combine comparison arms", {
  result <- template_g_km(
    comp_arm = c("ARM A", "ARM B"),
    ref_arm = c("ARM C", "ARM D"),
    compare_arm = TRUE,
    combine_comp_arms = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
