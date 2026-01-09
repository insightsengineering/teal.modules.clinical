skip("CI test")
testthat::test_that("template_g_ipp works as expected with default arguments", {
  result <- template_g_ipp(
    paramcd = "PARAMCD", arm_var = "ARMCD",
    arm_levels = letters[1:3], avalu_first = letters[4], paramcd_first = letters[5]
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_g_ipp works as expected with non-default arguments", {
  result <- template_g_ipp(
    dataname = "adlb",
    paramcd = "PARAM",
    arm_levels = letters[1:3],
    avalu_first = letters[4],
    paramcd_first = letters[5],
    aval_var = "AVAL",
    avalu_var = "AVALU",
    arm_var = "ARMCD",
    id_var = "SUBJID",
    visit_var = "AVISIT",
    baseline_var = "BASE",
    add_baseline_hline = TRUE,
    separate_by_obs = TRUE,
    add_avalu = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
