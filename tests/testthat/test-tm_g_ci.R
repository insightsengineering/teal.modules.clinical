# Test correspond to sections in the TLG catalog.
testthat::test_that("1. and 2. Mean and 95% CIs for mean", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "SEX",
    stat = "mean"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
  # Check the output.
  # eval(result) ; gg # nolint: commented_code.
})

testthat::test_that("3. Confidence Interval Plot (using different stratification variable)", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "STRATA2",
    stat = "mean"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
  # Check the output.
  # eval(result) ; gg # nolint: commented_code.
})

testthat::test_that("4. Median and 95% CIs for median", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "STRATA1",
    stat = "median"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
  # Check the output.
  # eval(result) ; gg # nolint: commented_code.
})

testthat::test_that("5. Using different alpha level", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "SEX",
    stat = "mean",
    conf_level = 0.90
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
  # Check the output.
  # eval(result) ; gg # nolint: commented_code.
})
