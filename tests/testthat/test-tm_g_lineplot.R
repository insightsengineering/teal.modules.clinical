skip("CI test")
testthat::test_that("template_g_lineplot works as expected with default arguments", {
  result <- template_g_lineplot()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_g_lineplot gives correct data expression with custom arguments", {
  result <- template_g_lineplot(
    group_var = "ARMCD",
    y = "CHG",
    mid = "median",
    interval = "median_ci",
    whiskers = "median_ci_upr",
    table = c("mean_sd", "median", "median_ci"),
    mid_type = "l",
    conf_level = 0.9,
    incl_screen = FALSE,
    title = "Line Plot"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
