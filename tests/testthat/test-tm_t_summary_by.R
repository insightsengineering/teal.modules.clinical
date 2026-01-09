skip("CI test")
testthat::test_that("template_summary_by generates correct expressions", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions when `parallel_vars` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL", "CHG"),
    add_total = TRUE,
    parallel_vars = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = FALSE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions when `row_groups` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = FALSE,
    parallel_vars = FALSE,
    row_groups = TRUE,
    by_vars = c("SEX", "COUNTRY"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions for customized numeric statistics", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    numeric_stats = c("n"),
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions for `drop_zero_levels` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
