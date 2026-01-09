skip("CI test")
testthat::test_that("template_summary generates correct expressions", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARM",
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    add_total = FALSE,
    var_labels = character(),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary can generate customized table", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    sum_vars = "RACE",
    add_total = TRUE,
    var_labels = c(RACE = "Race"),
    na.rm = TRUE,
    denominator = "omit",
    drop_arm_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary generates correct expressions for multiple grouping variables", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = c("ARM", "STRATA1"),
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    add_total = FALSE,
    var_labels = character(),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "template_summary generates correct expressions for multiple grouping variables and all patients",
  {
    result <- template_summary(
      dataname = "adrs",
      parentname = "adsl",
      arm_var = c("ARM", "STRATA1"),
      sum_vars = c("RACE", "COUNTRY", "AGE"),
      add_total = TRUE,
      var_labels = character(),
      na.rm = FALSE,
      denominator = "N",
      drop_arm_levels = TRUE
    )

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("template_summary generates correct expressions for customized numeric statistics", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = c("ARM", "STRATA1"),
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    add_total = FALSE,
    var_labels = character(),
    na.rm = FALSE,
    numeric_stats = c("n"),
    denominator = "N",
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary generates correct expressions when arm variable labels are added", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = c("ARM", "SEX"),
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    arm_var_labels = c("Arm", "Sex")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
