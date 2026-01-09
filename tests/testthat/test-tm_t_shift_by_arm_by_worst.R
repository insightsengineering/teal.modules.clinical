skip("CI test")
testthat::test_that("template_shift_by_arm generates correct expressions with default arguments", {
  result <- template_shift_by_arm_by_worst(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    aval_var = "AVALC",
    baseline_var = "BASEC",
    worst_flag_var = "WORS02FL",
    worst_flag = "Y",
    na_level = "<Missing>"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_shift_by_arm generates correct expressions with add_total being TRUE", {
  result <- template_shift_by_arm_by_worst(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    aval_var = "AVALC",
    baseline_var = "BASEC",
    worst_flag_var = "WORS02FL",
    worst_flag = "Y",
    na_level = "<Missing>",
    add_total = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_shift_by_arm generates correct expressions with na.rm being TRUE", {
  result <- template_shift_by_arm_by_worst(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    aval_var = "AVALC",
    baseline_var = "BASEC",
    worst_flag_var = "WORS02FL",
    worst_flag = "Y",
    na.rm = TRUE,
    na_level = "<Missing>"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
