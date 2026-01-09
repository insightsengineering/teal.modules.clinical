testthat::test_that("template_shift_by_arm generates correct expressions with default arguments", {
  result <- template_shift_by_arm(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    visit_var = "AVISIT",
    aval_var = "ANRIND",
    baseline_var = "BNRIND",
    na_level = "<Missing>"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_shift_by_arm generates correct expressions with add_total being TRUE", {
  result <- template_shift_by_arm(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    visit_var = "AVISIT",
    aval_var = "ANRIND",
    baseline_var = "BNRIND",
    na_level = "<Missing>",
    add_total = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_shift_by_arm generates correct expressions with na.rm being TRUE", {
  result <- template_shift_by_arm(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    visit_var = "AVISIT",
    aval_var = "ANRIND",
    baseline_var = "BNRIND",
    na.rm = TRUE,
    na_level = "<Missing>"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
