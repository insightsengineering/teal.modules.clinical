skip("CI test")
testthat::test_that("template_coxreg generates correct univariate cox regression expressions", {
  result <- template_coxreg_u(
    dataname = "adrs",
    cov_var = NULL,
    arm_var = "ARMCD",
    cnsr_var = "CNSR",
    aval_var = "AVAL",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    paramcd = "OS",
    at = list(AGE = c(35, 45)),
    strata_var = "STRATA1",
    combine_comp_arms = FALSE,
    control = control_coxreg(
      pval_method = "wald",
      ties = "efron"
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_coxreg generates correct univariate cox regression expressions with interactions", {
  result <- template_coxreg_u(
    dataname = "adrs",
    cov_var = NULL,
    arm_var = "ARMCD",
    cnsr_var = "CNSR",
    aval_var = "AVAL",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    paramcd = "OS",
    at = list(AGE = c(35, 45)),
    strata_var = "STRATA1",
    combine_comp_arms = FALSE,
    control = control_coxreg(
      pval_method = "wald",
      ties = "efron",
      interaction = TRUE
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_coxreg generates correct multivariate cox regression expressions", {
  result <- template_coxreg_m(
    dataname = "adrs",
    cov_var = c("AGE", "SEX"),
    arm_var = "ARM",
    cnsr_var = "CNSR",
    aval_var = "AVAL",
    ref_arm = "A: Drug X",
    comp_arm = c("B: Placebo", "C: Combination"),
    paramcd = "OS",
    combine_comp_arms = TRUE,
    control = control_coxreg()
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
