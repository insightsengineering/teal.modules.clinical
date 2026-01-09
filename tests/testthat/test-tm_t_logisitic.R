testthat::test_that("template_logistic generates correct expressions", {
  result <- template_logistic(
    dataname = "ANL",
    arm_var = "ARMCD",
    aval_var = "AVALC",
    label_paramcd = "Best Confirmed Overall Response by Investigator",
    cov_var = c("AGE", "SEX"),
    interaction_var = "AGE",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = "ARM C",
    conf_level = 0.95,
    combine_comp_arms = FALSE,
    responder_val = c("CR"),
    topleft = "BESRSPI",
    at = c(30, 40)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_logistic generates correct expressions for no arm variable", {
  result <- template_logistic(
    dataname = "ANL",
    arm_var = NULL,
    aval_var = "AVALC",
    label_paramcd = "Best Confirmed Overall Response by Investigator",
    cov_var = c("AGE", "SEX"),
    interaction_var = "AGE",
    conf_level = 0.95,
    combine_comp_arms = FALSE,
    responder_val = c("CR"),
    topleft = "BESRSPI",
    at = c(30, 40)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_logistic generates correct expressions", {
  result <- template_logistic(
    dataname = "ANL",
    arm_var = "ARMCD",
    aval_var = "AVALC",
    label_paramcd = "Best Confirmed Overall Response by Investigator",
    cov_var = c("AGE", "SEX"),
    interaction_var = "AGE",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = "ARM C",
    conf_level = 0.95,
    combine_comp_arms = FALSE,
    responder_val = c("CR"),
    topleft = "BESRSPI",
    at = c(30, 40)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
