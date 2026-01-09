skip("CI test")
testthat::test_that("template_binary_outcome generates standard expressions", {
  result <- template_binary_outcome(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    paramcd = "BESRSPI",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    compare_arm = TRUE,
    combine_comp_arms = FALSE,
    aval_var = "AVALC",
    show_rsp_cat = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_binary_outcome generates right expressions with non-default", {
  result <- template_binary_outcome(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    paramcd = "BESRSPI",
    aval_var = "AVALC",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = TRUE,
    combine_comp_arms = FALSE,
    show_rsp_cat = FALSE,
    responder_val = c("PR", "SD")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_binary_outcome generates expression without arm comparison", {
  result <- template_binary_outcome(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    paramcd = "BESRSPI",
    aval_var = "AVALC",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = FALSE,
    show_rsp_cat = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_binary_outcome generates expression with non-default controls and strata.", {
  result <- template_binary_outcome(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    paramcd = "BESRSPI",
    aval_var = "AVALC",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = TRUE,
    show_rsp_cat = TRUE,
    control = list(
      global = list(method = "jeffreys", conf_level = 0.80),
      unstrat = list(
        method_ci = "ha", method_test = "chisq", odds = TRUE
      ),
      strat = list(method_ci = "cmh", method_test = "cmh", strat = "SEX")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_binary_outcome can combine comparison arms", {
  result <- template_binary_outcome(
    dataname = "adrs",
    parentname = "ADSL",
    arm_var = "ARMCD",
    paramcd = "BESRSPI",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    compare_arm = TRUE,
    combine_comp_arms = TRUE,
    aval_var = "AVALC",
    show_rsp_cat = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_binary_outcome can combine comparison arms when compare arms is FALSE", {
  result <- template_binary_outcome(
    dataname = "adrs",
    parentname = "ADSL",
    arm_var = "ARMCD",
    paramcd = "BESRSPI",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    compare_arm = FALSE,
    combine_comp_arms = TRUE,
    aval_var = "AVALC",
    show_rsp_cat = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("split_col_expr prepare the right four possible expressions", {
  result <- list(
    split_col_expr(compare = TRUE, combine = TRUE, arm_var = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = TRUE, combine = FALSE, arm_var = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = FALSE, combine = TRUE, arm_var = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = FALSE, combine = FALSE, arm_var = "ARMCD", ref = "ARM C")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_binary_outcome can combine refs", {
  result <- template_binary_outcome(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    paramcd = "BESRSPI",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = "ARM C",
    compare_arm = TRUE,
    combine_comp_arms = FALSE,
    show_rsp_cat = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
