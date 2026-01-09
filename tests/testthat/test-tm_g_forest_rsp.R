skip("CI test")
testthat::test_that("template_forest_rsp generates correct expressions", {
  result <- template_forest_rsp(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    aval_var = "AVALC",
    responders = c("CR", "PR"),
    subgroup_var = c("SEX", "STRATA2"),
    strata_var = NULL,
    conf_level = 0.95
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_forest_rsp works with risk difference column added", {
  result <- template_forest_rsp(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    aval_var = "AVALC",
    responders = c("CR", "PR"),
    subgroup_var = c("SEX", "STRATA2"),
    strata_var = NULL,
    stats = c("n_tot", "or", "ci"),
    riskdiff = control_riskdiff(col_label = "Prop. Diff"),
    conf_level = 0.95
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
