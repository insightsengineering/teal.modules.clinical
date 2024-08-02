testthat::test_that("template_forest_tte generates correct expressions", {
  result <- template_forest_tte(
    dataname = "adtte",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    subgroup_var = c("SEX", "BMRKR2"),
    strata_var = "STRATA2",
    conf_level = 0.90,
    col_symbol_size = NULL
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_forest_tte works with risk difference column added", {
  result <- template_forest_tte(
    dataname = "adtte",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    subgroup_var = c("SEX", "BMRKR2"),
    strata_var = "STRATA2",
    stats = c("n_tot", "hr", "ci"),
    riskdiff = control_riskdiff(col_label = "Prop. Diff"),
    conf_level = 0.90,
    col_symbol_size = NULL
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
