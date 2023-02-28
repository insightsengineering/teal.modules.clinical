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
