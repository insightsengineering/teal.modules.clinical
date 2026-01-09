skip("CI test")
testthat::test_that("template_ancova generates expressions with multiple endpoints", {
  result <- template_ancova(
    dataname = "adqs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    cov_var = c("BASE", "STRATA1"),
    paramcd_levels = c("FKSI-FWB", "BFIALL"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("Function/Well-Being (GF1,GF3,GF7)", "BFI All Questions"),
    visit_var = "AVISIT",
    visit_levels = "WEEK 1 DAY 8"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_ancova generates expressions with multiple endpoints with combined comparison arms", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = TRUE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    paramcd_levels = c("A", "B"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("A", "B"),
    visit_var = "AVISIT",
    visit_levels = "WEEK 1 DAY 8"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_ancova generates expressions with multiple endpoints with combined reference arms", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    arm_var = "ARMCD",
    ref_arm = c("ARM B", "ARM C"),
    comp_arm = "ARM A",
    combine_comp_arms = FALSE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    paramcd_levels = c("A", "B"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("A", "B"),
    visit_var = "AVISIT",
    visit_levels = "WEEK 2 DAY 1"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_ancova generates expressions with single endpoint", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    paramcd_levels = c("MYFAVORITE"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("MYFAVORITE"),
    visit_var = "AVISIT",
    visit_levels = ""
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_ancova generates expressions with multiple endpoints", {
  result <- template_ancova(
    dataname = "adqs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    cov_var = c("BASE", "STRATA1"),
    include_interact = TRUE,
    interact_var = "SEX",
    interact_y = "M",
    paramcd_levels = c("FKSI-FWB", "BFIALL"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("Function/Well-Being (GF1,GF3,GF7)", "BFI All Questions"),
    visit_var = "AVISIT",
    visit_levels = "WEEK 1 DAY 8"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_ancova generates expressions with discrete interaction variable", {
  result <- template_ancova(
    dataname = "adqs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    cov_var = c("BASE", "STRATA1"),
    include_interact = TRUE,
    interact_var = "SEX",
    interact_y = "M",
    paramcd_levels = c("FKSI-FWB", "BFIALL"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("Function/Well-Being (GF1,GF3,GF7)", "BFI All Questions"),
    visit_var = "AVISIT",
    visit_levels = "WEEK 1 DAY 8"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_ancova generates expressions with continuous interaction variable", {
  result <- template_ancova(
    dataname = "adqs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    cov_var = c("BASE", "STRATA1"),
    include_interact = TRUE,
    interact_var = "BASE",
    paramcd_levels = c("FKSI-FWB", "BFIALL"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("Function/Well-Being (GF1,GF3,GF7)", "BFI All Questions"),
    visit_var = "AVISIT",
    visit_levels = "WEEK 1 DAY 8"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
