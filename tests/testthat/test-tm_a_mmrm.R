skip("CI test")
testthat::test_that("template_fit_mmrm works as expected when not combining comparison arms", {
  result <- template_fit_mmrm(
    parentname = "adsl",
    dataname = "adqs",
    aval_var = "AVAL",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    id_var = "USUBJID",
    visit_var = "AVISIT",
    cov_var = c()
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_fit_mmrm works as expected when combining combination arms", {
  result <- template_fit_mmrm(
    parentname = "adsl",
    dataname = "adqs",
    aval_var = "AVAL",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = TRUE,
    id_var = "USUBJID",
    visit_var = "AVISIT",
    cov_var = c("SEX", "BASE", "AVISIT"),
    parallel = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_mmrm_tables works as expected", {
  result <- template_mmrm_tables(
    parentname = "ADSL",
    dataname = "ANL",
    fit_name = "fit_mmrm",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    visit_var = "AVISIT",
    paramcd = "ALBUMIN",
    show_relative = "increase"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_mmrm_tables works as expected when arm is not considered in the model", {
  result <- template_mmrm_tables(
    parentname = "ADSL",
    dataname = "ANL",
    fit_name = "fit_mmrm",
    arm_var = NULL,
    ref_arm = NULL,
    visit_var = "AVISIT",
    paramcd = "ALBUMIN",
    show_relative = NULL
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_mmrm_plots works as expected", {
  result <- template_mmrm_plots(
    "fit_mmrm",
    lsmeans_plot = list(
      select = c("estimates", "contrasts"),
      width = 0.6,
      show_pval = FALSE
    ),
    diagnostic_plot = list(
      type = "fit-residual",
      z_threshold = NULL
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
