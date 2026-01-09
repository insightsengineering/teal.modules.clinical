skip("CI test")
testthat::test_that("template_a_gee t_gee_cov table works as expected with default input", {
  result <- template_a_gee(
    output_table = "t_gee_cov",
    aval_var = "AVAL",
    id_var = "USUBJID",
    arm_var = "ARMCD",
    visit_var = "AVISIT",
    split_covariates = c(),
    cor_struct = "unstructured"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_a_gee t_gee_coef table works as expected with default input", {
  result <- template_a_gee(
    output_table = "t_gee_coef",
    aval_var = "AVAL",
    id_var = "USUBJID",
    arm_var = "ARMCD",
    visit_var = "AVISIT",
    split_covariates = c(),
    cor_struct = "unstructured"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_a_gee works as expected with non-default reference arm", {
  result <- template_a_gee(
    output_table = "t_gee_lsmeans",
    aval_var = "AVAL",
    id_var = "USUBJID",
    arm_var = "ARMCD",
    ref_group = "B: Placebo",
    visit_var = "AVISIT",
    split_covariates = c(),
    cor_struct = "unstructured"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_a_gee works as expected when arm is not considered in the model", {
  result <- template_a_gee(
    output_table = "t_gee_lsmeans",
    aval_var = "AVAL",
    id_var = "USUBJID",
    arm_var = NULL,
    ref_group = NULL,
    visit_var = "AVISIT",
    split_covariates = c(),
    cor_struct = "unstructured"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
