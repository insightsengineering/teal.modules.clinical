skip("CI test")
testthat::test_that("template_tte produces healthy standard output", {
  result <- template_tte(
    dataname = "ANL",
    parentname = "ANL_ADSL",
    arm_var = "ARM",
    paramcd = "OS",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = FALSE,
    combine_comp_arms = FALSE,
    aval_var = "AVAL",
    cnsr_var = "CNSR",
    strata_var = NULL,
    time_points = c(183, 365, 548),
    time_unit_var = "AVALU",
    event_desc_var = "EVNTDESC",
    control = control_tte(
      coxph = control_coxph(),
      surv_time = control_surv_time(),
      surv_timepoint = control_surv_timepoint()
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_tte produces correct data expression when not comparing arms", {
  result <- template_tte(
    dataname = "ANL",
    parentname = "ANL_ADSL",
    arm_var = "ARM",
    paramcd = "OS",
    ref_arm = NULL,
    comp_arm = c("A: Drug X", "B: Placebo", "C: Combination"),
    compare_arm = FALSE,
    combine_comp_arms = TRUE,
    aval_var = "AVAL",
    cnsr_var = "CNSR",
    strata_var = NULL,
    time_points = c(183, 365, 548),
    time_unit_var = "AVALU",
    event_desc_var = "EVNTDESC",
    control = control_tte(
      coxph = control_coxph(),
      surv_time = control_surv_time(),
      surv_timepoint = control_surv_timepoint()
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_tte produces correct data expression when comparing and combining arms", {
  result <- template_tte(
    dataname = "ANL",
    parentname = "ANL_ADSL",
    arm_var = "ARM",
    paramcd = "OS",
    ref_arm = NULL,
    comp_arm = c("A: Drug X", "B: Placebo", "C: Combination"),
    compare_arm = TRUE,
    combine_comp_arms = TRUE,
    aval_var = "AVAL",
    cnsr_var = "CNSR",
    strata_var = NULL,
    time_points = c(183, 365, 548),
    time_unit_var = "AVALU",
    event_desc_var = "EVNTDESC",
    control = control_tte(
      coxph = control_coxph(),
      surv_time = control_surv_time(),
      surv_timepoint = control_surv_timepoint()
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_tte produces correct data expression when comparing arms", {
  result <- template_tte(
    dataname = "ANL",
    parentname = "ANL_ADSL",
    arm_var = "ARM",
    paramcd = "OS",
    ref_arm = NULL,
    comp_arm = c("A: Drug X", "B: Placebo", "C: Combination"),
    compare_arm = TRUE,
    combine_comp_arms = FALSE,
    aval_var = "AVAL",
    cnsr_var = "CNSR",
    strata_var = NULL,
    time_points = c(183, 365, 548),
    time_unit_var = "AVALU",
    event_desc_var = "EVNTDESC",
    control = control_tte(
      coxph = control_coxph(),
      surv_time = control_surv_time(),
      surv_timepoint = control_surv_timepoint()
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
