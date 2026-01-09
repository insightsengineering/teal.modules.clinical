skip("CI test")
testthat::test_that("template_abnormality_by_worst_grade generates correct expressions with default arguments", {
  result <- template_abnormality_by_worst_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARMCD",
    id_var = "USUBJID",
    paramcd = "PARAMCD",
    atoxgr_var = "ATOXGR",
    worst_high_flag_var = "WGRHIFL",
    worst_low_flag_var = "WGRLOFL",
    worst_flag_indicator = "Y",
    add_total = FALSE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_abnormality_by_worst_grade generates correct expressions with custom arguments", {
  result <- template_abnormality_by_worst_grade(
    parentname = "myadsl",
    dataname = "myadlb",
    arm_var = "ARMCD",
    id_var = "USUBJID",
    paramcd = "myPARAMCD",
    atoxgr_var = "ATOXGR",
    worst_high_flag_var = "WGRHIFL",
    worst_low_flag_var = "WGRLOFL",
    worst_flag_indicator = "Y",
    add_total = FALSE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_abnormality_by_worst_grade throws an error when ATOXGR contains NA values", {
  adsl <- tmc_ex_adsl
  adlb <- tmc_ex_adlb
  adlb$ATOXGR[1:100] <- NA

  template <- template_abnormality_by_worst_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARMCD",
    id_var = "USUBJID",
    paramcd = "PARAMCD",
    atoxgr_var = "ATOXGR",
    worst_high_flag_var = "WGRHIFL",
    worst_low_flag_var = "WGRLOFL",
    worst_flag_indicator = "Y",
    add_total = FALSE,
    drop_arm_levels = TRUE
  )

  testthat::expect_error(mapply(eval, template))
})
