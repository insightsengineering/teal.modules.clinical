skip("CI test")
testthat::test_that("template_shift_by_grade generates correct expressions with default arguments", {
  result <- template_shift_by_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    visit_var = "AVISIT",
    worst_flag_var = c("WGRLOVFL"),
    worst_flag_indicator = "Y",
    anl_toxgrade_var = "ATOXGR",
    base_toxgrade_var = "BTOXGR",
    paramcd = "PARAMCD",
    drop_arm_levels = TRUE,
    add_total = FALSE,
    na_level = "<Missing>",
    code_missing_baseline = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_shift_by_grade generates correct expressions with custom arguments", {
  result <- template_shift_by_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "MYUSUBJID",
    visit_var = "AVISIT",
    worst_flag_var = c("WGRLOVFL"),
    worst_flag_indicator = "YY",
    anl_toxgrade_var = "MYATOXGR",
    base_toxgrade_var = "MYBTOXGR",
    paramcd = "PARAMCD",
    drop_arm_levels = TRUE,
    add_total = FALSE,
    na_level = "<MYMissing>",
    code_missing_baseline = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "template_shift_by_grade throws an error when worst_flag_var is not one of WGRLOVFL, WGRLOFL, WGRHIVFL, WGRHIFL",
  {
    testthat::expect_error(
      result <- template_shift_by_grade(
        parentname = "adsl",
        dataname = "adlb",
        arm_var = "ARM",
        id_var = "USUBJID",
        visit_var = "AVISIT",
        worst_flag_var = c("another_value"),
        worst_flag_indicator = "Y",
        anl_toxgrade_var = "ATOXGR",
        base_toxgrade_var = "BTOXGR",
        paramcd = "PARAMCD",
        drop_arm_levels = TRUE,
        add_total = FALSE,
        na_level = "<Missing>",
        code_missing_baseline = FALSE
      )
    )
  }
)

testthat::test_that(
  "template_shift_by_grade keeps the same number of missing data ('<Missing>') after preprocessing",
  {
    adsl <- tmc_ex_adsl
    adlb <- tmc_ex_adlb %>% dplyr::filter(WGRLOVFL == "Y")
    adlb$ATOXGR[1] <- NA
    adlb <- df_explicit_na(adlb)
    expected_missing_n <- sum(adlb$ATOXGR == "<Missing>")
    template <- template_shift_by_grade(
      parentname = "adsl",
      dataname = "adlb",
      arm_var = "ARM",
      id_var = "USUBJID",
      visit_var = "AVISIT",
      worst_flag_var = c(c("WGRLOVFL")),
      worst_flag_indicator = "Y",
      anl_toxgrade_var = "ATOXGR",
      base_toxgrade_var = "BTOXGR",
      paramcd = "PARAMCD",
      drop_arm_levels = TRUE,
      add_total = FALSE,
      na_level = "<Missing>",
      code_missing_baseline = FALSE
    )

    template_data <- template$data
    data <- eval(template_data)
    result_missing_n <- sum(data$ATOXGR_GP == "Missing")

    testthat::expect_equal(expected_missing_n, result_missing_n)
  }
)

testthat::test_that(
  "template_shift_by_grade handles empty dataset after worst flag filtering gracefully",
  {
    adsl <- tmc_ex_adsl
    adlb <- tmc_ex_adlb
    # Set all worst flag values to "N" to simulate no matching records
    adlb$WGRLOVFL <- "N"
    template <- template_shift_by_grade(
      parentname = "adsl",
      dataname = "adlb",
      arm_var = "ARM",
      id_var = "USUBJID",
      visit_var = "AVISIT",
      worst_flag_var = c("WGRLOVFL"),
      worst_flag_indicator = "Y", # Looking for "Y" but all are "N"
      anl_toxgrade_var = "ATOXGR",
      base_toxgrade_var = "BTOXGR",
      paramcd = "PARAMCD",
      drop_arm_levels = TRUE,
      add_total = FALSE,
      na_level = "<Missing>",
      code_missing_baseline = FALSE
    )

    template_data <- template$data
    # Should not hang and produce an empty dataset
    data <- eval(template_data)
    testthat::expect_equal(nrow(data), 0)
  }
)

testthat::test_that(
  "template_shift_by_grade handles invalid worst flag indicator values",
  {
    adsl <- tmc_ex_adsl
    adlb <- tmc_ex_adlb
    # Set all worst flag values to "NA" to simulate the reported issue
    adlb$WGRLOFL <- "NA"
    template <- template_shift_by_grade(
      parentname = "adsl",
      dataname = "adlb",
      arm_var = "ARM",
      id_var = "USUBJID",
      visit_var = "AVISIT",
      worst_flag_var = c("WGRLOFL"),
      worst_flag_indicator = "Y", # Looking for "Y" but all are "NA"
      anl_toxgrade_var = "ATOXGR",
      base_toxgrade_var = "BTOXGR",
      paramcd = "PARAMCD",
      drop_arm_levels = TRUE,
      add_total = FALSE,
      na_level = "<Missing>",
      code_missing_baseline = FALSE
    )

    template_data <- template$data
    # Should not hang and produce an empty dataset
    data <- eval(template_data)
    testthat::expect_equal(nrow(data), 0)
  }
)
