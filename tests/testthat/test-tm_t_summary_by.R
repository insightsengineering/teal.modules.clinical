testthat::test_that("template_summary_by generates correct expressions", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions when `parallel_vars` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL", "CHG"),
    add_total = TRUE,
    parallel_vars = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = FALSE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions when `row_groups` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = FALSE,
    parallel_vars = FALSE,
    row_groups = TRUE,
    by_vars = c("SEX", "COUNTRY"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions for customized numeric statistics", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    numeric_stats = c("n"),
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions for `drop_zero_levels` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::describe("template_summary_by rtables output for different statistics", {
  data <- within(teal.data::teal_data(), {
    ADSL <- teal.modules.clinical::tmc_ex_adsl
    ADLB <- teal.modules.clinical::tmc_ex_adlb
    ADLB$AVALC <- ifelse(abs(rnorm(nrow(ADLB))) >= 0.5, "Y", "N")
    ADLB <- teal.data::col_relabel(ADLB, AVALC = "Analysis Value Category")
    ADLB <- dplyr::inner_join(x = ADLB, y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE], by = c("STUDYID", "USUBJID"))
    ANL_1 <- ADSL %>% dplyr::select(STUDYID, USUBJID, ARM)
    ANL_2 <- ADLB %>% dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL, AVALC)
    ANL_3 <- ADLB %>% dplyr::filter(PARAMCD == "ALT") %>% dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT)
    ANL <- ANL_1
    ANL <- dplyr::inner_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))
    ANL <- dplyr::inner_join(ANL, ANL_3, by = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
    ANL <- ANL %>%
      teal.data::col_relabel(
        ARM = "Description of Planned Arm",
        USUBJID = "Unique Subject Identifier",
        AVAL = "Analysis Value",
        AVALC = "Analysis Value Category",
        AVISIT = "Analysis Visit",
        PARAMCD = "Parameter Code"
      )
    ANL_ADSL_1 <- ADSL %>% dplyr::select(STUDYID, USUBJID, ARM)
    ANL_ADSL <- ANL_ADSL_1
    ANL_ADSL <- ANL_ADSL %>% teal.data::col_relabel(ARM = "Description of Planned Arm")
  })
  it("adds count to the statistics", {
    result <- template_summary_by(
      parentname = "ADSL",
      dataname = "ADLB",
      arm_var = "ARM",
      id_var = "USUBJID",
      sum_vars = c("AVALC"),
      add_total = TRUE,
      by_vars = c("AVISIT"),
      na.rm = FALSE,
      numeric_stats = c("count"),
      denominator = "N",
      drop_arm_levels = TRUE,
      drop_zero_levels = FALSE
    )

    teal.code::eval_code(data, as.expression(unname(result)))
    res <- testthat::expect_silent(result)

    testthat::expect_snapshot(res)
  })
  it("adds n to the statistics", {})
})