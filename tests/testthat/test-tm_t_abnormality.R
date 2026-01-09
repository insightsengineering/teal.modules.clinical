skip("CI test")
testthat::test_that("template_abnormality generates correct expressions with default arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    by_vars = c("AVISIT", "PARAM"),
    abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
    grade = "ANRIND",
    add_total = FALSE,
    exclude_base_abn = FALSE,
    drop_arm_levels = TRUE,
    tbl_title = "my_title"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_abnormality generates correct expressions with custom arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    by_vars = c("AVISIT", "PARAMCD"),
    abnormal = list(Low = "LOW", Medium = "MEDIUM"),
    grade = "MYANRIND",
    baseline_var = "MYBASELINE",
    treatment_flag_var = "MYTRTFL",
    treatment_flag = "YES",
    add_total = TRUE,
    exclude_base_abn = TRUE,
    drop_arm_levels = FALSE,
    tbl_title = "my_title"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_abnormality generates correct expressions with customized na_level", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    by_vars = c("AVISIT", "PARAM"),
    abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
    grade = "ANRIND",
    add_total = FALSE,
    exclude_base_abn = FALSE,
    drop_arm_levels = TRUE,
    na_level = "NA",
    tbl_title = "my_title"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
