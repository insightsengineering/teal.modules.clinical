testthat::test_that("template_exposure generates correct expressions with default arguments", {
  result <- template_exposure(
    parentname = "adsl",
    dataname = "adex",
    paramcd = "TDURD",
    id_var = "USUBJID",
    row_by_var = "RACE",
    col_by_var = "SEX",
    add_total = FALSE,
    drop_levels = TRUE,
    na_level = "<Missing>",
    aval_var = "AVAL",
    avalu_var = "Days"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_exposure generates correct expressions with custom arguments", {
  result <- template_exposure(
    parentname = "myadsl",
    dataname = "myadex",
    paramcd = "myTDURD",
    id_var = "USUBJID",
    row_by_var = "myRACE",
    col_by_var = "SEX",
    add_total = FALSE,
    drop_levels = TRUE,
    na_level = "<myMissing>",
    aval_var = "myAVAL",
    avalu_var = "Days"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_exposure generates correct expressions with paramcd_label", {
  result <- template_exposure(
    parentname = "adsl",
    dataname = "adex",
    paramcd = "TDURD",
    paramcd_label = "Total Duration (Days)",
    id_var = "USUBJID",
    row_by_var = "RACE",
    col_by_var = "SEX",
    add_total = FALSE,
    drop_levels = TRUE,
    na_level = "<Missing>",
    aval_var = "AVAL",
    avalu_var = "Days"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
