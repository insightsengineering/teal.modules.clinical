testthat::test_that("template_events_by_grade generates standard expressions", {
  result <- template_events_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    label_hlt = "Body System or Organ Class",
    label_llt = "Dictionary-Derived Term",
    grade = "AESEV",
    label_grade = "Analysis Toxicity Grade",
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_by_grade generates standard expressions with pruning conditions", {
  result <- template_events_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    label_hlt = "Body System or Organ Class",
    label_llt = "Dictionary-Derived Term",
    grade = "AESEV",
    label_grade = "Severity/Intensity",
    prune_freq = 0.4,
    prune_diff = 0.1,
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_by_grade without adding total column option works as expected", {
  result <- template_events_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    label_hlt = "Body System or Organ Class",
    label_llt = "Dictionary-Derived Term",
    grade = "AESEV",
    label_grade = "Severity/Intensity",
    add_total = FALSE,
    drop_arm_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_by_grade with hlt only works", {
  result <- template_events_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = NULL,
    label_hlt = "Body System or Organ Class",
    label_llt = NULL,
    grade = "AESEV",
    label_grade = "Severity/Intensity",
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_col_by_grade generates standard expressions", {
  result <- template_events_col_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    label_hlt = "Body System or Organ Class",
    label_llt = "Dictionary-Derived Term",
    grade = "AETOXGR",
    label_grade = "Analysis Toxicity Grade",
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_col_by_grade works with custom values", {
  result <- template_events_col_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = NULL,
    llt = "AEDECOD",
    label_hlt = NULL,
    label_llt = "Dictionary-Derived Term",
    grade = "AETOXGR",
    label_grade = "Analysis Toxicity Grade",
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_col_by_grade without adding total column option works as expected", {
  result <- template_events_col_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = NULL,
    llt = "AEDECOD",
    label_hlt = NULL,
    label_llt = "Dictionary-Derived Term",
    grade = "AETOXGR",
    label_grade = "Analysis Toxicity Grade",
    add_total = FALSE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_col_by_grade without dropping arm levels option works as expected", {
  result <- template_events_col_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = NULL,
    llt = "AEDECOD",
    label_hlt = NULL,
    label_llt = "Dictionary-Derived Term",
    grade = "AETOXGR",
    label_grade = "Analysis Toxicity Grade",
    add_total = FALSE,
    drop_arm_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
