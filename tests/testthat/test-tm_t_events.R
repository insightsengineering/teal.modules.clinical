skip("CI test")
testthat::test_that("template_events generates correct expressions", {
  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    label_hlt = "Body System",
    label_llt = "Adverse Event Code",
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events generates correct expressions for nested columns", {
  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = c("ACTARM", "ACTARMCD"),
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    label_hlt = "Body System",
    label_llt = "Adverse Event Code",
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events can generate customized table", {
  result <- template_events(
    dataname = "adcm",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = NULL,
    llt = "CMDECOD",
    label_hlt = NULL,
    label_llt = "Con Med Code",
    add_total = FALSE,
    event_type = "treatment",
    drop_arm_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events can generate customized table with alphabetical sorting", {
  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    label_hlt = "Body System",
    label_llt = "Adverse Event Code",
    add_total = TRUE,
    event_type = "event",
    sort_criteria = "alpha",
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events can generate customized table with pruning", {
  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    label_hlt = "Body System",
    label_llt = "Adverse Event Code",
    add_total = TRUE,
    event_type = "event",
    prune_freq = 0.4,
    prune_diff = 0.1,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events can generate customized table with pruning for nested column", {
  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = c("ACTARM", "ACTARMCD"),
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    label_hlt = "Body System",
    label_llt = "Adverse Event Code",
    add_total = TRUE,
    event_type = "event",
    prune_freq = 0.4,
    prune_diff = 0.1,
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
