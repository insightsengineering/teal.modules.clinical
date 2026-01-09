testthat::test_that("template_mult_events generates correct expressions with 1 HLT parameter", {
  result <- template_mult_events(
    dataname = "adcm",
    parentname = "adsl",
    arm_var = "ARM",
    seq_var = "ASEQ",
    hlt = "ATC1",
    llt = "CMDECOD",
    add_total = TRUE,
    event_type = "treatment",
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "template_mult_events generates correct expressions with 2 HLT parameters and drop_arm_levels = FALSE",
  {
    result <- template_mult_events(
      dataname = "adcm",
      parentname = "adsl",
      arm_var = "ARM",
      seq_var = "ASEQ",
      hlt = c("ATC1", "ATC2"),
      llt = "CMDECOD",
      add_total = TRUE,
      event_type = "treatment",
      drop_arm_levels = FALSE
    )

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("template_mult_events generates correct expressions with 3 HLT parameters", {
  result <- template_mult_events(
    dataname = "adcm",
    parentname = "adsl",
    arm_var = "ARM",
    seq_var = "ASEQ",
    hlt = c("ATC1", "ATC2", "ATC3"),
    llt = "CMDECOD",
    add_total = TRUE,
    event_type = "treatment",
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_mult_events generates correct expressions with 4 HLT parameters", {
  result <- template_mult_events(
    dataname = "adcm",
    parentname = "adsl",
    arm_var = "ARM",
    seq_var = "ASEQ",
    hlt = c("ATC1", "ATC2", "ATC3", "ATC4"),
    llt = "CMDECOD",
    add_total = TRUE,
    event_type = "treatment",
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_mult_events generates correct expressions with no HLT parameters", {
  result <- template_mult_events(
    dataname = "adcm",
    parentname = "adsl",
    arm_var = "ARM",
    seq_var = "ASEQ",
    hlt = NULL,
    llt = "CMDECOD",
    add_total = TRUE,
    event_type = "treatment",
    drop_arm_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "template_mult_events generates correct expressions with 1 HLT parameter and without 'All Patients' column",
  {
    result <- template_mult_events(
      dataname = "adcm",
      parentname = "adsl",
      arm_var = "ARM",
      seq_var = "ASEQ",
      hlt = "ATC1",
      llt = "CMDECOD",
      add_total = FALSE,
      event_type = "treatment",
      drop_arm_levels = TRUE
    )

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)
