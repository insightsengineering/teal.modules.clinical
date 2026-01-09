skip("CI test")
testthat::test_that("template_events_patyear generates standard expressions", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARMCD",
    events_var = "n_events",
    label_paramcd = "Time to first occurrence of any adverse event",
    aval_var = "AVAL",
    add_total = TRUE,
    drop_arm_levels = TRUE,
    control = control_incidence_rate()
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_patyear generates right expressions with non-default", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARM",
    events_var = "n_events",
    label_paramcd = "Time to first occurrence of any adverse event",
    aval_var = "AVAL",
    add_total = FALSE,
    drop_arm_levels = FALSE,
    control = control_incidence_rate()
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_patyear generates right expressions with non-default controls", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = c("ARM", "SEX"),
    aval_var = "AVAL",
    events_var = "n_events",
    label_paramcd = "Time to first occurrence of any adverse event",
    add_total = TRUE,
    drop_arm_levels = TRUE,
    control = control_incidence_rate(
      conf_level = 0.9,
      conf_type = "exact",
      input_time_unit = "month",
      num_pt_year = 80
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
