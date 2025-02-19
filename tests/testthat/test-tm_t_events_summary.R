testthat::test_that("template_events_summary generates minimal table", {
  result <- template_events_summary(
    anl_name = "adae",
    parentname = "adsl",
    arm_var = "ARM"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_events_summary generates table with multiple flags", {
  result <- template_events_summary(
    anl_name = "adae",
    parentname = "adsl",
    arm_var = "ARM",
    add_total = TRUE,
    flag_var_anl = c("A", "B", "C"),
    flag_var_aesi = c("X", "Y"),
    count_dth = FALSE,
    count_wd = FALSE,
    count_subj = TRUE,
    count_pt = TRUE,
    count_events = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
