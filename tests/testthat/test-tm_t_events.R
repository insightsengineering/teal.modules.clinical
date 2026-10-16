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

count_fixed <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (length(matches) == 1L && matches[[1]] == -1L) {
    0L
  } else {
    length(matches)
  }
}

testthat::test_that("template_events can omit per-HLT patient and event summary rows", {
  base_args <- list(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    add_total = TRUE
  )

  both_off <- do.call(
    template_events,
    c(base_args, list(incl_num_patients_hlt = FALSE, incl_num_events_hlt = FALSE))
  )
  layout_off <- paste(deparse(both_off$layout), collapse = " ")
  sort_off <- paste(deparse(both_off$sort), collapse = " ")
  # The patient row stays in the layout so frequency sorting still uses it.
  testthat::expect_equal(count_fixed(layout_off, "summarize_num_patients"), 2L)
  testthat::expect_equal(count_fixed(layout_off, "Overall total number of events"), 1L)
  testthat::expect_true(grepl("drop_rows", sort_off, fixed = TRUE))
  testthat::expect_false(grepl("scorefun_hlt_no_sum", sort_off, fixed = TRUE))

  events_off <- do.call(
    template_events,
    c(base_args, list(incl_num_patients_hlt = TRUE, incl_num_events_hlt = FALSE))
  )
  layout_events_off <- paste(deparse(events_off$layout), collapse = " ")
  testthat::expect_equal(count_fixed(layout_events_off, "Overall total number of events"), 1L)
  testthat::expect_equal(
    count_fixed(layout_events_off, "Total number of patients with at least one event"),
    2L
  )
  testthat::expect_false(
    grepl("scorefun_hlt_no_sum", paste(deparse(events_off$sort), collapse = " "), fixed = TRUE)
  )
})
