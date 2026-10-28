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

build_events_table <- function(incl_num_events_hlt = TRUE, sort_criteria = "freq_desc") {
  calls <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    add_total = TRUE,
    sort_criteria = sort_criteria,
    incl_num_events_hlt = incl_num_events_hlt
  )
  obj <- within(teal.data::teal_data(), {
    adae <- tern::tern_ex_adae
    adsl <- tern::tern_ex_adsl
  })
  obj <- teal.code::eval_code(obj, as.expression(unlist(calls)))
  obj[["pruned_and_sorted_result"]]
}

event_row_labels <- function(tbl) {
  vapply(rtables::row_paths(tbl), function(path) tail(path, 1), character(1))
}

testthat::test_that("default incl_num_events_hlt output matches main", {
  base_args <- list(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    add_total = TRUE
  )
  # Default expressions are main's layout; the snapshots above lock that code.
  testthat::expect_identical(
    do.call(template_events, base_args),
    do.call(template_events, c(base_args, list(incl_num_events_hlt = TRUE)))
  )
})

testthat::test_that("incl_num_events_hlt = FALSE drops nonunique and keeps unique", {
  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    incl_overall_sum = FALSE,
    incl_num_events_hlt = FALSE
  )
  layout <- paste(deparse(result$layout), collapse = " ")
  testthat::expect_false(grepl("nonunique", layout, fixed = TRUE))
  testthat::expect_true(grepl("unique", layout, fixed = TRUE))
})

testthat::test_that("hiding per-term event totals keeps term order for freq_desc and alpha", {
  drop_per_term_events <- function(tbl) {
    paths <- rtables::row_paths(tbl)
    drop <- vapply(paths, function(path) {
      length(path) > 3L && tail(path, 1) == "Overall total number of events"
    }, logical(1))
    tbl[!drop, , keep_topleft = TRUE, keep_titles = TRUE, keep_footers = TRUE, reindex_refs = TRUE]
  }

  for (sort_criteria in c("freq_desc", "alpha")) {
    tbl_on <- build_events_table(TRUE, sort_criteria)
    tbl_off <- build_events_table(FALSE, sort_criteria)
    testthat::expect_equal(
      event_row_labels(tbl_off),
      event_row_labels(drop_per_term_events(tbl_on))
    )
    testthat::expect_identical(tbl_off, drop_per_term_events(tbl_on))
  }
})
