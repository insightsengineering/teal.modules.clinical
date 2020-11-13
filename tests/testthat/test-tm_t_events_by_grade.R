test_that("template_events_by_grade generates standard expressions", {
  result <- template_events_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    grade = "AESEV",
    add_total = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adae # nolintr
      grade_groups <- list("- Any Intensity -" = levels(adae$AESEV))
      col_counts <- table(adsl$ACTARM)
      col_counts <- c(col_counts, `All Patients` = sum(col_counts))
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ACTARM") %>%
        add_overall_col(label = "All Patients") %>%
        add_colcounts() %>%
        summarize_occurrences_by_grade(
          var = "AESEV",
          grade_groups = grade_groups) %>%
        split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = -1L) %>%
        summarize_occurrences_by_grade(
          var = "AESEV",
          grade_groups = grade_groups) %>%
        split_rows_by(
          "AEDECOD",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = -1L) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = "unique",
          .labels = c("- Any Intensity -")) %>%
        count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L)
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        col_counts = col_counts
      )
    ),
    pruned_and_sorted_result = quote(
      result <- result %>%
        trim_rows() %>%
        sort_at_path(
          path = "AEBODSYS",
          scorefun = cont_n_onecol(length(col_counts)),
          decreasing = TRUE) %>%
        sort_at_path(
          path = c("AEBODSYS", "*", "AEDECOD"),
          scorefun = cont_n_onecol(length(col_counts)),
          decreasing = TRUE)
    )
  )

  expect_equal(result, expected)
})

test_that("template_events_by_grade without adding total column option works as expected", {
  result <- template_events_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    grade = "AESEV",
    add_total = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adae # nolintr
      grade_groups <- list("- Any Intensity -" = levels(adae$AESEV))
      col_counts <- table(adsl$ACTARM)
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ACTARM") %>%
        add_colcounts() %>%
        summarize_occurrences_by_grade(
          var = "AESEV",
          grade_groups = grade_groups
        ) %>%
        split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = TRUE, indent_mod = -1L) %>%
        summarize_occurrences_by_grade(
          var = "AESEV",
          grade_groups = grade_groups
        ) %>%
        split_rows_by(
          "AEDECOD",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = -1L) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = "unique",
          .labels = c("- Any Intensity -")) %>%
        count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L)
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        col_counts = col_counts
      )
    ),
    pruned_and_sorted_result = quote(
      result <- result %>%
        trim_rows() %>%
        sort_at_path(
          path = "AEBODSYS",
          scorefun = cont_n_allcols,
          decreasing = TRUE
        ) %>%
        sort_at_path(
          path = c("AEBODSYS", "*", "AEDECOD"),
          scorefun = cont_n_allcols,
          decreasing = TRUE
        )
    )
  )

  expect_equal(result, expected)
})


test_that("template_events_by_grade with hlt only works", {
  result <- template_events_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = NULL,
    grade = "AESEV",
    add_total = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adae # nolintr
      grade_groups <- list("- Any Intensity -" = levels(adae$AESEV))
      col_counts <- table(adsl$ACTARM)
      col_counts <- c(col_counts, `All Patients` = sum(col_counts))
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ACTARM") %>%
        add_overall_col(label = "All Patients") %>%
        add_colcounts() %>%
        summarize_occurrences_by_grade(
          var = "AESEV",
          grade_groups = grade_groups) %>%
        split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = -1L) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = "unique",
          .labels = c("- Any Intensity -")) %>%
        count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L)
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        col_counts = col_counts
      )
    ),
    pruned_and_sorted_result = quote(
      result <- result %>%
        trim_rows() %>%
        sort_at_path(
          path = "AEBODSYS",
          scorefun = cont_n_onecol(length(col_counts)),
          decreasing = TRUE
        )
    )
  )

  expect_equal(result, expected)
})
