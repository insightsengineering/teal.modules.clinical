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
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
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
          indent_mod = -1L,
          split_fun = split_fun
        ) %>%
        append_varlabels(adae, "AEBODSYS") %>%
        summarize_occurrences_by_grade(
          var = "AESEV",
          grade_groups = grade_groups) %>%
        split_rows_by(
          "AEDECOD",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = -1L,
          split_fun = split_fun
        ) %>%
        append_varlabels(adae, c("AEDECOD", "AESEV"), indent = TRUE) %>%
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
        alt_counts_df = adsl
      )
    ),
    pruned_and_sorted_result = quote(
      result <- result %>%
        trim_rows() %>%
        sort_at_path(
          path = "AEBODSYS",
          scorefun = cont_n_onecol(length(levels(adsl$ACTARM)) + 1),
          decreasing = TRUE) %>%
        sort_at_path(
          path = c("AEBODSYS", "*", "AEDECOD"),
          scorefun = cont_n_onecol(length(levels(adsl$ACTARM)) + 1),
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
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
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
          nested = TRUE,
          indent_mod = -1L,
          split_fun = split_fun
        ) %>%
        append_varlabels(adae, "AEBODSYS") %>%
        summarize_occurrences_by_grade(
          var = "AESEV",
          grade_groups = grade_groups
        ) %>%
        split_rows_by(
          "AEDECOD",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = -1L,
          split_fun = split_fun
        ) %>%
        append_varlabels(adae, c("AEDECOD", "AESEV"), indent = TRUE) %>%
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
        alt_counts_df = adsl
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
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
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
          indent_mod = -1L,
          split_fun = split_fun
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = "unique",
          .labels = c("- Any Intensity -")) %>%
        count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L) %>%
        append_varlabels(adae, c("AEBODSYS", "AESEV"))
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        alt_counts_df = adsl
      )
    ),
    pruned_and_sorted_result = quote(
      result <- result %>%
        trim_rows() %>%
        sort_at_path(
          path = "AEBODSYS",
          scorefun = cont_n_onecol(length(levels(adsl$ACTARM)) + 1),
          decreasing = TRUE
        )
    )
  )

  expect_equal(result, expected)
})
