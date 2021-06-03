test_that("template_events_by_grade generates standard expressions", {
  result <- template_events_by_grade(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    grade = "AESEV",
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adae # nolintr
      anl <- anl %>% mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% mutate(ACTARM = droplevels(ACTARM))
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
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adae["AEBODSYS"], fill = TRUE)
        ) %>%
        summarize_occurrences_by_grade(
          var = "AESEV",
          grade_groups = grade_groups) %>%
        split_rows_by(
          "AEDECOD",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adae["AEDECOD"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = "unique",
          .labels = c("- Any Intensity -")) %>%
        count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L) %>%
        append_varlabels(adae, "AESEV", indent = 2L)
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        alt_counts_df = adsl
      )
    ),
    pruned_and_sorted_result = quote({
      result <- result %>%
        trim_rows() %>%
        sort_at_path(
          path = "AEBODSYS",
          scorefun = cont_n_onecol(length(levels(adsl$ACTARM)) + 1),
          decreasing = TRUE) %>%
        sort_at_path(
          path = c("AEBODSYS", "*", "AEDECOD"),
          scorefun = cont_n_onecol(length(levels(adsl$ACTARM)) + 1),
          decreasing = TRUE
        )
      result
    })
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
    add_total = FALSE,
    drop_arm_levels = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adae # nolintr
      adsl <- adsl %>% mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(adsl[["ACTARM"]])
      anl <- anl %>% mutate(ACTARM = factor(ACTARM, levels = arm_levels))
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
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adae["AEBODSYS"], fill = TRUE)
        ) %>%
        summarize_occurrences_by_grade(
          var = "AESEV",
          grade_groups = grade_groups
        ) %>%
        split_rows_by(
          "AEDECOD",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adae["AEDECOD"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = "unique",
          .labels = c("- Any Intensity -")) %>%
        count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L) %>%
        append_varlabels(adae, "AESEV", indent = 2L)
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        alt_counts_df = adsl
      )
    ),
    pruned_and_sorted_result = quote({
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
      result
    })
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
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adae # nolintr
      anl <- anl %>% mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% mutate(ACTARM = droplevels(ACTARM))
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
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adae["AEBODSYS"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = "unique",
          .labels = c("- Any Intensity -")) %>%
        count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L) %>%
        append_varlabels(adae,  "AESEV", indent = 1L)
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        alt_counts_df = adsl
      )
    ),
    pruned_and_sorted_result = quote({
      result <- result %>%
        trim_rows() %>%
        sort_at_path(
          path = "AEBODSYS",
          scorefun = cont_n_onecol(length(levels(adsl$ACTARM)) + 1),
          decreasing = TRUE
        )
      result
    })
  )

  expect_equal(result, expected)
})
