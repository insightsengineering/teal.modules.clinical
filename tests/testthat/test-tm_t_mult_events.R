test_that("template_mult_events generates correct expressions with 1 HLT parameter", {
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

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "CMDECOD")))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        split_rows_by(
          "ATC1",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC1"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>%
        append_varlabels(adcm, "CMDECOD", indent = 1L)
    ),

    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    table_sorted = quote({
      sorted_result <- result %>% sort_at_path(path = c("ATC1", "*", "CMDECOD"), scorefun = score_occurrences)
    }),
    final_table = quote({
      result <- sorted_result
      result
    })
  )

  expect_equal(result, expected)
})


test_that("template_mult_events generates correct expressions with 2 HLT parameters and drop_arm_levels = FALSE", {
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

  expected <- list(
    data = quote({
      anl <- adcm
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(adsl[["ARM"]])
      anl <- anl %>% mutate(ARM = factor(ARM, levels = arm_levels))
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "ATC2", "CMDECOD")))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        split_rows_by("ATC1",
          child_labels = "visible", nested = FALSE,
          indent_mod = -1L, split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC1"], fill = TRUE)
        ) %>%
        split_rows_by("ATC2",
          child_labels = "visible", nested = TRUE, indent_mod = 0L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC2"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"), .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>%
        append_varlabels(adcm,
          "CMDECOD",
          indent = 2L
        )
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    table_sorted = quote({
      sorted_result <- result %>%
        sort_at_path(path = c("ATC1", "*", "ATC2", "*", "CMDECOD"), scorefun = score_occurrences)
    }),
    final_table = quote({
      result <- sorted_result
      result
    })
  )

  expect_equal(result, expected)
})


test_that("template_mult_events generates correct expressions with 3 HLT parameters", {
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

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "ATC2", "ATC3", "CMDECOD")))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        split_rows_by(
          "ATC1",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC1"], fill = TRUE)
        ) %>%
        split_rows_by(
          "ATC2",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = 0L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC2"], fill = TRUE)
        ) %>%
        split_rows_by(
          "ATC3",
          child_labels = "visible",
          nested = TRUE,
          indent_mod = 0L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC3"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>%
        append_varlabels(adcm, "CMDECOD", indent = 3L)
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    table_sorted = quote({
      sorted_result <- result %>%
        sort_at_path(
          path = c(
            "ATC1",
            "*",
            "ATC2",
            "*",
            "ATC3",
            "*",
            "CMDECOD"
          ),
          scorefun = score_occurrences
        )
    }),
    final_table = quote({
      result <- sorted_result
      result
    })
  )


  expect_equal(result, expected)
})

test_that("template_mult_events generates correct expressions with 4 HLT parameters", {
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

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      anl <- anl %>%
        df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "ATC2", "ATC3", "ATC4", "CMDECOD")))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        split_rows_by("ATC1",
          child_labels = "visible", nested = FALSE,
          indent_mod = -1L, split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC1"], fill = TRUE)
        ) %>%
        split_rows_by("ATC2",
          child_labels = "visible", nested = TRUE, indent_mod = 0L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC2"], fill = TRUE)
        ) %>%
        split_rows_by("ATC3",
          child_labels = "visible",
          nested = TRUE, indent_mod = 0L, split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC3"], fill = TRUE)
        ) %>%
        split_rows_by("ATC4",
          child_labels = "visible", nested = TRUE,
          indent_mod = 0L, split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC4"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"), .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>%
        append_varlabels(adcm,
          "CMDECOD",
          indent = 4L
        )
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    table_sorted = quote({
      sorted_result <- result %>%
        sort_at_path(
          path = c("ATC1", "*", "ATC2", "*", "ATC3", "*", "ATC4", "*", "CMDECOD"),
          scorefun = score_occurrences
        )
    }),
    final_table = quote({
      result <- sorted_result
      result
    })
  )


  expect_equal(result, expected)
})


test_that("template_mult_events generates correct expressions with no HLT parameters", {
  result <- template_mult_events(
    dataname = "adcm",
    parentname = "adsl",
    arm_var = "ARM",
    seq_var = "ASEQ",
    hlt = NULL,
    ll = "CMDECOD",
    add_total = TRUE,
    event_type = "treatment",
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), "CMDECOD"))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        count_occurrences(vars = "CMDECOD", .indent_mods = -1L) %>%
        append_varlabels(adcm, "CMDECOD", indent = 0L)
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    table_sorted = quote({
      sorted_result <- result %>% sort_at_path(path = "CMDECOD", scorefun = score_occurrences)
    }),
    final_table = quote({
      result <- sorted_result
      result
    })
  )


  expect_equal(result, expected)
})

test_that("template_mult_events generates correct expressions with 1 HLT parameter and without 'All Patients' column", {
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

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "CMDECOD")))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        split_rows_by(
          "ATC1",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adcm["ATC1"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>%
        count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>%
        append_varlabels(adcm, "CMDECOD", indent = 1L)
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    table_sorted = quote({
      sorted_result <- result %>% sort_at_path(path = c("ATC1", "*", "CMDECOD"), scorefun = score_occurrences)
    }),
    final_table = quote({
      result <- sorted_result
      result
    })
  )

  expect_equal(result, expected)
})
