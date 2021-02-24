test_that("template_mult_events generates correct expressions with 1 HLT parameter", {
  result <- template_mult_events(
    dataname = "adcm",
    parentname = "adsl",
    arm_var = "ARM",
    seq_var = "ASEQ",
    hlt = "ATC1",
    llt = "CMDECOD",
    add_total = TRUE,
    event_type = "treatment"
  )

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "CMDECOD")))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
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
          .stats = c("unique_count", "nonunique"),
          .labels = c(
            unique_count = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments")
        ) %>%
        split_rows_by(
          "ATC1",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun
        ) %>% summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>% count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>% append_topleft(
          paste(
            vapply(
              list(attr(adcm$ATC1,
                which = "label"
              )),
              eval,
              FUN.VALUE = character(1)
            ),
            collapse = "/"
          )
        ) %>%
        append_varlabels(adcm, "CMDECOD", indent = TRUE)
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


test_that("template_mult_events generates correct expressions with 2 HLT parameters", {
  result <- template_mult_events(
    dataname = "adcm",
    parentname = "adsl",
    arm_var = "ARM",
    seq_var = "ASEQ",
    hlt = c("ATC1", "ATC2"),
    llt = "CMDECOD",
    add_total = TRUE,
    event_type = "treatment"
  )

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "ATC2", "CMDECOD")))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
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
          .stats = c("unique_count", "nonunique"),
          .labels = c(
            unique_count = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments")
          ) %>%
        split_rows_by("ATC1",
          child_labels = "visible", nested = FALSE,
          indent_mod = -1L, split_fun = split_fun
        ) %>% split_rows_by("ATC2",
          child_labels = "visible", nested = TRUE, indent_mod = 0L,
          split_fun = split_fun
        ) %>% summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"), .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>% count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>% append_topleft(paste(vapply(list(attr(adcm$ATC1,
          which = "label"
        ), attr(adcm$ATC2, which = "label")), eval,
        FUN.VALUE = character(1)
        ), collapse = "/")) %>% append_varlabels(adcm,
          "CMDECOD",
          indent = TRUE
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
    event_type = "treatment"
  )

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "ATC2", "ATC3", "CMDECOD")))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
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
          .stats = c("unique_count", "nonunique"),
          .labels = c(
            unique_count = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments")
          ) %>%
        split_rows_by("ATC1",
          child_labels = "visible", nested = FALSE,
          indent_mod = -1L, split_fun = split_fun
        ) %>% split_rows_by("ATC2",
          child_labels = "visible", nested = TRUE, indent_mod = 0L,
          split_fun = split_fun
        ) %>% split_rows_by("ATC3",
          child_labels = "visible",
          nested = TRUE, indent_mod = 0L, split_fun = split_fun
        ) %>%
        summarize_num_patients(var = "USUBJID", .stats = c(
          "unique",
          "nonunique"
        ), .labels = c(
          unique = "Total number of patients with at least one treatment",
          nonunique = "Total number of treatments"
        )) %>% count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>% append_topleft(paste(vapply(list(attr(adcm$ATC1,
          which = "label"
        ), attr(adcm$ATC2, which = "label"), attr(adcm$ATC3,
          which = "label"
        )), eval, FUN.VALUE = character(1)), collapse = "/")) %>%
        append_varlabels(adcm, "CMDECOD", indent = TRUE)
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
    event_type = "treatment"
  )

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>%
        df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "ATC2", "ATC3", "ATC4", "CMDECOD")))
        anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
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
          .stats = c("unique_count", "nonunique"),
          .labels = c(
            unique_count = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments")
          ) %>%
        split_rows_by("ATC1",
          child_labels = "visible", nested = FALSE,
          indent_mod = -1L, split_fun = split_fun
        ) %>% split_rows_by("ATC2",
          child_labels = "visible", nested = TRUE, indent_mod = 0L,
          split_fun = split_fun
        ) %>% split_rows_by("ATC3",
          child_labels = "visible",
          nested = TRUE, indent_mod = 0L, split_fun = split_fun
        ) %>%
        split_rows_by("ATC4",
          child_labels = "visible", nested = TRUE,
          indent_mod = 0L, split_fun = split_fun
        ) %>% summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"), .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>% count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>% append_topleft(paste(vapply(list(attr(adcm$ATC1,
          which = "label"
        ), attr(adcm$ATC2, which = "label"), attr(adcm$ATC3,
          which = "label"
        ), attr(adcm$ATC4, which = "label")), eval,
        FUN.VALUE = character(1)
        ), collapse = "/")) %>% append_varlabels(adcm,
          "CMDECOD",
          indent = TRUE
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
    event_type = "treatment"
  )

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), "CMDECOD"))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
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
          .stats = c("unique_count", "nonunique"),
          .labels = c(
            unique_count = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments")
        ) %>%
        count_occurrences(vars = "CMDECOD", .indent_mods = -1L) %>%
        append_varlabels(adcm, "CMDECOD", indent = FALSE)
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
    event_type = "treatment"
  )

  expected <- list(
    data = quote({
      anl <- adcm
      anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), c("ATC1", "CMDECOD")))
      anl <- anl %>% mutate(ASEQ = as.factor(ASEQ))
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        summarize_num_patients(
          var = "USUBJID",
          count_by = "ASEQ",
          .stats = c("unique_count", "nonunique"),
          .labels = c(
            unique_count = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments")
        ) %>%
        split_rows_by(
          "ATC1",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun
        ) %>% summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Total number of treatments"
          )
        ) %>% count_occurrences(
          vars = "CMDECOD",
          .indent_mods = -1L
        ) %>% append_topleft(
          paste(
            vapply(
              list(attr(adcm$ATC1,
                which = "label"
              )),
              eval,
              FUN.VALUE = character(1)
            ),
            collapse = "/"
          )
        ) %>%
        append_varlabels(adcm, "CMDECOD", indent = TRUE)
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
