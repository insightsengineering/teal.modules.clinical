test_that("template_shift_by_grade generates correct expressions with default arguments", {
  result <- template_shift_by_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    visit_var = "AVISIT",
    worst_flag_var = c("WGRLOVFL"),
    worst_flag_indicator = "Y",
    anl_toxgrade_var = "ATOXGR",
    base_toxgrade_var = "BTOXGR",
    paramcd = "PARAMCD",
    drop_arm_levels = TRUE,
    add_total = FALSE,
    na_level = "<Missing>",
    code_missing_baseline = FALSE
    )

  expected <- list(
    data = quote({
      anl <- adlb %>% filter(WGRLOVFL == "Y")
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      anl <- df_explicit_na(anl, na_level = "<Missing>")
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      by_visit <- TRUE
      anl <- mutate(
        anl,
        ATOXGR_GP = factor(
          case_when(
            ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
            ATOXGR == -1 ~ "1", ATOXGR == -2 ~ "2",
            ATOXGR == -3 ~ "3", ATOXGR == -4 ~ "4",
            ATOXGR == "<Missing>" ~ "Missing"
            )
          ),
        BTOXGR_GP = factor(
          case_when(
            BTOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
            BTOXGR == -1 ~ "1", BTOXGR == -2 ~ "2",
            BTOXGR == -3 ~ "3", BTOXGR == -4 ~ "4",
            BTOXGR == "<Missing>" ~ "Missing"
            )
          )
        )

      anl <- mutate(
        anl,
        ATOXGR_GP = factor(
          ATOXGR_GP,
          levels = c(
            if_else("WGRLOVFL" %in% c("WGRLOVFL", "WGRLOFL"), "Not Low", "Not High"),
            "1", "2", "3", "4", "<Missing>")
          ),
        BTOXGR_GP = factor(
          BTOXGR_GP,
          levels = c(
            if_else("WGRLOVFL" %in% c("WGRLOVFL", "WGRLOFL"), "Not Low", "Not High"),
            "1", "2", "3", "4", "<Missing>")
          )
        )
      anl <- var_relabel(
        anl,
        PARAMCD = var_labels(anl)[["PARAMCD"]],
        AVISIT = var_labels(anl)[["AVISIT"]],
        ATOXGR_GP = if_else(TRUE, "Grade at Visit", "Post-baseline Grade"),
        BTOXGR_GP = "Baseline Grade"
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        split_rows_by(
          var = "PARAMCD", split_fun = split_fun, label_pos = "topleft",
          split_label = var_labels(anl)[["PARAMCD"]]
          ) %>%
        split_rows_by(
          "AVISIT", split_fun = split_fun, label_pos = "topleft",
          split_label = var_labels(anl)[["AVISIT"]]
          ) %>%
        split_rows_by(
          var = "ATOXGR_GP", split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(anl)[["ATOXGR_GP"]]
          ) %>%
        summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
        count_occurrences(var = "BTOXGR_GP", denom = "n", drop = TRUE) %>%
        append_varlabels(anl, "BTOXGR_GP", indent = 3L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>%
        prune_table()
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_shift_by_grade generates correct expressions with custom arguments", {
  result <- template_shift_by_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "MYUSUBJID",
    visit_var = "AVISIT",
    worst_flag_var = c("WGRLOVFL"),
    worst_flag_indicator = "YY",
    anl_toxgrade_var = "MYATOXGR",
    base_toxgrade_var = "MYBTOXGR",
    paramcd = "PARAMCD",
    drop_arm_levels = TRUE,
    add_total = FALSE,
    na_level = "<MYMissing>",
    code_missing_baseline = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% filter(WGRLOVFL == "YY")
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      anl <- df_explicit_na(anl, na_level = "<MYMissing>")
      adsl <- df_explicit_na(adsl, na_level = "<MYMissing>")
      by_visit <- TRUE
      anl <- mutate(
        anl,
        ATOXGR_GP = factor(
          case_when(
            MYATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
            MYATOXGR == -1 ~ "1", MYATOXGR == -2 ~ "2",
            MYATOXGR == -3 ~ "3", MYATOXGR == -4 ~ "4",
            MYATOXGR == "<MYMissing>" ~ "Missing"
          )
        ),
        BTOXGR_GP = factor(
          case_when(
            MYBTOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
            MYBTOXGR == -1 ~ "1", MYBTOXGR == -2 ~ "2",
            MYBTOXGR == -3 ~ "3", MYBTOXGR == -4 ~ "4",
            MYBTOXGR == "<MYMissing>" ~ "Missing"
            )
          )
      )
      anl <- mutate(
        anl,
        ATOXGR_GP = factor(
          ATOXGR_GP,
          levels = c(
            if_else("WGRLOVFL" %in% c("WGRLOVFL", "WGRLOFL"), "Not Low", "Not High"),
            "1", "2", "3", "4", "<MYMissing>")
        ),
        BTOXGR_GP = factor(
          BTOXGR_GP,
          levels = c(
            if_else("WGRLOVFL" %in% c("WGRLOVFL", "WGRLOFL"), "Not Low", "Not High"),
            "1", "2", "3", "4", "<MYMissing>")
        )
      )
      anl <- var_relabel(
        anl,
        PARAMCD = var_labels(anl)[["PARAMCD"]],
        AVISIT = var_labels(anl)[["AVISIT"]],
        ATOXGR_GP = if_else(TRUE, "Grade at Visit", "Post-baseline Grade"),
        BTOXGR_GP = "Baseline Grade"
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        split_rows_by(
          var = "PARAMCD", split_fun = split_fun, label_pos = "topleft",
          split_label = var_labels(anl)[["PARAMCD"]]
        ) %>%
        split_rows_by(
          "AVISIT", split_fun = split_fun, label_pos = "topleft",
          split_label = var_labels(anl)[["AVISIT"]]
        ) %>%
        split_rows_by(
          var = "ATOXGR_GP", split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(anl)[["ATOXGR_GP"]]
        ) %>%
        summarize_num_patients(var = "MYUSUBJID", .stats = c("unique_count")) %>%
        count_occurrences(var = "BTOXGR_GP", denom = "n", drop = TRUE) %>%
        append_varlabels(anl, "BTOXGR_GP", indent = 3L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>%
        prune_table()
      result
    })
  )
  expect_equal(result, expected)
})

test_that(
  "template_shift_by_grade throws an error when worst_flag_var
  is not one of “WGRLOVFL”, “WGRLOFL”, “WGRHIVFL”, “WGRHIFL”", {
  expect_error(
    result <- template_shift_by_grade(
      parentname = "adsl",
      dataname = "adlb",
      arm_var = "ARM",
      id_var = "USUBJID",
      visit_var = "AVISIT",
      worst_flag_var = c("another_value"),
      worst_flag_indicator = "Y",
      anl_toxgrade_var = "ATOXGR",
      base_toxgrade_var = "BTOXGR",
      paramcd = "PARAMCD",
      drop_arm_levels = TRUE,
      add_total = FALSE,
      na_level = "<Missing>",
      code_missing_baseline = FALSE
    )
  )
})
