test_that("template_abnormality generates correct expressions with default arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    by_vars = c("AVISIT", "PARAM"),
    abnormal = c(Low = "LOW"),
    grade = "ANRIND",
    add_total = FALSE,
    exclude_base_abn = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% filter(ONTRTFL == "Y" & !is.na(ANRIND))
      n_col_counts <- table(adsl$ARM)
    }),
    layout = quote(
      lyt <- basic_table() %>% split_cols_by(var = "ARM") %>% add_colcounts() %>% # nolint
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          visible_label = TRUE
        ) %>%
        split_rows_by("PARAM",
          split_label = var_labels(adlb)[["PARAM"]],
          visible_label = TRUE
        ) %>%
        count_abnormal(
          var = "ANRIND",
          abnormal = c(low = "LOW"),
          variables = list(id = "USUBJID", baseline = "BNRIND"),
          exclude_base_abn = FALSE
        ) %>%
        append_varlabels(adlb, "ANRIND")
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = n_col_counts) %>%
        prune_table()
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_abnormality generates correct expressions with custom arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    by_vars = c("AVISIT", "PARAMCD"),
    abnormal = c(Low = "LOW", Medium = "MEDIUM"),
    grade = "MYANRIND",
    baseline_var = "MYBASELINE",
    treatment_flag_var = "MYTRTFL",
    treatment_flag = "YES",
    add_total = TRUE,
    exclude_base_abn = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% filter(MYTRTFL == "YES" & !is.na(MYANRIND))
      n_col_counts <- table(adsl$ARM)
      n_col_counts <- c(n_col_counts, Total = sum(n_col_counts))
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by("AVISIT", split_label = var_labels(adlb)[["AVISIT"]], visible_label = TRUE) %>%
        split_rows_by("PARAMCD", split_label = var_labels(adlb)[["PARAMCD"]], visible_label = TRUE) %>%
        count_abnormal(
          var = "MYANRIND",
          abnormal = c(low = "LOW", medium = "MEDIUM"),
          variables = list(id = "USUBJID", baseline = "MYBASELINE"),
          exclude_base_abn = TRUE
      ) %>%
        append_varlabels(adlb, "MYANRIND")
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = n_col_counts) %>%
        prune_table()
      result
    })
  )
  expect_equal(result, expected)
})
