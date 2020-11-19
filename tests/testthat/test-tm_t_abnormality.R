test_that("template_abnormality generates correct expressions with default arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    by_vars = c("AVISIT", "PARAM"),
    abnormal = c(Low = "LOW"),
    grade = "ANRIND",
    treatment_flag_var = "ONTRTFL",
    treatment_flag = "Y",
    add_total = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% filter(ONTRTFL == "Y" & !is.na(ANRIND))
      n_col_counts <- table(adsl$ARM)
    }),
    layout = quote(
      lyt <- basic_table() %>% split_cols_by(var = "ARM") %>% add_colcounts() %>% # nolint
        split_rows_by("AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          visible_label = TRUE
        ) %>% split_rows_by("PARAM",
          split_label = var_labels(adlb)[["PARAM"]],
          visible_label = TRUE
        ) %>% count_abnormal("ANRIND", abnormal = c(low = "LOW"))
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, col_counts = n_col_counts) %>%
        prune_table()
    )
  )
  expect_equal_expr_list(result, expected)
})

test_that("template_abnormality generates correct expressions with custom arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    by_vars = c("AVISIT", "PARAMCD"),
    abnormal = c(Low = "LOW", Medium = "MEDIUM"),
    grade = "MYANRIND",
    treatment_flag_var = "MYONTRTFL",
    treatment_flag = "YES",
    add_total = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% filter(MYONTRTFL == "YES" & !is.na(MYANRIND))
      n_col_counts <- table(adsl$ARM)
      n_col_counts <- c(n_col_counts, Total = sum(n_col_counts))
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by("AVISIT", split_label = var_labels(adlb)[["AVISIT"]], visible_label = TRUE) %>%
        split_rows_by("PARAMCD", split_label = var_labels(adlb)[["PARAMCD"]], visible_label = TRUE) %>%
        count_abnormal("MYANRIND", abnormal = c(low = "LOW", medium = "MEDIUM"))
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, col_counts = n_col_counts) %>%
        prune_table()
    )
  )
  expect_equal_expr_list(result, expected)
})
