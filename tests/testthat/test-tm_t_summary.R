test_that("template_summary generates correct expressions", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARM",
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    var_labels = character(),
    add_total = FALSE,
    na.rm = FALSE,
    denominator = "N"
  )
  expected <- list(
    data = quote({
      col_counts <- table(adsl$ARM)
      anl <- adrs
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        summarize_vars(
          vars = c("RACE", "COUNTRY", "AGE"),
          na.rm = FALSE,
          denom = "N_col",
          .stats = c("n", "mean_sd", "median", "range", "count_fraction")
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      print(result)
    })
  )
  expect_equal_expr_list(result, expected)
})

test_that("template_summary can generate customized table", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    sum_vars = "RACE",
    var_labels = c(RACE = "Race"),
    add_total = TRUE,
    na.rm = TRUE,
    denominator = "omit"
  )
  expected <- list(
    data = quote({
      col_counts <- table(adsl$ARMCD)
      col_counts <- c(col_counts, sum(col_counts))
      anl <- adrs
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARMCD", split_fun = add_overall_level(label = "All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        summarize_vars(
          vars = "RACE",
          var_labels = c(RACE = "Race"),
          na.rm = TRUE,
          denom = "N_col",
          .stats = c("n", "mean_sd", "median", "range", "count")
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      print(result)
    })
  )
  expect_equal_expr_list(result, expected)
})
