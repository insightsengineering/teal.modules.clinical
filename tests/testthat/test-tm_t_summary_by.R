test_that("template_summary_by generates correct expressions", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    sum_vars = c("AVAL"),
    by_vars = c("AVISIT"),
    add_total = TRUE,
    na.rm = FALSE,
    denominator = "N"
  )

  expected <- list(
    data = quote(anl <- adlb),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          split_fun = drop_split_levels,
          visible_label = TRUE
        ) %>%
        summarize_vars(
          vars = "AVAL",
          na.rm = FALSE,
          denom = "N_col",
          .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
          .formats = c(
            n = "xx",
            mean_sd = "xx.xx (xx.xx)",
            median = "xx.xx",
            range = "xx.xx - xx.xx",
            count_fraction = "xx (xx.%)"
          )
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = c(table(adsl$ARM), sum(table(adsl$ARM))))
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_summary_by generates correct expressions when `parallel_vars` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    sum_vars = c("AVAL", "CHG"),
    parallel_vars = TRUE,
    by_vars = c("AVISIT"),
    add_total = TRUE,
    na.rm = FALSE,
    denominator = "N"
  )

  expected <- list(
    data = quote(anl <- adlb),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT", split_label = var_labels(adlb)[["AVISIT"]], split_fun = drop_split_levels,
          visible_label = TRUE
        ) %>%
        split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
        summarize_colvars(
          vars = c("AVAL", "CHG"),
          na.rm = FALSE,
          denom = "N_col",
          .stats = c("n", "mean_sd", "median", "range", "count_fraction")
        )
    ),
    table = quote({
      result <- build_table(
        lyt = lyt,
        df = anl,
        col_counts = c(
          rep(table(adsl$ARM), each = length(c("AVAL", "CHG"))),
          rep(sum(table(adsl$ARM)), each = length(c("AVAL", "CHG")))
          )
        )
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_summary_by generates correct expressions when `row_groups` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adsl",
    arm_var = "ARM",
    sum_vars = c("AVAL"),
    parallel_vars = FALSE,
    row_groups = TRUE,
    by_vars = c("SEX", "COUNTRY"),
    add_total = FALSE,
    na.rm = FALSE,
    denominator = "N"
  )

  expected <- list(
    data = quote(anl <- adsl),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        split_rows_by(
          "SEX",
          split_label = var_labels(adsl)[["SEX"]],
          split_fun = drop_split_levels,
          visible_label = TRUE
          ) %>%
        summarize_row_groups() %>%
        split_rows_by(
          "COUNTRY",
          split_label = var_labels(adsl)[["COUNTRY"]],
          split_fun = drop_split_levels,
          visible_label = TRUE
          ) %>%
        summarize_row_groups()
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = table(adsl$ARM))
      result
    })
  )
  expect_equal(result, expected)
})
