testthat::test_that("template_summary_by generates correct expressions", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>%
        df_explicit_na(
          omit_columns = setdiff(names(adlb), c("AVISIT", "AVAL")),
          na_level = "<Missing>"
        )
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table() %>%
        split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          split_fun = split_fun,
          label_pos = "topleft"
        ) %>%
        summarize_vars(
          vars = "AVAL",
          na.rm = FALSE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "count_fraction")
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_summary_by generates correct expressions when `parallel_vars` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL", "CHG"),
    add_total = TRUE,
    parallel_vars = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = FALSE,
    drop_zero_levels = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>%
        df_explicit_na(
          omit_columns = setdiff(names(adlb), c("AVISIT", c("AVAL", "CHG"))),
          na_level = "<Missing>"
        )
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(adsl[["ARM"]])
      anl <- anl %>% dplyr::mutate(ARM = factor(ARM, levels = arm_levels))
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table() %>%
        split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          split_fun = split_fun,
          label_pos = "topleft"
        ) %>%
        split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
        summarize_colvars(
          vars = c("AVAL", "CHG"),
          na.rm = FALSE,
          denom = "N_col",
          .stats = c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "count_fraction")
        )
    ),
    table = quote({
      result <- build_table(
        lyt = lyt,
        df = anl,
        alt_counts_df = adsl
        )
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_summary_by generates correct expressions when `row_groups` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = FALSE,
    parallel_vars = FALSE,
    row_groups = TRUE,
    by_vars = c("SEX", "COUNTRY"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adsl %>%
        df_explicit_na(
          omit_columns = setdiff(names(adsl), c(c("SEX", "COUNTRY"), "AVAL")),
          na_level = "<Missing>"
        )
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout_cfun = quote(
      cfun_unique <- function(x, labelstr = "", .N_col) { #nolint
        y <- length(unique(x))
        rcell(
          c(y , y / .N_col), #nolint
          label = labelstr
        )
      }
    ),
    layout = quote(
      lyt <- rtables::basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        split_rows_by(
          "SEX",
          split_label = var_labels(adsl)[["SEX"]],
          split_fun = split_fun,
          label_pos = "topleft"
        ) %>%
        summarize_row_groups(var = "USUBJID", cfun = cfun_unique) %>%
        split_rows_by(
          "COUNTRY",
          split_label = var_labels(adsl)[["COUNTRY"]],
          split_fun = split_fun,
          label_pos = "topleft"
        ) %>%
        summarize_row_groups(var = "USUBJID", cfun = cfun_unique)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_summary_by generates correct expressions for customized numeric statistics", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    numeric_stats = c("n"),
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>%
        df_explicit_na(
          omit_columns = setdiff(names(adlb), c("AVISIT", "AVAL")),
          na_level = "<Missing>"
        )
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table() %>%
        split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          split_fun = split_fun,
          label_pos = "topleft"
        ) %>%
        summarize_vars(
          vars = "AVAL",
          na.rm = FALSE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c("n", "count_fraction")
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_summary_by generates correct expressions for `drop_zero_levels` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>%
        df_explicit_na(
          omit_columns = setdiff(names(adlb), c("AVISIT", "AVAL")),
          na_level = "<Missing>"
        )
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table() %>%
        split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          split_fun = split_fun,
          label_pos = "topleft"
        ) %>%
        summarize_vars(
          vars = "AVAL",
          na.rm = FALSE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "count_fraction")
        )
    ),
    table = quote({
      all_zero <- function(tr) {
        if (!is(tr, "TableRow") || is(tr, "LabelRow"))
          return(FALSE)
        rvs <- unlist(unname(row_values(tr)))
        all(rvs == 0)
      }
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>%
        trim_rows(criteria = all_zero)
      result
    })
  )
  testthat::expect_equal(result, expected)
})
