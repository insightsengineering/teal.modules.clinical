testthat::test_that("template_summary generates correct expressions", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARM",
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    show_labels = "visible",
    add_total = FALSE,
    var_labels = character(),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE
  )
  expected <- list(
    data = quote({
      anl <- adrs %>%
        df_explicit_na(
          omit_columns = setdiff(names(adrs), c(c("RACE", "COUNTRY", "AGE"))),
          na_level = "<Missing>"
        )
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        main_footer = "n represent the number of unique subject ID such that the variable have non-NA values."
      ) %>%
        rtables::split_cols_by("ARM") %>%
        rtables::add_colcounts() %>%
        summarize_vars(
          vars = c("RACE", "COUNTRY", "AGE"),
          show_labels = "visible",
          na.rm = FALSE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c(
            "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean", "count_fraction"
          )
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_summary can generate customized table", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    sum_vars = "RACE",
    show_labels = "visible",
    add_total = TRUE,
    var_labels = c(RACE = "Race"),
    na.rm = TRUE,
    denominator = "omit",
    drop_arm_levels = FALSE
  )
  expected <- list(
    data = quote({
      anl <- adrs %>%
        df_explicit_na(
          omit_columns = setdiff(names(adrs), c("RACE")),
          na_level = "<Missing>"
        )
      adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
      arm_levels <- levels(adsl[["ARMCD"]])
      anl <- anl %>% dplyr::mutate(ARMCD = factor(ARMCD, levels = arm_levels))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        main_footer = "n represent the number of unique subject ID such that the variable have non-NA values."
      ) %>%
        rtables::split_cols_by("ARMCD") %>%
        rtables::add_overall_col("All Patients") %>%
        rtables::add_colcounts() %>%
        summarize_vars(
          vars = "RACE",
          var_labels = c(RACE = "Race"),
          show_labels = "visible",
          na.rm = TRUE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c(
            "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean", "count"
          )
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_summary generates correct expressions for multiple grouping variables", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = c("ARM", "STRATA1"),
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    show_labels = "visible",
    add_total = FALSE,
    var_labels = character(),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE
  )
  expected <- list(
    data = quote({
      anl <- adrs %>%
        df_explicit_na(
          omit_columns = setdiff(names(adrs), c(c("RACE", "COUNTRY", "AGE"))),
          na_level = "<Missing>"
        )
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      anl <- anl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
      arm_levels <- levels(anl[["STRATA1"]])
      adsl <- adsl %>% dplyr::filter(STRATA1 %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        main_footer = "n represent the number of unique subject ID such that the variable have non-NA values."
      ) %>%
        rtables::split_cols_by("ARM") %>%
        rtables::split_cols_by("STRATA1", split_fun = drop_split_levels) %>%
        rtables::add_colcounts() %>%
        summarize_vars(
          vars = c("RACE", "COUNTRY", "AGE"),
          show_labels = "visible",
          na.rm = FALSE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c(
            "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean", "count_fraction"
          )
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_summary generates correct expressions for multiple
  grouping variables and all patientts", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = c("ARM", "STRATA1"),
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    show_labels = "visible",
    add_total = TRUE,
    var_labels = character(),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE
  )
  expected <- list(
    data = quote({
      anl <- adrs %>%
        df_explicit_na(
          omit_columns = setdiff(names(adrs), c(c("RACE", "COUNTRY", "AGE"))),
          na_level = "<Missing>"
        )
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      anl <- anl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
      arm_levels <- levels(anl[["STRATA1"]])
      adsl <- adsl %>% dplyr::filter(STRATA1 %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        main_footer = "n represent the number of unique subject ID such that the variable have non-NA values."
      ) %>%
        rtables::split_cols_by("ARM") %>%
        rtables::split_cols_by("STRATA1", split_fun = drop_split_levels) %>%
        rtables::add_overall_col("All Patients") %>%
        rtables::add_colcounts() %>%
        summarize_vars(
          vars = c("RACE", "COUNTRY", "AGE"),
          show_labels = "visible",
          na.rm = FALSE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c(
            "n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range", "geom_mean", "count_fraction"
          )
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_summary generates correct expressions for customized numeric statistics", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = c("ARM", "STRATA1"),
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    show_labels = "visible",
    add_total = FALSE,
    var_labels = character(),
    na.rm = FALSE,
    numeric_stats = c("n"),
    denominator = "N",
    drop_arm_levels = TRUE
  )
  expected <- list(
    data = quote({
      anl <- adrs %>%
        df_explicit_na(
          omit_columns = setdiff(names(adrs), c(c("RACE", "COUNTRY", "AGE"))),
          na_level = "<Missing>"
        )
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      anl <- anl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
      arm_levels <- levels(anl[["STRATA1"]])
      adsl <- adsl %>% dplyr::filter(STRATA1 %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        main_footer = "n represent the number of unique subject ID such that the variable have non-NA values."
      ) %>%
        rtables::split_cols_by("ARM") %>%
        rtables::split_cols_by("STRATA1", split_fun = drop_split_levels) %>%
        rtables::add_colcounts() %>%
        summarize_vars(
          vars = c("RACE", "COUNTRY", "AGE"),
          show_labels = "visible",
          na.rm = FALSE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c("n", "count_fraction")
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})
