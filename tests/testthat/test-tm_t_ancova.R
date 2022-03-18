testthat::test_that("template_ancova generates expressions with multiple endpoints", {
  result <- template_ancova(
    dataname = "adqs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    cov_var = c("BASE", "STRATA1"),
    paramcd_levels = c("FKSI-FWB", "BFIALL"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("Function/Well-Being (GF1,GF3,GF7)", "BFI All Questions"),
    visit_var = "AVISIT",
    visit_levels = "WEEK 1 DAY 8"
  )
  expected <- list(
    data = quote({
      adqs <- adqs %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        droplevels() %>%
        df_explicit_na(na_level = "")
      adsl <- adsl %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        droplevels() %>%
        df_explicit_na(na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for Function/Well-Being (GF1,GF3,GF7) and BFI All Questions at WEEK 1 DAY 8 for Absolute Change from Baseline") %>% # nolint
        rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          "AVISIT",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::variable_labels(adqs["AVISIT"])
        ) %>%
        rtables::split_rows_by(
          "PARAMCD",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::variable_labels(adqs["PARAMCD"])
        ) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_ancova generates expressions with multiple endpoints with combined comparison arms", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = TRUE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    paramcd_levels = c("A", "B"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("A", "B"),
    visit_var = "AVISIT",
    visit_levels = "WEEK 1 DAY 8"
  )
  expected <- list(
    data = quote({
      adqs <- adqs %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        droplevels() %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"))) %>%
        df_explicit_na(na_level = "")
      adsl <- adsl %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        droplevels() %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"))) %>%
        df_explicit_na(na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for A and B at WEEK 1 DAY 8 for Absolute Change from Baseline") %>% # nolint
        rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          "AVISIT",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::variable_labels(adqs["AVISIT"])
        ) %>%
        rtables::split_rows_by(
          "PARAMCD",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::variable_labels(adqs["PARAMCD"])
        ) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_ancova generates expressions with multiple endpoints with combined reference arms", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    arm_var = "ARMCD",
    ref_arm = c("ARM B", "ARM C"),
    comp_arm = "ARM A",
    combine_comp_arms = FALSE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    paramcd_levels = c("A", "B"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("A", "B"),
    visit_var = "AVISIT",
    visit_levels = "WEEK 2 DAY 1"
  )

  expected <- list(
    data = quote({
      adqs <- adqs %>%
        dplyr::filter(ARMCD %in% c("ARM B", "ARM C", "ARM A")) %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"), new_level = "ARM B/ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM B/ARM C")) %>%
        droplevels() %>%
        df_explicit_na(na_level = "")
      adsl <- adsl %>%
        dplyr::filter(ARMCD %in% c("ARM B", "ARM C", "ARM A")) %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"), new_level = "ARM B/ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM B/ARM C")) %>%
        droplevels() %>%
        df_explicit_na(na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for A and B at WEEK 2 DAY 1 for Absolute Change from Baseline") %>% # nolint
        rtables::split_cols_by(var = "ARMCD", ref_group = "ARM B/ARM C") %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          "AVISIT",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::variable_labels(adqs["AVISIT"])
        ) %>%
        rtables::split_rows_by(
          "PARAMCD",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::variable_labels(adqs["PARAMCD"])
        ) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_ancova generates expressions with single endpoint", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    label_aval = "Absolute Change from Baseline",
    paramcd_levels = c("MYFAVORITE"),
    paramcd_var = "PARAMCD",
    label_paramcd = c("MYFAVORITE"),
    visit_var = "AVISIT",
    visit_levels = ""
  )

  expected <- list(
    data = quote({
      adqs <- adqs %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        droplevels() %>%
        df_explicit_na(na_level = "")
      adsl <- adsl %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        droplevels() %>%
        df_explicit_na(na_level = "")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for MYFAVORITE at  for Absolute Change from Baseline") %>% # nolint
        rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          "AVISIT",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = teal.data::variable_labels(adqs["AVISIT"])
        ) %>%
        rtables::append_topleft(paste0("  ", "MYFAVORITE")) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = NULL),
          conf_level = 0.95, var_labels = "Unadjusted comparison",
          .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
          table_names = "unadjusted_comparison"
        ) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95,
          var_labels = paste0(
            "Adjusted comparison (", paste(c("BASE", "STRATA1"), collapse = " + "), ")"
          ),
          table_names = "adjusted_comparison"
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
      result
    })
  )

  testthat::expect_equal(result, expected)
})
