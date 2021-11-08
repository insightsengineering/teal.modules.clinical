test_that("template_ancova generates expressions with multiple endpoints", {
  result <- template_ancova(
    dataname = "adqs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    aval_var = "CHG",
    cov_var = c("BASE", "STRATA1"),
    paramcd_levels = c("FKSI-ALL", "BKWBXYZ"),
    paramcd_var = "PARAMCD",
    visit_var = "AVISIT"
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
      lyt <- basic_table(
        title = paste(
          "Table of",
          paste(head(c("FKSI-ALL", "BKWBXYZ"), -1), collapse = ", "),
          ifelse(length(c("FKSI-ALL", "BKWBXYZ")) > 1, "and", ""),
          tail(c("FKSI-ALL", "BKWBXYZ"), 1),
          ifelse(length(c("FKSI-ALL", "BKWBXYZ")) > 1, "parameters", "parameter"),
          "at",
          ifelse(length(unique(adqs[["AVISIT"]])) > 1, "visits", "visit"),
          paste(head(unique(adqs[["AVISIT"]]), -1), collapse = ", "),
          ifelse(length(unique(adqs[["AVISIT"]])) > 1, "and", ""),
          tail(unique(adqs[["AVISIT"]]), 1), "for", "CHG")
        ) %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adqs["AVISIT"], fill = TRUE)
        ) %>%
        split_rows_by(
          "PARAMCD",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adqs["PARAMCD"], fill = TRUE)
        ) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_ancova generates expressions with multiple endpoints with combined comparison arms", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = TRUE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd_levels = c("A", "B"),
    paramcd_var = "PARAMCD",
    visit_var = "AVISIT"
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
      lyt <- basic_table(
       title = paste(
          "Table of",
          paste(head(c("A", "B"), -1), collapse = ", "),
          ifelse(length(c("A", "B")) > 1, "and", ""),
          tail(c("A", "B"), 1),
          ifelse(length(c("A", "B")) > 1, "parameters", "parameter"),
          "at",
          ifelse(length(unique(adqs[["AVISIT"]])) > 1, "visits", "visit"),
          paste(head(unique(adqs[["AVISIT"]]), -1), collapse = ", "),
          ifelse(length(unique(adqs[["AVISIT"]])) > 1, "and", ""),
          tail(unique(adqs[["AVISIT"]]), 1), "for", "CHG")
          ) %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adqs["AVISIT"], fill = TRUE)
        ) %>%
        split_rows_by(
          "PARAMCD",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adqs["PARAMCD"], fill = TRUE)
        ) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_ancova generates expressions with multiple endpoints with combined reference arms", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    arm_var = "ARMCD",
    ref_arm = c("ARM B", "ARM C"),
    comp_arm = "ARM A",
    combine_comp_arms = FALSE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd_levels = c("A", "B"),
    paramcd_var = "PARAMCD",
    visit_var = "AVISIT"
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
      lyt <- basic_table(
        title = paste(
          "Table of",
          paste(head(c("A", "B"), -1), collapse = ", "),
          ifelse(length(c("A", "B")) > 1, "and", ""),
          tail(c("A", "B"), 1),
          ifelse(length(c("A", "B")) > 1, "parameters", "parameter"),
          "at",
          ifelse(length(unique(adqs[["AVISIT"]])) > 1, "visits", "visit"),
          paste(head(unique(adqs[["AVISIT"]]), -1), collapse = ", "),
          ifelse(length(unique(adqs[["AVISIT"]])) > 1, "and", ""),
          tail(unique(adqs[["AVISIT"]]), 1), "for", "CHG")
      ) %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM B/ARM C") %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adqs["AVISIT"], fill = TRUE)
        ) %>%
        split_rows_by(
          "PARAMCD",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adqs["PARAMCD"], fill = TRUE)
        ) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_ancova generates expressions with single endpoint", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd_levels = c("MYFAVORITE"),
    paramcd_var = "PARAMCD",
    visit_var = "AVISIT"
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
      lyt <- basic_table(
        title = paste(
          "Table of",
          paste(head("MYFAVORITE", -1), collapse = ", "),
          ifelse(length("MYFAVORITE") > 1, "and", ""),
          tail("MYFAVORITE", 1),
          ifelse(length("MYFAVORITE") > 1, "parameters", "parameter"),
          "at",
          ifelse(length(unique(adqs[["AVISIT"]])) > 1, "visits", "visit"),
          paste(head(unique(adqs[["AVISIT"]]), -1), collapse = ", "),
          ifelse(length(unique(adqs[["AVISIT"]])) > 1, "and", ""),
          tail(unique(adqs[["AVISIT"]]), 1), "for", "CHG")
      ) %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(adqs["AVISIT"], fill = TRUE)
        ) %>%
        append_topleft(paste0("  ", "MYFAVORITE")) %>%
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
      result <- build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
      result
    })
  )

  expect_equal(result, expected)
})
