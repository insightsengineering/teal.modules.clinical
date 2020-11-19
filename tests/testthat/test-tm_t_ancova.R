test_that("template_ancova generates expressions with multiple endpoints", {
  result <- template_ancova(
    anl_name = "adqs",
    parent_name = "adsl",
    arm_var = "ARMCD",
    arm_ref_comp = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    aval_var = "CHG",
    cov_var = c("BASE", "STRATA1"),
    paramcd = c("BFIALL", "FATIGI")
  )

  expected <- list(
    data = quote({
      adqs <- filter(adqs, ARMCD %in% c("ARM A", "ARM B", "ARM C"))
      adsl <- filter(adsl, ARMCD %in% c("ARM A", "ARM B", "ARM C"))
      adqs$ARMCD <- droplevels(relevel(adqs$ARMCD, "ARM A")) # nolint
      adsl$ARMCD <- droplevels(relevel(adsl$ARMCD, "ARM A")) # nolint
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
        split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = adqs, col_counts = table(adsl$ARMCD)) # nolint
    )
  )

  expect_equal_expr_list(result, expected)
})

test_that("template_ancova generates expressions with multiple endpoints with combined arms", {
  result <- template_ancova(
    parent_name = "adsl",
    anl_name = "adqs",
    arm_var = "ARMCD",
    arm_ref_comp = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = TRUE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd = c("BFIALL", "FATIGI")
  )

  expected <- list(
    data = quote({
      adqs <- filter(adqs, ARMCD %in% c("ARM A", "ARM B", "ARM C"))
      adsl <- filter(adsl, ARMCD %in% c("ARM A", "ARM B", "ARM C"))
      adqs$ARMCD <- droplevels(relevel(adqs$ARMCD, "ARM A")) # nolint
      adsl$ARMCD <- droplevels(relevel(adsl$ARMCD, "ARM A")) # nolint
      adqs$ARMCD <- combine_levels(x = adqs$ARMCD, levels = c("ARM B", "ARM C")) # nolint
      adsl$ARMCD <- combine_levels(x = adsl$ARMCD, levels = c("ARM B", "ARM C")) # nolint
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
        split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = adqs, col_counts = table(adsl$ARMCD)) # nolint
    )
  )

  expect_equal_expr_list(result, expected)
})

test_that("template_ancova generates expressions with single endpoint", {
  result <- template_ancova(
    parent_name = "adsl",
    anl_name = "adqs",
    arm_var = "ARMCD",
    arm_ref_comp = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd = "FKSI-FWB"
  )

  expected <- list(
    data = quote({
      adqs <- filter(adqs, ARMCD %in% c("ARM A", "ARM B", "ARM C")) # nolint
      adsl <- filter(adsl, ARMCD %in% c("ARM A", "ARM B", "ARM C")) # nolint
      adqs$ARMCD <- droplevels(relevel(adqs$ARMCD, "ARM A")) # nolint
      adsl$ARMCD <- droplevels(relevel(adsl$ARMCD, "ARM A")) # nolint
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = NULL),
          conf_level = 0.95, var_labels = "Unadjusted comparison",
          .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means")
        ) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95,
          var_labels = paste0(
            "Adjusted comparison (", paste(c("BASE", "STRATA1"), collapse = " + "), ")"
          )
        )
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = adqs, col_counts = table(adsl$ARMCD)) # nolint
    )
  )

  expect_equal_expr_list(result, expected)
})
