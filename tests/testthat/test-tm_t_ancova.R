test_that("template_ancova generates expressions with multiple endpoints", {
  result <- template_ancova(
    anl_name = "adqs",
    parent_name = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    aval_var = "CHG",
    cov_var = c("BASE", "STRATA1"),
    paramcd = c("BFIALL", "FATIGI")
  )

  expected <- list(
    data = quote({
      adqs <- adqs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        droplevels() %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A"))
      adsl <- adsl %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        droplevels() %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A"))
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
      result <- build_table(lyt = lyt, df = adqs, col_counts = table(adsl$ARMCD))
    )
  )

  expect_equal(result, expected)
})

test_that("template_ancova generates expressions with multiple endpoints with combined comparison arms", {
  result <- template_ancova(
    parent_name = "adsl",
    anl_name = "adqs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = TRUE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd = c("BFIALL", "FATIGI")
  )

  expected <- list(
    data = quote({
      adqs <- adqs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        droplevels() %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C")))
      adsl <- adsl %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        droplevels() %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C")))
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
      result <- build_table(lyt = lyt, df = adqs, col_counts = table(adsl$ARMCD))
    )
  )

  expect_equal(result, expected)
})

test_that("template_ancova generates expressions with multiple endpoints with combined reference arms", {
  result <- template_ancova(
    parent_name = "adsl",
    anl_name = "adqs",
    arm_var = "ARMCD",
    ref_arm = c("ARM B", "ARM C"),
    comp_arm = "ARM A",
    combine_comp_arms = FALSE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd = c("BFIALL", "FATIGI")
  )

  expected <- list(
    data = quote({
      adqs <- adqs %>%
        filter(ARMCD %in% c("ARM B", "ARM C", "ARM A")) %>%
        droplevels() %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"), new_level = "ARM B/ARM C"))
      adsl <- adsl %>%
        filter(ARMCD %in% c("ARM B", "ARM C", "ARM A")) %>%
        droplevels() %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"), new_level = "ARM B/ARM C"))
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM B/ARM C") %>%
        split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
        split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = adqs, col_counts = table(adsl$ARMCD))
    )
  )

  expect_equal(result, expected)
})


test_that("template_ancova generates expressions with single endpoint", {
  result <- template_ancova(
    parent_name = "adsl",
    anl_name = "adqs",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd = "FKSI-FWB"
  )

  expected <- list(
    data = quote({
      adqs <- adqs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        droplevels() %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A"))
      adsl <- adsl %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        droplevels() %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A"))
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
      result <- build_table(lyt = lyt, df = adqs, col_counts = table(adsl$ARMCD))
    )
  )

  expect_equal(result, expected)
})
