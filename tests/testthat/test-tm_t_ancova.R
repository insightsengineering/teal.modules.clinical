
test_that("template_ancova generates expressions with multiple endpoints", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    avisit = "WEEK 1 DAY 8",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd = c("BFIALL", "FATIGI")
  )

  expected <- list(
    data = quote(
      anl <- adqs %>%
        filter(AVISIT %in% "WEEK 1 DAY 8" & PARAMCD %in% c("BFIALL", "FATIGI")) %>%
        droplevels()
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
        split_rows_by("PARAMCD") %>%
        summarize_ancova(
          vars = "CHG",
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
          conf_level = 0.95, var_labels = "Adjusted mean", show_labels = "hidden"
        )
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, col_counts = table(droplevels(adsl$ARMCD)))
    )
  )

  expect_equal(result, expected)
})

test_that("template_ancova generates expressions with single endpoint", {
  result <- template_ancova(
    parentname = "adsl",
    dataname = "adqs",
    avisit = "WEEK 1 DAY 8",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    cov_var = c("BASE", "STRATA1"),
    aval_var = "CHG",
    paramcd = "FKSI-FWB"
  )

  expected <- list(
    data = quote(
      anl <- adqs %>%
        filter(AVISIT %in% "WEEK 1 DAY 8" & PARAMCD %in% "FKSI-FWB") %>%
        droplevels()
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
        summarize_ancova(
          vars = "CHG", variables = list(arm = "ARMCD", covariates = NULL),
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
      result <- build_table(lyt = lyt, df = anl, col_counts = table(droplevels(adsl$ARMCD)))
    )
  )

  expect_equal(result, expected)
})
