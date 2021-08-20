test_that("template_rsp generates standard expressions", {
  test.nest::skip_if_too_deep(0)

  result <- template_rsp(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    compare_arm = TRUE,
    combine_comp_arms = FALSE,
    aval_var = "AVALC",
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
        df_explicit_na(na_level = "")
      adsl <- adsl %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        df_explicit_na(na_level = "")
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "prop_est"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp",
          show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "u_prop_diff"
        ) %>%
        test_proportion_diff(
          vars = "is_rsp",
          method = "schouten",
          table_names = "u_test_diff"
        ) %>%
        estimate_odds_ratio(
          vars = "is_rsp",
          conf_level = 0.95,
          table_names = "u_est_or"
        ) %>%
        estimate_multinomial_response(
          var = "AVALC",
          conf_level = 0.95,
          method = "waldcc"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_rsp generates right expressions with non-default", {
  test.nest::skip_if_too_deep(0)

  result <- template_rsp(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    aval_var = "AVALC",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = TRUE,
    combine_comp_arms = FALSE,
    show_rsp_cat = FALSE
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        filter(ARM %in% c("B: Placebo", "A: Drug X", "C: Combination")) %>%
        mutate(ARM = relevel(ARM, ref = "B: Placebo")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
        df_explicit_na(na_level = "")
      ADSL <- ADSL %>% # nolint
        filter(ARM %in% c("B: Placebo", "A: Drug X", "C: Combination")) %>%
        mutate(ARM = relevel(ARM, ref = "B: Placebo")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        df_explicit_na(na_level = "")
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "prop_est"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp",
          show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "u_prop_diff"
          ) %>%
        test_proportion_diff(
          vars = "is_rsp",
          method = "schouten",
          table_names = "u_test_diff"
        ) %>%
        estimate_odds_ratio(
          vars = "is_rsp",
          conf_level = 0.95,
          table_names = "u_est_or"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_rsp generates expression without arm comparison", {
  test.nest::skip_if_too_deep(0)

  result <- template_rsp(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    aval_var = "AVALC",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = FALSE,
    show_rsp_cat = FALSE
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
        df_explicit_na(na_level = "")
      ADSL <- ADSL %>%  # nolint
        mutate(ARM = droplevels(ARM)) %>%
        df_explicit_na(na_level = "")
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "prop_est"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_rsp generates expression with non-default controls and strata.", {
  test.nest::skip_if_too_deep(0)

  result <- template_rsp(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    aval_var = "AVALC",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = TRUE,
    show_rsp_cat = TRUE,
    control = list(
      global = list(method = "jeffreys", conf_level = 0.80),
      unstrat = list(
        method_ci = "ha", method_test = "chisq", odds = TRUE
      ),
      strat = list(method_ci = "cmh", method_test = "cmh", strat = "SEX")
    )
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        filter(ARM %in% c("B: Placebo", "A: Drug X", "C: Combination")) %>%
        mutate(ARM = relevel(ARM, ref = "B: Placebo")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
        df_explicit_na(na_level = "")
      ADSL <- ADSL %>% # nolint
        filter(ARM %in% c("B: Placebo", "A: Drug X", "C: Combination")) %>% #nolint
        mutate(ARM = relevel(ARM, ref = "B: Placebo")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        df_explicit_na(na_level = "")
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = 0.8,
          method = "jeffreys",
          table_names = "prop_est"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp",
          show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = 0.8,
          method = "ha",
          table_names = "u_prop_diff"
        ) %>%
        test_proportion_diff(
          vars = "is_rsp",
          method = "chisq",
          table_names = "u_test_diff"
        ) %>%
        estimate_odds_ratio(
          vars = "is_rsp",
          conf_level = 0.8,
          table_names = "u_est_or"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Stratified Analysis", variables = list(strata = "SEX"),
          conf_level = 0.8, method = "cmh",
          table_names = "s_prop_diff"
        ) %>%
        test_proportion_diff(
          vars = "is_rsp",
          method = "cmh",
          variables = list(strata = "SEX"),
          table_names = "s_test_diff"
        ) %>%
        estimate_odds_ratio(
          vars = "is_rsp",
          variables = list(arm = "ARM", strata = "SEX"),
          conf_level = 0.8,
          table_names = "s_est_or"
        ) %>%
        estimate_multinomial_response(
          var = "AVALC", conf_level = 0.8, method = "jeffreys"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_rsp can combine comparison arms", {
  test.nest::skip_if_too_deep(0)

  result <- template_rsp(
    dataname = "adrs",
    parentname = "ADSL",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    compare_arm = TRUE,
    combine_comp_arms = TRUE,
    aval_var = "AVALC",
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
        df_explicit_na(na_level = "")
      ADSL <- ADSL %>% # nolint
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        df_explicit_na(na_level = "")
    }),
    combine_comp_arms = quote(
      groups <- combine_groups(fct = ADSL[["ARMCD"]], ref = "ARM A")
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by_groups(
          var = "ARMCD", groups_list = groups, ref_group = names(groups)[1]
        ) %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "prop_est"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp",
          show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "u_prop_diff"
        ) %>%
        test_proportion_diff(
          vars = "is_rsp",
          method = "schouten",
          table_names = "u_test_diff"
        ) %>%
        estimate_odds_ratio(
          vars = "is_rsp",
          conf_level = 0.95,
          table_names = "u_est_or"
        ) %>%
        estimate_multinomial_response(
          var = "AVALC", conf_level = 0.95, method = "waldcc"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      result
    })
  )

  expect_equal(result, expected)
})

test_that("split_col_expr prepare the right four possible expressions", {
  test.nest::skip_if_too_deep(0)

  result <- list(
    split_col_expr(compare = TRUE, combine = TRUE, arm_var = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = TRUE, combine = FALSE, arm_var = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = FALSE, combine = TRUE, arm_var = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = FALSE, combine = FALSE, arm_var = "ARMCD", ref = "ARM C")
  )
  expected <- list(
    quote(split_cols_by_groups(var = "ARMCD", groups_list = groups, ref_group = names(groups)[1])),
    quote(split_cols_by(var = "ARMCD", ref_group = "ARM C")),
    quote(split_cols_by(var = "ARMCD")),
    quote(split_cols_by(var = "ARMCD"))
  )
  expect_equal(result, expected)
})

test_that("template_rsp can combine refs", {
  test.nest::skip_if_too_deep(0)

  result <- template_rsp(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = "ARM C",
    compare_arm = TRUE,
    combine_comp_arms = FALSE,
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A/ARM B")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
        df_explicit_na(na_level = "")
      adsl <- adsl %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A/ARM B")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        df_explicit_na(na_level = "")
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A/ARM B") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "prop_est"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp",
          show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "u_prop_diff"
        ) %>%
        test_proportion_diff(
          vars = "is_rsp",
          method = "schouten",
          table_names = "u_test_diff"
        ) %>%
        estimate_odds_ratio(
          vars = "is_rsp",
          conf_level = 0.95,
          table_names = "u_est_or"
        ) %>%
        estimate_multinomial_response(
          var = "AVALC", conf_level = 0.95, method = "waldcc"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )

  expect_equal(result, expected)
})
