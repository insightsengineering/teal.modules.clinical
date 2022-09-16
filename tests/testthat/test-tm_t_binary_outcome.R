testthat::test_that("template_binary_outcome generates standard expressions", {
  result <- template_binary_outcome(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    paramcd = "BESRSPI",
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
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(is_rsp = AVALC %in% c("Complete Response (CR)", "Partial Response (PR)")) %>%
        dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", "Partial Response (PR)")))
      adsl <- adsl %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        df_explicit_na()
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders",
        subtitles = ""
      ) %>%
        rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        rtables::add_colcounts() %>%
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_binary_outcome generates right expressions with non-default", {
  result <- template_binary_outcome(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    paramcd = "BESRSPI",
    aval_var = "AVALC",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = TRUE,
    combine_comp_arms = FALSE,
    show_rsp_cat = FALSE,
    responder_val = c("PR", "SD")
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        dplyr::filter(ARM %in% c("B: Placebo", "A: Drug X", "C: Combination")) %>%
        dplyr::mutate(ARM = stats::relevel(ARM, ref = "B: Placebo")) %>%
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        dplyr::mutate(is_rsp = AVALC %in% c("PR", "SD")) %>%
        dplyr::mutate(AVALC = factor(AVALC, levels = c("PR", "SD")))
      ADSL <- ADSL %>% # nolint
        dplyr::filter(ARM %in% c("B: Placebo", "A: Drug X", "C: Combination")) %>%
        dplyr::mutate(ARM = stats::relevel(ARM, ref = "B: Placebo")) %>%
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        df_explicit_na()
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "Table of BESRSPI for PR and SD Responders",
        subtitles = ""
      ) %>%
        rtables::split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
        rtables::add_colcounts() %>%
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_binary_outcome generates expression without arm comparison", {
  result <- template_binary_outcome(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    paramcd = "BESRSPI",
    aval_var = "AVALC",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = FALSE,
    show_rsp_cat = FALSE
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        dplyr::mutate(is_rsp = AVALC %in% c("Complete Response (CR)", "Partial Response (PR)")) %>%
        dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", "Partial Response (PR)")))
      ADSL <- ADSL %>% # nolint
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        df_explicit_na()
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders",
        subtitles = ""
      ) %>%
        rtables::split_cols_by(var = "ARM") %>%
        rtables::add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = 0.95,
          method = "waldcc",
          table_names = "prop_est"
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_binary_outcome generates expression with non-default controls and strata.", {
  result <- template_binary_outcome(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    paramcd = "BESRSPI",
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
        dplyr::filter(ARM %in% c("B: Placebo", "A: Drug X", "C: Combination")) %>%
        dplyr::mutate(ARM = stats::relevel(ARM, ref = "B: Placebo")) %>%
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        dplyr::mutate(is_rsp = AVALC %in% c("Complete Response (CR)", "Partial Response (PR)")) %>%
        dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", "Partial Response (PR)")))
      ADSL <- ADSL %>% # nolint
        dplyr::filter(ARM %in% c("B: Placebo", "A: Drug X", "C: Combination")) %>% # nolint
        dplyr::mutate(ARM = stats::relevel(ARM, ref = "B: Placebo")) %>%
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        df_explicit_na()
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders",
        subtitles = "Stratified by SEX"
      ) %>%
        rtables::split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
        rtables::add_colcounts() %>%
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_binary_outcome can combine comparison arms", {
  result <- template_binary_outcome(
    dataname = "adrs",
    parentname = "ADSL",
    arm_var = "ARMCD",
    paramcd = "BESRSPI",
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
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(is_rsp = AVALC %in% c("Complete Response (CR)", "Partial Response (PR)")) %>%
        dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", "Partial Response (PR)")))
      ADSL <- ADSL %>% # nolint
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        df_explicit_na()
    }),
    combine_comp_arms = quote(
      groups <- combine_groups(fct = ADSL[["ARMCD"]], ref = "ARM A")
    ),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders",
        subtitles = ""
      ) %>%
        split_cols_by_groups(
          var = "ARMCD", groups_list = groups, ref_group = names(groups)[1]
        ) %>%
        rtables::add_colcounts() %>%
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_binary_outcome can combine comparison arms when compare arms is FALSE", {
  result <- template_binary_outcome(
    dataname = "adrs",
    parentname = "ADSL",
    arm_var = "ARMCD",
    paramcd = "BESRSPI",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    compare_arm = FALSE,
    combine_comp_arms = TRUE,
    aval_var = "AVALC",
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(is_rsp = AVALC %in% c(
          "Complete Response (CR)",
          "Partial Response (PR)"
        )) %>%
        dplyr::mutate(AVALC = factor(AVALC,
          levels = c("Complete Response (CR)", "Partial Response (PR)")
        ))
      ADSL <- ADSL %>% # nolint
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        df_explicit_na()
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders",
        subtitles = ""
      ) %>%
        rtables::split_cols_by(var = "ARMCD") %>%
        rtables::add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.95, method = "waldcc",
          table_names = "prop_est"
        ) %>%
        estimate_multinomial_response(
          var = "AVALC",
          conf_level = 0.95, method = "waldcc"
        )
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("split_col_expr prepare the right four possible expressions", {
  result <- list(
    split_col_expr(compare = TRUE, combine = TRUE, arm_var = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = TRUE, combine = FALSE, arm_var = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = FALSE, combine = TRUE, arm_var = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = FALSE, combine = FALSE, arm_var = "ARMCD", ref = "ARM C")
  )
  expected <- list(
    quote(split_cols_by_groups(var = "ARMCD", groups_list = groups, ref_group = names(groups)[1])),
    quote(rtables::split_cols_by(var = "ARMCD", ref_group = "ARM C")),
    quote(rtables::split_cols_by(var = "ARMCD")),
    quote(rtables::split_cols_by(var = "ARMCD"))
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_binary_outcome can combine refs", {
  result <- template_binary_outcome(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    paramcd = "BESRSPI",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = "ARM C",
    compare_arm = TRUE,
    combine_comp_arms = FALSE,
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A/ARM B")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(is_rsp = AVALC %in% c("Complete Response (CR)", "Partial Response (PR)")) %>%
        dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", "Partial Response (PR)")))
      adsl <- adsl %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A/ARM B")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        df_explicit_na()
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders",
        subtitles = ""
      ) %>%
        rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A/ARM B") %>%
        rtables::add_colcounts() %>%
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )

  testthat::expect_equal(result, expected)
})
