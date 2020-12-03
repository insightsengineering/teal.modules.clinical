test_that("template_rsp generates standard expressions", {
  result <- template_rsp(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    aval_var = "AVALC",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    compare_arm = TRUE,
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      adsl <- adsl %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD))
    }),
    col_counts = quote(col_counts <- combine_counts(fct = adsl[["ARMCD"]])),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = 0.95,
          method = "waldcc"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp",
          show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = 0.95,
          method = "waldcc"
        ) %>%
        test_proportion_diff(
          vars = "is_rsp",
          method = "schouten"
        ) %>%
        estimate_odds_ratio(
          vars = "is_rsp",
          conf_level = 0.95
        ) %>%
        estimate_multinomial_response(
          var = "rsp_lab",
          conf_level = 0.95,
          method = "waldcc"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})


test_that("template_rsp generates right expressions with non-default", {
  result <- template_rsp(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    aval_var = "AVALC",
    ref_arm = "ARM B",
    comp_arm = c("ARM A", "ARM C"),
    compare_arm = TRUE,
    show_rsp_cat = FALSE
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      ADSL <- ADSL %>% # nolint
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM))
    }),
    col_counts = quote(col_counts <- combine_counts(fct = ADSL[["ARM"]])),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM", ref_group = "ARM B") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.95, method = "waldcc"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Unstratified Analysis", conf_level = 0.95,
          method = "waldcc") %>%
        test_proportion_diff(vars = "is_rsp", method = "schouten") %>%
        estimate_odds_ratio(vars = "is_rsp", conf_level = 0.95)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})

test_that("template_rsp generates expression without arm comparison", {
  result <- template_rsp(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    aval_var = "AVALC",
    ref_arm = "ARM B",
    comp_arm = c("ARM A", "ARM C"),
    compare_arm = FALSE,
    show_rsp_cat = FALSE
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      ADSL <- ADSL %>%  # nolint
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM))
    }),
    col_counts = quote(col_counts <- combine_counts(fct = ADSL[["ARM"]])),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.95, method = "waldcc"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})


test_that("template_rsp generates expression with non-default controls.", {
  result <- template_rsp(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    aval_var = "AVALC",
    ref_arm = "ARM B",
    comp_arm = c("ARM A", "ARM C"),
    compare_arm = TRUE,
    show_rsp_cat = TRUE,
    control = list(
      global = list(method = "jeffreys", conf_level = 0.80),
      unstrat = list(
        method_ci = "ha", method_test = "prop_chisq", odds = TRUE
      ),
      strat = list(method_ci = "cmh", method_test = "cmh", strat = "SEX")
    )
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      ADSL <- ADSL %>% # nolint
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>% #nolint
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM))
    }),
    col_counts = quote(col_counts <- combine_counts(fct = ADSL[["ARM"]])),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM", ref_group = "ARM B") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.8, method = "jeffreys"
        ) %>% estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = 0.8, method = "ha"
        ) %>%
        test_proportion_diff(vars = "is_rsp", method = "prop_chisq") %>%
        estimate_odds_ratio(vars = "is_rsp", conf_level = 0.8) %>%
        estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Stratified Analysis", variables = list(strata = "SEX"),
          conf_level = 0.8, method = "cmh"
        ) %>%
        test_proportion_diff(
          vars = "is_rsp", method = "cmh", variables = list(strata = "SEX")
        ) %>%
        estimate_multinomial_response(
          var = "rsp_lab", conf_level = 0.8, method = "jeffreys"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})

test_that("template_rsp can combine arms", {
  result <- template_rsp(
    dataname = "adrs",
    parentname = "ADSL",
    arm_var = "ARMCD",
    aval_var = "AVALC",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    compare_arm = TRUE,
    combine_arm = TRUE,
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      ADSL <- ADSL %>% # nolint
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD))
    }),
    combine_arm = quote(
      groups <- combine_groups(fct = anl[["ARMCD"]], ref = "ARM A")
    ),
    col_counts = quote(col_counts <- combine_counts(fct = ADSL[["ARMCD"]], groups_list = groups)),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by_groups(
          var = "ARMCD", groups_list = groups, ref_group = names(groups)[1]
        ) %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.95, method = "waldcc"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Unstratified Analysis", conf_level = 0.95,
          method = "waldcc"
        ) %>%
        test_proportion_diff(vars = "is_rsp", method = "schouten") %>%
        estimate_odds_ratio(vars = "is_rsp", conf_level = 0.95) %>%
        estimate_multinomial_response(
          var = "rsp_lab", conf_level = 0.95, method = "waldcc"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})

test_that("split_col_expr prepare the right four possible expressions", {
  result <- list(
    split_col_expr(compare = TRUE, combine = TRUE, group = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = TRUE, combine = FALSE, group = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = FALSE, combine = TRUE, group = "ARMCD", ref = "ARM C"),
    split_col_expr(compare = FALSE, combine = FALSE, group = "ARMCD", ref = "ARM C")
  )
  expected <- list(
    quote(split_cols_by_groups(var = "ARMCD", groups_list = groups, ref_group = names(groups)[1])),
    quote(split_cols_by(var = "ARMCD", ref_group = "ARM C")),
    quote(split_cols_by_groups(var = "ARMCD", groups_list = groups)),
    quote(split_cols_by(var = "ARMCD"))
  )
  expect_equal_expr_list(result, expected)
})

test_that("template_rsp generates standard expressions", {
  result <- template_rsp(
    dataname = "adrs",
    parentname = "ADSL",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    compare_arm = TRUE,
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      ADSL <- ADSL %>% # nolint
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD))
    }),
    col_counts = quote(col_counts <- combine_counts(fct = ADSL[["ARMCD"]])),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp",
          conf_level = 0.95,
          method = "waldcc"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp",
          show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = 0.95,
          method = "waldcc"
        ) %>%
        test_proportion_diff(
          vars = "is_rsp",
          method = "schouten"
        ) %>%
        estimate_odds_ratio(
          vars = "is_rsp",
          conf_level = 0.95
        ) %>%
        estimate_multinomial_response(
          var = "rsp_lab",
          conf_level = 0.95,
          method = "waldcc"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})


test_that("template_rsp generates right expressions with non-default", {
  result <- template_rsp(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    ref_arm = "ARM B",
    comp_arm = c("ARM A", "ARM C"),
    compare_arm = TRUE,
    show_rsp_cat = FALSE
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      ADSL <- ADSL %>% # nolint
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM))
    }),
    col_counts = quote(col_counts <- combine_counts(fct = ADSL[["ARM"]])),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM", ref_group = "ARM B") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.95, method = "waldcc"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Unstratified Analysis", conf_level = 0.95,
          method = "waldcc") %>%
        test_proportion_diff(vars = "is_rsp", method = "schouten") %>%
        estimate_odds_ratio(vars = "is_rsp", conf_level = 0.95)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})

test_that("template_rsp generates expression without arm comparison", {
  result <- template_rsp(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    ref_arm = "ARM B",
    comp_arm = c("ARM A", "ARM C"),
    compare_arm = FALSE,
    show_rsp_cat = FALSE
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      ADSL <- ADSL %>% # nolint
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM))
    }),
    col_counts = quote(col_counts <- combine_counts(fct = ADSL[["ARM"]])),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.95, method = "waldcc"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})


test_that("template_rsp generates expression with non-default controls.", {
  result <- template_rsp(
    dataname = "ADRS",
    parentname = "ADSL",
    arm_var = "ARM",
    ref_arm = "ARM B",
    comp_arm = c("ARM A", "ARM C"),
    compare_arm = TRUE,
    show_rsp_cat = TRUE,
    control = list(
      global = list(method = "jeffreys", conf_level = 0.80),
      unstrat = list(
        method_ci = "ha", method_test = "prop_chisq", odds = TRUE
      ),
      strat = list(method_ci = "cmh", method_test = "cmh", strat = "SEX")
    )
  )

  expected <- list(
    data = quote({
      anl <- ADRS %>%
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      ADSL <- ADSL %>% # nolint
        filter(ARM %in% c("ARM B", "ARM A", "ARM C")) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B")) %>%
        mutate(ARM = droplevels(ARM))
    }),
    col_counts = quote(col_counts <- combine_counts(fct = ADSL[["ARM"]])),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM", ref_group = "ARM B") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.8, method = "jeffreys"
        ) %>% estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Unstratified Analysis",
          conf_level = 0.8, method = "ha"
        ) %>%
        test_proportion_diff(vars = "is_rsp", method = "prop_chisq") %>%
        estimate_odds_ratio(vars = "is_rsp", conf_level = 0.8) %>%
        estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Stratified Analysis", variables = list(strata = "SEX"),
          conf_level = 0.8, method = "cmh"
        ) %>%
        test_proportion_diff(
          vars = "is_rsp", method = "cmh", variables = list(strata = "SEX")
        ) %>%
        estimate_multinomial_response(
          var = "rsp_lab", conf_level = 0.8, method = "jeffreys"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})

test_that("template_rsp can combine refs", {
  result <- template_rsp(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = "ARM C",
    compare_arm = TRUE,
    combine_arm = FALSE,
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A/ARM B")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR"))
      adsl <- adsl %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A/ARM B")) %>%
        mutate(ARMCD = droplevels(ARMCD))
    }),
    col_counts = quote(col_counts <- combine_counts(fct = adsl[["ARMCD"]])),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A/ARM B") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.95, method = "waldcc"
        ) %>%
        estimate_proportion_diff(
          vars = "is_rsp", show_labels = "visible",
          var_labels = "Unstratified Analysis", conf_level = 0.95,
          method = "waldcc"
        ) %>%
        test_proportion_diff(vars = "is_rsp", method = "schouten") %>%
        estimate_odds_ratio(vars = "is_rsp", conf_level = 0.95) %>%
        estimate_multinomial_response(
          var = "rsp_lab", conf_level = 0.95, method = "waldcc"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, col_counts = col_counts)
      result
    })
  )

  expect_equal_expr_list(result, expected)
})
