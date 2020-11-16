
test_that("template_rsp generates standard expressions", {
  result <- template_rsp(
    dataname = "adrs",
    param = "INVET",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    compare_arm = TRUE,
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote(
      anl <- adrs %>%
        filter(PARAMCD == "INVET") %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(
          is_rsp = rsp_lab %in% c(
            "Complete Response (CR)", "Partial Response (PR)"
          )
        ) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A"))
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
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
    table = quote(result <- build_table(lyt = lyt, df = anl))
  )

  expect_equal(result, expected)
})


test_that("template_rsp generates right expressions with non-default", {
  result <- template_rsp(
    dataname = "ADRS",
    param = "BESRSPI",
    arm_var = "ARM",
    ref_arm = "ARM B",
    compare_arm = TRUE,
    show_rsp_cat = FALSE
  )

  expected <- list(
    data = quote(
      anl <- ADRS %>%
        filter(PARAMCD == "BESRSPI") %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(
          is_rsp = rsp_lab %in%
            c("Complete Response (CR)", "Partial Response (PR)")
        ) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B"))
    ),
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
    table = quote(result <- build_table(lyt = lyt, df = anl))
  )

  expect_equal(result, expected)
})

test_that("template_rsp generates expression without arm comparison", {
  result <- template_rsp(
    dataname = "ADRS",
    param = "BESRSPI",
    arm_var = "ARM",
    ref_arm = "ARM B",
    compare_arm = FALSE,
    show_rsp_cat = FALSE
  )

  expected <- list(
    data = quote(
      anl <- ADRS %>%
        filter(PARAMCD == "BESRSPI") %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>% mutate(
          is_rsp = rsp_lab %in%
            c("Complete Response (CR)", "Partial Response (PR)")
        ) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B"))
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        estimate_proportion(
          vars = "is_rsp", conf_level = 0.95, method = "waldcc"
        )
    ),
    table = quote(result <- build_table(lyt = lyt, df = anl))
  )

  expect_equal(result, expected)
})


test_that("template_rsp generates expression with non-default controls.", {
  result <- template_rsp(
    dataname = "ADRS",
    param = "BESRSPI",
    arm_var = "ARM",
    ref_arm = "ARM B",
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
    data = quote(
      anl <- ADRS %>%
        filter(PARAMCD == "BESRSPI") %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>% mutate(
          is_rsp = rsp_lab %in%
            c("Complete Response (CR)", "Partial Response (PR)")
        ) %>%
        mutate(ARM = relevel(ARM, ref = "ARM B"))
    ),
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
    table = quote(result <- build_table(lyt = lyt, df = anl))
  )
  expect_equal(result, expected)
})

test_that("template_rsp can combine arms", {
  result <- template_rsp(
    dataname = "adrs",
    param = "INVET",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    compare_arm = TRUE,
    combine_arm = TRUE,
    show_rsp_cat = TRUE
  )

  expected <- list(
    data = quote(
      anl <- adrs %>%
        filter(PARAMCD == "INVET") %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(
          is_rsp = rsp_lab %in% c(
            "Complete Response (CR)", "Partial Response (PR)"
          )
        ) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A"))
    ),
    combine_arm = quote(
      groups <- combine_groups(fct = anl[["ARMCD"]], ref = "ARM A")
    ),
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
    table = quote(result <- build_table(lyt = lyt, df = anl))
  )

  expect_equal(result, expected)
})
