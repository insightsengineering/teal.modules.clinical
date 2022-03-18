testthat::test_that("template_logistic generates correct expressions", {
  result <- template_logistic(
    dataname = "ANL",
    arm_var = "ARMCD",
    aval_var = "AVALC",
    paramcd = "PARAMCD",
    label_paramcd = "Best Confirmed Overall Response by Investigator",
    cov_var = c("AGE", "SEX"),
    interaction_var = "AGE",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = "ARM C",
    conf_level = 0.95,
    combine_comp_arms = FALSE,
    responder_val = c("CR"),
    topleft = "BESRSPI",
    at = c(30, 40)
  )

  expected <- list(
    arm_lab = quote(arm_var_lab <- teal.data::variable_labels(ANL["ARMCD"], fill = FALSE)),
    data = quote({
      ANL <- ANL %>% # nolint
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A/ARM B")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD))
      ANL <- ANL %>% # nolint
        dplyr::mutate(Response = AVALC %in% "CR") %>%
        df_explicit_na(na_level = "")
    }),
    relabel = quote(teal.data::variable_labels(ANL["ARMCD"]) <- arm_var_lab), # nolint
    model = quote(
      mod <- fit_logistic(
        ANL,
        variables = list(response = "Response", arm = "ARMCD", covariates = c("AGE", "SEX"), interaction = "AGE")
      ) %>%
        broom::tidy(conf_level = 0.95, at = c(30, 40)) %>%
        df_explicit_na(na_level = "")
    ),
    table = quote({
      result <- rtables::basic_table(title = "Summary of Logistic Regression Analysis for Best Confirmed Overall Response by Investigator for CR Responders") %>% # nolint
        summarize_logistic(conf_level = 0.95) %>%
        rtables::append_topleft("BESRSPI") %>%
        rtables::build_table(df = mod)
      result
    })
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_logistic generates correct expressions for no arm variable", {
  result <- template_logistic(
    dataname = "ANL",
    arm_var = NULL,
    aval_var = "AVALC",
    paramcd = "PARAMCD",
    label_paramcd = "Best Confirmed Overall Response by Investigator",
    cov_var = c("AGE", "SEX"),
    interaction_var = "AGE",
    conf_level = 0.95,
    combine_comp_arms = FALSE,
    responder_val = c("CR"),
    topleft = "BESRSPI",
    at = c(30, 40)
  )

  expected <- list(
    data = quote({
      ANL <- ANL %>% # nolint
        dplyr::mutate(Response = AVALC %in% "CR") %>%
        df_explicit_na(na_level = "")
    }),
    model = quote(
      mod <- fit_logistic(
        ANL,
        variables = list(response = "Response", arm = NULL, covariates = c("AGE", "SEX"), interaction = "AGE")
      ) %>%
        broom::tidy(conf_level = 0.95, at = c(30, 40)) %>%
        df_explicit_na(na_level = "")
    ),
    table = quote({
      result <- rtables::basic_table(title = "Summary of Logistic Regression Analysis for Best Confirmed Overall Response by Investigator for CR Responders") %>% # nolint
        summarize_logistic(conf_level = 0.95) %>%
        rtables::append_topleft("BESRSPI") %>%
        rtables::build_table(df = mod)
      result
    })
  )

  testthat::expect_equal(result, expected)
})
