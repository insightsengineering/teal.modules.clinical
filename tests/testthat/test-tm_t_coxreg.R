test_that("template_coxreg generates correct univariate cox regression expressions", {
  result <- template_coxreg(
    dataname = "adrs",
    cov_var = NULL,
    arm_var = "ARMCD",
    cnsr_var = "CNSR",
    aval_var = "AVAL",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    paramcd = "OS",
    at = list(AGE = c(35, 45)),
    strata = "STRATA1",
    combine_comp_arms = FALSE,
    multivariate = FALSE,
    control = control_coxreg(
      pval_method = "wald",
      ties = "efron"
    )
  )

  lapply(result, styled_expr)
  expected <- list(
    data = quote({
      anl <- adrs %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(event = 1 - CNSR) %>%
        df_explicit_na(na_level = "")
      variables <- list(time = "AVAL", event = "event", arm = "ARMCD", covariates = NULL)
      variables$strata <- "STRATA1"
      model <- fit_coxreg_univar(
        variables = variables,
        data = anl,
        control = list(
          pval_method = "wald",
          ties = "efron",
          conf_level = 0.95,
          interaction = FALSE
          ),
        at = list(AGE = c(35, 45))
        )
      df <- broom::tidy(model)
    }),
    layout = quote(
      lyt <- basic_table(title = paste("Cox Regression for", "OS")) %>%
        split_rows_by("effect") %>%
        append_topleft("OS") %>%
        split_rows_by("term", child_labels = "hidden") %>%
        summarize_coxreg(multivar = FALSE, conf_level = 0.95, vars = c("n", "hr", "ci", "pval"))
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = df)
      result
    })
  )
  expect_equal(result, expected)
})


test_that("template_coxreg generates correct univariate cox regression expressions with interactions", {
  result <- template_coxreg(
    dataname = "adrs",
    cov_var = NULL,
    arm_var = "ARMCD",
    cnsr_var = "CNSR",
    aval_var = "AVAL",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    paramcd = "OS",
    at = list(AGE = c(35, 45)),
    strata = "STRATA1",
    combine_comp_arms = FALSE,
    multivariate = FALSE,
    control = control_coxreg(
      pval_method = "wald",
      ties = "efron",
      interaction = TRUE
    )
  )

  lapply(result, styled_expr)
  expected <- list(
    data = quote({
      anl <- adrs %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(event = 1 - CNSR) %>%
        df_explicit_na(na_level = "")
      variables <- list(time = "AVAL", event = "event", arm = "ARMCD", covariates = NULL)
      variables$strata <- "STRATA1"
      model <- fit_coxreg_univar(
        variables = variables,
        data = anl,
        control = list(
          pval_method = "wald",
          ties = "efron",
          conf_level = 0.95,
          interaction = TRUE
        ),
        at = list(AGE = c(35, 45))
      )
      df <- broom::tidy(model)
    }),
    layout = quote(
      lyt <- basic_table(title = paste("Cox Regression for", "OS")) %>%
        split_rows_by("effect") %>%
        append_topleft("OS") %>%
        split_rows_by("term", child_labels = "hidden") %>%
        summarize_coxreg(multivar = FALSE, conf_level = 0.95, vars = c("n", "hr", "ci", "pval", "pval_inter"))
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = df)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_coxreg generates correct multivariate cox regression expressions", {
  result <- template_coxreg(
    dataname = "adrs",
    cov_var = c("AGE", "SEX"),
    arm_var = "ARM",
    cnsr_var = "CNSR",
    aval_var = "AVAL",
    ref_arm = "A: Drug X",
    comp_arm = c("B: Placebo", "C: Combination"),
    paramcd = "OS",
    combine_comp_arms = TRUE,
    multivariate = TRUE,
    control = control_coxreg()
  )

  expected <- list(
    data = quote({
      anl <- adrs %>%
        dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo", "C: Combination")) %>%
        dplyr::mutate(ARM = stats::relevel(ARM, ref = "A: Drug X")) %>%
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        dplyr::mutate(ARM = combine_levels(x = ARM, levels = c("B: Placebo", "C: Combination"))) %>%
        dplyr::mutate(event = 1 - CNSR) %>%
        df_explicit_na(na_level = "")
      variables <- list(time = "AVAL", event = "event", arm = "ARM", covariates = c("AGE", "SEX"))
      model <- fit_coxreg_multivar(
        variables = variables,
        data = anl,
        control = list(
          pval_method = "wald",
          ties = "exact",
          conf_level = 0.95,
          interaction = FALSE
          )
        )
      df <- broom::tidy(model)
    }),
    layout = quote(
      lyt <- basic_table(title = paste("Multi-Variable Cox Regression for", "OS")) %>%
        append_topleft("OS") %>%
        split_rows_by("term", child_labels = "hidden") %>%
        summarize_coxreg(multivar = TRUE, conf_level = 0.95, vars = c("n", "hr", "ci", "pval"))
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = df)
      result
    })
  )
  expect_equal(result, expected)
})
