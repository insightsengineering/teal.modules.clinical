test_that("template_fit_mmrm works as expected when not combining comparison arms", {
  result1 <- template_fit_mmrm(
    parentname = "adsl",
    dataname = "adqs",
    paramcd = "FKSI-FWB",
    aval_var = "AVAL",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arm = FALSE,
    id_var = "USUBJID",
    visit_var = "AVISIT",
    cov_var = c()
  )
  expected1 <- list(
    data = quote({
      anl <- adqs %>%
        filter(PARAMCD %in% "FKSI-FWB") %>%
        droplevels()
      adsl <- filter(adsl, ARMCD %in% c("ARM A", c("ARM B", "ARM C")))
      anl <- filter(anl, ARMCD %in% c("ARM A", c("ARM B", "ARM C")))
      adsl$ARMCD <- droplevels(relevel(adsl$ARMCD, "ARM A")) # nolint
      anl$ARMCD <- droplevels(relevel(anl$ARMCD, "ARM A")) # nolint
    }),
    col_counts = quote(
      col_counts <- table(adsl$ARMCD) # nolint
    ),
    fit = quote(
      fit <- fit_mmrm(
        vars = list(
          response = "AVAL", covariates = NULL,
          id = "USUBJID", arm = "ARMCD", visit = "AVISIT"
        ),
        data = anl,
        conf_level = 0.95,
        cor_struct = "unstructured",
        weights_emmeans = "proportional",
        optimizer = "automatic",
        parallel = FALSE)
    )
  )
  expect_equal_expr_list(result1, expected1)
})

test_that("template_fit_mmrm works as expected when combining combination arms", {
  result2 <- template_fit_mmrm(
    parentname = "adsl",
    dataname = "adqs",
    paramcd = "FKSI-FWB",
    aval_var = "AVAL",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arm = TRUE,
    id_var = "USUBJID",
    visit_var = "AVISIT",
    cov_var = c("SEX", "BASE", "AVISIT"),
    parallel = TRUE
  )
  expected2 <- list(
    data = quote({
      anl <- adqs %>%
        filter(PARAMCD %in% "FKSI-FWB") %>%
        droplevels()
      adsl <- filter(adsl, ARMCD %in% c("ARM A", c("ARM B", "ARM C")))
      anl <- filter(anl, ARMCD %in% c("ARM A", c("ARM B", "ARM C")))
      adsl$ARMCD <- droplevels(relevel(adsl$ARMCD, "ARM A")) # nolint
      anl$ARMCD <- droplevels(relevel(anl$ARMCD, "ARM A")) # nolint
      adsl$ARMCD <- combine_levels(x = adsl$ARMCD, levels = c("ARM B", "ARM C")) # nolint
      anl$ARMCD <- combine_levels(x = anl$ARMCD, levels = c("ARM B", "ARM C")) # nolint
    }),
    col_counts = quote(
      col_counts <- table(adsl$ARMCD) # nolint
    ),
    fit = quote(
      fit <- fit_mmrm(
        vars = list(
          response = "AVAL",
          covariates = c("SEX", "BASE", "AVISIT"),
          id = "USUBJID",
          arm = "ARMCD",
          visit = "AVISIT"
        ),
        data = anl,
        conf_level = 0.95,
        cor_struct = "unstructured",
        weights_emmeans = "proportional",
        optimizer = "automatic",
        parallel = TRUE)
    )
  )
  expect_equal_expr_list(result2, expected2)
})

test_that("template_mmrm_tables works as expected", {
  result <- template_mmrm_tables(
    "fit_mmrm",
    "col_counts",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    visit_var = "AVISIT",
    show_relative = "increase"
  )
  expected <- list(
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        add_colcounts() %>%
        split_rows_by("AVISIT") %>%
        summarize_lsmeans(show_relative = "increase")
    ),
    lsmeans_table = quote(
      lsmeans_table <- build_table(lyt = lyt, df = broom::tidy(fit_mmrm), col_counts = col_counts)
    ),
    cov_matrix = quote(cov_matrix <- as.rtable(fit_mmrm, type = "cov")),
    fixed_effects = quote(fixed_effects <- as.rtable(fit_mmrm, type = "fixed")),
    diagnostic_table = quote(diagnostic_table <- as.rtable(fit_mmrm, type = "diagnostic"))
  )
  expect_equal_expr_list(result, expected)
})


test_that("template_mmrm_plots works as expected", {
  result <- template_mmrm_plots(
    "fit_mmrm",
    lsmeans_plot = list(
      select = c("estimates", "contrasts"),
      width = 0.6,
      show_pval = FALSE
    ),
    diagnostic_plot = list(
      type = "fit-residual",
      z_threshold = NULL
    )
  )
  expected <- list(
    lsmeans_plot = quote(
      lsmeans_plot <- g_mmrm_lsmeans(
        fit_mmrm, select = c("estimates", "contrasts"),
        width = 0.6, show_pval = FALSE
      )
    ),
    diagnostic_plot = quote(
      diagnostic_plot <- g_mmrm_diagnostic(
        fit_mmrm, type = "fit-residual",
        z_threshold = NULL
      )
    )
  )
  expect_equal_expr_list(result, expected)
})
