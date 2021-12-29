test_that("template_fit_mmrm works as expected when not combining comparison arms", {
  result <- template_fit_mmrm(
    parentname = "adsl",
    dataname = "adqs",
    aval_var = "AVAL",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    combine_comp_arms = FALSE,
    id_var = "USUBJID",
    visit_var = "AVISIT",
    cov_var = c()
  )
  expected <- list(
    data = quote({
      anl <- adqs %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        df_explicit_na(na_level = "")
      adsl <- adsl %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        df_explicit_na(na_level = "")
    }),
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
  expect_equal(result, expected)
})

test_that("template_fit_mmrm works as expected when combining combination arms", {
  result <- template_fit_mmrm(
    parentname = "adsl",
    dataname = "adqs",
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
  expected <- list(
    data = quote({
      anl <- adqs %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"))) %>%
        df_explicit_na(na_level = "")
      adsl <- adsl %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"))) %>%
        df_explicit_na(na_level = "")
    }),
    fit = substitute(
      expr = fit <- fit_mmrm(
        vars = vars,
        data = anl,
        conf_level = 0.95,
        cor_struct = "unstructured",
        weights_emmeans = "proportional",
        optimizer = "automatic",
        parallel = TRUE
      ),
      env = list(
        vars = list(
          response = "AVAL",
          covariates = c("SEX", "BASE", "AVISIT"),
          id = "USUBJID",
          arm = "ARMCD",
          visit = "AVISIT"
        )
      )
    )
  )
  expect_equal(result, expected)
})

test_that("template_mmrm_tables works as expected", {
  result <- template_mmrm_tables(
    parentname = "ADSL",
    dataname = "ANL",
    fit_name = "fit_mmrm",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    visit_var = "AVISIT",
    paramcd = "ALBUMIN",
    show_relative = "increase"
  )
  expected <- list(
    layout = quote(
      lyt <- rtables::basic_table() %>%
        split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
        add_colcounts() %>%
        split_rows_by("AVISIT") %>%
        append_varlabels(ANL, "AVISIT") %>%
        summarize_lsmeans(show_relative = "increase") %>%
        append_topleft(paste0("  ", "ALBUMIN"))
    ),
    cov_matrix = quote({
      cov_matrix <- as.rtable(fit_mmrm, type = "cov")
      cov_matrix
    })
  )
  expect_equal(result, expected)
})

test_that("template_mmrm_tables works as expected when arm is not considered in the model", {
  result <- template_mmrm_tables(
    parentname = "ADSL",
    dataname = "ANL",
    fit_name = "fit_mmrm",
    arm_var = NULL,
    ref_arm = NULL,
    visit_var = "AVISIT",
    paramcd = "ALBUMIN",
    show_relative = NULL
  )
  expected <- list(
    layout = quote(
      lyt <- rtables::basic_table() %>%
        split_rows_by("AVISIT") %>%
        summarize_lsmeans(
          arms = FALSE
        ) %>%
        append_topleft(paste0("  ", "ALBUMIN"))
    ),
    cov_matrix = quote({
      cov_matrix <- as.rtable(fit_mmrm, type = "cov")
      cov_matrix
    })
  )
  expect_equal(result, expected)
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
    lsmeans_plot = substitute(
      expr = {
        lsmeans_plot <- g_mmrm_lsmeans(
          fit_mmrm,
          select = select,
          width = 0.6,
          show_pval = FALSE,
          titles = if (is.null(fit_mmrm$vars$arm)) {
            c(estimates = paste("Adjusted mean of", fit_mmrm$labels$response, " at visits"),
              contrasts = " ")
          } else {
            c(
              estimates = paste(
                "Adjusted mean of",
                fit_mmrm$labels$response,
                "by treatment at visits"
              ),
              contrasts = paste0(
                "Differences of ",
                fit_mmrm$labels$response,
                " adjusted means vs. control ('",
                fit_mmrm$ref_level,
                "')"
              )
            )
          }
        )
        lsmeans_plot
      },
      env = list(
        select = c("estimates", "contrasts")
      )
    ),
    diagnostic_plot = quote({
      diagnostic_plot <- g_mmrm_diagnostic(fit_mmrm, type = "fit-residual", z_threshold = NULL)
      diagnostic_plot
    })
  )
  expect_equal(result, expected)
})
