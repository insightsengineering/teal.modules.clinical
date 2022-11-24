testthat::test_that("template_a_gee works as expected with default input", {
  result <- template_a_gee(
    output_table = "t_gee_cov",
    aval_var = "AVAL",
    id_var = "USUBJID",
    arm_var = "ARMCD",
    visit_var = "AVISIT",
    split_covariates = c(),
    cor_struct = "unstructured"
  )
  expected <- list(
    model = quote({
      model_fit <- tern.gee::fit_gee(
        vars = tern.gee::vars_gee(
          response = as.vector("AVAL"),
          covariates = as.vector(NULL),
          id = as.vector("USUBJID"),
          arm = as.vector("ARMCD"),
          visit = as.vector("AVISIT")
        ),
        data = ANL,
        regression = "logistic",
        cor_struct = "unstructured"
      )
    }),
    table = quote({
      result_table <- tern.gee::as.rtable(model_fit, type = "cov")
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_a_gee works as expected with non-default reference arm", {
  result <- template_a_gee(
    output_table = "t_gee_lsmeans",
    aval_var = "AVAL",
    id_var = "USUBJID",
    arm_var = "ARMCD",
    ref_group = "B: Placebo",
    visit_var = "AVISIT",
    split_covariates = c(),
    cor_struct = "unstructured"
  )
  expected <- list(
    model = quote({
      model_fit <- tern.gee::fit_gee(
        vars = tern.gee::vars_gee(
          response = as.vector("AVAL"),
          covariates = as.vector(NULL),
          id = as.vector("USUBJID"),
          arm = as.vector("ARMCD"),
          visit = as.vector("AVISIT")
        ),
        data = ANL,
        regression = "logistic",
        cor_struct = "unstructured"
      )
    }),
    table = quote({
      lsmeans_fit_model <- tern.gee::lsmeans(model_fit, 0.95)
      result_table <- rtables::basic_table() %>%
        rtables::split_cols_by(
          var = "ARM",
          ref_group = model_fit$ref_level
        ) %>%
        rtables::add_colcounts() %>%
        tern.gee::summarize_gee_logistic() %>%
        rtables::build_table(df = lsmeans_fit_model, alt_counts_df = ANL_ADSL)
      result_table
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_a_gee works as expected when arm is not considered in the model", {
  result <- template_a_gee(
    output_table = "t_gee_lsmeans",
    aval_var = "AVAL",
    id_var = "USUBJID",
    arm_var = NULL,
    ref_group = NULL,
    visit_var = "AVISIT",
    split_covariates = c(),
    cor_struct = "unstructured"
  )
  expected <- list(
    model = quote({
      model_fit <- tern.gee::fit_gee(
        vars = tern.gee::vars_gee(
          response = as.vector("AVAL"),
          covariates = as.vector(NULL),
          id = as.vector("USUBJID"),
          arm = as.vector(NULL),
          visit = as.vector("AVISIT")
        ),
        data = ANL,
        regression = "logistic",
        cor_struct = "unstructured"
      )
    }),
    table = quote({
      lsmeans_fit_model <- tern.gee::lsmeans(model_fit, 0.95)
      result_table <- rtables::basic_table() %>%
        rtables::split_cols_by(
          var = "ARM",
          ref_group = model_fit$ref_level
        ) %>%
        rtables::add_colcounts() %>%
        tern.gee::summarize_gee_logistic() %>%
        rtables::build_table(df = lsmeans_fit_model, alt_counts_df = ANL_ADSL)
      result_table
    })
  )
  testthat::expect_equal(result, expected)
})
