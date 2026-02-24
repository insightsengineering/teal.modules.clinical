testthat::test_that("template_summary_by generates correct expressions", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions when `parallel_vars` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL", "CHG"),
    add_total = TRUE,
    parallel_vars = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = FALSE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions when `row_groups` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = FALSE,
    parallel_vars = FALSE,
    row_groups = TRUE,
    by_vars = c("SEX", "COUNTRY"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions for customized numeric statistics", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    numeric_stats = c("n"),
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("template_summary_by generates correct expressions for `drop_zero_levels` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE,
    drop_zero_levels = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::describe("template_summary_by rtables output for different statistics", {
  data <- within(teal.data::teal_data(), {
    ADSL <- teal.modules.clinical::tmc_ex_adsl
    ADLB <- teal.modules.clinical::tmc_ex_adlb
    ADLB$AVALC <- ifelse(abs(rnorm(nrow(ADLB))) >= 0.5, "Y", "N")
    ADLB <- teal.data::col_relabel(ADLB, AVALC = "Analysis Value Category")
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[c("ADSL", "ADLB")]

  mod <- tm_t_summary_by(
    label = "Summary by Row Groups",
    dataname = "ADLB",
    parentname = "ADSL",
    arm_var = choices_selected("ARM", fixed = TRUE),
    by_vars = choices_selected("AVISIT", fixed = TRUE),
    summarize_vars = choices_selected("AVALC", fixed = TRUE),
    categorical_stats = "count"
  )

  it("adds 'fraction' to the statistics", {
    shiny::testServer(
      mod$server,
      args = c(list(id = "test_data", data = reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          "arm_var-dataset_ADSL_singleextract-select" = "ARM",
          "by_vars-dataset_ADLB_singleextract-select" = "AVISIT",
          "summarize_vars-dataset_ADLB_singleextract-select" = "AVALC",
          "paramcd-dataset_ADLB_singleextract-filter1-vals" = "ALT",
          "id_var-dataset_ADLB_singleextract-select" = "USUBJID",
          "paramcd-dataset_ADLB_singleextract-filter1-col" = "PARAMCD",
          drop_zero_levels = TRUE,
          add_total = TRUE,
          row_groups = FALSE,
          parallel_vars = FALSE,
          drop_arm_levels = TRUE,
          useNA = "ifany",
          denominator = "omit",
          categorical_stats = "fraction"
        )
        testthat::expect_all_true(
          c("fraction.N", "fraction.Y") %in% rtables::as_result_df(session$returned()$table)$row_name
        )
      }
    )
  })
  it("adds 'count' to the statistics", {
    shiny::testServer(
      mod$server,
      args = c(list(id = "test_data", data = reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          "arm_var-dataset_ADSL_singleextract-select" = "ARM",
          "by_vars-dataset_ADLB_singleextract-select" = "AVISIT",
          "summarize_vars-dataset_ADLB_singleextract-select" = "AVALC",
          "paramcd-dataset_ADLB_singleextract-filter1-vals" = "ALT",
          "id_var-dataset_ADLB_singleextract-select" = "USUBJID",
          "paramcd-dataset_ADLB_singleextract-filter1-col" = "PARAMCD",
          drop_zero_levels = TRUE,
          add_total = TRUE,
          row_groups = FALSE,
          parallel_vars = FALSE,
          drop_arm_levels = TRUE,
          useNA = "ifany",
          denominator = "omit",
          categorical_stats = "count"
        )
        testthat::expect_all_true(
          c("count.N", "count.Y") %in% rtables::as_result_df(session$returned()$table)$row_name
        )
      }
    )
  })
})
