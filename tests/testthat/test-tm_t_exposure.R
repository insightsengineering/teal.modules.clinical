testthat::test_that("template_exposure generates correct expressions with default arguments", {
  result <- template_exposure(
    parentname = "adsl",
    dataname = "adex",
    paramcd = "TDURD",
    id_var = "USUBJID",
    row_by_var = "RACE",
    col_by_var = "SEX",
    add_total = FALSE,
    drop_levels = TRUE,
    na_level = "<Missing>",
    aval_var = "AVAL",
    avalu_var = "Days"
  )
  expected <- list(
    data = quote({
      anl <- adex
      anl <- df_explicit_na(anl, na_level = "<Missing>")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table() %>%
        rtables::split_cols_by("SEX") %>%
        rtables::add_colcounts() %>%
        summarize_patients_exposure_in_cols(
          var = "AVAL",
          col_split = TRUE,
          .labels = c(
            n_patients = "Patients",
            sum_exposure = paste("Sum of", "TDURD", sprintf("(%s)", "Days"))
          )
        ) %>%
        rtables::split_rows_by(
          "RACE",
          label_pos = "topleft",
          split_fun = split_fun,
          split_label = rtables::var_labels(adex["RACE"], fill = TRUE),
          nested = FALSE
        ) %>%
        summarize_patients_exposure_in_cols(var = "AVAL", col_split = FALSE)
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_exposure generates correct expressions with custom arguments", {
  result <- template_exposure(
    parentname = "myadsl",
    dataname = "myadex",
    paramcd = "myTDURD",
    id_var = "USUBJID",
    row_by_var = "myRACE",
    col_by_var = "SEX",
    add_total = FALSE,
    drop_levels = TRUE,
    na_level = "<myMissing>",
    aval_var = "myAVAL",
    avalu_var = "Days"
  )
  expected <- list(
    data = quote({
      anl <- myadex
      anl <- df_explicit_na(anl, na_level = "<myMissing>")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table() %>%
        rtables::split_cols_by("SEX") %>%
        rtables::add_colcounts() %>%
        summarize_patients_exposure_in_cols(
          var = "myAVAL",
          col_split = TRUE,
          .labels = c(
            n_patients = "Patients",
            sum_exposure = paste("Sum of", "myTDURD", sprintf("(%s)", "Days"))
          )
        ) %>%
        rtables::split_rows_by(
          "myRACE",
          label_pos = "topleft",
          split_fun = split_fun,
          split_label = rtables::var_labels(myadex["myRACE"], fill = TRUE),
          nested = FALSE
        ) %>%
        summarize_patients_exposure_in_cols(var = "myAVAL", col_split = FALSE)
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = myadsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})
