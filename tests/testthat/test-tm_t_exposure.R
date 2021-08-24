test_that("template_exposure generates correct expressions with default arguments", {
  test.nest::skip_if_too_deep(0)

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
      lyt <- basic_table() %>%
        split_cols_by("SEX") %>%
        add_colcounts() %>%
        summarize_patients_exposure_in_cols(
          var = "AVAL",
          col_split = TRUE,
          .labels = c(
            n_patients = "Patients",
            sum_exposure = paste("Sum of", "TDURD", sprintf("(%s)", "Days"))
            )
          ) %>%
        split_rows_by(
          "RACE",
          label_pos = "topleft",
          split_fun = split_fun,
          split_label = var_labels(adex["RACE"]),
          nested = FALSE) %>%
        summarize_patients_exposure_in_cols(var = "AVAL", col_split = FALSE)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_exposure generates correct expressions with custom arguments", {
  test.nest::skip_if_too_deep(0)

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
      lyt <- basic_table() %>%
        split_cols_by("SEX") %>%
        add_colcounts() %>%
        summarize_patients_exposure_in_cols(
          var = "myAVAL",
          col_split = TRUE,
          .labels = c(
            n_patients = "Patients",
            sum_exposure = paste("Sum of", "myTDURD", sprintf("(%s)", "Days"))
          )
        ) %>%
        split_rows_by(
          "myRACE",
          label_pos = "topleft",
          split_fun = split_fun,
          split_label = var_labels(myadex["myRACE"]),
          nested = FALSE) %>%
        summarize_patients_exposure_in_cols(var = "myAVAL", col_split = FALSE)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = myadsl)
      result
    })
  )
  expect_equal(result, expected)
})
