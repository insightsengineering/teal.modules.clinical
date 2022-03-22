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
      lyt <- rtables::basic_table(main_footer = "* Person time is the sum of TDURD") %>%
        rtables::split_cols_by("SEX") %>%
        rtables::add_colcounts() %>%
        summarize_patients_exposure_in_cols(
          var = "AVAL",
          col_split = TRUE,
          .labels = c(
            n_patients = "Patient time*",
            sum_exposure = paste("Sum of", "TDURD", sprintf("(%s)", "Days"))
          )
        ) %>%
        rtables::split_rows_by(
          "RACE",
          label_pos = "topleft",
          split_fun = split_fun,
          split_label = formatable::var_labels(adex["RACE"]),
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
      lyt <- rtables::basic_table(main_footer = "* Person time is the sum of myTDURD") %>%
        rtables::split_cols_by("SEX") %>%
        rtables::add_colcounts() %>%
        summarize_patients_exposure_in_cols(
          var = "myAVAL",
          col_split = TRUE,
          .labels = c(
            n_patients = "Patient time*",
            sum_exposure = paste("Sum of", "myTDURD", sprintf("(%s)", "Days"))
          )
        ) %>%
        rtables::split_rows_by(
          "myRACE",
          label_pos = "topleft",
          split_fun = split_fun,
          split_label = formatable::var_labels(myadex["myRACE"]),
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

testthat::test_that("template_exposure generates correct expressions with paramcd_label", {
  result <- template_exposure(
    parentname = "adsl",
    dataname = "adex",
    paramcd = "TDURD",
    paramcd_label = "Total Duration (Days)",
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
      lyt <- rtables::basic_table(main_footer = "* Person time is the sum of Total Duration (Days)") %>%
        rtables::split_cols_by("SEX") %>%
        rtables::add_colcounts() %>%
        summarize_patients_exposure_in_cols(
          var = "AVAL",
          col_split = TRUE,
          .labels = c(
            n_patients = "Patient time*",
            sum_exposure = paste("Sum of", "TDURD", sprintf("(%s)", "Days"))
          )
        ) %>%
        rtables::split_rows_by(
          "RACE",
          label_pos = "topleft",
          split_fun = split_fun,
          split_label = formatable::var_labels(adex["RACE"]),
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
