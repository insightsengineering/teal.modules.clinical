testthat::test_that("template_shift_by_arm generates correct expressions with default arguments", {
  result <- template_shift_by_arm_by_worst(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    aval_var = "AVALC",
    base_var = "BASEC",
    worst_flag_var = "WORS02FL",
    worst_flag = "Y",
    na_level = "<Missing>"
  )

  expected <- list(
    data = quote({
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>%
        dplyr::filter(ONTRTFL == "Y", WORS02FL == "Y") %>%
        dplyr::mutate(postbaseline_label = "Post-Baseline")
      attr(adeg$BASEC, "label") <- "Baseline Assessment"
    }),
    layout = quote(
      lyt <- rtables::basic_table(title = "Shift by Arm by Worst Table") %>%
        rtables::split_cols_by("postbaseline_label", split_fun = drop_split_levels) %>%
        rtables::split_cols_by("AVALC") %>%
        rtables::split_rows_by(
          "ARM",
          split_fun = drop_split_levels,
          label_pos = "topleft",
          split_label = obj_label(adeg$ARM)
        ) %>%
        add_rowcounts() %>%
        summarize_vars("BASEC", denom = "N_row", na_level = "<Missing>", na.rm = FALSE, .stats = "count_fraction") %>%
        append_varlabels(adeg, "BASEC", indent = 1L)
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = adeg)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_shift_by_arm generates correct expressions with add_total being TRUE", {
  result <- template_shift_by_arm_by_worst(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    aval_var = "AVALC",
    base_var = "BASEC",
    worst_flag_var = "WORS02FL",
    worst_flag = "Y",
    na_level = "<Missing>",
    add_total = TRUE
  )

  expected <- list(
    data = quote({
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>%
        dplyr::filter(ONTRTFL == "Y", WORS02FL == "Y") %>%
        dplyr::mutate(postbaseline_label = "Post-Baseline")
      attr(adeg$BASEC, "label") <- "Baseline Assessment"
    }),
    layout = quote(
      lyt <- rtables::basic_table(title = "Shift by Arm by Worst Table") %>%
        rtables::split_cols_by("postbaseline_label", split_fun = drop_split_levels) %>%
        rtables::split_cols_by("AVALC") %>%
        rtables::split_rows_by(
          "ARM",
          split_fun = add_overall_level("All Patients", first = FALSE),
          label_pos = "topleft",
          split_label = obj_label(adeg$ARM)
        ) %>%
        add_rowcounts() %>%
        summarize_vars("BASEC", denom = "N_row", na_level = "<Missing>", na.rm = FALSE, .stats = "count_fraction") %>%
        append_varlabels(adeg, "BASEC", indent = 1L)
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = adeg)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_shift_by_arm generates correct expressions with na.rm being TRUE", {
  result <- template_shift_by_arm_by_worst(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    aval_var = "AVALC",
    base_var = "BASEC",
    worst_flag_var = "WORS02FL",
    worst_flag = "Y",
    na.rm = TRUE,
    na_level = "<Missing>"
  )

  expected <- list(
    data = quote({
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>%
        dplyr::filter(ONTRTFL == "Y", WORS02FL == "Y") %>%
        dplyr::mutate(postbaseline_label = "Post-Baseline")
      attr(adeg$BASEC, "label") <- "Baseline Assessment"
    }),
    layout = quote(
      lyt <- rtables::basic_table(title = "Shift by Arm by Worst Table") %>%
        rtables::split_cols_by("postbaseline_label", split_fun = drop_split_levels) %>%
        rtables::split_cols_by("AVALC") %>%
        rtables::split_rows_by(
          "ARM",
          split_fun = drop_split_levels,
          label_pos = "topleft",
          split_label = obj_label(adeg$ARM)
        ) %>%
        add_rowcounts() %>%
        summarize_vars("BASEC", denom = "N_row", na_level = "<Missing>", na.rm = TRUE, .stats = "count_fraction") %>%
        append_varlabels(adeg, "BASEC", indent = 1L)
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = adeg)
      result
    })
  )
  testthat::expect_equal(result, expected)
})
