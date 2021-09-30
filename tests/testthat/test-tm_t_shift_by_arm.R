test_that("template_shift_by_arm generates correct expressions with default arguments", {
  result <- template_shift_by_arm(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    visit_var = "AVISIT",
    aval_var = "ANRIND",
    base_var = "BNRIND",
    na_level = "<Missing>"
    )

  expected <- list(
    data = quote({
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>%
        filter(ONTRTFL == "Y")
      attr(adeg$BNRIND, "label") <- "Baseline Assessment"
    }),

    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("AVISIT", split_fun = drop_split_levels) %>%
        split_cols_by("ANRIND") %>%
        split_rows_by(
          "ARM",
          split_fun = drop_split_levels,
          label_pos = "topleft",
          split_label = obj_label(adeg$ARM)
        ) %>%
        add_rowcounts() %>%
        summarize_vars("BNRIND", denom = "N_row", na_level = "<Missing>", na.rm = FALSE, .stats = "count_fraction") %>%
        append_varlabels(adeg, "BNRIND", indent = 1L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = adeg)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_shift_by_arm generates correct expressions with add_total being TRUE", {
  result <- template_shift_by_arm(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    visit_var = "AVISIT",
    aval_var = "ANRIND",
    base_var = "BNRIND",
    na_level = "<Missing>",
    add_total = TRUE
  )

  expected <- list(
    data = quote({
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>%
        filter(ONTRTFL == "Y")
      attr(adeg$BNRIND, "label") <- "Baseline Assessment"
    }),

    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("AVISIT", split_fun = drop_split_levels) %>%
        split_cols_by("ANRIND") %>%
        split_rows_by(
          "ARM",
          split_fun = add_overall_level("All Patients", first = FALSE),
          label_pos = "topleft",
          split_label = obj_label(adeg$ARM)
        ) %>%
        add_rowcounts() %>%
        summarize_vars("BNRIND", denom = "N_row", na_level = "<Missing>", na.rm = FALSE, .stats = "count_fraction") %>%
        append_varlabels(adeg, "BNRIND", indent = 1L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = adeg)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_shift_by_arm generates correct expressions with na.rm being TRUE", {
  result <- template_shift_by_arm(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    visit_var = "AVISIT",
    aval_var = "ANRIND",
    base_var = "BNRIND",
    na.rm = TRUE,
    na_level = "<Missing>"
  )

  expected <- list(
    data = quote({
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>%
        filter(ONTRTFL == "Y")
      attr(adeg$BNRIND, "label") <- "Baseline Assessment"
    }),

    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("AVISIT", split_fun = drop_split_levels) %>%
        split_cols_by("ANRIND") %>%
        split_rows_by(
          "ARM",
          split_fun = drop_split_levels,
          label_pos = "topleft",
          split_label = obj_label(adeg$ARM)
        ) %>%
        add_rowcounts() %>%
        summarize_vars("BNRIND", denom = "N_row", na_level = "<Missing>", na.rm = TRUE, .stats = "count_fraction") %>%
        append_varlabels(adeg, "BNRIND", indent = 1L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = adeg)
      result
    })
  )
  expect_equal(result, expected)
})
