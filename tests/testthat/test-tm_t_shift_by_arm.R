test_that("template_shift_by_arm generates correct expressions with default arguments", {
  result <- template_shift_by_arm(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    visit_var = "AVISIT",
    anrind_var = "ANRIND",
    bnrind_var = "BNRIND",
    na_level = "<Missing>"
    )

  expected <- list(
    data = quote({
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>%
        filter(ONTRTFL == "Y")
      attr(adeg$BNRIND, "label") <- "Baseline Assessment"
      anl <- adeg %>% mutate(col_label = AVISIT)
    }),

    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("col_label", split_fun = drop_split_levels) %>%
        split_cols_by("ANRIND") %>%
        split_rows_by(
          "ARM",
          split_fun = drop_split_levels,
          label_pos = "topleft",
          split_label = obj_label(adeg$ARM)
        ) %>%
        add_rowcounts() %>%
        summarize_vars("BNRIND", denom = "N_row") %>%
        append_varlabels(adeg, "BNRIND", indent = 1L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl)
      result
    })
  )
  expect_equal(result, expected)
})
