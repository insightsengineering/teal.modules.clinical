test_that("template_shift_by_arm generates correct expressions with default arguments", {
  result <- template_shift_by_arm(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    visit = "AVISIT",
    anrind_var = "ANRIND",
    bnrind_var = "BNRIND",
    anrind_levels = c("LOW", "NORMAL", "HIGH", "<Missing>"),
    bnrind_levels = c("LOW", "NORMAL", "HIGH", "<Missing>"),
    anrind_labels = c("LOW", "NORMAL", "HIGH", "Missing"),
    bnrind_labels = c("LOW", "NORMAL", "HIGH", "Missing"),
    drop_arm_levels = TRUE,
    na_level = "<Missing>"
    )

  expected <- list(
    data = quote({
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      adeg <- df_explicit_na(adeg, na_level = "<Missing>")
      adeg$BNRIND <- factor(
        adeg$BNRIND,
        levels = c("LOW", "NORMAL", "HIGH", "<Missing>"),
        labels = c("LOW", "NORMAL", "HIGH", "Missing")
      )
      adeg$ANRIND <- factor(
        adeg$ANRIND,
        levels = c("LOW", "NORMAL", "HIGH", "<Missing>"),
        labels = c("LOW", "NORMAL", "HIGH", "Missing")
      )
      anl <- adeg %>% mutate(col_label = "AVISIT")
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
    }),

    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("col_label") %>%
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

test_that("template_shift_by_arm generates correct expressions with customized order of levels", {
  result <- template_shift_by_arm(
    parentname = "adsl",
    dataname = "adeg",
    arm_var = "ARM",
    paramcd = "PARAMCD",
    visit = "AVISIT",
    anrind_var = "ANRIND",
    bnrind_var = "BNRIND",
    anrind_levels = c("HIGH", "LOW", "NORMAL", "<Missing>"),
    bnrind_levels = c("HIGH", "LOW", "NORMAL", "<Missing>"),
    anrind_labels = c("HIGH", "LOW", "NORMAL", "Missing"),
    bnrind_labels = c("HIGH", "LOW", "NORMAL", "Missing"),
    drop_arm_levels = TRUE,
    na_level = "<Missing>"
  )

  expected <- list(
    data = quote({
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      adeg <- df_explicit_na(adeg, na_level = "<Missing>")
      adeg$BNRIND <- factor(
        adeg$BNRIND,
        levels = c("HIGH", "LOW", "NORMAL", "<Missing>"),
        labels = c("HIGH", "LOW", "NORMAL", "Missing")
      )
      adeg$ANRIND <- factor(
        adeg$ANRIND,
        levels = c("HIGH", "LOW", "NORMAL", "<Missing>"),
        labels = c("HIGH", "LOW", "NORMAL", "Missing")
      )
      anl <- adeg %>% mutate(col_label = "AVISIT")
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
    }),

    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("col_label") %>%
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
