test_that("template_smq generates correct expressions with default arguments", {
  result <- template_smq(
    parentname = "adsl",
    dataname = "adae",
    arm_var = c("ARMCD", "SEX"),
    id_var = "USUBJID",
    llt = "AEDECOD",
    add_total = FALSE,
    drop_arm_levels = FALSE,
    na_level = "<Missing>",
    smq_varlabel = "Standardized MedDRA Query",
    baskets = c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"),
    sort_criteria = c("freq_desc")
    )

  expected <- list(
    data = quote({
      anl <- adae
      adsl <- adsl %>% mutate(ARMCD = droplevels(ARMCD))
      arm_levels <- levels(adsl[["ARMCD"]])
      anl <- anl %>% mutate(ARMCD = factor(ARMCD, levels = arm_levels))
      adsl <- adsl %>% mutate(SEX = droplevels(SEX))
      arm_levels <- levels(adsl[["SEX"]])
      anl <- anl %>% mutate(SEX = factor(SEX, levels = arm_levels))
      anl <- h_stack_by_baskets(
        df = anl, baskets = c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"),
        smq_varlabel = "Standardized MedDRA Query",
        keys = c("STUDYID", "USUBJID", c("ARMCD", "SEX"), "AEDECOD")
        )
      anl <- df_explicit_na(anl, na_level = "<Missing>")
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD") %>%
        split_cols_by(var = "SEX") %>%
        add_colcounts() %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique"),
          .labels = c(
            unique = "Total number of patients with at least one adverse event"
            )
          ) %>%
        split_rows_by(
          "SMQ",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(anl)[["SMQ"]]) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one adverse event",
            nonunique = "Total number of events")
          ) %>%
        count_occurrences(vars = "AEDECOD") %>%
        append_varlabels(anl, "AEDECOD", indent = 1L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>%
        sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
        sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_smq generates correct expressions with custom arguments", {
  result <- template_smq(
    parentname = "myadsl",
    dataname = "myadae",
    arm_var = "myARMCD",
    id_var = "myUSUBJID",
    llt = "myAEDECOD",
    add_total = FALSE,
    drop_arm_levels = FALSE,
    na_level = "<Missing>",
    smq_varlabel = "mylabel",
    baskets = c("mybaskets"),
    sort_criteria = c("freq_desc")
  )

  expected <- list(
    data = quote({
      anl <- myadae
      myadsl <- myadsl %>% mutate(myARMCD = droplevels(myARMCD))
      arm_levels <- levels(myadsl[["myARMCD"]])
      anl <- anl %>% mutate(myARMCD = factor(myARMCD, levels = arm_levels))
      anl <- h_stack_by_baskets(
        df = anl, baskets = "mybaskets",
        smq_varlabel = "mylabel",
        keys = c("STUDYID", "myUSUBJID", "myARMCD", "myAEDECOD")
      )
      anl <- df_explicit_na(anl, na_level = "<Missing>")
      myadsl <- df_explicit_na(myadsl, na_level = "<Missing>")
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "myARMCD") %>%
        add_colcounts() %>%
        summarize_num_patients(
          var = "myUSUBJID",
          .stats = c("unique"),
          .labels = c(
            unique = "Total number of patients with at least one adverse event"
          )
        ) %>%
        split_rows_by(
          "SMQ",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = var_labels(anl)[["SMQ"]]) %>%
        summarize_num_patients(
          var = "myUSUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one adverse event",
            nonunique = "Total number of events")
        ) %>%
        count_occurrences(vars = "myAEDECOD") %>%
        append_varlabels(anl, "myAEDECOD", indent = 1L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = myadsl) %>%
        sort_at_path(path = c("SMQ"), scorefun = cont_n_allcols) %>%
        sort_at_path(path = c("SMQ", "*", "myAEDECOD"), scorefun = score_occurrences)
      result
    })
  )
  expect_equal(result, expected)
})
