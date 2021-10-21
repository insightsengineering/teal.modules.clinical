test_that("template_abnormality generates correct expressions with default arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    by_vars = c("AVISIT", "PARAM"),
    abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
    grade = "ANRIND",
    add_total = FALSE,
    exclude_base_abn = FALSE,
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% dplyr::filter(ONTRTFL == "Y" & !is.na(ANRIND))
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      anl <- df_explicit_na(anl, na_level = "")
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_prep = quote({
      map <- unique(anl[c(c("AVISIT", "PARAM"), "ANRIND")])
      map <- data.frame(lapply(map, as.character), stringsAsFactors = FALSE)
      map <- map %>%
        group_by(across(all_of(c("AVISIT", "PARAM")))) %>%
        filter(n() > 1 || anl["ANRIND"] %in% list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH"))) %>%
        ungroup()
    }),
    layout = quote(
      lyt <- basic_table() %>% split_cols_by(var = "ARM") %>% add_colcounts() %>% # nolint
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map)
        ) %>%
        split_rows_by("PARAM",
          split_label = var_labels(adlb)[["PARAM"]],
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map)
        ) %>%
        count_abnormal(
          var = "ANRIND",
          abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
          variables = list(id = "USUBJID", baseline = "BNRIND"),
          exclude_base_abn = FALSE
        ) %>%
        append_varlabels(adlb, "ANRIND", indent = 2L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>%
        prune_table()
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_abnormality generates correct expressions with custom arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    by_vars = c("AVISIT", "PARAMCD"),
    abnormal = list(Low = "LOW", Medium = "MEDIUM"),
    grade = "MYANRIND",
    baseline_var = "MYBASELINE",
    treatment_flag_var = "MYTRTFL",
    treatment_flag = "YES",
    add_total = TRUE,
    exclude_base_abn = TRUE,
    drop_arm_levels = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% dplyr::filter(MYTRTFL == "YES" & !is.na(MYANRIND))
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(adsl[["ARM"]])
      anl <- anl %>% dplyr::mutate(ARM = factor(ARM, levels = arm_levels))
      anl <- df_explicit_na(anl, na_level = "")
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_prep = quote({
      map <- unique(anl[c(c("AVISIT", "PARAMCD"), "MYANRIND")])
      map <- data.frame(lapply(map, as.character), stringsAsFactors = FALSE)
      map <- map %>%
        group_by(across(all_of(c("AVISIT", "PARAMCD")))) %>%
        filter(n() > 1 || anl["MYANRIND"] %in% list(Low = "LOW", Medium = "MEDIUM")) %>%
        ungroup()
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map)
        ) %>%
        split_rows_by(
          "PARAMCD",
          split_label = var_labels(adlb)[["PARAMCD"]],
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map)
        ) %>%
        count_abnormal(
          var = "MYANRIND",
          abnormal = list(Low = "LOW", Medium = "MEDIUM"),
          variables = list(id = "USUBJID", baseline = "MYBASELINE"),
          exclude_base_abn = TRUE
      ) %>%
        append_varlabels(adlb, "MYANRIND", indent = 2L)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>%
        prune_table()
      result
    })
  )
  expect_equal(result, expected)
})
