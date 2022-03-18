testthat::test_that("template_abnormality generates correct expressions with default arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    by_vars = c("AVISIT", "PARAM"),
    abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
    grade = "ANRIND",
    add_total = FALSE,
    exclude_base_abn = FALSE,
    drop_arm_levels = TRUE,
    tbl_title = "my_title"
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% dplyr::filter(ONTRTFL == "Y" & !is.na(ANRIND) & ANRIND != "<Missing>")
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      anl <- df_explicit_na(anl, na_level = "<Missing>")
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
    }),
    layout_prep = quote({
      map <- h_map_for_count_abnormal(
        df = anl,
        variables = list(anl = "ANRIND", split_rows = c("AVISIT", "PARAM")),
        abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
        method = "default",
        na_level = "<Missing>"
      )
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "my_title",
        main_footer = "by variables without observed abnormalities are excluded."
      ) %>%
        rtables::split_cols_by(var = "ARM") %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          "AVISIT",
          split_label = teal.data::variable_labels(adlb, fill = FALSE)[["AVISIT"]],
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map)
        ) %>%
        rtables::split_rows_by("PARAM",
          split_label = teal.data::variable_labels(adlb, fill = FALSE)[["PARAM"]],
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>%
        rtables::prune_table()
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_abnormality generates correct expressions with custom arguments", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    by_vars = c("AVISIT", "PARAMCD"),
    abnormal = list(Low = "LOW", Medium = "MEDIUM"),
    grade = "MYANRIND",
    baseline_var = "MYBASELINE",
    treatment_flag_var = "MYTRTFL",
    treatment_flag = "YES",
    add_total = TRUE,
    exclude_base_abn = TRUE,
    drop_arm_levels = FALSE,
    tbl_title = "my_title"
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% dplyr::filter(MYTRTFL == "YES" & !is.na(MYANRIND) & MYANRIND != "<Missing>")
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(adsl[["ARM"]])
      anl <- anl %>% dplyr::mutate(ARM = factor(ARM, levels = arm_levels))
      anl <- df_explicit_na(anl, na_level = "<Missing>")
      adsl <- df_explicit_na(adsl, na_level = "<Missing>")
    }),
    layout_prep = quote({
      map <- h_map_for_count_abnormal(
        df = anl,
        variables = list(anl = "MYANRIND", split_rows = c("AVISIT", "PARAMCD")),
        abnormal = list(Low = "LOW", Medium = "MEDIUM"),
        method = "default",
        na_level = "<Missing>"
      )
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "my_title",
        main_footer = "by variables without observed abnormalities are excluded."
      ) %>%
        rtables::split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          "AVISIT",
          split_label = teal.data::variable_labels(adlb, fill = FALSE)[["AVISIT"]],
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map)
        ) %>%
        rtables::split_rows_by(
          "PARAMCD",
          split_label = teal.data::variable_labels(adlb, fill = FALSE)[["PARAMCD"]],
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>%
        rtables::prune_table()
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_abnormality generates correct expressions with customized na_level", {
  result <- template_abnormality(
    dataname = "adlb",
    parentname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    by_vars = c("AVISIT", "PARAM"),
    abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
    grade = "ANRIND",
    add_total = FALSE,
    exclude_base_abn = FALSE,
    drop_arm_levels = TRUE,
    na_level = "NA",
    tbl_title = "my_title"
  )

  expected <- list(
    data = quote({
      anl <- adlb %>% dplyr::filter(ONTRTFL == "Y" & !is.na(ANRIND) & ANRIND != "NA")
      anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      anl <- df_explicit_na(anl, na_level = "NA")
      adsl <- df_explicit_na(adsl, na_level = "NA")
    }),
    layout_prep = quote({
      map <- h_map_for_count_abnormal(
        df = anl,
        variables = list(anl = "ANRIND", split_rows = c("AVISIT", "PARAM")),
        abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
        method = "default",
        na_level = "NA"
      )
    }),
    layout = quote(
      lyt <- rtables::basic_table(
        title = "my_title",
        main_footer = "by variables without observed abnormalities are excluded."
      ) %>%
        rtables::split_cols_by(var = "ARM") %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          "AVISIT",
          split_label = teal.data::variable_labels(adlb, fill = FALSE)[["AVISIT"]],
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map)
        ) %>%
        rtables::split_rows_by("PARAM",
          split_label = teal.data::variable_labels(adlb, fill = FALSE)[["PARAM"]],
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
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>%
        rtables::prune_table()
      result
    })
  )
  testthat::expect_equal(result, expected)
})
