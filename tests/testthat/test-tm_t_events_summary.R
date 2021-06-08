test_that("template_events_summary generates minimal table", {

  result <- template_events_summary(
    anl = "adae",
    parentname = "adsl",
    arm_var = "ARM"
  )

  expected <- list(
    data = quote({
      anl <- adae
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(adsl[["ARM"]])
      anl <- anl %>% mutate(ARM = factor(ARM, levels = arm_levels))
      study_id <- unique(anl[["STUDYID"]])
      anl <- anl %>%
        dplyr::mutate(
          AEDECOD = as.character(AEDECOD),
          USUBJID_AESEQ = paste(USUBJID, AESEQ, sep = "@@")
        )
      anl <- df_explicit_na(anl, na_level = "")
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_parent = quote(
      lyt_parent <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        count_values(
          "DTHFL",
          values = "Y",
          .labels = c(count_fraction = "Total number of deaths"),
          denom = "N_col"
        ) %>%
        count_values(
          "DCSREAS",
          values = "ADVERSE EVENT",
          .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
          denom = "N_col"
        )
    ),
    table_parent = quote(
      result_parent <- build_table(lyt = lyt_parent, df = adsl, alt_counts_df = adsl)
    ),
    layout_anl = quote(
      lyt_anl <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        count_patients_with_event(
          vars = "USUBJID",
          filters = c(STUDYID = study_id),
          denom = "N_col",
          .stats = "count_fraction",
          .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
          .indent_mods = c(count_fraction = 0L), table_names = "total_pts_at_least_one") %>%
        count_values(
          "STUDYID",
          values = study_id,
          .stats = "count",
          .labels = c(count = "Total AEs"),
          table_names = "total_aes"
        )
    ),
    table_anl = quote(
      result_anl <- build_table(lyt = lyt_anl, df = anl, alt_counts_df = adsl)
    ),
    table = quote({
      col_info(result_parent) <- col_info(result_anl)
      result <- rbind(result_anl, result_parent)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_events_summary generates table with multiple flags", {

  result <- template_events_summary(
    anl = "adae",
    parentname = "adsl",
    arm_var = "ARM",
    add_total = TRUE,
    flag_var_anl = c("A", "B", "C"),
    flag_var_aesi = c("X", "Y"),
    count_subj = TRUE,
    count_pt = TRUE,
    count_events = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adae
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(adsl[["ARM"]])
      anl <- anl %>% mutate(ARM = factor(ARM, levels = arm_levels))
      study_id <- unique(anl[["STUDYID"]])
      anl <- anl %>%
        dplyr::mutate(
          AEDECOD = as.character(AEDECOD),
          USUBJID_AESEQ = paste(USUBJID, AESEQ, sep = "@@")
        )
      flag_var_anl_label <- var_labels(anl[, c("A", "B", "C")])
      flag_var_aesi_label <- var_labels(anl[, c("X", "Y")])
      anl <- df_explicit_na(anl, na_level = "")
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout_parent = quote(
      lyt_parent <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        count_values(
          "DTHFL",
          values = "Y",
          .labels = c(count_fraction = "Total number of deaths"),
          denom = "N_col"
        ) %>%
        count_values(
          "DCSREAS",
          values = "ADVERSE EVENT",
          .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
          denom = "N_col"
        )
    ),
    table_parent = quote(
      result_parent <- build_table(lyt = lyt_parent, df = adsl, alt_counts_df = adsl)
    ),
    layout_anl = quote(
      lyt_anl <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        count_patients_with_event(
          vars = "USUBJID",
          filters = c(STUDYID = study_id),
          denom = "N_col",
          .stats = "count_fraction",
          .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
          .indent_mods = c(count_fraction = 0L), table_names = "total_pts_at_least_one") %>%
        count_values(
          "STUDYID",
          values = study_id,
          .stats = "count",
          .labels = c(count = "Total AEs"),
          table_names = "total_aes"
        ) %>%
        count_patients_with_flags(
          var = "USUBJID",
          flag_variables = flag_var_anl_label,
          table_names = paste0("count_subj_", c("A", "B", "C")),
          .indent_mods = 1L
        ) %>%
        count_patients_with_flags(
          var = "AEDECOD",
          flag_variables = flag_var_anl_label,
          table_names = paste0("count_pt_", c("A", "B", "C")),
          .stats = "count",
          .formats = c(count = "xx"),
          .indent_mods = 1L
        ) %>%
        count_patients_with_flags(
          var = "USUBJID_AESEQ",
          flag_variables = flag_var_anl_label,
          table_names = paste0("count_events_", c("A", "B", "C")),
          .stats = "count",
          .formats = c(count = "xx"),
          .indent_mods = 1L
        ) %>%
        count_patients_with_flags(
          var = "USUBJID",
          flag_variables = flag_var_aesi_label,
          table_names = paste0("count_subj_", c("X", "Y")),
          .indent_mods = 1L
        ) %>%
        count_patients_with_flags(
          var = "AEDECOD",
          flag_variables = flag_var_aesi_label,
          table_names = paste0("count_pt_", c("X", "Y")),
          .stats = "count",
          .formats = c(count = "xx"),
          .indent_mods = 1L
        ) %>%
        count_patients_with_flags(
          var = "USUBJID_AESEQ",
          flag_variables = flag_var_aesi_label,
          table_names = paste0("count_events_", c("X", "Y")),
          .stats = "count",
          .formats = c(count = "xx"),
          .indent_mods = 1L
        )
    ),
    table_anl = quote(
      result_anl <- build_table(lyt = lyt_anl, df = anl, alt_counts_df = adsl) %>%
        insert_rrow(rrow("Total number of patients with at least one"), at = 3) %>%
        insert_rrow(rrow("Total number of unique preferred terms which are"), at = 7) %>%
        insert_rrow(rrow("Total number of adverse events which are"), at = 11) %>%
        insert_rrow(rrow("Medical concepts: number of patients with"), at = 15) %>%
        insert_rrow(rrow("Medical concepts: number of unique preferred terms which are part of"), at = 18) %>%
        insert_rrow(rrow("Medical concepts: number of adverse events which are part of"), at = 21)
    ),
    table = quote({
      col_info(result_parent) <- col_info(result_anl)
      result <- rbind(result_anl[1:2, ], result_parent, result_anl[3:nrow(result_anl), ])
      result
    })
  )
  expect_equal(result, expected)
})

test_that("h_count_rows works as expected", {

  vars_anl <- c("A", "B", "C")
  vars_aesi <- c("Y", "Z")

  # Only analysis flag.
  expect_identical(
    h_count_rows(x_anl = TRUE, vars_anl = vars_anl),
    1
  )

  expect_identical(
    h_count_rows(x_anl = c(TRUE, TRUE), vars_anl = vars_anl),
    5
  )

  expect_identical(
    h_count_rows(x_anl = c(TRUE, TRUE, TRUE), vars_anl = vars_anl),
    9
  )

  # Only AESI flag.
  expect_identical(
    h_count_rows(x_aesi = TRUE, vars_aesi = vars_aesi),
    1
  )

  expect_identical(
    h_count_rows(x_aesi = c(TRUE, TRUE), vars_aesi = vars_aesi),
    4
  )

  expect_identical(
    h_count_rows(x_aesi = c(TRUE, TRUE, TRUE), vars_aesi = vars_aesi),
    7
  )

  # Both flags.
  expect_identical(
    h_count_rows(x_anl = TRUE, x_aesi = TRUE, vars_anl = vars_anl, vars_aesi = vars_aesi),
    5
  )

  expect_identical(
    h_count_rows(x_anl = c(TRUE, TRUE), x_aesi = TRUE, vars_anl = vars_anl, vars_aesi = vars_aesi),
    9
  )

  expect_identical(
    h_count_rows(
      x_anl = c(TRUE, TRUE),
      x_aesi = c(TRUE, TRUE),
      vars_anl = vars_anl,
      vars_aesi = vars_aesi
    ),
    12
  )
})
