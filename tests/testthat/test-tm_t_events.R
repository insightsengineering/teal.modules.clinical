test_that("template_events generates correct expressions", {

  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adae
      anl <- anl %>% mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% mutate(ACTARM = droplevels(ACTARM))
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ACTARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one event",
                      nonunique = "Overall total number of events")
        ) %>%
        split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun
        ) %>%
        append_varlabels(adae, "AEBODSYS") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one event",
                      nonunique = "Overall total number of events")
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
        append_varlabels(adae, "AEDECOD", indent = TRUE)
      ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(
          path = c("AEBODSYS"),
          scorefun = cont_n_onecol(length(levels(adsl$ACTARM)) + 1)
        ) %>%
        sort_at_path(
          path =  c("AEBODSYS", "*", "AEDECOD"),
          scorefun = score_occurrences_cols(col_indices = length(levels(adsl$ACTARM)) + 1)
        )
      pruned_and_sorted_result
    })
  )
  expect_equal(result, expected)
})

test_that("template_events can generate customized table", {

  result <- template_events(
    dataname = "adcm",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = NULL,
    llt = "CMDECOD",
    add_total = FALSE,
    event_type = "treatment",
    drop_arm_levels = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adcm
      adsl <- adsl %>% mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(adsl[["ACTARM"]])
      anl <- anl %>% mutate(ACTARM = factor(ACTARM, levels = arm_levels))
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), "CMDECOD")
      )
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ACTARM") %>%
        add_colcounts() %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one treatment",
                      nonunique = "Overall total number of treatments")
        ) %>%
        count_occurrences(vars = "CMDECOD", .indent_mods = -1L) %>%
        append_varlabels(adcm, "CMDECOD")
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(path =  c("CMDECOD"), scorefun = score_occurrences)
      pruned_and_sorted_result
    })
  )
  expect_equal(result, expected)
})

test_that("template_events can generate customized table with alphabetical sorting", {

  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    add_total = TRUE,
    event_type = "event",
    sort_criteria = "alpha",
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adae
      anl <- anl %>% mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% mutate(ACTARM = droplevels(ACTARM))
      anl <- anl %>% dplyr::mutate(AEBODSYS = as.character(AEBODSYS))
      anl <- anl %>% dplyr::mutate(AEDECOD = as.character(AEDECOD))
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ACTARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one event",
                      nonunique = "Overall total number of events")
        ) %>%
        split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun
        ) %>%
        append_varlabels(adae, "AEBODSYS") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one event",
                      nonunique = "Overall total number of events")
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
        append_varlabels(adae, "AEDECOD", indent = TRUE)
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result
      pruned_and_sorted_result
    })
  )
  expect_equal(result, expected)
})

test_that("template_events can generate customized table with pruning", {

  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    add_total = TRUE,
    event_type = "event",
    prune_freq = 0.4,
    prune_diff = 0.1,
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adae
      anl <- anl %>% mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% mutate(ACTARM = droplevels(ACTARM))
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ACTARM") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one event",
                      nonunique = "Overall total number of events")
        ) %>%
        split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun
        ) %>%
        append_varlabels(adae, "AEBODSYS") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one event",
                      nonunique = "Overall total number of events")
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
        append_varlabels(adae, "AEDECOD", indent = TRUE)
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% prune_table()
      col_indices <- seq_along(table(adsl$ACTARM))
      row_condition <- has_fraction_in_any_col(atleast = 0.4, col_indices = col_indices) &
        has_fractions_difference(atleast = 0.1, col_indices = col_indices)
      pruned_result <- pruned_result %>% prune_table(keep_rows(row_condition))
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(
          path =  c("AEBODSYS"),
          scorefun = cont_n_onecol(length(levels(adsl$ACTARM)) + 1)
        ) %>%
        sort_at_path(
          path =  c("AEBODSYS", "*", "AEDECOD"),
          scorefun =  score_occurrences_cols(col_indices = length(levels(adsl$ACTARM)) + 1)
        )
      criteria_fun <- function(tr) {
        is(tr, "ContentRow")
      }
      pruned_and_sorted_result <- trim_rows(pruned_and_sorted_result, criteria = criteria_fun)
      pruned_and_sorted_result
    })
  )
  expect_equal(result, expected)
})
