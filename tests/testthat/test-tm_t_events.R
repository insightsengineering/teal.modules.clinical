testthat::test_that("template_events generates correct expressions", {
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
      anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      adsl <- df_explicit_na(adsl, na_level = "")
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
    title = quote({
      title <- paste0(
        "Event Summary by Term : ", rtables::var_labels(adae)["AEBODSYS"], " and ", rtables::var_labels(adae)["AEDECOD"]
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table(title = title) %>%
        rtables::split_cols_by(var = "ACTARM") %>%
        rtables::add_colcounts() %>%
        rtables::add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        rtables::split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = rtables::var_labels(adae["AEBODSYS"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
        append_varlabels(adae, "AEDECOD", indent = 1L)
    ),
    table = quote(
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% rtables::prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(
          path = c("AEBODSYS"),
          scorefun = cont_n_onecol(ncol(result))
        ) %>%
        sort_at_path(
          path = c("AEBODSYS", "*", "AEDECOD"),
          scorefun = score_occurrences_cols(col_indices = seq(1, ncol(result)))
        )
      pruned_and_sorted_result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_events generates correct expressions for nested columns", {
  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = c("ACTARM", "ACTARMCD"),
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    add_total = TRUE,
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adae
      anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      anl <- anl %>% dplyr::mutate(ACTARMCD = droplevels(ACTARMCD))
      arm_levels <- levels(anl[["ACTARMCD"]])
      adsl <- adsl %>% dplyr::filter(ACTARMCD %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ACTARMCD = droplevels(ACTARMCD))
      adsl <- df_explicit_na(adsl, na_level = "")
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
    title = quote({
      title <- paste0(
        "Event Summary by Term : ", rtables::var_labels(adae)["AEBODSYS"], " and ", rtables::var_labels(adae)["AEDECOD"]
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table(title = title) %>%
        rtables::split_cols_by(var = "ACTARM") %>%
        rtables::split_cols_by("ACTARMCD", split_fun = drop_split_levels) %>%
        rtables::add_colcounts() %>%
        rtables::add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        rtables::split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = rtables::var_labels(adae["AEBODSYS"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
        append_varlabels(adae, "AEDECOD", indent = 1L)
    ),
    table = quote(
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% rtables::prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(
          path = c("AEBODSYS"),
          scorefun = cont_n_onecol(ncol(result))
        ) %>%
        sort_at_path(
          path = c("AEBODSYS", "*", "AEDECOD"),
          scorefun = score_occurrences_cols(col_indices = seq(1, ncol(result)))
        )
      pruned_and_sorted_result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_events can generate customized table", {
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
      adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(adsl[["ACTARM"]])
      anl <- anl %>% dplyr::mutate(ACTARM = factor(ACTARM, levels = arm_levels))
      adsl <- df_explicit_na(adsl, na_level = "")
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), "CMDECOD")
      )
    }),
    title = quote({
      title <- paste0("Event Summary by Term : ", rtables::var_labels(adcm)["CMDECOD"])
    }),
    layout = quote(
      lyt <- rtables::basic_table(title = title) %>%
        rtables::split_cols_by(var = "ACTARM") %>%
        rtables::add_colcounts() %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one treatment",
            nonunique = "Overall total number of treatments"
          )
        ) %>%
        count_occurrences(vars = "CMDECOD", .indent_mods = -1L) %>%
        append_varlabels(adcm, "CMDECOD")
    ),
    table = quote(
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% rtables::prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(path = c("CMDECOD"), scorefun = score_occurrences)
      pruned_and_sorted_result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_events can generate customized table with alphabetical sorting", {
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
      anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      adsl <- df_explicit_na(adsl, na_level = "")
      anl <- anl %>% dplyr::mutate(AEBODSYS = as.character(AEBODSYS))
      anl <- anl %>% dplyr::mutate(AEDECOD = as.character(AEDECOD))
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
    title = quote({
      title <- paste0(
        "Event Summary by Term : ", rtables::var_labels(adae)["AEBODSYS"], " and ", rtables::var_labels(adae)["AEDECOD"]
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table(title = title) %>%
        rtables::split_cols_by(var = "ACTARM") %>%
        rtables::add_colcounts() %>%
        rtables::add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        rtables::split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = rtables::var_labels(adae["AEBODSYS"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
        append_varlabels(adae, "AEDECOD", indent = 1L)
    ),
    table = quote(
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% rtables::prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result
      pruned_and_sorted_result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_events can generate customized table with pruning", {
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
      anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      adsl <- df_explicit_na(adsl, na_level = "")
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
    title = quote({
      title <- paste0(
        "Event Summary by Term : ", rtables::var_labels(adae)["AEBODSYS"], " and ", rtables::var_labels(adae)["AEDECOD"]
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table(title = title) %>%
        rtables::split_cols_by(var = "ACTARM") %>%
        rtables::add_colcounts() %>%
        rtables::add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        rtables::split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = rtables::var_labels(adae["AEBODSYS"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
        append_varlabels(adae, "AEDECOD", indent = 1L)
    ),
    table = quote(
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% rtables::prune_table()
      col_indices <- 1:(ncol(result) - TRUE)
      row_condition <- has_fraction_in_any_col(atleast = 0.4, col_indices = col_indices) &
        has_fractions_difference(atleast = 0.1, col_indices = col_indices)
      pruned_result <- pruned_result %>% rtables::prune_table(keep_rows(row_condition))
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(
          path = c("AEBODSYS"),
          scorefun = cont_n_onecol(ncol(result))
        ) %>%
        sort_at_path(
          path = c("AEBODSYS", "*", "AEDECOD"),
          scorefun = score_occurrences_cols(col_indices = seq(1, ncol(result)))
        )
      criteria_fun <- function(tr) {
        inherits(tr, "ContentRow")
      }
      pruned_and_sorted_result <- rtables::trim_rows(pruned_and_sorted_result, criteria = criteria_fun)
      pruned_and_sorted_result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_events can generate customized table with pruning for nested column", {
  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = c("ACTARM", "ACTARMCD"),
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
      anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      arm_levels <- levels(anl[["ACTARM"]])
      adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
      anl <- anl %>% dplyr::mutate(ACTARMCD = droplevels(ACTARMCD))
      arm_levels <- levels(anl[["ACTARMCD"]])
      adsl <- adsl %>% dplyr::filter(ACTARMCD %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ACTARMCD = droplevels(ACTARMCD))
      adsl <- df_explicit_na(adsl, na_level = "")
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
    title = quote({
      title <- paste0(
        "Event Summary by Term : ", rtables::var_labels(adae)["AEBODSYS"], " and ", rtables::var_labels(adae)["AEDECOD"]
      )
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- rtables::basic_table(title = title) %>%
        rtables::split_cols_by(var = "ACTARM") %>%
        rtables::split_cols_by("ACTARMCD", split_fun = drop_split_levels) %>%
        rtables::add_colcounts() %>%
        rtables::add_overall_col(label = "All Patients") %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        rtables::split_rows_by(
          "AEBODSYS",
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = split_fun,
          label_pos = "topleft",
          split_label = rtables::var_labels(adae["AEBODSYS"], fill = TRUE)
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Overall total number of events"
          )
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
        append_varlabels(adae, "AEDECOD", indent = 1L)
    ),
    table = quote(
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
    ),
    prune = quote({
      pruned_result <- result %>% rtables::prune_table()
      col_indices <- 1:(ncol(result) - TRUE)
      row_condition <- has_fraction_in_any_col(atleast = 0.4, col_indices = col_indices) &
        has_fractions_difference(atleast = 0.1, col_indices = col_indices)
      pruned_result <- pruned_result %>% rtables::prune_table(keep_rows(row_condition))
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(
          path = c("AEBODSYS"),
          scorefun = cont_n_onecol(ncol(result))
        ) %>%
        sort_at_path(
          path = c("AEBODSYS", "*", "AEDECOD"),
          scorefun = score_occurrences_cols(col_indices = seq(1, ncol(result)))
        )
      criteria_fun <- function(tr) {
        inherits(tr, "ContentRow")
      }
      pruned_and_sorted_result <- rtables::trim_rows(pruned_and_sorted_result, criteria = criteria_fun)
      pruned_and_sorted_result
    })
  )
  testthat::expect_equal(result, expected)
})
