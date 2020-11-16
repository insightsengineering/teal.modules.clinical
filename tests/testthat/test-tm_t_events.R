test_that("template_events generates correct expressions", {

  result <- template_events(
    dataname = "adae",
    parentname = "adsl",
    arm_var = "ACTARM",
    hlt = "AEBODSYS",
    llt = "AEDECOD",
    add_total = TRUE
    )

  expected <- list(
    data = quote({
      col_n <- table(adsl$ACTARM)
      col_n <- c(col_n, "All Patients" = sum(col_n))
      anl <- adae
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
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
        split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = -1L) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one event",
                      nonunique = "Overall total number of events")
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L)
      ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, col_counts = col_n)
    ),
    prune = quote({
      pruned_result <- result %>% prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(
          path =  c("AEBODSYS"),
          scorefun = cont_n_onecol(length(col_n))
        ) %>%
        sort_at_path(
          path =  c("AEBODSYS", "*", "AEDECOD"),
          scorefun =  score_occurrences_cols(col_indices = length(col_n))
        )
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
    event_type = "treatment"
  )

  expected <- list(
    data = quote({
      col_n <- table(adsl$ACTARM)
      anl <- adcm
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
        count_occurrences(vars = "CMDECOD", .indent_mods = -1L)
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, col_counts = col_n)
    ),
    prune = quote({
      pruned_result <- result %>% prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(path =  c("CMDECOD"), scorefun = score_occurrences)
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
    sort_criteria = "alpha"
  )

  expected <- list(
    data = quote({
      col_n <- table(adsl$ACTARM)
      col_n <- c(col_n, "All Patients" = sum(col_n))
      anl <- adae
      anl <- anl %>% dplyr::mutate(AEBODSYS = as.character(AEBODSYS))
      anl <- anl %>% dplyr::mutate(AEDECOD = as.character(AEDECOD))
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
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
        split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = -1L) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one event",
                      nonunique = "Overall total number of events")
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L)
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, col_counts = col_n)
    ),
    prune = quote({
      pruned_result <- result %>% prune_table()
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result
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
    prune_diff = 0.1
  )

  expected <- list(
    data = quote({
      col_n <- table(adsl$ACTARM)
      col_n <- c(col_n, "All Patients" = sum(col_n))
      anl <- adae
      anl <- anl %>% df_explicit_na(
        omit_columns = setdiff(names(anl), c("AEBODSYS", "AEDECOD"))
      )
    }),
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
        split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = -1L) %>%
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one event",
                      nonunique = "Overall total number of events")
        ) %>%
        count_occurrences(vars = "AEDECOD", .indent_mods = -1L)
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = anl, col_counts = col_n)
    ),
    prune = quote({
      pruned_result <- result %>% prune_table()
      col_indices <- seq_along(1:(length(col_n) - 1))
      row_condition <- has_fraction_in_any_col(atleast = 0.4, col_indices = col_indices) &
        has_fractions_difference(atleast = 0.1, col_indices = col_indices)
      pruned_result <- pruned_result %>% prune_table(keep_rows(row_condition))
    }),
    sort = quote({
      pruned_and_sorted_result <- pruned_result %>%
        sort_at_path(
          path =  c("AEBODSYS"),
          scorefun = cont_n_onecol(length(col_n))
        ) %>%
        sort_at_path(
          path =  c("AEBODSYS", "*", "AEDECOD"),
          scorefun =  score_occurrences_cols(col_indices = length(col_n))
        )
      criteria_fun <- function(tr) {
        is(tr, "ContentRow")
      }
      pruned_and_sorted_result <- trim_rows(pruned_and_sorted_result, criteria = criteria_fun)

    })
  )
  expect_equal(result, expected)
})
