test_that("template_tte produces healthy standard output", {
  result <- template_tte(
    dataname = "ANL",
    parentname = "ANL_ADSL",
    arm_var = "ARM",
    paramcd = "OS",
    ref_arm = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = FALSE,
    combine_comp_arms = FALSE,
    aval_var = "AVAL",
    cnsr_var = "CNSR",
    strata_var = NULL,
    time_points = c(183, 365, 548),
    time_unit_var = "AVALU",
    event_desc_var = "EVNTDESC",
    control = control_tte(
      coxph = control_coxph(),
      surv_time = control_surv_time(),
      surv_timepoint = control_surv_timepoint()
    )
  )

  expected <- list(
    data = quote({
      anl <- ANL %>%
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        dplyr::mutate(
          is_event = CNSR == 0,
          is_not_event = CNSR == 1,
          EVNT1 = factor(
            dplyr::case_when(
              is_event == TRUE ~ "Patients with event (%)",
              is_event == FALSE ~ "Patients without event (%)"
            ),
            levels = c("Patients with event (%)", "Patients without event (%)")
          ),
          EVNTDESC = factor(EVNTDESC)
        ) %>%
        df_explicit_na()
      ANL_ADSL <- ANL_ADSL %>%# nolint
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        df_explicit_na()
    }),
    layout = quote(
      lyt <- rtables::basic_table(title = "Time-To-Event Table for OS") %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        summarize_vars(
          "is_event",
          .stats = "count_fraction",
          .labels = c(count_fraction = "Patients with event (%)")
        ) %>%
        split_rows_by(
          "EVNT1",
          split_label = "Earliest contributing event",
          split_fun = keep_split_levels("Patients with event (%)"),
          label_pos = "visible",
          child_labels = "hidden",
          indent_mod = 1L,
        ) %>%
        split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
        summarize_row_groups(format = "xx") %>%
        summarize_vars(
          "is_not_event",
          .stats = "count_fraction",
          .labels = c(count_fraction = "Patients without event (%)"),
          nested = FALSE,
          show_labels = "hidden"
        ) %>%
        surv_time(
          vars = "AVAL",
          var_labels = paste0("Time to Event (", as.character(anl$AVALU[1]), ")"),
          is_event = "is_event",
          control = list(
            conf_level = 0.95,
            conf_type = "plain",
            quantiles = c(0.25, 0.75)
          ),
          table_names = "time_to_event"
        ) %>%
        surv_timepoint(
          vars = "AVAL",
          var_labels = as.character(anl$AVALU[1]),
          is_event = "is_event",
          time_point = c(183, 365, 548),
          method = "surv",
          control = control_surv_timepoint(
            conf_level = 0.95,
            conf_type = "plain"
          ),
          .indent_mods = NULL,
          table_names = "time_points"
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = ANL_ADSL)
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_tte produces correct data expression when not comparing arms", {
  result <- template_tte(
    dataname = "ANL",
    parentname = "ANL_ADSL",
    arm_var = "ARM",
    paramcd = "OS",
    ref_arm = NULL,
    comp_arm = c("A: Drug X", "B: Placebo", "C: Combination"),
    compare_arm = FALSE,
    combine_comp_arms = TRUE,
    aval = "AVAL",
    cnsr = "CNSR",
    strata_var = NULL,
    time_points = c(183, 365, 548),
    time_unit_var = "AVALU",
    event_desc_var = "EVNTDESC",
    control = control_tte(
      coxph = control_coxph(),
      surv_time = control_surv_time(),
      surv_timepoint = control_surv_timepoint()
    )
  )

  expected_data <- quote({
    anl <- ANL %>%
      dplyr::mutate(ARM = droplevels(ARM)) %>%
      dplyr::mutate(
        is_event = CNSR == 0,
        is_not_event = CNSR == 1,
        EVNT1 = factor(dplyr::case_when(
          is_event == TRUE ~ "Patients with event (%)",
          is_event == FALSE ~ "Patients without event (%)"),
          levels = c("Patients with event (%)", "Patients without event (%)")
        ),
        EVNTDESC = factor(EVNTDESC)
      ) %>%
      df_explicit_na()
    ANL_ADSL <- ANL_ADSL %>% #nolint
      dplyr::mutate(ARM = droplevels(ARM)) %>%
      df_explicit_na()
  })
  expect_equal(result$data, expected_data)
})
