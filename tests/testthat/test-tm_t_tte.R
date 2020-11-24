test_that("template_tte healthy standard output", {

  result <- template_tte(
    anl_name = "ANL",
    parent_name = "ANL_ADSL",
    arm_var = "ARM",
    arm_ref_comp = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = FALSE,
    combine_comp_arms = FALSE,
    aval = "AVAL",
    cnsr = "CNSR",
    strata_var = NULL,
    time_points = c(183, 365, 548),
    time_unit = "Days",
    event_desc_var = "EVNTDESC",
    control = control_tte(
      coxph = control_coxph(),
      surv_time = control_surv_time(),
      surv_timepoint = control_surv_timepoint()
    )
  )

  expected <- list(
    data = bracket_expr(
      list(
        quote(
          ANL <- ANL %>% # nolint
            mutate(
              is_event = CNSR == 0,
              is_not_event = CNSR ==  1,
              EVNT1 = factor(case_when(
                is_event == TRUE ~ "Patients with event (%)",
                is_event == FALSE ~ "Patients without event (%)"
              )),
              EVNTDESC = factor(EVNTDESC)
            ) %>%
            filter(ARM %in% c("B: Placebo", c("A: Drug X", "C: Combination"))) %>%
            mutate(ARM = relevel(ARM, ref = "B: Placebo")) %>%
            mutate(ARM = droplevels(ARM))
        ),
        quote(
          ANL_ADSL <- ANL_ADSL %>% # nolint
            filter(ARM %in% c("B: Placebo", c("A: Drug X", "C: Combination"))) %>%
            mutate(ARM = relevel(ARM, ref = "B: Placebo")) %>%
            mutate(ARM = droplevels(ARM))
        )
      )
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        split_rows_by(
          var = "EVNT1", split_fun = keep_split_levels("Patients with event (%)")
        ) %>%
        summarize_row_groups() %>%
        summarize_vars(vars = "EVNTDESC", .stats = "count") %>%
        summarize_vars(
          "is_not_event", .stats = "count_fraction",
          .labels = c(count_fraction = "Patients without event (%)"),
          nested = FALSE, show_labels = "hidden"
        ) %>%
        surv_time(
          vars = "AVAL",
          var_labels = "Time to Event (Months)",
          is_event = "is_event",
          control = list(
            conf_level = 0.95,
            conf_type = "plain",
            quantiles = c(0.25, 0.75)
          )
        ) %>%
        surv_timepoint(
          vars = "AVAL",
          var_labels = "Days",
          is_event = "is_event",
          time_point = c(183, 365, 548),
          method = "surv"
        )
    ),
    table = quote(
      result <- build_table(lyt = lyt, df = ANL, col_counts = table(ANL_ADSL$ARM))
    )
  )

  expect_equal_expr_list(result, expected)
})
