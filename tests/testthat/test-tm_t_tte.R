
test_that("template_tte healthy standard output", {

  result <- template_tte(
    dataname = "adtte",
    parentname = "adsl",
    arm_var = "ARM",
    arm_ref_comp = "B: Placebo",
    comp_arm = c("A: Drug X", "C: Combination"),
    compare_arm = FALSE,
    combine_comp_arms = FALSE,
    paramcd = "OS",
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
    data = quote({
      anl <- adtte %>%
        filter(PARAMCD == "OS") %>%
        mutate(
          is_event = CNSR == 0,
          is_not_event = CNSR == 1,
          EVNT1 = factor(
            case_when(
              is_event == TRUE ~ "Patients with event (%)",
              is_event == FALSE ~ "Patients without event (%)"
            )
          ),
          EVNTDESC = factor(EVNTDESC)
        )
      anl <- anl[anl$ARM %in% c("B: Placebo", c("A: Drug X", "C: Combination")), ]
      adsl <- adsl[adsl$ARM %in% c("B: Placebo", c("A: Drug X", "C: Combination")), ]
      anl$ARM <- droplevels(relevel(anl$ARM, "B: Placebo")) # nolint
      adsl$ARM <- droplevels(relevel(adsl$ARM, "B: Placebo")) # nolint
    }),
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
      result <- build_table(lyt = lyt, df = anl, col_counts = table(adsl$ARM))
    )
  )

  expect_equal(result, expected)
})
