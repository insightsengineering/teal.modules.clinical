test_that("template_events_patyear generates standard expressions", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARMCD",
    paramcd = "AETTE1",
    control = control_incidence_rate(),
    event_indicator = 0,
    add_total = TRUE
  )

  expected <- list(
    data = quote(
      anl <- adaette %>%
        filter(PARAMCD == "AETTE1") %>%
        mutate(is_event = CNSR == 0)
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        estimate_incidence_rate(
          vars = "AVAL",
          is_event = "is_event",
          control = control_incidence_rate(
            conf_level = 0.95,
            conf_type = "normal",
            time_unit = 1
          )
        )
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        col_counts = c(table(adsl$ARMCD), `All Patients` = sum(table(adsl$ARMCD)))
      )
    )
  )

  expect_equal_expr_list(result, expected)
})

test_that("template_events_patyear generates right expressions with non-default", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARM",
    paramcd = "AETTE2",
    control = control_incidence_rate(),
    event_indicator = 0,
    add_total = FALSE
  )

  expected <- list(
    data = quote(
      anl <- adaette %>%
        filter(PARAMCD == "AETTE2") %>%
        mutate(is_event = CNSR == 0)
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        estimate_incidence_rate(
          vars = "AVAL",
          is_event = "is_event",
          control = control_incidence_rate(
            conf_level = 0.95,
            conf_type = "normal",
            time_unit = 1
          )
        )
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        col_counts = table(adsl$ARM)
      )
    )
  )

  expect_equal_expr_list(result, expected)
})

test_that("template_events_patyear generates right expressions with non-default controls", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARMCD",
    paramcd = "AETTE1",
    control = control_incidence_rate(
      conf_level = 0.9,
      conf_type = "exact",
      time_unit = 100
    ),
    event_indicator = 0,
    add_total = TRUE
  )

  expected <- list(
    data = quote(
      anl <- adaette %>%
        filter(PARAMCD == "AETTE1") %>%
        mutate(is_event = CNSR == 0)
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        estimate_incidence_rate(
          vars = "AVAL",
          is_event = "is_event",
          control = control_incidence_rate(
            conf_level = 0.9,
            conf_type = "exact",
            time_unit = 100
          )
        )
    ),
    table = quote(
      result <- build_table(
        lyt = lyt,
        df = anl,
        col_counts = c(table(adsl$ARMCD), `All Patients` = sum(table(adsl$ARMCD)))
      )
    )
  )

  expect_equal_expr_list(result, expected)
})
