test_that("template_events_patyear generates standard expressions", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARMCD",
    events_var = "n_events",
    aval_var = "AVAL",
    control = control_incidence_rate(),
    add_total = TRUE
  )

  expected <- list(
    data = quote(
      anl <- adaette
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        estimate_incidence_rate(
          vars = "AVAL",
          n_events = "n_events",
          control = control_incidence_rate(
            conf_level = 0.95,
            conf_type = "normal",
            time_unit_input = "year",
            time_unit_output = 1
          )
        )
    ),
    table = quote({
      result <- build_table(
        lyt = lyt,
        df = anl,
        alt_counts_df = adsl
      )
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_events_patyear generates right expressions with non-default", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARM",
    events_var = "n_events",
    aval_var = "AVAL",
    control = control_incidence_rate(),
    add_total = FALSE
  )

  expected <- list(
    data = quote(
      anl <- adaette
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        estimate_incidence_rate(
          vars = "AVAL",
          n_events = "n_events",
          control = control_incidence_rate(
            conf_level = 0.95,
            conf_type = "normal",
            time_unit_input = "year",
            time_unit_output = 1
          )
        )
    ),
    table = quote({
      result <- build_table(
        lyt = lyt,
        df = anl,
        alt_counts_df = adsl
      )
      result
    })
  )

  expect_equal(result, expected)
})

test_that("template_events_patyear generates right expressions with non-default controls", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARMCD",
    events_var = "n_events",
    aval_var = "AVAL",
    control = control_incidence_rate(
      conf_level = 0.9,
      conf_type = "exact",
      time_unit_input = "month",
      time_unit_output = 100
    ),
    add_total = TRUE
  )

  expected <- list(
    data = quote(
      anl <- adaette
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by(var = "ARMCD") %>%
        add_colcounts() %>%
        add_overall_col(label = "All Patients") %>%
        estimate_incidence_rate(
          vars = "AVAL",
          n_events = "n_events",
          control = control_incidence_rate(
            conf_level = 0.9,
            conf_type = "exact",
            time_unit_input = "month",
            time_unit_output = 100
          )
        )
    ),
    table = quote({
      result <- build_table(
        lyt = lyt,
        df = anl,
        alt_counts_df = adsl
      )
      result
    })
  )

  expect_equal(result, expected)
})
