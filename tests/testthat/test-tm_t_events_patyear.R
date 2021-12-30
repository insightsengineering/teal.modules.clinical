testthat::test_that("template_events_patyear generates standard expressions", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARMCD",
    events_var = "n_events",
    aval_var = "AVAL",
    add_total = TRUE,
    drop_arm_levels = TRUE,
    control = control_incidence_rate()
  )

  expected <- list(
    data = quote({
      anl <- adaette
      anl <- anl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
      arm_levels <- levels(anl[["ARMCD"]])
      adsl <- adsl %>% dplyr::filter(ARMCD %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
      anl <- df_explicit_na(anl, na_level = "")
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout = quote(
      lyt <- rtables::basic_table() %>%
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

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_events_patyear generates right expressions with non-default", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARM",
    events_var = "n_events",
    aval_var = "AVAL",
    add_total = FALSE,
    drop_arm_levels = FALSE,
    control = control_incidence_rate()
  )

  expected <- list(
    data = quote({
      anl <- adaette
      adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
      arm_levels <- levels(adsl[["ARM"]])
      anl <- anl %>% dplyr::mutate(ARM = factor(ARM, levels = arm_levels))
      anl <- df_explicit_na(anl, na_level = "")
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout = quote(
      lyt <- rtables::basic_table() %>%
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

  testthat::expect_equal(result, expected)
})

testthat::test_that("template_events_patyear generates right expressions with non-default controls", {
  result <- template_events_patyear(
    dataname = "adaette",
    parentname = "adsl",
    arm_var = "ARMCD",
    aval_var = "AVAL",
    events_var = "n_events",
    add_total = TRUE,
    drop_arm_levels = TRUE,
    control = control_incidence_rate(
      conf_level = 0.9,
      conf_type = "exact",
      time_unit_input = "month",
      time_unit_output = 100
    )
  )

  expected <- list(
    data = quote({
      anl <- adaette
      anl <- anl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
      arm_levels <- levels(anl[["ARMCD"]])
      adsl <- adsl %>% dplyr::filter(ARMCD %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
      anl <- df_explicit_na(anl, na_level = "")
      adsl <- df_explicit_na(adsl, na_level = "")
    }),
    layout = quote(
      lyt <- rtables::basic_table() %>%
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

  testthat::expect_equal(result, expected)
})
