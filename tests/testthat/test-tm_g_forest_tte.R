testthat::test_that("template_forest_tte generates correct expressions", {
  result <- template_forest_tte(
    dataname = "adtte",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    subgroup_var = c("SEX", "BMRKR2"),
    strata_var = "STRATA2",
    conf_level = 0.90,
    col_symbol_size = NULL
  )

  expected <- list(
    data = quote({
      anl <- adtte %>%
        dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(ARMCD = combine_levels(ARMCD, c("ARM B", "ARM C"))) %>%
        dplyr::mutate(is_event = CNSR == 0)
      parent <- ANL_ADSL %>%
        dplyr::filter(ARMCD %in% c(
          "ARM A", "ARM B",
          "ARM C"
        )) %>%
        dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A")) %>%
        dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
        dplyr::mutate(ARMCD = combine_levels(
          ARMCD,
          c("ARM B", "ARM C")
        ))
    }),
    summary = quote({
      df <- extract_survival_subgroups(
        variables = list(
          tte = "AVAL",
          is_event = "is_event",
          arm = "ARMCD",
          subgroups = c("SEX", "BMRKR2"),
          strat = "STRATA2"
        ),
        control = control_coxph(conf_level = 0.9),
        data = anl
      )
    }),
    table = quote({
      result <- rtables::basic_table() %>%
        tabulate_survival_subgroups(
          df,
          vars = c("n_tot", "n_tot_events", "n", "n_events", "median", "hr", "ci"),
          time_unit = as.character(anl$AVALU[1])
        )
    }),
    plot = quote({
      p <- decorate_grob(g_forest(tbl = result, col_symbol_size = NULL),
        titles = "Forest plot of survival duration for ", footnotes = "",
        gp_footnotes = grid::gpar(fontsize = 12)
      )
      grid::grid.newpage()
      grid::grid.draw(p)
    })
  )
  testthat::expect_equal(result, expected)
})
