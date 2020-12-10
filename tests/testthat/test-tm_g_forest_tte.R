test_that("template_forest_tte generates correct expressions", {
  result <- template_forest_tte(
    anl_name = "adtte",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    subgroup_var = c("SEX", "BMRKR2"),
    strata_var = "STRATA2",
    conf_level = 0.90,
    col_symbol_size = 1
  )

  expected <- list(
    data = quote({
      anl <- adtte %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(
          ARMCD = ARMCD %>%
            relevel("ARM A") %>%
            droplevels() %>%
            combine_levels(c("ARM B", "ARM C"))
          ) %>%
        mutate(is_event = CNSR == 0)
      parent <- ANL_ADSL %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(
          ARMCD = ARMCD %>%
            relevel("ARM A") %>%
            droplevels() %>%
            combine_levels(c("ARM B", "ARM C"))
          )
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
    table = quote(
      result <- basic_table() %>%
        tabulate_survival_subgroups(df, vars = c("n_tot", "n", "n_events", "median", "hr", "ci"))
    ),
    plot = quote({
      p <- g_forest(
        tbl = result,
        col_x = 8,
        col_ci = 9,
        vline = 1,
        forest_header = paste0(rev(levels(anl[["ARMCD"]])), "\nbetter"),
        xlim = c(0.1, 10),
        logx = TRUE,
        x_at = c(0.1, 1, 10),
        width_row_names = NULL,
        width_columns = NULL,
        width_forest = unit(1, "null"),
        col_symbol_size = 1,
        draw = TRUE,
        newpage = TRUE
        )
      if (!is.null(footnotes(p))) {
        p <- decorate_grob(p, title = "Forest plot", footnotes = footnotes(p),
                           gp_footnotes = gpar(fontsize = 12))
      }
      grid::grid.newpage()
      grid::grid.draw(p)
      })
    )
  expect_equal(result, expected)
})
