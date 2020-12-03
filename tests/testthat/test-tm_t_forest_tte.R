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
          subgroups = c("SEX", "BMRKR2")
          ),
        data = anl
        )
      tbl_survtime <- basic_table() %>%
        tabulate_survival_subgroups(
          vars = c("n", "median"),
          control = control_coxph(conf_level = 0.9)
          ) %>%
        build_table(df$survtime)
      tbl_hr <- basic_table() %>%
        tabulate_survival_subgroups(
          vars = c("n_tot", "hr", "ci"),
          control = control_coxph(conf_level = 0.9)
          ) %>%
        build_table(df$hr)
    }),
    table = quote(
      result <- cbind_rtables(tbl_hr[, 1], tbl_survtime, tbl_hr[, 2:3])
    ),
    plot = quote({
      p <- g_forest(
        tbl = result,
        col_x = 6,
        col_ci = 7,
        vline = NULL,
        forest_header = NULL,
        xlim = NULL,
        logx = FALSE,
        x_at = NULL,
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
