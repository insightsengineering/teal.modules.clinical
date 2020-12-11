test_that("template_forest_rsp generates correct expressions", {

  result <- template_forest_rsp(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    aval_var = "AVALC",
    responders = c("CR", "PR"),
    subgroup_var = c("SEX", "STRATA2"),
    strata_var = NULL,
    conf_level = 0.95
  )

  lapply(result, styled_expr)
  expected <- list(
    data = quote({
      adrs <- adrs %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"))
        )
      parent <- adsl %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(ARMCD = relevel(ARMCD, ref = "ARM A")) %>%
        mutate(ARMCD = droplevels(ARMCD)) %>%
        mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", "ARM C"))
        )
    }),
    summary = quote({
      df <- extract_rsp_subgroups(
        variables = list(rsp = "is_rsp", arm = "ARMCD", subgroups = c("SEX", "STRATA2"), strat = NULL),
        data = adrs, conf_level = 0.95
      )
    }),
    table = quote(
      result <- basic_table() %>%
        tabulate_rsp_subgroups(df, vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))
    ),
    plot = quote({
      p <- g_forest(
        tbl = result, col_x = 8, col_ci = 9, vline = 1,
        forest_header = paste0(levels(adrs[["ARMCD"]]), "\nbetter"),
        xlim = c(0.1, 10), logx = TRUE, x_at = c(0.1, 1, 10),
        draw = FALSE, col_symbol_size = NULL
      )
      if (!is.null(footnotes(p))) {
        p <- decorate_grob(
          p, title = "Forest plot", footnotes = footnotes(p),
          gp_footnotes = gpar(fontsize = 12)
        )
      }
      grid::grid.newpage()
      grid::grid.draw(p)
    })
  )

  expect_equal(result, expected)
})
