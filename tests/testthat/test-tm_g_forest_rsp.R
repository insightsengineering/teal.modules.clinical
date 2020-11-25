test_that("template_forest_rsp generates correct expressions", {

  result <- template_forest_rsp(
    anl_name = "adrs",
    parent_name = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    aval_var = "AVALC",
    responders = c("CR", "PR"),
    subgroup_var = c("SEX", "STRATA2"),
    strata_var = NULL,
    conf_level = 0.95
  )

  expected <- list(
    data = quote({
      # nolint start
      adrs <- adrs %>%
        mutate(is_rsp = AVALC %in% c("CR", "PR")) %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(
          ARMCD = relevel(ARMCD, ref = "ARM A") %>%
            droplevels() %>%
            combine_levels(levels = c("ARM B", "ARM C"))
        )
      parent <- adsl %>%
        filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
        mutate(
          ARMCD = relevel(ARMCD, ref = "ARM A") %>%
            droplevels() %>%
            combine_levels(levels = c("ARM B", "ARM C"))
        )
      # nolint end
    }),
    summary = quote({
      df <- extract_rsp_subgroups(
        variables = list(rsp = "is_rsp", arm = "ARMCD", subgroups = c("SEX", "STRATA2"), strata_var = NULL),
        data = adrs
      )
      rsp_tab <- basic_table() %>%
        tabulate_rsp_subgroups(vars = c("n", "prop")) %>%
        build_table(df$prop)
      or_tab <- basic_table() %>%
        tabulate_rsp_subgroups(vars = c("n_tot", "or", "ci"), conf_level = 0.95) %>%
        build_table(df$or)
    }),
    table = quote(
      result <- cbind_rtables(or_tab[, 1], rsp_tab, or_tab[, -1])
    ),
    plot = quote({
      p <- g_forest(
        tbl = result, col_x = 6, col_ci = 7, vline = 1,
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
      grid.newpage()
      grid.draw(p)
    })
  )

  expect_equal_expr_list(result, expected)
})
