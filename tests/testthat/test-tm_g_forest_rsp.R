test_that("template_events generates correct expressions", {

  result <- template_forest_rsp(
    anl_name = "adrs",
    parent_name = "adsl",
    arm_var = "ARMCD",
    ref_arm = "ARM A",
    comp_arm = c("ARM B", "ARM C"),
    subgroup_var = c("SEX", "STRATA2"),
    strata_var = NULL,
    conf_level = 0.95
  )

  expected <- list(
    data = quote({
      adrs <- adrs %>%
        mutate(rsp_lab = d_onco_rsp_label(AVALC)) %>%
        mutate(
          is_rsp = rsp_lab %in% c("Complete Response (CR)", "Partial Response (PR)")
        )
      # nolint start
      adrs <- filter(adrs, ARMCD %in% c("ARM A", "ARM B", "ARM C"))
      adsl <- filter(adsl, ARMCD %in% c("ARM A", "ARM B", "ARM C"))
      adrs$ARMCD <- droplevels(relevel(adrs$ARMCD, "ARM A"))
      adsl$ARMCD <- droplevels(relevel(adsl$ARMCD, "ARM A"))
      adrs$ARMCD <- combine_levels(x = adrs$ARMCD, levels = c("ARM B", "ARM C"))
      adsl$ARMCD <- combine_levels(x = adsl$ARMCD, levels = c("ARM B", "ARM C"))
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
    )
  )

  expect_equal_expr_list(result, expected)
})
