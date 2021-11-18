test_that("template_logistic generates correct expressions", {
  result <- template_logistic(
    dataname = "ANL",
    arm_var = "ARMCD",
    aval_var = "AVALC",
    paramcd = "PARAMCD",
    cov_var = c("AGE", "SEX"),
    interaction_var = "AGE",
    ref_arm = c("ARM A", "ARM B"),
    comp_arm = "ARM C",
    conf_level = 0.95,
    combine_comp_arms = FALSE,
    responder_val = c("CR"),
    topleft = "BESRSPI",
    at = c(30, 40)
  )

  expected <- list(
    arm_lab = quote(arm_var_lab <- var_labels(ANL["ARMCD"])),
    data = quote(
    anl <- ANL %>%
      dplyr::filter(ARMCD %in% c("ARM A", "ARM B", "ARM C")) %>%
      dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>%
      dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A/ARM B")) %>%
      dplyr::mutate(ARMCD = droplevels(ARMCD)) %>%
      dplyr::mutate(Response = AVALC %in% "CR") %>%
      df_explicit_na(na_level = "")
    ),
    relabel = quote(rtables::var_labels(anl["ARMCD"]) <- arm_var_lab),
    model = quote(
    mod <- fit_logistic(
      anl, variables = list(response = "Response", arm = "ARMCD", covariates = c("AGE", "SEX"), interaction = "AGE")
    ) %>%
      broom::tidy(conf_level = 0.95, at = c(30, 40)) %>%
      df_explicit_na(na_level = "")
    ),
    table = quote({
      result <- basic_table(
        title = paste(
          "Table of", "PARAMCD",
          "for",
          paste(head("CR", -1), collapse = ", "),
          ifelse(length("CR") > 1, "and", ""),
          tail("CR", 1),
          "Responders")
        ) %>%
        summarize_logistic(conf_level = 0.95) %>%
        append_topleft("BESRSPI") %>%
        build_table(df = mod)
      result
    })
  )

  expect_equal(result, expected)
})
