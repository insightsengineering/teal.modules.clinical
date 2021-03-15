test_that("template_summary generates correct expressions", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARM",
    sum_vars = c("RACE", "COUNTRY", "AGE"),
    add_total = FALSE,
    var_labels = character(),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE
  )
  expected <- list(
    data = quote({
      anl <- adrs %>%
        df_explicit_na(
          omit_columns = setdiff(names(adrs), c(c("RACE", "COUNTRY", "AGE"))),
          na_level = "<Missing>"
        )
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        summarize_vars(
          vars = c("RACE", "COUNTRY", "AGE"),
          na.rm = FALSE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c("n", "mean_sd", "median", "range", "count_fraction")
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_summary can generate customized table", {
  result <- template_summary(
    dataname = "adrs",
    parentname = "adsl",
    arm_var = "ARMCD",
    sum_vars = "RACE",
    add_total = TRUE,
    var_labels = c(RACE = "Race"),
    na.rm = TRUE,
    denominator = "omit",
    drop_arm_levels = FALSE
  )
  expected <- list(
    data = quote({
      anl <- adrs %>%
        df_explicit_na(
          omit_columns = setdiff(names(adrs), c("RACE")),
          na_level = "<Missing>"
        )
      adsl <- adsl %>% mutate(ARMCD = droplevels(ARMCD))
      arm_levels <- levels(adsl[["ARMCD"]])
      anl <- anl %>% mutate(ARMCD = factor(ARMCD, levels = arm_levels))
    }),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARMCD", split_fun = add_overall_level(label = "All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        summarize_vars(
          vars = "RACE",
          var_labels = c(RACE = "Race"),
          na.rm = TRUE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c("n", "mean_sd", "median", "range", "count")
        )
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  expect_equal(result, expected)
})
