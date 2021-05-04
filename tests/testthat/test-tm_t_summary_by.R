test_that("template_summary_by generates correct expressions", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>%
        df_explicit_na(
          omit_columns = setdiff(names(adlb), c("AVISIT", "AVAL")),
          na_level = "<Missing>"
        )
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          split_fun = split_fun,
          visible_label = TRUE
        ) %>%
        summarize_vars(
          vars = "AVAL",
          na.rm = FALSE,
          na_level = "<Missing>",
          denom = "N_col",
          .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
          .formats = c(
            n = "xx",
            mean_sd = "xx.xx (xx.xx)",
            median = "xx.xx",
            range = "xx.xx - xx.xx",
            count_fraction = "xx (xx.%)"
          )
        ) %>%
        append_varlabels(adlb, "AVAL")
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_summary_by generates correct expressions when `parallel_vars` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL", "CHG"),
    add_total = TRUE,
    parallel_vars = TRUE,
    by_vars = c("AVISIT"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = FALSE
  )

  expected <- list(
    data = quote({
      anl <- adlb %>%
        df_explicit_na(
          omit_columns = setdiff(names(adlb), c("AVISIT", c("AVAL", "CHG"))),
          na_level = "<Missing>"
        )
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(adsl[["ARM"]])
      anl <- anl %>% mutate(ARM = factor(ARM, levels = arm_levels))
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
        add_colcounts() %>%
        split_rows_by(
          "AVISIT",
          split_label = var_labels(adlb)[["AVISIT"]],
          split_fun = split_fun,
          visible_label = TRUE
        ) %>%
        split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
        summarize_colvars(
          vars = c("AVAL", "CHG"),
          na.rm = FALSE,
          denom = "N_col",
          .stats = c("n", "mean_sd", "median", "range", "count_fraction")
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

test_that("template_summary_by generates correct expressions when `row_groups` is true", {
  result <- template_summary_by(
    parentname = "adsl",
    dataname = "adsl",
    arm_var = "ARM",
    id_var = "USUBJID",
    sum_vars = c("AVAL"),
    add_total = FALSE,
    parallel_vars = FALSE,
    row_groups = TRUE,
    by_vars = c("SEX", "COUNTRY"),
    na.rm = FALSE,
    denominator = "N",
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl <- adsl %>%
        df_explicit_na(
          omit_columns = setdiff(names(adsl), c(c("SEX", "COUNTRY"), "AVAL")),
          na_level = "<Missing>"
        )
      anl <- anl %>% mutate(ARM = droplevels(ARM))
      arm_levels <- levels(anl[["ARM"]])
      adsl <- adsl %>% filter(ARM %in% arm_levels)
      adsl <- adsl %>% mutate(ARM = droplevels(ARM))
    }),
    layout_prep = quote(split_fun <- drop_split_levels),
    layout_cfun = quote(
      cfun_unique <- function(x, labelstr = "", .N_col) { #nolint
        y <- length(unique(x))
        rcell(
          c(y , y / .N_col), #nolint
          label = labelstr
        )
      }
    ),
    layout = quote(
      lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        split_rows_by(
          "SEX",
          split_label = var_labels(adsl)[["SEX"]],
          split_fun = split_fun,
          visible_label = TRUE
        ) %>%
        summarize_row_groups(var = "USUBJID", cfun = cfun_unique) %>%
        split_rows_by(
          "COUNTRY",
          split_label = var_labels(adsl)[["COUNTRY"]],
          split_fun = split_fun,
          visible_label = TRUE
        ) %>%
        summarize_row_groups(var = "USUBJID", cfun = cfun_unique)
    ),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  expect_equal(result, expected)
})
