adlb <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adlb
ANL <- adlb %>% dplyr::filter(PARAMCD == "ALT") # nolint

test_that("template_g_lineplot works as expected with default arguments", {
  result <- template_g_lineplot()
  expected <- list(
    data = quote({
      anl <- ANL
    }),
    variables = quote(
      variables <- control_lineplot_vars(
        x = "AVISIT", y = "AVAL", strata = "ARM", paramcd = "PARAMCD", y_unit = "AVALU")
    ),
    graph = quote({
      grid::grid.newpage()
      result <- g_lineplot(
        df = anl,
        variables = variables,
        interval = "mean_ci",
        mid = "mean",
        whiskers = c("mean_ci_lwr", "mean_ci_upr"),
        table = c("n", "mean_sd", "median", "range"),
        mid_type = "pl",
        mid_point_size = 2,
        table_font_size = 4,
        newpage = FALSE,
        title = paste0("Plot of ",
                       names(which(c(n = "n",
                                     Mean = "mean",
                                     `Standard Deviation` = "sd",
                                     Median = "median") == "mean")), " and ",
                       ifelse("mean_ci" %in% c("mean_ci", "median_ci"),
                              paste0(as.character(0.95 * 100), "% "), ""),
                       names(which(c(`Mean Confidence Interval` = "mean_ci",
                                     `Median Confidence Interval` = "median_ci",
                                     `25% and 75% Quantiles` = "quantiles",
                                     Range = "range") == "mean_ci")), " by Visit"),
        y_lab = paste("AVAL", names(which(c(n = "n",
                                            Mean = "mean",
                                            `Standard Deviation` = "sd",
                                            Median = "median") == "mean")), "Values for"),
        ggtheme = theme_minimal(),
        control = control_summarize_vars(conf_level = 0.95),
        subtitle_add_paramcd = FALSE,
        subtitle_add_unit = FALSE)
    })
  )
  expect_equal(result, expected)
})

test_that("template_g_lineplot gives correct data expression with custom arguments", {
  result <- template_g_lineplot(
    arm_var = "ARMCD",
    y_var = "CHG",
    mid = "median",
    interval = "median_ci",
    whiskers = "median_ci_upr",
    table = c("mean_sd", "median", "median_ci"),
    mid_type = "l",
    conf_level = 0.9,
    incl_screen = FALSE,
    title = "Line Plot"
  )
  expected <- list(
    data = quote({
      anl <- ANL %>% dplyr::filter(AVISIT != "SCREENING") %>% dplyr::mutate(AVISIT = droplevels(AVISIT))
    }),
    variables = quote(
      variables <- control_lineplot_vars(
        x = "AVISIT", y = "CHG", strata = "ARMCD", paramcd = "PARAMCD", y_unit = "AVALU")
    ),
    graph = quote({
      grid::grid.newpage()
      result <- g_lineplot(
        df = anl,
        variables = variables,
        interval = "median_ci",
        mid = "median",
        whiskers = "median_ci_upr",
        table = c("mean_sd", "median", "median_ci"),
        mid_type = "l",
        mid_point_size = 2,
        table_font_size = 4,
        newpage = FALSE,
        title = paste0("Plot of ",
                       names(which(c(n = "n",
                                     Mean = "mean",
                                     `Standard Deviation` = "sd",
                                     Median = "median") == "median")), " and ",
                       ifelse("median_ci" %in% c("mean_ci", "median_ci"),
                              paste0(as.character(0.9 * 100), "% "), ""),
                       names(which(c(`Mean Confidence Interval` = "mean_ci",
                                     `Median Confidence Interval` = "median_ci",
                                     `25% and 75% Quantiles` = "quantiles",
                                     Range = "range") == "median_ci")), " by Visit"),
        y_lab = paste("CHG", names(which(c(n = "n",
                                           Mean = "mean",
                                           `Standard Deviation` = "sd",
                                           Median = "median") == "median")), "Values for"),
        ggtheme = theme_minimal(),
        control = control_summarize_vars(conf_level = 0.9),
        subtitle_add_paramcd = FALSE,
        subtitle_add_unit = FALSE)
    })
  )
  expect_equal(result, expected)
})
