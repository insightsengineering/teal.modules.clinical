adlb <- scda::synthetic_cdisc_data("rcd_2022_02_28")$adlb
ANL <- adlb %>% dplyr::filter(PARAMCD == "ALT") # nolint

testthat::test_that("template_g_lineplot works as expected with default arguments", {
  result <- template_g_lineplot()
  expected <- list(
    data = quote({
      anl <- ANL
    }),
    variables = quote(
      variables <- control_lineplot_vars(
        x = "AVISIT", y = "AVAL", strata = "ARM", paramcd = "PARAMCD", y_unit = "AVALU"
      )
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
        title = "Plot of Mean and 95% Mean Confidence Interval of AVAL by Visit",
        subtitle = "", caption = NULL,
        y_lab = "AVAL Mean Values for",
        legend_title = NULL,
        ggtheme = ggplot2::theme_minimal(),
        control = control_summarize_vars(conf_level = 0.95),
        subtitle_add_paramcd = FALSE,
        subtitle_add_unit = FALSE
      )
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_g_lineplot gives correct data expression with custom arguments", {
  result <- template_g_lineplot(
    strata = "ARMCD",
    y = "CHG",
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
      anl <- ANL %>%
        dplyr::filter(AVISIT != "SCREENING") %>%
        dplyr::mutate(AVISIT = droplevels(AVISIT))
    }),
    variables = quote(
      variables <- control_lineplot_vars(
        x = "AVISIT", y = "CHG", strata = "ARMCD", paramcd = "PARAMCD", y_unit = "AVALU"
      )
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
        title = "Plot of Median and 90% Median Confidence Interval of CHG by Visit",
        subtitle = "", caption = NULL, y_lab = "CHG Median Values for",
        legend_title = NULL,
        ggtheme = ggplot2::theme_minimal(),
        control = control_summarize_vars(conf_level = 0.9),
        subtitle_add_paramcd = FALSE,
        subtitle_add_unit = FALSE
      )
      result
    })
  )
  testthat::expect_equal(result, expected)
})
