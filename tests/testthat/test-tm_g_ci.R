library(scda)
adlb <- synthetic_cdisc_data("rcd_2021_05_05")$adlb
ANL <- adlb %>% dplyr::filter(PARAMCD == "ALT", AVISIT == "BASELINE") # nolint

# Test correspond to sections in the TLG catalog.
testthat::test_that("1. and 2. Mean and 95% CIs for mean", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "SEX",
    stat = "mean"
  )

  expected <- quote({
    gg <- ggplot(
      data = ANL,
      mapping = aes(
        x = ARMCD, y = AVAL, color = SEX,
        lty = SEX, shape = SEX
      )
    ) +
      stat_summary(
        fun.data = stat_mean_ci,
        geom = "errorbar",
        width = 0.1,
        position = position_dodge(width = 0.5)
      ) +
      stat_summary(
        fun = mean,
        geom = "point",
        position = position_dodge(width = 0.5)
      ) +
      ggplot2::labs(
        title = "Confidence Interval Plot by Treatment Group",
        caption = "Mean and 95% CIs for mean are displayed.",
        x = "Treatment Group"
      )
    print(gg)
  })

  testthat::expect_equal(result, expected)
  # Check the output.
  # eval(result) ; gg # nolint
})

testthat::test_that("3. Confidence Interval Plot (using different stratification variable)", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "STRATA2",
    stat = "mean"
  )

  expected <- quote({
    gg <- ggplot(
      data = ANL,
      mapping = aes(
        x = ARMCD, y = AVAL, color = STRATA2,
        lty = STRATA2, shape = STRATA2
      )
    ) +
      stat_summary(
        fun.data = stat_mean_ci,
        geom = "errorbar",
        width = 0.1,
        position = position_dodge(width = 0.5)
      ) +
      stat_summary(
        fun = mean,
        geom = "point",
        position = position_dodge(width = 0.5)
      ) +
      ggplot2::labs(
        title = "Confidence Interval Plot by Treatment Group",
        caption = "Mean and 95% CIs for mean are displayed.",
        x = "Treatment Group"
      )
    print(gg)
  })

  testthat::expect_equal(result, expected)
  # Check the output.
  # eval(result) ; gg # nolint
})

testthat::test_that("4. Median and 95% CIs for median", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "STRATA1",
    stat = "median"
  )

  expected <- quote({
    gg <- ggplot(
      data = ANL,
      mapping = aes(
        x = ARMCD, y = AVAL, color = STRATA1,
        lty = STRATA1, shape = STRATA1
      )
    ) +
      stat_summary(
        fun.data = stat_median_ci,
        geom = "errorbar",
        width = 0.1,
        position = position_dodge(width = 0.5)
      ) +
      stat_summary(
        fun = median,
        geom = "point",
        position = position_dodge(width = 0.5)
      ) +
      ggplot2::labs(
        title = "Confidence Interval Plot by Treatment Group",
        caption = "Median and 95% CIs for median are displayed.",
        x = "Treatment Group"
      )
    print(gg)
  })

  testthat::expect_equal(result, expected)
  # Check the output.
  # eval(result) ; gg # nolint
})

testthat::test_that("5. Using different alpha level", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "SEX",
    stat = "mean",
    conf_level = 0.90
  )

  expected <- quote({
    gg <- ggplot(
      data = ANL,
      mapping = aes(
        x = ARMCD, y = AVAL, color = SEX,
        lty = SEX, shape = SEX
      )
    ) +
      stat_summary(
        fun.data = function(x) stat_mean_ci(x, conf_level = 0.9),
        geom = "errorbar",
        width = 0.1,
        position = position_dodge(width = 0.5)
      ) +
      stat_summary(
        fun = mean,
        geom = "point",
        position = position_dodge(width = 0.5)
      ) +
      ggplot2::labs(
        title = "Confidence Interval Plot by Treatment Group",
        caption = "Mean and 90% CIs for mean are displayed.",
        x = "Treatment Group"
      )
    print(gg)
  })

  testthat::expect_equal(result, expected)
  # Check the output.
  # eval(result) ; gg # nolint
})
