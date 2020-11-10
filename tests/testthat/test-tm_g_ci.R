library(random.cdisc.data)
adlb <- radlb(cached = TRUE)

# Test correspond to sections in the TLG catalog.
test_that("1. and 2. Mean and 95% CIs for mean", {

  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "SEX",
    paramcd = "ALT",
    avisit = "BASELINE",
    stat = "mean"
  )

  expected <- list(
    data = quote(anl <- adlb %>% filter(PARAMCD == "ALT", AVISIT == "BASELINE")),
    graph = quote(
      ggplot(
        data = anl,
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
        labs(
          title = "Confidence Interval Plot for ALT by Treatment Group",
          subtitle = "Visit: BASELINE",
          caption = "Mean and 95% CIs for mean are displayed.",
          x = "Treatment Group",
          y = paste0(
            "ALT", " (", unique(anl$AVALU),
            ")"
          )
        )
    )
  )

  expect_identical(result, expected)
  # Check the output.
  # mapply(eval, result)$graph # nolint
})

test_that("3. Confidence Interval Plot (using different stratification variable)", {

  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "STRATA2",
    paramcd = "ALT",
    avisit = "BASELINE",
    stat = "mean"
  )

  expected <- list(
    data = quote(anl <- adlb %>% filter(PARAMCD == "ALT", AVISIT == "BASELINE")),
    graph = quote(
      ggplot(
        data = anl,
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
        labs(
          title = "Confidence Interval Plot for ALT by Treatment Group",
          subtitle = "Visit: BASELINE",
          caption = "Mean and 95% CIs for mean are displayed.",
          x = "Treatment Group",
          y = paste0(
            "ALT", " (", unique(anl$AVALU),
            ")"
          )
        )
    )
  )

  expect_identical(result, expected)
  # Check the output.
  # mapply(eval, result)$graph # nolint
})

test_that("4. Median and 95% CIs for median", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "STRATA1",
    paramcd = "ALT",
    avisit = "BASELINE",
    stat = "median"
  )

  expected <- list(
    data = quote(anl <- adlb %>% filter(PARAMCD == "ALT", AVISIT == "BASELINE")),
    graph = quote(
      ggplot(
        data = anl,
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
        labs(
          title = "Confidence Interval Plot for ALT by Treatment Group",
          subtitle = "Visit: BASELINE",
          caption = "Median and 95% CIs for median are displayed.",
          x = "Treatment Group",
          y = paste0(
            "ALT", " (", unique(anl$AVALU),
            ")"
          )
        )
    )
  )

  expect_identical(result, expected)
  # Check the output.
  # mapply(eval, result)$graph # nolint
})

test_that("5. Using different alpha level", {
  result <- template_g_ci(
    dataname = "adlb",
    x_var = "ARMCD",
    y_var = "AVAL",
    grp_var = "SEX",
    paramcd = "ALT",
    avisit = "BASELINE",
    stat = "mean",
    conf_level = 0.90
  )

  expected <- list(
    data = quote(anl <- adlb %>% filter(PARAMCD == "ALT", AVISIT == "BASELINE")),
    graph = quote(
      ggplot(
        data = anl,
        mapping = aes(
          x = ARMCD, y = AVAL, color = SEX,
          lty = SEX, shape = SEX
        )
      ) +
        stat_summary(
          fun.data = function(x) stat_mean_ci(x, conf_level = 0.90),
          geom = "errorbar",
          width = 0.1,
          position = position_dodge(width = 0.5)
        ) +
        stat_summary(
          fun = mean,
          geom = "point",
          position = position_dodge(width = 0.5)
        ) +
        labs(
          title = "Confidence Interval Plot for ALT by Treatment Group",
          subtitle = "Visit: BASELINE",
          caption = "Mean and 90% CIs for mean are displayed.",
          x = "Treatment Group",
          y = paste0(
            "ALT", " (", unique(anl$AVALU),
            ")"
          )
        )
    )
  )

  expect_equivalent(result, expected)
  # Check the output.
  # mapply(eval, result)$graph # nolint
})
