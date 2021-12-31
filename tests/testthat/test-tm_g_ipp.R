testthat::test_that("template_g_ipp works as expected with default arguments", {
  result <- template_g_ipp(
    paramcd = "PARAMCD", arm_var = "ARMCD",
    arm_levels = letters[1:3], avalu_first = letters[4], paramcd_first = letters[5]
  )

  expected <- list(
    data = quote(anl <- ANL %>% droplevels()),
    graph = quote({
      plot <- h_g_ipp(
        df = anl, xvar = "AVISIT", yvar = "AVAL",
        xlab = "Visit", ylab = "e (d)", title = "Individual Patient Plot for e Values (d) over Time",
        subtitle = "a, b, c", id_var = "USUBJID", add_baseline_hline = FALSE,
        yvar_baseline = "BASE"
      )
      grid::grid.newpage()
      grid::grid.draw(plot)
    })
  )
  testthat::expect_equal(result, expected)
})


testthat::test_that("template_g_ipp works as expected with non-default arguments", {
  result <- template_g_ipp(
    dataname = "adlb",
    paramcd = "PARAM",
    arm_levels = letters[1:3],
    avalu_first = letters[4],
    paramcd_first = letters[5],
    aval_var = "AVAL",
    avalu_var = "AVALU",
    arm_var = "ARMCD",
    id_var = "SUBJID",
    visit_var = "AVISIT",
    base_var = "BASE",
    add_baseline_hline = TRUE,
    separate_by_obs = TRUE
  )

  expected <- list(
    data = quote(anl <- adlb %>% droplevels()),
    graph = quote({
      plot <- h_g_ipp(
        df = anl, xvar = "AVISIT", yvar = "AVAL", xlab = "Visit", ylab = "e (d)",
        title = "Individual Patient Plot for e Values (d) over Time",
        subtitle = "a, b, c", id_var = "SUBJID", add_baseline_hline = TRUE,
        yvar_baseline = "BASE"
      )
      plot <- plot + ggplot2::facet_grid(rows = vars(SUBJID))
      grid::grid.newpage()
      grid::grid.draw(plot)
    })
  )
  testthat::expect_equal(result, expected)
})
