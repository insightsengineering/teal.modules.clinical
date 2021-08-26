test_that("template_g_ipp works as expected with default arguments", {
  result <- template_g_ipp(paramcd = "PARAMCD", arm_var = "ARMCD")

  expected <- list(
    data = quote({
      anl <- ANL %>% droplevels()
    }),
    graph = quote({
      plot <- h_g_ipp(
        df = anl,
        xvar = "AVISIT",
        yvar = "AVAL",
        xlab = "Visit",
        ylab = paste0(
          anl[["PARAMCD"]][1],
          " (",
          anl[["AVALU"]][1],
          ")"
        ),
        title = paste0(
          "Individual Patient Plot for ",
          anl[["PARAMCD"]][1],
          " Values ",
          "(",
          anl[["AVALU"]][1],
          ")",
          " over Time"
        ),
        subtitle = paste(
          levels(anl[["ARMCD"]]),
          collapse = ", "
          ),
        id_var = "USUBJID",
        add_baseline_hline = FALSE,
        yvar_baseline = "BASE"
      )
      grid::grid.newpage()
      grid::grid.draw(plot)
    })
  )
  expect_equal(result, expected)
})


test_that("template_g_ipp works as expected with non-default arguments", {
  result <- template_g_ipp(
    dataname = "adlb",
    paramcd = "PARAM",
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
    data = quote({
      anl <- adlb %>% droplevels()
    }),
    graph = quote({
      plot <- h_g_ipp(
        df = anl,
        xvar = "AVISIT",
        yvar = "AVAL",
        xlab = "Visit",
        ylab = paste0(
          anl[["PARAM"]][1],
          " (",
          anl[["AVALU"]][1],
          ")"
        ),
        title = paste0(
          "Individual Patient Plot for ",
          anl[["PARAM"]][1],
          " Values ",
          "(",
          anl[["AVALU"]][1],
          ")",
          " over Time"
        ),
        subtitle = paste(
          levels(anl[["ARMCD"]]),
          collapse = ", "
          ),
        id_var = "SUBJID",
        add_baseline_hline = TRUE,
        yvar_baseline = "BASE"
      )
      plot <- plot + ggplot2::facet_grid(rows = vars(SUBJID))
      grid::grid.newpage()
      grid::grid.draw(plot)
    })
  )
  expect_equal(result, expected)
})
