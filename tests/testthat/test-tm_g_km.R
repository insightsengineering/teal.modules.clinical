testthat::test_that("template_g_km works as expected with default arguments", {
  result <- template_g_km(comp_arm = c("ARM A", "ARM B"))
  expected <- list(
    data = quote({
      anl <- ANL %>%
        dplyr::mutate(ARM = droplevels(ARM)) %>%
        dplyr::mutate(is_event = CNSR == 0)
    }),
    variables = quote(
      variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARM")
    ),
    graph = quote({
      grid::grid.newpage()
      lyt <- grid::grid.layout(nrow = nlevels(ANL$SEX), ncol = 1) %>%
        grid::viewport(layout = .) %>%
        grid::pushViewport()
      result <- mapply(
        df = split(anl, f = anl$SEX), nrow = seq_along(levels(anl$SEX)),
        FUN = function(df_i, nrow_i) {
          if (nrow(df_i) == 0) {
            grid::grid.text("No data found for a given facet value.",
              x = 0.5, y = 0.5, vp = grid::viewport(
                layout.pos.row = nrow_i,
                layout.pos.col = 1
              )
            )
          } else {
            g_km(
              df = df_i, variables = variables, font_size = 8,
              xlab = paste0(
                "Survival time",
                " (",
                gsub(
                  "(^|[[:space:]])([[:alpha:]])",
                  "\\1\\U\\2",
                  tolower(anl$AVALU[1]),
                  perl = TRUE
                ),
                ")"
              ), yval = "Survival", xticks = NULL, newpage = FALSE,
              title = ifelse(
                length(NULL) == 0,
                paste0("KM Plot", ", ", quote(SEX), " = ", as.character(unique(df_i$SEX))),
                paste(paste0("KM Plot", ", ", quote(SEX), " = ", as.character(unique(df_i$SEX))),
                  paste("Stratified by", paste(NULL, collapse = ", ")),
                  sep = "\n"
                )
              ),
              footnotes = if (TRUE) {
                paste(
                  "Ties for Coxph (Hazard Ratio):", "efron",
                  "\n", "p-value Method for Coxph (Hazard Ratio):",
                  "log-rank"
                )
              } else {
                NULL
              },
              ggtheme = ggplot2::theme_minimal(), annot_surv_med = TRUE,
              annot_coxph = TRUE, control_surv = control_surv_timepoint(conf_level = 0.95),
              control_coxph_pw = control_coxph(
                conf_level = 0.95,
                pval_method = "log-rank", ties = "efron"
              ),
              ci_ribbon = FALSE, vp = grid::viewport(
                layout.pos.row = nrow_i,
                layout.pos.col = 1
              ), draw = TRUE
            )
          }
        }, SIMPLIFY = FALSE
      )
      km_grobs <- tern::stack_grobs(grobs = result)
      km_grobs
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_g_km gives correct data expression when we want to compare versus reference arms", {
  result <- template_g_km(
    comp_arm = c("ARM A", "ARM B"),
    ref_arm = c("ARM C", "ARM D"),
    compare_arm = TRUE
  )
  expected_data <- quote({
    anl <- ANL %>%
      dplyr::filter(ARM %in% c("ARM C", "ARM D", "ARM A", "ARM B")) %>%
      dplyr::mutate(ARM = combine_levels(ARM, levels = c("ARM C", "ARM D"), new_level = "ARM C/ARM D")) %>%
      dplyr::mutate(ARM = stats::relevel(ARM, ref = "ARM C/ARM D")) %>%
      dplyr::mutate(ARM = droplevels(ARM)) %>%
      dplyr::mutate(is_event = CNSR == 0)
  })
  testthat::expect_equal(result$data, expected_data)
})

testthat::test_that("template_g_km gives correct data expression when we want to combine comparison arms", {
  result <- template_g_km(
    comp_arm = c("ARM A", "ARM B"),
    ref_arm = c("ARM C", "ARM D"),
    compare_arm = TRUE,
    combine_comp_arms = TRUE
  )
  expected_data <- quote({
    anl <- ANL %>%
      dplyr::filter(ARM %in% c("ARM C", "ARM D", "ARM A", "ARM B")) %>%
      dplyr::mutate(ARM = combine_levels(ARM, levels = c("ARM C", "ARM D"), new_level = "ARM C/ARM D")) %>%
      dplyr::mutate(ARM = stats::relevel(ARM, ref = "ARM C/ARM D")) %>%
      dplyr::mutate(ARM = droplevels(ARM)) %>%
      dplyr::mutate(is_event = CNSR == 0) %>%
      dplyr::mutate(ARM = combine_levels(ARM, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B"))
  })
  testthat::expect_equal(result$data, expected_data)
})
