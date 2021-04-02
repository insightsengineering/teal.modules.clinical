test_that("template_g_km works as expected with default arguments", {
  result <- template_g_km(comp_arm = c("ARM A", "ARM B"))
  expected <- list(
    data = quote({
      anl <- ANL %>%
        mutate(ARM = droplevels(ARM)) %>%
        mutate(is_event = CNSR == 0)
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
          }
          else {
            g_km(
              df = df_i, variables = variables, font_size = 8,
              xlab = paste0(
                "Survival time", " (", anl$AVALU[1],
                ")"
              ), yval = "Survival", xticks = NULL, newpage = FALSE,
              title = paste("KM Plot", quote(SEX), "=", as.character(unique(df_i$SEX))),
              ggtheme = theme_minimal(), annot_surv_med = TRUE,
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
  expect_equal(result, expected)
})

test_that("template_g_km gives correct data expression when we want to compare versus reference arms", {
  result <- template_g_km(
    comp_arm = c("ARM A", "ARM B"),
    ref_arm = c("ARM C", "ARM D"),
    compare_arm = TRUE
  )
  expected_data <- quote({
    anl <- ANL %>%
      filter(ARM %in% c("ARM C", "ARM D", "ARM A", "ARM B")) %>%
      mutate(ARM = combine_levels(ARM, levels = c("ARM C", "ARM D"), new_level = "ARM C/ARM D")) %>%
      mutate(ARM = relevel(ARM, ref = "ARM C/ARM D")) %>%
      mutate(ARM = droplevels(ARM)) %>%
      mutate(is_event = CNSR == 0)
  })
  expect_equal(result$data, expected_data)
})

test_that("template_g_km gives correct data expression when we want to combine comparison arms", {
  result <- template_g_km(
    comp_arm = c("ARM A", "ARM B"),
    ref_arm = c("ARM C", "ARM D"),
    compare_arm = TRUE,
    combine_comp_arms = TRUE
  )
  expected_data <- quote({
    anl <- ANL %>%
      filter(ARM %in% c("ARM C", "ARM D", "ARM A", "ARM B")) %>%
      mutate(ARM = combine_levels(ARM, levels = c("ARM C", "ARM D"), new_level = "ARM C/ARM D")) %>%
      mutate(ARM = relevel(ARM, ref = "ARM C/ARM D")) %>%
      mutate(ARM = droplevels(ARM)) %>%
      mutate(is_event = CNSR == 0) %>%
      mutate(ARM = combine_levels(ARM, levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B"))
  })
  expect_equal(result$data, expected_data)
})
