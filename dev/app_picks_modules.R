# Run a teal app exercising all modules changed on this feature branch.
# Run from the package root with: source("dev/app.R")

#pkgload::load_all()

devtools::load_all("teal")
devtools::load_all("teal.picks")
# Package name is teal.modules.clinical (with "s"); "teal.module.clinical" is a typo and will not load sources.
devtools::load_all("teal.modules.clinical")

data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADSL$EOSDY[1] <- NA_integer_

  ADAE <- tmc_ex_adae

  ADTTE <- tmc_ex_adtte

  ADLB <- tmc_ex_adlb

  ADEG <- tmc_ex_adeg

  ADEX <- teal.data::rADEX
  set.seed(1, kind = "Mersenne-Twister")
  .labels <- col_labels(ADEX, fill = FALSE)
  ADEX <- ADEX %>%
    dplyr::distinct(USUBJID, .keep_all = TRUE) %>%
    dplyr::mutate(
      PARAMCD = "TDURD",
      PARAM = "Overall duration (days)",
      AVAL = sample(x = seq(1, 200), size = dplyr::n(), replace = TRUE),
      AVALU = "Days"
    ) %>%
    dplyr::bind_rows(ADEX)
  col_labels(ADEX) <- .labels

  .names_baskets <- grep("^(SMQ|CQ).*NAM$", names(ADAE), value = TRUE)
  .names_scopes <- grep("^SMQ.*SC$", names(ADAE), value = TRUE)
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

arm_ref_comp <- list(
  ACTARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  ),
  ARM = list(
    ref = "B: Placebo",
    comp = c("A: Drug X", "C: Combination")
  )
)

app <- init(
  data = data,
  modules = modules(
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      summarize_vars = variables(
        choices = any_of(c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE")),
        selected = c("SEX", "RACE", "AGE")
      ),
      add_total = TRUE,
      useNA = "ifany"
    ),
    tm_t_summary_by(
      label = "Summary by Row Groups Table",
      dataname = "ADLB",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      by_vars = variables(choices = any_of(c("PARAM", "AVISIT")), selected = "AVISIT"),
      summarize_vars = variables(choices = any_of(c("AVAL", "CHG")), selected = "AVAL"),
      paramcd = variables(choices = "PARAMCD", selected = "PARAMCD"),
      add_total = TRUE,
      useNA = "ifany"
    ),
    tm_t_events(
      label = "Adverse Events Table",
      dataname = "ADAE",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      hlt = variables(choices = any_of(c("AEBODSYS", "AESOC")), selected = "AEBODSYS"),
      llt = variables(choices = any_of(c("AETERM", "AEDECOD")), selected = "AEDECOD"),
      add_total = TRUE,
      event_type = "adverse event"
    ),
    tm_t_smq(
      label = "Adverse Events by SMQ Table",
      dataname = "ADAE",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      baskets = variables(choices = starts_with("SMQ") | starts_with("CQ")),
      scopes = variables(choices = ends_with("SC")),
      llt = variables(choices = "AEDECOD", selected = "AEDECOD"),
      add_total = FALSE
    ),
    tm_t_shift_by_grade(
      label = "Grade Laboratory Abnormality Table",
      dataname = "ADLB",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      paramcd = variables(choices = "PARAMCD", selected = "PARAMCD"),
      worst_flag_var = variables(
        choices = any_of(c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL")),
        selected = "WGRLOVFL"
      ),
      add_total = FALSE
    ),
    tm_t_tte(
      label = "Time To Event Table",
      dataname = "ADTTE",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD", "ACTARMCD"))),
      arm_ref_comp = arm_ref_comp,
      paramcd = variables(choices = "PARAMCD", selected = "PARAMCD"),
      strata_var = variables(choices = any_of(c("SEX", "BMRKR2")), selected = "SEX"),
      time_points = teal.transform::choices_selected(c(182, 243), 182)
    ),
    tm_t_shift_by_arm(
      label = "Shift by Arm (picks)",
      dataname = "ADEG",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      paramcd = variables(choices = "PARAMCD", selected = "PARAMCD"),
      visit_var = variables(choices = "AVISIT", selected = "AVISIT"),
      aval_var = variables(choices = "ANRIND", selected = "ANRIND", fixed = TRUE),
      baseline_var = variables(choices = "BNRIND", selected = "BNRIND", fixed = TRUE)
    ),
    tm_t_shift_by_arm_by_worst(
      label = "Shift by Arm by Worst (picks)",
      dataname = "ADEG",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      paramcd = variables(choices = "PARAMCD", selected = "PARAMCD"),
      worst_flag_var = variables(
        choices = any_of(c("WORS02FL", "WORS01FL")),
        selected = "WORS02FL"
      ),
      worst_flag = teal.transform::choices_selected("Y"),
      aval_var = variables(choices = any_of(c("AVALC", "ANRIND")), selected = "ANRIND"),
      baseline_var = variables(choices = any_of(c("BASEC", "BNRIND")), selected = "BNRIND")
    )
  ),
  filter = teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y"))
)

shinyApp(app$ui, app$server)
