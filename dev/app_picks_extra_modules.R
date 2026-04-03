devtools::load_all("teal")
devtools::load_all("teal.picks")
devtools::load_all("teal.modules.clinical")

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(dplyr)
  library(formatters)
  library(tern)
  library(teal.data)

  ADSL <- tmc_ex_adsl %>%
    mutate(
      DTHFL = case_when(
        !is.na(DTHDT) ~ "Y",
        TRUE ~ ""
      ) %>% with_label("Subject Death Flag")
    )

  ADLB <- tmc_ex_adlb %>%
    mutate(
      ONTRTFL = case_when(
        AVISIT %in% c("SCREENING", "BASELINE") ~ "",
        TRUE ~ "Y"
      ) %>% with_label("On Treatment Record Flag")
    )

  .lbls_adae <- col_labels(tmc_ex_adae)
  ADAE <- tmc_ex_adae

  .add_event_flags <- function(dat) {
    dat <- dat %>%
      mutate(
        TMPFL_SER = AESER == "Y",
        TMPFL_REL = AEREL == "Y",
        TMPFL_GR5 = AETOXGR == "5",
        TMP_SMQ01 = !is.na(SMQ01NAM),
        TMP_SMQ02 = !is.na(SMQ02NAM),
        TMP_CQ01 = !is.na(CQ01NAM)
      )
    column_labels <- list(
      TMPFL_SER = "Serious AE",
      TMPFL_REL = "Related AE",
      TMPFL_GR5 = "Grade 5 AE",
      TMP_SMQ01 = aesi_label(dat[["SMQ01NAM"]], dat[["SMQ01SC"]]),
      TMP_SMQ02 = aesi_label("Y.9.9.9.9/Z.9.9.9.9 AESI"),
      TMP_CQ01 = aesi_label(dat[["CQ01NAM"]])
    )
    col_labels(dat)[names(column_labels)] <- as.character(column_labels)
    dat
  }
  ADAE <- ADAE %>% .add_event_flags()
  ADAE <- ADAE %>% mutate_if(is.character, as.factor)
  col_labels(ADAE)[names(.lbls_adae)] <- .lbls_adae

  .ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
  .aesi_vars <- names(ADAE)[startsWith(names(ADAE), "TMP_")]

  ADAETTE <- tmc_ex_adaette %>%
    filter(PARAMCD %in% c("AETTE1", "AETTE2", "AETTE3")) %>%
    mutate(is_event = CNSR == 0) %>%
    mutate(n_events = as.integer(is_event))

  ADTTE <- tmc_ex_adtte

  ADEX <- tmc_ex_adex
  set.seed(1, kind = "Mersenne-Twister")
  .labels <- col_labels(ADEX, fill = FALSE)
  ADEX <- ADEX %>%
    distinct(USUBJID, .keep_all = TRUE) %>%
    mutate(
      PARAMCD = "TDURD",
      PARAM = "Overall duration (days)",
      AVAL = sample(x = seq(1, 200), size = n(), replace = TRUE),
      AVALU = "Days"
    ) %>%
    bind_rows(ADEX)
  col_labels(ADEX) <- .labels

  ADRS <- tmc_ex_adrs %>%
    mutate(
      AVALC = d_onco_rsp_label(AVALC) %>%
        with_label("Character Result/Finding")
    ) %>%
    filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
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

arm_ref_binary <- list(
  ARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
  ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
)

app <- init(
  data = data,
  modules = modules(
    tm_t_abnormality(
      label = "Laboratory Abnormality (picks)",
      dataname = "ADLB",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      add_total = FALSE,
      by_vars = variables(
        choices = any_of(c("LBCAT", "PARAM", "AVISIT")),
        selected = c("LBCAT", "PARAM"),
        multiple = TRUE,
        ordered = TRUE
      ),
      baseline_var = variables(choices = any_of(c("BNRIND"))),
      grade = variables(choices = any_of(c("ANRIND"))),
      abnormal = list(low = "LOW", high = "HIGH"),
      exclude_base_abn = FALSE
    ),
    tm_t_abnormality_by_worst_grade(
      label = "Abnormality by Worst Grade (picks)",
      dataname = "ADLB",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      paramcd = variables(choices = any_of(c("PARAMCD"))),
      add_total = FALSE
    ),
    tm_t_events_patyear(
      label = "Events per Patient-Year (picks)",
      dataname = "ADAETTE",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      add_total = TRUE,
      events_var = variables(choices = any_of(c("n_events"))),
      paramcd = variables(choices = any_of(c("PARAMCD"))),
      aval_var = variables(choices = any_of(c("AVAL"))),
      avalu_var = variables(choices = any_of(c("AVALU")))
    ),
    tm_t_events_summary(
      label = "Events Summary (picks)",
      dataname = "ADAE",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      flag_var_anl = variables(
        choices = data[[".ae_anl_vars"]],
        selected = data[[".ae_anl_vars"]][1],
        multiple = TRUE,
        ordered = TRUE,
        fixed = FALSE
      ),
      flag_var_aesi = variables(
        choices = data[[".aesi_vars"]],
        selected = data[[".aesi_vars"]][1],
        multiple = TRUE,
        ordered = TRUE,
        fixed = FALSE
      ),
      add_total = TRUE
    ),
    tm_t_exposure(
      label = "Exposure (picks)",
      dataname = "ADEX",
      paramcd = variables(choices = any_of(c("PARAMCD"))),
      col_by_var = variables(
        choices = any_of(c("SEX", "ARM", "ARMCD")),
        selected = "SEX"
      ),
      row_by_var = variables(
        choices = any_of(c("RACE", "REGION1", "STRATA1", "SEX")),
        selected = "RACE"
      ),
      parcat = variables(choices = any_of(c("PARCAT1", "PARCAT2"))),
      add_total = FALSE
    ),
    tm_t_glm_counts(
      label = "GLM Counts (picks)",
      dataname = "ADTTE",
      arm_var = variables(
        choices = any_of(c("ARM", "ARMCD", "ACTARMCD")),
        selected = "ARMCD"
      ),
      arm_ref_comp = arm_ref_comp,
      aval_var = variables(choices = any_of(c("AVAL"))),
      strata_var = variables(choices = any_of(c("SEX", "BMRKR2")), selected = NULL),
      offset_var = variables(choices = any_of(c("AGE")), selected = NULL),
      cov_var = variables(choices = any_of(c("SITEID")), selected = NULL)
    ),
    tm_t_events_by_grade(
      label = "Events by Grade (picks)",
      dataname = "ADAE",
      arm_var = variables(choices = any_of(c("ARM", "ARMCD"))),
      llt = variables(
        choices = any_of(c("AETERM", "AEDECOD")),
        selected = "AEDECOD"
      ),
      hlt = variables(
        choices = any_of(c("AEBODSYS", "AESOC")),
        selected = "AEBODSYS"
      ),
      grade = variables(
        choices = any_of(c("AETOXGR", "AESEV")),
        selected = "AETOXGR"
      )
    ),
    tm_t_coxreg(
      label = "Cox Regression (picks)",
      dataname = "ADTTE",
      arm_var = variables(
        choices = any_of(c("ARM", "ARMCD", "ACTARMCD")),
        selected = "ARM"
      ),
      arm_ref_comp = arm_ref_comp,
      paramcd = variables(choices = any_of(c("PARAMCD"))),
      strata_var = variables(
        choices = any_of(c("COUNTRY", "STRATA1", "STRATA2")),
        selected = "STRATA1"
      ),
      cov_var = variables(
        choices = any_of(c("AGE", "BMRKR1", "BMRKR2", "REGION1")),
        selected = "AGE",
        multiple = TRUE,
        ordered = TRUE
      ),
      multivariate = TRUE
    ),
    tm_t_binary_outcome(
      label = "Binary Outcome (picks)",
      dataname = "ADRS",
      paramcd = variables(choices = any_of(c("PARAMCD"))),
      arm_var = variables(
        choices = any_of(c("ARM", "ARMCD", "ACTARMCD")),
        selected = "ARM"
      ),
      arm_ref_comp = arm_ref_binary,
      strata_var = variables(
        choices = any_of(c("SEX", "BMRKR2", "RACE")),
        selected = "RACE"
      ),
      default_responses = list(
        BESRSPI = list(
          rsp = c("Complete Response (CR)", "Partial Response (PR)"),
          levels = c(
            "Complete Response (CR)", "Partial Response (PR)",
            "Stable Disease (SD)", "Progressive Disease (PD)"
          )
        ),
        INVET = list(
          rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
          levels = c(
            "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
            "Progressive Disease (PD)", "Stable Disease (SD)"
          )
        ),
        OVRINV = list(
          rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
          levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
        )
      ),
      denom = "N_col"
    )
  ),
  filter = teal_slices(
    teal_slice("ADSL", "SAFFL", selected = "Y"),
    teal_slice("ADLB", "ONTRTFL", selected = "Y")
  )
)

shinyApp(app$ui, app$server)
