library(teal.modules.general)
library(teal.modules.clinical)
options(shiny.useragg = FALSE)

## Data reproducible code ----
data <- teal_data()
data <- within(data, {
  library(random.cdisc.data)
  library(dplyr)
  library(nestcolor)
  # optional libraries
  library(sparkline)

  # Generate CDISC ADaM datasets
  ADSL <- cadsl
  ADAE <- cadae
  ADAETTE <- cadaette
  ADAETTE <- ADAETTE %>%
    mutate(is_event = case_when(
      grepl("TOT", .data$PARAMCD, fixed = TRUE) ~ TRUE,
      TRUE ~ CNSR == 0
    )) %>%
    mutate(n_events = case_when(
      grepl("TOT", .data$PARAMCD, fixed = TRUE) ~ as.integer(.data$AVAL),
      TRUE ~ as.integer(is_event)
    )) %>%
    teal.data::col_relabel(is_event = "Is an Event") %>%
    teal.data::col_relabel(n_events = "Number of Events")
  .ADAETTE_AE <- filter(ADAETTE, grepl("TOT", .data$PARAMCD, fixed = TRUE)) %>% select(-"AVAL")
  .ADAETTE_OTH <- filter(ADAETTE, !(grepl("TOT", .data$PARAMCD, fixed = TRUE)))

  .ADAETTE_TTE <- ADAETTE %>%
    filter(PARAMCD == "AEREPTTE") %>%
    select(USUBJID, ARM, ARMCD, AVAL)

  .ADAETTE_AE <- full_join(.ADAETTE_AE, .ADAETTE_TTE, by = c("USUBJID", "ARM", "ARMCD"))
  ADAETTE <- rbind(.ADAETTE_AE, .ADAETTE_OTH)

  ADEX <- cadex
  .ADEX_labels <- teal.data::col_labels(ADEX, fill = FALSE)
  # Below steps are done to simulate data with TDURD parameter as it is not in the ADEX data from `random.cdisc.data` package
  set.seed(1, kind = "Mersenne-Twister")
  ADEX <- ADEX %>%
    distinct(USUBJID, .keep_all = TRUE) %>%
    mutate(
      PARAMCD = "TDURD",
      PARAM = "Overall duration (days)",
      AVAL = sample(
        x = seq(1, 200),
        size = n(),
        replace = TRUE
      ),
      AVALU = "Days",
      PARCAT1 = "OVERALL"
    ) %>%
    bind_rows(ADEX)
  ADEX <- ADEX %>%
    filter(PARCAT1 == "OVERALL" &
      PARAMCD %in% c("TDOSE", "TNDOSE", "TDURD"))
  teal.data::col_labels(ADEX) <- .ADEX_labels

  ADLB <- cadlb
  ADEG <- cadeg

  # For real data, ADVS needs some preprocessing like group different ANRIND and BNRIND into abnormal
  ADVS <- cadvs %>%
    mutate(ONTRTFL = ifelse(AVISIT %in% c("SCREENING", "BASELINE"), "", "Y")) %>%
    teal.data::col_relabel(ONTRTFL = "On Treatment Record Flag") %>%
    mutate(ANRIND = as.character(ANRIND), BNRIND = as.character(BNRIND)) %>%
    mutate(
      ANRIND = case_when(
        ANRIND == "HIGH HIGH" ~ "HIGH",
        ANRIND == "LOW LOW" ~ "LOW",
        TRUE ~ ANRIND
      ),
      BNRIND = case_when(
        BNRIND == "HIGH HIGH" ~ "HIGH",
        BNRIND == "LOW LOW" ~ "LOW",
        TRUE ~ BNRIND
      )
    )

  ADCM <- cadcm %>% mutate(CMSEQ = as.integer(CMSEQ))

  # Create additional datasets for patient profiles and statistical analysis
  ADMH <- cadmh  # Medical history
  #ADPM <- cadpm  # Prior medication
  
  # Create response dataset for statistical analysis
  ADRS <- cadrs  # Response dataset
  
  # Create efficacy dataset
  #ADEF <- cadef  # Efficacy dataset

  # define study-specific analysis subgroups and baskets from ADAE
  .add_event_flags <- function(dat) {
    dat %>%
      dplyr::mutate(
        TMPFL_SER = AESER == "Y",
        TMPFL_REL = AEREL == "Y",
        TMPFL_GR5 = AETOXGR == "5",
        TMP_SMQ01 = !is.na(SMQ01NAM),
        TMP_SMQ02 = !is.na(SMQ02NAM),
        TMP_CQ01 = !is.na(CQ01NAM)
      ) %>%
      teal.data::col_relabel(
        TMPFL_SER = "Serious AE",
        TMPFL_REL = "Related AE",
        TMPFL_GR5 = "Grade 5 AE",
        TMP_SMQ01 = aesi_label(dat$SMQ01NAM, dat$SMQ01SC),
        TMP_SMQ02 = aesi_label(dat$SMQ02NAM, dat$SMQ02SC),
        TMP_CQ01 = aesi_label(dat$CQ01NAM)
      )
  }

  ADAE <- ADAE %>%
    .add_event_flags()
})

join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADAE", "ADAETTE", "ADEX", "ADLB", "ADEG", "ADVS", "ADCM", "ADMH",# "ADPM",
 "ADRS")]# "ADEF")]

## App configuration ----
ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]
ADAETTE <- data[["ADAETTE"]]
ADEX <- data[["ADEX"]]
ADLB <- data[["ADLB"]]
ADEG <- data[["ADEG"]]
ADVS <- data[["ADVS"]]
ADCM <- data[["ADCM"]]
ADMH <- data[["ADMH"]]
#ADPM <- data[["ADPM"]]
ADRS <- data[["ADRS"]]
#ADEF <- data[["ADEF"]]

arm_vars <- c("ACTARMCD", "ACTARM")

## Create variable type lists
date_vars_adsl <-
  names(ADSL)[vapply(ADSL, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt"))
  }, logical(1))]
demog_vars_adsl <-
  names(ADSL)[!(names(ADSL) %in% c("USUBJID", "STUDYID", date_vars_adsl))]

cs_arm_var <-
  choices_selected(
    choices = variable_choices(ADSL, subset = arm_vars),
    selected = "ACTARM"
  )

ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
# flag variables for AE baskets; set to NULL if not applicable to study
aesi_vars <-
  names(ADAE)[startsWith(names(ADAE), "TMP_SMQ") |
    startsWith(names(ADAE), "TMP_CQ")]

## App header and footer ----
nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
app_source <- "https://github.com/insightsengineering/teal.gallery/tree/main/safety"
gh_issues_page <- "https://github.com/insightsengineering/teal.gallery/issues"

header <- tags$span(
  style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
  tags$span("Complete Teal Clinical Modules Demo", style = "font-size: 30px;"),
  tags$span(
    style = "display: flex; align-items: center;",
    tags$img(src = nest_logo, alt = "NEST logo", height = "45px", style = "margin-right:10px;"),
    tags$span(style = "font-size: 24px;", "NEST @ Roche")
  )
)

footer <- tags$p(
  "This teal app demonstrates ALL 37 available modules from teal.modules.clinical package.
        For more information, please visit:",
  tags$a(href = app_source, target = "_blank", "Source Code"), ", ",
  tags$a(href = gh_issues_page, target = "_blank", "Report Issues")
)

## Setup App ----
app <- teal::init(
  data = data,
  # Set initial filter state as safety-evaluable population
  filter = teal_slices(
    count_type = "all",
    teal_slice(dataname = "ADSL", varname = "SAFFL", selected = "Y"),
    teal_slice(dataname = "ADSL", varname = "SEX"),
    teal_slice(dataname = "ADSL", varname = "AGE"),
    teal_slice(dataname = "ADLB", varname = "AVAL"),
    teal_slice(dataname = "ADEX", varname = "AVAL"),
    teal_slice(dataname = "ADEG", varname = "AVAL")
  ),
  modules = modules(
    # Front page with app information
    tm_front_page(
      label = "App Info",
      header_text = c(
        "About this Demo" = "This app demonstrates ALL 37 available modules from the teal.modules.clinical package.",
        "Data Source" = "This app uses CDISC ADaM datasets randomly generated by `random.cdisc.data` R package",
        "Available Modules" = "The app includes modules for demographics, adverse events, laboratory data, exposure, vital signs, concomitant medications, ECG data, patient profiles, statistical analysis, and graphics."
      ),
      tables = list(`NEST packages used in this demo app` = data.frame(
        Packages = c(
          "teal.modules.general",
          "teal.modules.clinical",
          "random.cdisc.data"
        ),
        Description = c(
          "General purpose modules for data exploration",
          "Clinical trial specific modules (37 total)",
          "Random CDISC data generation"
        )
      ))
    ),
    
    # Data exploration modules
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    
    # Demographics module
    tm_t_summary(
      label = "Demographic Summary",
      dataname = "ADSL",
      arm_var = cs_arm_var,
      summarize_vars = choices_selected(
        choices = variable_choices(ADSL, demog_vars_adsl),
        selected = c("SEX", "AGE", "RACE", "ETHNIC")
      )
    ),
    
    # Adverse Events section
    modules(
      label = "Adverse Events",
      tm_t_events_summary(
        label = "AE Summary",
        dataname = "ADAE",
        arm_var = cs_arm_var,
        flag_var_anl = choices_selected(
          choices = variable_choices("ADAE", ae_anl_vars),
          selected = ae_anl_vars,
          keep_order = TRUE
        ),
        flag_var_aesi = choices_selected(
          choices = variable_choices("ADAE", aesi_vars),
          selected = aesi_vars,
          keep_order = TRUE
        ),
        add_total = TRUE
      ),
      tm_t_events(
        label = "AE by Term",
        dataname = "ADAE",
        arm_var = cs_arm_var,
        llt = choices_selected(
          choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
          selected = c("AEDECOD")
        ),
        hlt = choices_selected(
          choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
          selected = "AEBODSYS"
        ),
        add_total = TRUE,
        event_type = "adverse event"
      ),
      tm_t_events_by_grade(
        label = "AE Table by Grade",
        dataname = "ADAE",
        arm_var = cs_arm_var,
        llt = choices_selected(
          choices = variable_choices(ADAE, c("AEDECOD")),
          selected = c("AEDECOD")
        ),
        hlt = choices_selected(
          choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
          selected = "AEBODSYS"
        ),
        grade = choices_selected(
          choices = variable_choices(ADAE, c("AETOXGR")),
          selected = "AETOXGR"
        ),
        add_total = TRUE
      ),
      tm_t_events_patyear(
        label = "AE Rates Adjusted for Patient-Years at Risk",
        dataname = "ADAETTE",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
          selected = "AETTE1"
        ),
        events_var = choices_selected(
          choices = variable_choices(ADAETTE, "n_events"),
          selected = "n_events",
          fixed = TRUE
        )
      ),
      tm_t_smq(
        label = "Adverse Events by SMQ Table",
        dataname = "ADAE",
        arm_var = choices_selected(
          choices = variable_choices(ADSL, subset = c(arm_vars, "SEX")),
          selected = "ACTARM"
        ),
        add_total = FALSE,
        baskets = choices_selected(
          choices = variable_choices(ADAE, subset = grep("^(SMQ|CQ).*NAM$", names(ADAE), value = TRUE)),
          selected = grep("^(SMQ|CQ).*NAM$", names(ADAE), value = TRUE)
        ),
        scopes = choices_selected(
          choices = variable_choices(ADAE, subset = grep("^SMQ.*SC$", names(ADAE), value = TRUE)),
          selected = grep("^SMQ.*SC$", names(ADAE), value = TRUE),
          fixed = TRUE
        ),
        llt = choices_selected(
          choices = variable_choices(ADAE, subset = c("AEDECOD")),
          selected = "AEDECOD"
        )
      )
    ),
    
    # Laboratory Data section
    modules(
      label = "Laboratory Data",
      tm_t_summary_by(
        label = "Labs Summary",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        by_vars = choices_selected(
          choices = variable_choices(ADLB, c("PARAM", "AVISIT")),
          selected = c("PARAM", "AVISIT"),
          fixed = TRUE
        ),
        summarize_vars = choices_selected(
          choices = variable_choices(ADLB, c("AVAL", "CHG")),
          selected = c("AVAL")
        ),
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        )
      ),
      tm_t_shift_by_grade(
        label = "Grade Laboratory Abnormality Table",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        ),
        worst_flag_var = choices_selected(
          choices = variable_choices(ADLB, subset = c(
            "WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"
          )),
          selected = c("WGRLOVFL")
        ),
        worst_flag_indicator = choices_selected(
          value_choices(ADLB, "WGRLOVFL"),
          selected = "Y",
          fixed = TRUE
        ),
        anl_toxgrade_var = choices_selected(
          choices = variable_choices(ADLB, subset = c("ATOXGR")),
          selected = c("ATOXGR"),
          fixed = TRUE
        ),
        base_toxgrade_var = choices_selected(
          choices = variable_choices(ADLB, subset = c("BTOXGR")),
          selected = c("BTOXGR"),
          fixed = TRUE
        ),
        add_total = FALSE
      ),
      tm_t_abnormality_by_worst_grade(
        label = "Laboratory test results with highest grade post-baseline",
        dataname = "ADLB",
        arm_var = choices_selected(
          choices = variable_choices(ADSL, subset = c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = c("ALT", "CRP", "IGA")
        ),
        add_total = FALSE
      )
    ),
    
    # Exposure section
    modules(
      label = "Exposure",
      tm_t_summary_by(
        label = "Exposure Summary",
        dataname = "ADEX",
        arm_var = cs_arm_var,
        by_vars = choices_selected(
          choices = variable_choices(ADEX, c("PARCAT2", "PARAM")),
          selected = c("PARCAT2", "PARAM"),
          fixed = TRUE
        ),
        summarize_vars = choices_selected(
          choices = variable_choices(ADEX, "AVAL"),
          selected = c("AVAL"),
          fixed = TRUE
        ),
        paramcd = choices_selected(
          choices = value_choices(ADEX, "PARAMCD", "PARAM"),
          selected = "TDOSE"
        ),
        denominator = choices_selected(
          choices = c("n", "N", "omit"),
          selected = "n"
        )
      ),
      tm_t_exposure(
        label = "Duration of Exposure Table",
        dataname = "ADEX",
        paramcd = choices_selected(
          choices = value_choices(ADEX, "PARAMCD", "PARAM"),
          selected = "TDURD",
          fixed = TRUE
        ),
        col_by_var = choices_selected(
          choices = variable_choices(ADEX, subset = c(arm_vars, "SEX")),
          selected = "SEX"
        ),
        row_by_var = choices_selected(
          choices = variable_choices(
            ADEX,
            subset = c("RACE", "REGION1", "STRATA1", "SEX")
          ),
          selected = "RACE"
        ),
        parcat = choices_selected(
          choices = value_choices(ADEX, "PARCAT2"),
          selected = "Drug A"
        ),
        add_total = FALSE
      )
    ),
    
    # Vital Signs section
    tm_t_abnormality(
      label = "Vital Signs Abnormality",
      dataname = "ADVS",
      arm_var = cs_arm_var,
      id_var = choices_selected(
        choices = variable_choices(ADSL, subset = "USUBJID"),
        selected = "USUBJID",
        fixed = TRUE
      ),
      by_vars = choices_selected(
        choices = variable_choices(ADVS, subset = c("PARAM", "AVISIT")),
        selected = c("PARAM"),
        keep_order = TRUE
      ),
      grade = choices_selected(
        choices = variable_choices(ADVS, subset = "ANRIND"),
        selected = "ANRIND",
        fixed = TRUE
      ),
      abnormal = list(low = "LOW", high = "HIGH")
    ),
    
    # Concomitant Medications section
    tm_t_mult_events(
      label = "Concomitant Medication",
      dataname = "ADCM",
      arm_var = cs_arm_var,
      seq_var = choices_selected("CMSEQ", selected = "CMSEQ", fixed = TRUE),
      hlt = choices_selected(
        choices = variable_choices(ADCM, c(
          "ATC1", "ATC2", "ATC3", "ATC4"
        )),
        selected = "ATC2"
      ),
      llt = choices_selected(
        choices = variable_choices(ADCM, c("CMDECOD")),
        selected = "CMDECOD",
        fixed = TRUE
      ),
      add_total = TRUE,
      event_type = "treatment"
    ),
    
    # ECG section
    modules(
      label = "ECG Data",
      tm_t_shift_by_arm(
        label = "ECG Shift Table by Arm",
        dataname = "ADEG",
        arm_var = cs_arm_var,
        paramcd = choices_selected(value_choices(ADEG, "PARAMCD"),
          selected = "HR"
        ),
        visit_var = choices_selected(value_choices(ADEG, "AVISIT"),
          selected = "POST-BASELINE MINIMUM"
        ),
        aval_var = choices_selected(
          variable_choices(ADEG, subset = "ANRIND"),
          selected = "ANRIND",
          fixed = TRUE
        ),
        baseline_var = choices_selected(
          variable_choices(ADEG, subset = "BNRIND"),
          selected = "BNRIND",
          fixed = TRUE
        )
      ),
      tm_t_shift_by_arm_by_worst(
        label = "ECG Shift by Arm by Worst Grade",
        dataname = "ADEG",
        arm_var = cs_arm_var,
        paramcd = choices_selected(value_choices(ADEG, "PARAMCD"),
          selected = "HR"
        ),
        visit_var = choices_selected(value_choices(ADEG, "AVISIT"),
          selected = "POST-BASELINE MINIMUM"
        ),
        aval_var = choices_selected(
          variable_choices(ADEG, subset = "ANRIND"),
          selected = "ANRIND",
          fixed = TRUE
        ),
        baseline_var = choices_selected(
          variable_choices(ADEG, subset = "BNRIND"),
          selected = "BNRIND",
          fixed = TRUE
        )
      )
    ),
    
    # Patient Profiles section
    modules(
      label = "Patient Profiles",
      tm_t_pp_basic_info(
        label = "Patient Profile - Basic Info",
        dataname = "ADSL",
        patient_id = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        arm_var = cs_arm_var
      ),
      tm_t_pp_laboratory(
        label = "Patient Profile - Laboratory",
        dataname = "ADLB",
        patient_id = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        )
      ),
      tm_t_pp_medical_history(
        label = "Patient Profile - Medical History",
        dataname = "ADMH",
        patient_id = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        arm_var = cs_arm_var
      )#,
    #  tm_t_pp_prior_medication(
    #    label = "Patient Profile - Prior Medication",
    #    dataname = "ADPM",
    #    patient_id = choices_selected(
    #      choices = variable_choices(ADSL, subset = "USUBJID"),
    #      selected = "USUBJID",
    #      fixed = TRUE
    #    ),
    #    arm_var = cs_arm_var
    #  )
    ),
    
    # Statistical Analysis section
    modules(
      label = "Statistical Analysis",
      tm_t_ancova(
        label = "Analysis of Covariance (ANCOVA)",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        aval_var = choices_selected(
          choices = variable_choices(ADLB, subset = "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        cov_var = choices_selected(
          choices = variable_choices(ADSL, subset = c("AGE", "SEX")),
          selected = "AGE"
        ),
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        )
      ),
      tm_t_binary_outcome(
        label = "Binary Outcome Analysis",
        dataname = "ADRS",
        arm_var = cs_arm_var,
        response_var = choices_selected(
          choices = variable_choices(ADRS, subset = "AVALC"),
          selected = "AVALC",
          fixed = TRUE
        ),
        paramcd = choices_selected(
          choices = value_choices(ADRS, "PARAMCD", "PARAM"),
          selected = "BESRSPI"
        )
      ),
      tm_t_coxreg(
        label = "Cox Regression Analysis",
        dataname = "ADAETTE",
        arm_var = cs_arm_var,
        aval_var = choices_selected(
          choices = variable_choices(ADAETTE, subset = "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        cnsr_var = choices_selected(
          choices = variable_choices(ADAETTE, subset = "CNSR"),
          selected = "CNSR",
          fixed = TRUE
        ),
        paramcd = choices_selected(
          choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
          selected = "AETTE1"
        )
      ),
      tm_t_logistic(
        label = "Logistic Regression Analysis",
        dataname = "ADRS",
        arm_var = cs_arm_var,
        response_var = choices_selected(
          choices = variable_choices(ADRS, subset = "AVALC"),
          selected = "AVALC",
          fixed = TRUE
        ),
        cov_var = choices_selected(
          choices = variable_choices(ADSL, subset = c("AGE", "SEX")),
          selected = "AGE"
        ),
        paramcd = choices_selected(
          choices = value_choices(ADRS, "PARAMCD", "PARAM"),
          selected = "BESRSPI"
        )
      ),
      tm_t_glm_counts(
        label = "Generalized Linear Models for Counts",
        dataname = "ADAE",
        arm_var = cs_arm_var,
        response_var = choices_selected(
          choices = variable_choices(ADAE, subset = "AETOXGR"),
          selected = "AETOXGR",
          fixed = TRUE
        ),
        paramcd = choices_selected(
          choices = value_choices(ADAE, "AEDECOD"),
          selected = "HEADACHE"
        )
      ),
      tm_t_tte(
        label = "Time-to-Event Analysis",
        dataname = "ADAETTE",
        arm_var = cs_arm_var,
        aval_var = choices_selected(
          choices = variable_choices(ADAETTE, subset = "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        cnsr_var = choices_selected(
          choices = variable_choices(ADAETTE, subset = "CNSR"),
          selected = "CNSR",
          fixed = TRUE
        ),
        paramcd = choices_selected(
          choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
          selected = "AETTE1"
        )
      ),
      tm_a_gee(
        label = "Generalized Estimating Equations (GEE)",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        aval_var = choices_selected(
          choices = variable_choices(ADLB, subset = "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        id_var = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        visit_var = choices_selected(
          choices = variable_choices(ADLB, subset = "AVISIT"),
          selected = "AVISIT",
          fixed = TRUE
        ),
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        )
      ),
      tm_a_mmrm(
        label = "Mixed Models for Repeated Measures (MMRM)",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        aval_var = choices_selected(
          choices = variable_choices(ADLB, subset = "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        id_var = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        visit_var = choices_selected(
          choices = variable_choices(ADLB, subset = "AVISIT"),
          selected = "AVISIT",
          fixed = TRUE
        ),
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        )
      )
    ),
    
    # Graphics section
    modules(
      label = "Graphics",
      tm_g_lineplot(
        label = "Laboratory Values Over Time",
        dataname = "ADLB",
        strata = cs_arm_var,
        x = choices_selected(variable_choices(ADLB, "AVISIT"), "AVISIT", fixed = TRUE),
        y = choices_selected(variable_choices(ADLB, c(
          "AVAL", "BASE", "CHG", "PCHG"
        )), "AVAL"),
        y_unit = choices_selected(variable_choices(ADLB, "AVALU"), "AVALU", fixed = TRUE),
        paramcd = choices_selected(variable_choices(ADLB, "PARAMCD"), "PARAMCD", fixed = TRUE),
        param = choices_selected(value_choices(ADLB, "PARAMCD", "PARAM"), "ALT"),
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_km(
        label = "Kaplan-Meier Plot",
        dataname = "ADAETTE",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
          selected = "AETTE1"
        ),
        cnsr_var = choices_selected(
          choices = variable_choices(ADAETTE, "CNSR"),
          selected = "CNSR",
          fixed = TRUE
        ),
        aval_var = choices_selected(
          choices = variable_choices(ADAETTE, "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_barchart_simple(
        label = "Simple Bar Chart",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        ),
        aval_var = choices_selected(
          choices = variable_choices(ADLB, subset = "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_ci(
        label = "Confidence Interval Plot",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        ),
        aval_var = choices_selected(
          choices = variable_choices(ADLB, subset = "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_forest_rsp(
        label = "Forest Plot for Response",
        dataname = "ADRS",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADRS, "PARAMCD", "PARAM"),
          selected = "BESRSPI"
        ),
        response_var = choices_selected(
          choices = variable_choices(ADRS, subset = "AVALC"),
          selected = "AVALC",
          fixed = TRUE
        ),
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_forest_tte(
        label = "Forest Plot for Time-to-Event",
        dataname = "ADAETTE",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
          selected = "AETTE1"
        ),
        aval_var = choices_selected(
          choices = variable_choices(ADAETTE, subset = "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        cnsr_var = choices_selected(
          choices = variable_choices(ADAETTE, subset = "CNSR"),
          selected = "CNSR",
          fixed = TRUE
        ),
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_ipp(
        label = "Individual Patient Plots",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        ),
        aval_var = choices_selected(
          choices = variable_choices(ADLB, subset = "AVAL"),
          selected = "AVAL",
          fixed = TRUE
        ),
        id_var = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_pp_adverse_events(
        label = "Patient Profile - Adverse Events Plots",
        dataname = "ADAE",
        patient_id = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        arm_var = cs_arm_var,
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_pp_patient_timeline(
        label = "Patient Profile - Timeline Plots",
        dataname = "ADSL",
        patient_id = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        arm_var = cs_arm_var,
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_pp_therapy(
        label = "Patient Profile - Therapy Plots",
        dataname = "ADEX",
        patient_id = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        arm_var = cs_arm_var,
        plot_height = c(1000L, 200L, 4000L)
      ),
      tm_g_pp_vitals(
        label = "Patient Profile - Vital Signs Plots",
        dataname = "ADVS",
        patient_id = choices_selected(
          choices = variable_choices(ADSL, subset = "USUBJID"),
          selected = "USUBJID",
          fixed = TRUE
        ),
        arm_var = cs_arm_var,
        plot_height = c(1000L, 200L, 4000L)
      )
    )
  )
) |>
  modify_title(
    title = "Complete Teal Clinical Modules Demo App",
    favicon = nest_logo
  ) |>
  modify_header(header) |>
  modify_footer(footer)

shinyApp(app$ui, app$server) 