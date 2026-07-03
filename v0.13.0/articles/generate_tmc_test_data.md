# Example Data Generation

## Generating minimal data to test `teal.modules.clinical`

The following script is used to create and save cached synthetic `CDISC`
data to the `data/` directory to use in examples and tests in the
`teal.modules.clinical` package. This script/vignette was initialized by
Emily de la Rua in `tern`.

*Disclaimer*: this vignette concerns mainly the development of minimal
and stable test data and it is kept internal for feature tracking.

## Setup & Helper Functions

``` r

library(dplyr)
library(teal.data)

study_duration_secs <- lubridate::seconds(lubridate::years(2))

sample_fct <- function(x, N, ...) { 
  checkmate::assert_number(N)
  factor(sample(x, N, replace = TRUE, ...), levels = if (is.factor(x)) levels(x) else x)
}

retain <- function(df, value_var, event, outside = NA) {
  indices <- c(1, which(event == TRUE), nrow(df) + 1)
  values <- c(outside, value_var[event == TRUE])
  rep(values, diff(indices))
}

relvar_init <- function(relvar1, relvar2) {
  if (length(relvar1) != length(relvar2)) {
    message(simpleError(
      "The argument value length of relvar1 and relvar2 differ. They must contain the same number of elements."
    ))
    return(NA)
  }
  List("relvar1" = relvar1, "relvar2" = relvar2)
}

rel_var <- function(df = NULL, var_name = NULL, var_values = NULL, related_var = NULL) {
  if (is.null(df)) {
    message("Missing data frame argument value.")
    NA
  } else {
    n_relvar1 <- length(unique(df[, related_var, drop = TRUE]))
    n_relvar2 <- length(var_values)
    if (n_relvar1 != n_relvar2) {
      message(paste("Unequal vector lengths for", related_var, "and", var_name))
      NA
    } else {
      relvar1 <- unique(df[, related_var, drop = TRUE])
      relvar2_values <- rep(NA, nrow(df))
      for (r in seq_along(relvar1)) {
        matched <- which(df[, related_var, drop = TRUE] == relvar1[r])
        relvar2_values[matched] <- var_values[r]
      }
      relvar2_values
    }
  }
}

visit_schedule <- function(visit_format = "WEEK",
                           n_assessments = 10L,
                           n_days = 5L) {
  if (!(toupper(visit_format) %in% c("WEEK", "CYCLE"))) {
    message("Visit format value must either be: WEEK or CYCLE")
    return(NA)
  }
  if (toupper(visit_format) == "WEEK") {
    assessments <- 1:n_assessments
    assessments_ord <- -1:n_assessments
    visit_values <- c("SCREENING", "BASELINE", paste(toupper(visit_format), assessments, "DAY", (assessments * 7) + 1))
  } else if (toupper(visit_format) == "CYCLE") {
    cycles <- sort(rep(1:n_assessments, times = 1, each = n_days))
    days <- rep(seq(1:n_days), times = n_assessments, each = 1)
    assessments_ord <- 0:(n_assessments * n_days)
    visit_values <- c("SCREENING", paste(toupper(visit_format), cycles, "DAY", days))
  }
  visit_values <- stats::reorder(factor(visit_values), assessments_ord)
}

rtpois <- function(n, lambda) stats::qpois(stats::runif(n, stats::dpois(0, lambda), 1), lambda)

rtexp <- function(n, rate, l = NULL, r = NULL) {
  if (!is.null(l)) {
    l - log(1 - stats::runif(n)) / rate
  } else if (!is.null(r)) {
    -log(1 - stats::runif(n) * (1 - exp(-r * rate))) / rate
  } else {
    stats::rexp(n, rate)
  }
}

str_extract <- function(string, pattern) regmatches(string, gregexpr(pattern, string))

with_label <- function(x, label) {
  attr(x, "label") <- as.vector(label)
  x
}

common_var_labels <- c(
  USUBJID = "Unique Subject Identifier",
  STUDYID = "Study Identifier",
  PARAM = "Parameter",
  PARAMCD = "Parameter Code",
  AVISIT = "Analysis Visit",
  AVISITN = "Analysis Visit (N)",
  AVAL = "Analysis Value",
  AVALU = "Analysis Value Unit",
  AVALC = "Character Result/Finding",
  BASE = "Baseline Value",
  BASE2 = "Screening Value",
  ABLFL = "Baseline Record Flag",
  ABLFL2 = "Screening Record Flag",
  CHG = "Absolute Change from Baseline",
  PCHG = "Percentage Change from Baseline",
  ANRIND = "Analysis Reference Range Indicator",
  BNRIND = "Baseline Reference Range Indicator",
  ANRLO = "Analysis Normal Range Lower Limit",
  ANRHI = "Analysis Normal Range Upper Limit",
  CNSR = "Censor",
  ADTM = "Analysis Datetime",
  ADY = "Analysis Relative Day",
  ASTDY = "Analysis Start Relative Day",
  AENDY = "Analysis End Relative Day",
  ASTDTM = "Analysis Start Datetime",
  AENDTM = "Analysis End Datetime",
  VISITDY = "Planned Study Day of Visit",
  EVNTDESC = "Event or Censoring Description",
  CNSDTDSC = "Censor Date Description",
  BASETYPE = "Baseline Type",
  DTYPE = "Derivation Type",
  ONTRTFL = "On Treatment Record Flag",
  WORS01FL = "Worst Observation in Window Flag 01",
  WORS02FL = "Worst Post-Baseline Observation"
)
```

## `ADSL`

``` r

generate_adsl <- function(N = 200) { 
  set.seed(1)
  sys_dtm <- lubridate::fast_strptime("20/2/2019 11:16:16.683", "%d/%m/%Y %H:%M:%OS", tz = "UTC")
  country_site_prob <- c(.5, .121, .077, .077, .075, .052, .046, .025, .014, .003)

  adsl <- tibble::tibble(
    STUDYID = rep("AB12345", N) %>% with_label("Study Identifier"),
    COUNTRY = sample_fct(
      c("CHN", "USA", "BRA", "PAK", "NGA", "RUS", "JPN", "GBR", "CAN", "CHE"),
      N,
      prob = country_site_prob
    ) %>% with_label("Country"),
    SITEID = sample_fct(1:20, N, prob = rep(country_site_prob, times = 2)),
    SUBJID = paste("id", seq_len(N), sep = "-") %>% with_label("Subject Identifier for the Study"),
    AGE = (sapply(stats::rchisq(N, df = 5, ncp = 10), max, 0) + 20) %>% with_label("Age"),
    SEX = c("F", "M") %>% sample_fct(N, prob = c(.52, .48)) %>% with_label("Sex"),
    ARMCD = c("ARM A", "ARM B", "ARM C") %>% sample_fct(N) %>% with_label("Planned Arm Code"),
    ARM = dplyr::recode(
      .data$ARMCD,
      "ARM A" = "A: Drug X", "ARM B" = "B: Placebo", "ARM C" = "C: Combination"
    ) %>% with_label("Description of Planned Arm"),
    ACTARMCD = .data$ARMCD %>% with_label("Actual Arm Code"),
    ACTARM = .data$ARM %>% with_label("Description of Actual Arm"),
    RACE = c(
      "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
      "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "OTHER", "UNKNOWN"
    ) %>%
      sample_fct(N, prob = c(.55, .23, .16, .05, .004, .003, .002, .002)) %>%
      with_label("Race"),
    TRTSDTM = sys_dtm + sample(seq(0, study_duration_secs), size = N, replace = TRUE) %>%
      with_label("Datetime of First Exposure to Treatment"),
    TRTEDTM = c(TRTSDTM + study_duration_secs) %>%
      with_label("Datetime of Last Exposure to Treatment"),
    EOSDY = ceiling(as.numeric(difftime(TRTEDTM, TRTSDTM, units = "days"))) %>%
      with_label("End of Study Relative Day"),
    EOSDT = lubridate::date(TRTEDTM) %>% with_label("End of Study Date"),
    STRATA1 = c("A", "B", "C") %>% sample_fct(N) %>% with_label("Stratification Factor 1"),
    STRATA2 = c("S1", "S2") %>% sample_fct(N) %>% with_label("Stratification Factor 2"),
    BMRKR1 = stats::rchisq(N, 6) %>% with_label("Continuous Level Biomarker 1"),
    BMRKR2 = sample_fct(c("LOW", "MEDIUM", "HIGH"), N) %>% with_label("Continuous Level Biomarker 2")
  )

  # associate sites with countries and regions
  adsl <- adsl %>%
    dplyr::mutate(
      SITEID = paste0(.data$COUNTRY, "-", .data$SITEID) %>% with_label("Study Site Identifier"),
      REGION1 = factor(dplyr::case_when(
        COUNTRY %in% c("NGA") ~ "Africa",
        COUNTRY %in% c("CHN", "JPN", "PAK") ~ "Asia",
        COUNTRY %in% c("RUS") ~ "Eurasia",
        COUNTRY %in% c("GBR") ~ "Europe",
        COUNTRY %in% c("CAN", "USA") ~ "North America",
        COUNTRY %in% c("BRA") ~ "South America",
        TRUE ~ as.character(NA)
      )) %>% with_label("Geographic Region 1"),
      SAFFL = factor("Y") %>% with_label("Safety Population Flag")
    ) %>%
    dplyr::mutate(
      USUBJID = paste(.data$STUDYID, .data$SITEID, .data$SUBJID, sep = "-") %>%
        with_label("Unique Subject Identifier")
    )

  # disposition related variables
  # using probability of 1 for the "DEATH" level to ensure at least one death record exists
  l_dcsreas <- list(
    choices = c(
      "ADVERSE EVENT", "DEATH", "LACK OF EFFICACY", "PHYSICIAN DECISION",
      "PROTOCOL VIOLATION", "WITHDRAWAL BY PARENT/GUARDIAN", "WITHDRAWAL BY SUBJECT"
    ),
    prob = c(.2, 1, .1, .1, .2, .1, .1)
  )
  l_dthcat_other <- list(
    choices = c(
      "Post-study reporting of death", "LOST TO FOLLOW UP", "MISSING", "SUICIDE", "UNKNOWN"
    ),
    prob = c(.1, .3, .3, .2, .1)
  )

  adsl <- adsl %>%
    dplyr::mutate(
      EOSSTT = dplyr::case_when(
        EOSDY == max(EOSDY, na.rm = TRUE) ~ "COMPLETED",
        EOSDY < max(EOSDY, na.rm = TRUE) ~ "DISCONTINUED",
        is.na(TRTEDTM) ~ "ONGOING"
      ) %>% with_label("End of Study Status")
    ) %>%
    dplyr::mutate(
      EOTSTT = .data$EOSSTT %>% with_label("End of Treatment Status")
    ) %>%
    dplyr::mutate(
      DCSREAS = ifelse(
        .data$EOSSTT == "DISCONTINUED",
        sample(x = l_dcsreas$choices, size = N, replace = TRUE, prob = l_dcsreas$prob),
        as.character(NA)
      ) %>% with_label("Reason for Discontinuation from Study")
    )

  tmc_ex_adsl <- adsl %>%
    dplyr::mutate(DTHDT = dplyr::case_when(
      DCSREAS == "DEATH" ~ lubridate::date(TRTEDTM + lubridate::days(sample(0:50, size = N, replace = TRUE)))
    ) %>% with_label("Date of Death"))

  save(tmc_ex_adsl, file = "data/tmc_ex_adsl.rda", compress = "xz")
}
```

## `ADAE`

``` r

generate_adae <- function(adsl = tmc_ex_adsl,
                          max_n_aes = 5) {
  set.seed(1)
  lookup_ae <- tibble::tribble(
    ~AEBODSYS, ~AELLT, ~AEDECOD, ~AEHLT, ~AEHLGT, ~AETOXGR, ~AESOC, ~AESER, ~AEREL,
    "cl A.1", "llt A.1.1.1.1", "dcd A.1.1.1.1", "hlt A.1.1.1", "hlgt A.1.1", "1", "cl A", "N", "N",
    "cl A.1", "llt A.1.1.1.2", "dcd A.1.1.1.2", "hlt A.1.1.1", "hlgt A.1.1", "2", "cl A", "Y", "N",
    "cl B.1", "llt B.1.1.1.1", "dcd B.1.1.1.1", "hlt B.1.1.1", "hlgt B.1.1", "5", "cl B", "Y", "Y",
    "cl B.2", "llt B.2.1.2.1", "dcd B.2.1.2.1", "hlt B.2.1.2", "hlgt B.2.1", "3", "cl B", "N", "N",
    "cl B.2", "llt B.2.2.3.1", "dcd B.2.2.3.1", "hlt B.2.2.3", "hlgt B.2.2", "1", "cl B", "Y", "N",
    "cl C.1", "llt C.1.1.1.3", "dcd C.1.1.1.3", "hlt C.1.1.1", "hlgt C.1.1", "4", "cl C", "N", "Y",
    "cl C.2", "llt C.2.1.2.1", "dcd C.2.1.2.1", "hlt C.2.1.2", "hlgt C.2.1", "2", "cl C", "N", "Y",
    "cl D.1", "llt D.1.1.1.1", "dcd D.1.1.1.1", "hlt D.1.1.1", "hlgt D.1.1", "5", "cl D", "Y", "Y",
    "cl D.1", "llt D.1.1.4.2", "dcd D.1.1.4.2", "hlt D.1.1.4", "hlgt D.1.1", "3", "cl D", "N", "N",
    "cl D.2", "llt D.2.1.5.3", "dcd D.2.1.5.3", "hlt D.2.1.5", "hlgt D.2.1", "1", "cl D", "N", "Y"
  )

  aag <- utils::read.table(
    sep = ",", header = TRUE,
    text = paste(
      "NAMVAR,SRCVAR,GRPTYPE,REFNAME,REFTERM,SCOPE",
      "CQ01NAM,AEDECOD,CUSTOM,D.2.1.5.3/A.1.1.1.1 aesi,dcd D.2.1.5.3,",
      "CQ01NAM,AEDECOD,CUSTOM,D.2.1.5.3/A.1.1.1.1 aesi,dcd A.1.1.1.1,",
      "SMQ01NAM,AEDECOD,SMQ,C.1.1.1.3/B.2.2.3.1 aesi,dcd C.1.1.1.3,BROAD",
      "SMQ01NAM,AEDECOD,SMQ,C.1.1.1.3/B.2.2.3.1 aesi,dcd B.2.2.3.1,BROAD",
      "SMQ02NAM,AEDECOD,SMQ,Y.9.9.9.9/Z.9.9.9.9 aesi,dcd Y.9.9.9.9,NARROW",
      "SMQ02NAM,AEDECOD,SMQ,Y.9.9.9.9/Z.9.9.9.9 aesi,dcd Z.9.9.9.9,NARROW",
      sep = "\n"
    ), stringsAsFactors = FALSE
  )

  adae <- Map(
    function(id, sid) {
      n_aes <- sample(c(0, seq_len(max_n_aes)), 1)
      i <- sample(seq_len(nrow(lookup_ae)), n_aes, TRUE)
      dplyr::mutate(
        lookup_ae[i, ],
        USUBJID = id,
        STUDYID = sid
      )
    },
    adsl$USUBJID,
    adsl$STUDYID
  ) %>%
    Reduce(rbind, .) %>%
    `[`(c(10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9)) %>%
    dplyr::mutate(
      AETERM = gsub("dcd", "trm", .data$AEDECOD) %>% with_label("Reported Term for the Adverse Event"),
      AESEV = dplyr::case_when(
        AETOXGR == 1 ~ "MILD",
        AETOXGR %in% c(2, 3) ~ "MODERATE",
        AETOXGR %in% c(4, 5) ~ "SEVERE"
      ) %>% with_label("Severity/Intensity")
    )

  # merge adsl to be able to add AE date and study day variables
  adae <- dplyr::inner_join(adae, adsl, by = c("STUDYID", "USUBJID"), multiple = "all") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TRTENDT = lubridate::date(dplyr::case_when(
      is.na(TRTEDTM) ~ lubridate::floor_date(lubridate::date(TRTSDTM) + study_duration_secs, unit = "day"),
      TRUE ~ TRTEDTM
    ))) %>%
    dplyr::mutate(ASTDTM = sample(
      seq(lubridate::as_datetime(TRTSDTM), lubridate::as_datetime(TRTENDT), by = "day"),
      size = 1
    )) %>%
    dplyr::mutate(ASTDY = ceiling(difftime(ASTDTM, TRTSDTM, units = "days"))) %>%
    # add 1 to end of range incase both values passed to sample() are the same
    dplyr::mutate(AENDTM = sample(
      seq(lubridate::as_datetime(ASTDTM), lubridate::as_datetime(TRTENDT + 1), by = "day"),
      size = 1
    )) %>%
    dplyr::mutate(AENDY = ceiling(difftime(AENDTM, TRTSDTM, units = "days"))) %>%
    dplyr::mutate(LDOSEDTM = dplyr::case_when(
      TRTSDTM < ASTDTM ~ lubridate::as_datetime(stats::runif(1, TRTSDTM, ASTDTM)),
      TRUE ~ ASTDTM
    )) %>%
    dplyr::select(-TRTENDT) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ASTDTM, .data$AETERM)

  adae <- adae %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(AESEQ = seq_len(dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$ASTDTM,
      .data$AETERM,
      .data$AESEQ
    )

  outcomes <- c(
    "UNKNOWN",
    "NOT RECOVERED/NOT RESOLVED",
    "RECOVERED/RESOLVED WITH SEQUELAE",
    "RECOVERING/RESOLVING",
    "RECOVERED/RESOLVED"
  )

  adae <- adae %>%
    dplyr::mutate(
      AEOUT = factor(ifelse(
        .data$AETOXGR == "5",
        "FATAL",
        as.character(sample_fct(outcomes, nrow(adae), prob = c(0.1, 0.2, 0.1, 0.3, 0.3)))
      )) %>% with_label("Outcome of Adverse Event"),
      TRTEMFL = ifelse(.data$ASTDTM >= .data$TRTSDTM, "Y", "") %>%
        with_label("Treatment Emergent Analysis Flag")
    )

  l_aag <- split(aag, interaction(aag$NAMVAR, aag$SRCVAR, aag$GRPTYPE, drop = TRUE))

  # Create aesi flags
  l_aesi <- lapply(l_aag, function(d_adag, d_adae) {
    names(d_adag)[names(d_adag) == "REFTERM"] <- d_adag$SRCVAR[1]
    names(d_adag)[names(d_adag) == "REFNAME"] <- d_adag$NAMVAR[1]

    if (d_adag$GRPTYPE[1] == "CUSTOM") {
      d_adag <- d_adag[-which(names(d_adag) == "SCOPE")]
    } else if (d_adag$GRPTYPE[1] == "SMQ") {
      names(d_adag)[names(d_adag) == "SCOPE"] <- paste0(substr(d_adag$NAMVAR[1], 1, 5), "SC")
    }

    d_adag <- d_adag[-which(names(d_adag) %in% c("NAMVAR", "SRCVAR", "GRPTYPE"))]
    d_new <- dplyr::left_join(x = d_adae, y = d_adag, by = intersect(names(d_adae), names(d_adag)))
    d_new[, dplyr::setdiff(names(d_new), names(d_adae)), drop = FALSE]
  }, adae)
  adae <- dplyr::bind_cols(adae, l_aesi)

  actions <- c(
    "DOSE RATE REDUCED",
    "UNKNOWN",
    "NOT APPLICABLE",
    "DRUG INTERRUPTED",
    "DRUG WITHDRAWN",
    "DOSE INCREASED",
    "DOSE NOT CHANGED",
    "DOSE REDUCED",
    "NOT EVALUABLE"
  )

  tmc_ex_adae <- adae %>%
    dplyr::mutate(
      AEACN = factor(ifelse(
        .data$AETOXGR == "5",
        "NOT EVALUABLE",
        as.character(sample_fct(actions, nrow(adae), prob = c(0.05, 0.05, 0.05, 0.01, 0.05, 0.1, 0.45, 0.1, 0.05)))
      )) %>% with_label("Action Taken With Study Treatment")
    ) %>%
    col_relabel(
      AEBODSYS = "Body System or Organ Class",
      AELLT = "Lowest Level Term",
      AEDECOD = "Dictionary-Derived Term",
      AEHLT = "High Level Term",
      AEHLGT = "High Level Group Term",
      AETOXGR = "Analysis Toxicity Grade",
      AESOC = "Primary System Organ Class",
      AESER = "Serious Event",
      AEREL = "Analysis Causality",
      AESEQ = "Sponsor-Defined Identifier",
      LDOSEDTM = "End Time/Time of Last Dose",
      CQ01NAM = "CQ 01 Reference Name",
      SMQ01NAM = "SMQ 01 Reference Name",
      SMQ01SC = "SMQ 01 Scope",
      SMQ02NAM = "SMQ 02 Reference Name",
      SMQ02SC = "SMQ 02 Scope"
    )

  i_lbls <- sapply(
    names(col_labels(tmc_ex_adae)[is.na(col_labels(tmc_ex_adae))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_adae[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_adae, file = "data/tmc_ex_adae.rda", compress = "xz")
}
```

## `ADAETTE`

``` r

generate_adaette <- function(adsl = tmc_ex_adsl) {
  set.seed(1)
  lookup_adaette <- tibble::tribble(
    ~ARM, ~CATCD, ~CAT, ~LAMBDA, ~CNSR_P,
    "ARM A", "1", "any adverse event", 1 / 80, 0.4,
    "ARM B", "1", "any adverse event", 1 / 100, 0.2,
    "ARM C", "1", "any adverse event", 1 / 60, 0.42,
    "ARM A", "2", "any serious adverse event", 1 / 100, 0.3,
    "ARM B", "2", "any serious adverse event", 1 / 150, 0.1,
    "ARM C", "2", "any serious adverse event", 1 / 80, 0.32,
    "ARM A", "3", "a grade 3-5 adverse event", 1 / 80, 0.2,
    "ARM B", "3", "a grade 3-5 adverse event", 1 / 100, 0.08,
    "ARM C", "3", "a grade 3-5 adverse event", 1 / 60, 0.23
  )
  evntdescr_sel <- "Preferred Term"
  cnsdtdscr_sel <- c(
    "Clinical Cut Off",
    "Completion or Discontinuation",
    "End of AE Reporting Period"
  )

  random_patient_data <- function(patient_info) {
    startdt <- lubridate::date(patient_info$TRTSDTM)
    trtedtm <- lubridate::floor_date(dplyr::case_when(
      is.na(patient_info$TRTEDTM) ~ lubridate::date(patient_info$TRTSDTM) + study_duration_secs,
      TRUE ~ lubridate::date(patient_info$TRTEDTM)
    ), unit = "day")
    enddts <- c(patient_info$EOSDT, lubridate::date(trtedtm))
    enddts_min_index <- which.min(enddts)
    adt <- enddts[enddts_min_index]
    adtm <- lubridate::as_datetime(adt)
    ady <- as.numeric(adt - startdt + 1)
    data.frame(
      ARM = patient_info$ARM,
      STUDYID = patient_info$STUDYID,
      SITEID = patient_info$SITEID,
      USUBJID = patient_info$USUBJID,
      PARAMCD = "AEREPTTE",
      PARAM = "Time to end of AE reporting period",
      CNSR = 0,
      AVAL = lubridate::days(ady) / lubridate::years(1),
      AVALU = "YEARS",
      EVNTDESC = ifelse(enddts_min_index == 1, "Completion or Discontinuation", "End of AE Reporting Period"),
      CNSDTDSC = NA,
      ADTM = adtm,
      ADY = ady,
      stringsAsFactors = FALSE
    )
  }

  paramcd_hy <- c("HYSTTEUL", "HYSTTEBL")
  param_hy <- c("Time to Hy's Law Elevation in relation to ULN", "Time to Hy's Law Elevation in relation to Baseline")
  param_init_list <- relvar_init(param_hy, paramcd_hy)
  adsl_hy <- dplyr::select(adsl, "STUDYID", "USUBJID", "TRTSDTM", "SITEID", "ARM")
  adaette_hy <- expand.grid(
    STUDYID = unique(adsl$STUDYID),
    USUBJID = adsl$USUBJID,
    PARAM = as.factor(param_init_list$relvar1),
    stringsAsFactors = FALSE
  )

  adaette_hy <- dplyr::left_join(adaette_hy, adsl_hy, by = c("STUDYID", "USUBJID"), multiple = "all") %>%
    dplyr::mutate(
      PARAMCD = factor(rel_var(
        df = as.data.frame(adaette_hy),
        var_values = param_init_list$relvar2,
        related_var = "PARAM"
      ))
    ) %>%
    dplyr::mutate(
      CNSR = sample(c(0, 1), prob = c(0.1, 0.9), size = dplyr::n(), replace = TRUE),
      EVNTDESC = dplyr::if_else(
        .data$CNSR == 0,
        "First Post-Baseline Raised ALT or AST Elevation Result",
        NA_character_
      ),
      CNSDTDSC = dplyr::if_else(.data$CNSR == 0, NA_character_,
        sample(c("Last Post-Baseline ALT or AST Result", "Treatment Start"),
          prob = c(0.9, 0.1),
          size = dplyr::n(), replace = TRUE
        )
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ADTM = dplyr::case_when(
      CNSDTDSC == "Treatment Start" ~ TRTSDTM,
      TRUE ~ TRTSDTM + sample(seq(0, study_duration_secs), size = dplyr::n(), replace = TRUE)
    )) %>%
    dplyr::mutate(
      ADY_int = lubridate::date(ADTM) - lubridate::date(TRTSDTM) + 1,
      ADY = as.numeric(ADY_int),
      AVAL = lubridate::days(ADY_int) / lubridate::weeks(1),
      AVALU = "WEEKS"
    ) %>%
    dplyr::select(-TRTSDTM, -ADY_int)

  random_ae_data <- function(lookup_info, patient_info, patient_data) {
    cnsr <- sample(c(0, 1), 1, prob = c(1 - lookup_info$CNSR_P, lookup_info$CNSR_P))
    ae_rep_tte <- patient_data$AVAL[patient_data$PARAMCD == "AEREPTTE"]
    data.frame(
      ARM = rep(patient_data$ARM, 2),
      STUDYID = rep(patient_data$STUDYID, 2),
      SITEID = rep(patient_data$SITEID, 2),
      USUBJID = rep(patient_data$USUBJID, 2),
      PARAMCD = c(
        paste0("AETTE", lookup_info$CATCD),
        paste0("AETOT", lookup_info$CATCD)
      ),
      PARAM = c(
        paste("Time to first occurrence of", lookup_info$CAT),
        paste("Number of occurrences of", lookup_info$CAT)
      ),
      CNSR = c(cnsr, NA),
      AVAL = c(
        ifelse(cnsr == 1, ae_rep_tte, rtexp(1, lookup_info$LAMBDA * 365.25, r = ae_rep_tte)),
        ifelse(cnsr == 1, 0, rtpois(1, lookup_info$LAMBDA * 365.25))
      ),
      AVALU = c("YEARS", NA),
      EVNTDESC = c(ifelse(cnsr == 0, sample(evntdescr_sel, 1), ""), NA),
      CNSDTDSC = c(ifelse(cnsr == 1, sample(cnsdtdscr_sel, 1), ""), NA),
      stringsAsFactors = FALSE
    ) %>% dplyr::mutate(
      ADY = dplyr::if_else(is.na(AVALU), NA_real_, ceiling(as.numeric(lubridate::dyears(AVAL), "days"))),
      ADTM = dplyr::if_else(
        is.na(AVALU),
        lubridate::as_datetime(NA),
        patient_info$TRTSDTM + lubridate::days(ADY)
      )
    )
  }

  adaette <- split(adsl, adsl$USUBJID) %>%
    lapply(function(patient_info) {
      patient_data <- random_patient_data(patient_info)
      lookup_arm <- lookup_adaette %>%
        dplyr::filter(.data$ARM == as.character(patient_info$ARMCD))
      ae_data <- split(lookup_arm, lookup_arm$CATCD) %>%
        lapply(random_ae_data, patient_data = patient_data, patient_info = patient_info) %>%
        Reduce(rbind, .)
      dplyr::bind_rows(patient_data, ae_data)
    }) %>%
    Reduce(rbind, .)
  adaette <- rbind(adaette, adaette_hy)

  tmc_ex_adaette <- adsl %>%
    dplyr::inner_join(
      dplyr::select(adaette, -"SITEID", -"ARM"),
      by = c("STUDYID", "USUBJID"),
      multiple = "all"
    ) %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::arrange(.data$ADTM) %>%
    dplyr::mutate(PARAM = as.factor(.data$PARAM)) %>%
    dplyr::mutate(PARAMCD = as.factor(.data$PARAMCD)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$PARAMCD,
      .data$ADTM
    )

  i_lbls <- sapply(
    names(col_labels(tmc_ex_adaette)[is.na(col_labels(tmc_ex_adaette))]),
    function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_adaette[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_adaette, file = "data/tmc_ex_adaette.rda", compress = "xz")
}
```

## `ADCM`

``` r

generate_adcm <- function(adsl = tmc_ex_adsl,
                          max_n_cms = 5L) {
  set.seed(1)
  lookup_cm <- tibble::tribble(
    ~CMCLAS, ~CMDECOD, ~ATIREL,
    "medcl A", "medname A_1/3", "PRIOR",
    "medcl A", "medname A_2/3", "CONCOMITANT",
    "medcl A", "medname A_3/3", "CONCOMITANT",
    "medcl B", "medname B_1/4", "CONCOMITANT",
    "medcl B", "medname B_2/4", "PRIOR",
    "medcl B", "medname B_3/4", "PRIOR",
    "medcl B", "medname B_4/4", "CONCOMITANT",
    "medcl C", "medname C_1/2", "CONCOMITANT",
    "medcl C", "medname C_2/2", "CONCOMITANT"
  )

  adcm <- Map(function(id, sid) {
    n_cms <- sample(c(0, seq_len(max_n_cms)), 1)
    i <- sample(seq_len(nrow(lookup_cm)), n_cms, TRUE)
    dplyr::mutate(
      lookup_cm[i, ],
      USUBJID = id,
      STUDYID = sid
    )
  }, adsl$USUBJID, adsl$STUDYID) %>%
    Reduce(rbind, .) %>%
    `[`(c(4, 5, 1, 2, 3)) %>%
    dplyr::mutate(CMCAT = .data$CMCLAS %>% with_label("Category for Medication"))

  # merge adsl to be able to add CM date and study day variables
  adcm <- dplyr::inner_join(
    adcm,
    adsl,
    by = c("STUDYID", "USUBJID"),
    multiple = "all"
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TRTENDT = lubridate::date(dplyr::case_when(
      is.na(TRTEDTM) ~ lubridate::floor_date(lubridate::date(TRTSDTM) + study_duration_secs, unit = "day"),
      TRUE ~ TRTEDTM
    ))) %>%
    dplyr::mutate(ASTDTM = sample(
      seq(lubridate::as_datetime(TRTSDTM), lubridate::as_datetime(TRTENDT), by = "day"),
      size = 1
    )) %>%
    dplyr::mutate(ASTDY = ceiling(difftime(ASTDTM, TRTSDTM, units = "days"))) %>%
    # add 1 to end of range incase both values passed to sample() are the same
    dplyr::mutate(AENDTM = sample(
      seq(lubridate::as_datetime(ASTDTM), lubridate::as_datetime(TRTENDT + 1), by = "day"),
      size = 1
    )) %>%
    dplyr::mutate(AENDY = ceiling(difftime(AENDTM, TRTSDTM, units = "days"))) %>%
    dplyr::select(-TRTENDT) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(STUDYID, USUBJID, ASTDTM)

  tmc_ex_adcm <- adcm %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(CMSEQ = seq_len(dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ASTDTM, .data$CMSEQ) %>%
    dplyr::mutate(
      ATC1 = paste("ATCCLAS1", substr(.data$CMDECOD, 9, 9)) %>% with_label("ATC Level 1 Text"),
      ATC2 = paste("ATCCLAS2", substr(.data$CMDECOD, 9, 9)) %>% with_label("ATC Level 2 Text"),
      ATC3 = paste("ATCCLAS3", substr(.data$CMDECOD, 9, 9)) %>% with_label("ATC Level 3 Text"),
      ATC4 = paste("ATCCLAS4", substr(.data$CMDECOD, 9, 9)) %>% with_label("ATC Level 4 Text")
    ) %>%
    dplyr::mutate(
      CMINDC = sample(c(
        "Nausea", "Hypertension", "Urticaria", "Fever",
        "Asthma", "Infection", "Diabete", "Diarrhea", "Pneumonia"
      ), dplyr::n(), replace = TRUE) %>% with_label("Indication"),
      CMDOSE = sample(1:99, dplyr::n(), replace = TRUE) %>% with_label("Dose per Administration"),
      CMTRT = substr(.data$CMDECOD, 9, 13) %>% with_label("Reported Name of Drug, Med, or Therapy"),
      CMDOSU = sample(c(
        "ug/mL", "ug/kg/day", "%", "uL", "DROP",
        "umol/L", "mg", "mg/breath", "ug"
      ), dplyr::n(), replace = TRUE) %>% with_label("Dose Units")
    ) %>%
    dplyr::mutate(
      CMROUTE = sample(c(
        "INTRAVENOUS", "ORAL", "NASAL",
        "INTRAMUSCULAR", "SUBCUTANEOUS", "INHALED", "RECTAL", "UNKNOWN"
      ), dplyr::n(), replace = TRUE) %>% with_label("Route of Administration"),
      CMDOSFRQ = sample(c(
        "Q4W", "QN", "Q4H", "UNKNOWN", "TWICE",
        "Q4H", "QD", "TID", "4 TIMES PER MONTH"
      ), dplyr::n(), replace = TRUE) %>% with_label("Dosing Frequency per Interval")
    ) %>%
    col_relabel(
      CMCLAS = "Medication Class",
      CMDECOD = "Standardized Medication Name",
      ATIREL = "Time Relation of Medication",
      CMSEQ = "Sponsor-Defined Identifier"
    )

  i_lbls <- sapply(
    names(col_labels(tmc_ex_adcm)[is.na(col_labels(tmc_ex_adcm))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_adcm[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_adcm, file = "data/tmc_ex_adcm.rda", compress = "xz")
}
```

## `ADEG`

``` r

generate_adeg <- function(adsl = tmc_ex_adsl,
                          n_assessments = 3L,
                          n_days = 3L,
                          max_n_eg = 3L) {
  set.seed(1)
  param <- c("QT Duration", "RR Duration", "Heart Rate", "ECG Interpretation")
  paramcd <- c("QT", "RR", "HR", "ECGINTP")
  paramu <- c("msec", "msec", "beats/min", "")
  visit_format <- "WEEK"

  param_init_list <- relvar_init(param, paramcd)
  unit_init_list <- relvar_init(param, paramu)

  adeg <- expand.grid(
    STUDYID = unique(adsl$STUDYID),
    USUBJID = adsl$USUBJID,
    PARAM = as.factor(param_init_list$relvar1),
    AVISIT = visit_schedule(visit_format = visit_format, n_assessments = n_assessments, n_days = n_days),
    stringsAsFactors = FALSE
  )

  adeg$PARAMCD <- as.factor(rel_var(
    df = adeg,
    var_name = "PARAMCD",
    var_values = param_init_list$relvar2,
    related_var = "PARAM"
  ))

  adeg <- adeg %>% dplyr::mutate(AVAL = dplyr::case_when(
    .data$PARAMCD == "QT" ~ stats::rnorm(nrow(adeg), mean = 350, sd = 100),
    .data$PARAMCD == "RR" ~ stats::rnorm(nrow(adeg), mean = 1050, sd = 300),
    .data$PARAMCD == "HR" ~ stats::rnorm(nrow(adeg), mean = 70, sd = 20),
    .data$PARAMCD == "ECGINTP" ~ NA_real_
  ))

  adeg <- adeg %>% dplyr::mutate(AVISITN = dplyr::case_when(
    AVISIT == "SCREENING" ~ -1,
    AVISIT == "BASELINE" ~ 0,
    (grepl("^WEEK", AVISIT) | grepl("^CYCLE", AVISIT)) ~ as.numeric(AVISIT) - 2,
    TRUE ~ NA_real_
  ))

  adeg$AVALU <- as.factor(rel_var(
    df = adeg,
    var_name = "AVALU",
    var_values = unit_init_list$relvar2,
    related_var = "PARAM"
  ))

  adeg <- adeg[order(adeg$STUDYID, adeg$USUBJID, adeg$PARAMCD, adeg$AVISITN), ]
  adeg <- Reduce(rbind, lapply(split(adeg, adeg$USUBJID), function(x) {
    x$STUDYID <- adsl$STUDYID[which(adsl$USUBJID == x$USUBJID[1])]
    x$ABLFL <- ifelse(toupper(visit_format) == "WEEK" & x$AVISIT == "BASELINE",
      "Y",
      ifelse(toupper(visit_format) == "CYCLE" & x$AVISIT == "CYCLE 1 DAY 1", "Y", "")
    )
    x
  }))

  adeg$BASE <- ifelse(adeg$AVISITN >= 0, retain(adeg, adeg$AVAL, adeg$ABLFL == "Y"), adeg$AVAL)
  adeg <- adeg %>%
    dplyr::mutate(ANRLO = dplyr::case_when(
      .data$PARAMCD == "QT" ~ 200,
      .data$PARAMCD == "RR" ~ 600,
      .data$PARAMCD == "HR" ~ 40,
      .data$PARAMCD == "ECGINTP" ~ NA_real_
    )) %>%
    dplyr::mutate(ANRHI = dplyr::case_when(
      .data$PARAMCD == "QT" ~ 500,
      .data$PARAMCD == "RR" ~ 1500,
      .data$PARAMCD == "HR" ~ 100,
      .data$PARAMCD == "ECGINTP" ~ NA_real_
    )) %>%
    dplyr::mutate(ANRIND = factor(dplyr::case_when(
      .data$AVAL < .data$ANRLO ~ "LOW",
      .data$AVAL >= .data$ANRLO & .data$AVAL <= .data$ANRHI ~ "NORMAL",
      .data$AVAL > .data$ANRHI ~ "HIGH"
    )))

  adeg <- adeg %>%
    dplyr::mutate(CHG = ifelse(.data$AVISITN > 0, .data$AVAL - .data$BASE, NA)) %>%
    dplyr::mutate(PCHG = ifelse(.data$AVISITN > 0, 100 * (.data$CHG / .data$BASE), NA)) %>%
    dplyr::mutate(BASETYPE = "LAST") %>%
    dplyr::group_by(.data$USUBJID, .data$PARAMCD, .data$BASETYPE) %>%
    dplyr::mutate(BNRIND = .data$ANRIND[.data$ABLFL == "Y"]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DTYPE = NA)

  adeg$ANRIND <- factor(adeg$ANRIND, levels = c("LOW", "NORMAL", "HIGH"))
  adeg$BNRIND <- factor(adeg$BNRIND, levels = c("LOW", "NORMAL", "HIGH"))

  adeg <- dplyr::inner_join(
    adsl,
    adeg,
    by = c("STUDYID", "USUBJID"),
    multiple = "all"
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TRTENDT = lubridate::date(dplyr::case_when(
      is.na(TRTEDTM) ~ lubridate::floor_date(lubridate::date(TRTSDTM) + study_duration_secs, unit = "day"),
      TRUE ~ TRTEDTM
    ))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::arrange(USUBJID, AVISITN) %>%
    dplyr::mutate(ADTM = rep(
      sort(sample(
        seq(lubridate::as_datetime(TRTSDTM[1]), lubridate::as_datetime(TRTENDT[1]), by = "day"),
        size = nlevels(AVISIT)
      )),
      each = n() / nlevels(AVISIT)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TRTENDT) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ADTM)

  adeg <- adeg %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$PARAMCD,
      .data$BASETYPE,
      .data$AVISITN,
      .data$DTYPE,
      .data$ADTM
    )

  adeg <- adeg %>%
    dplyr::mutate(ONTRTFL = factor(dplyr::case_when(
      is.na(.data$TRTSDTM) ~ "",
      is.na(.data$ADTM) ~ "Y",
      (.data$ADTM < .data$TRTSDTM) ~ "",
      (.data$ADTM > .data$TRTEDTM) ~ "",
      TRUE ~ "Y"
    ))) %>%
    dplyr::mutate(AVALC = ifelse(
      .data$PARAMCD == "ECGINTP",
      as.character(sample_fct(c("ABNORMAL", "NORMAL"), nrow(adeg), prob = c(0.25, 0.75))),
      as.character(.data$AVAL)
    ))

  adeg <- adeg %>% dplyr::mutate(row_check = seq_len(nrow(adeg)))
  get_groups <- function(data, minimum) {
    data <- data %>%
      dplyr::group_by(.data$USUBJID, .data$PARAMCD, .data$BASETYPE) %>%
      dplyr::arrange(.data$ADTM) %>%
      dplyr::filter(
        (.data$AVISIT != "BASELINE" & .data$AVISIT != "SCREENING") &
          (.data$ONTRTFL == "Y" | .data$ADTM <= .data$TRTSDTM)
      ) %>%
      {
        if (minimum == TRUE) {
          dplyr::filter(., .data$AVAL == min(.data$AVAL)) %>%
            dplyr::mutate(., DTYPE = "MINIMUM", AVISIT = "POST-BASELINE MINIMUM")
        } else {
          dplyr::filter(., .data$AVAL == max(.data$AVAL)) %>%
            dplyr::mutate(., DTYPE = "MAXIMUM", AVISIT = "POST-BASELINE MAXIMUM")
        }
      } %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
    data
  }

  lbls <- col_labels(adeg)
  adeg <- rbind(adeg, get_groups(adeg, TRUE), get_groups(adeg, FALSE)) %>%
    dplyr::arrange(.data$row_check) %>%
    dplyr::group_by(.data$USUBJID, .data$PARAMCD, .data$BASETYPE) %>%
    dplyr::arrange(.data$AVISIT, .by_group = TRUE) %>%
    dplyr::ungroup()
  col_labels(adeg) <- lbls

  adeg <- adeg[, -which(names(adeg) %in% c("row_check"))]
  flag_variables <- function(data, worst_obs) {
    data_compare <- data %>%
      dplyr::mutate(row_check = seq_len(nrow(data)))
    data <- data_compare %>%
      {
        if (worst_obs == FALSE) {
          dplyr::group_by(., .data$USUBJID, .data$PARAMCD, .data$BASETYPE, .data$AVISIT) %>%
            dplyr::arrange(., .data$ADTM)
        } else {
          dplyr::group_by(., .data$USUBJID, .data$PARAMCD, .data$BASETYPE)
        }
      } %>%
      dplyr::filter(
        .data$AVISITN > 0 & (.data$ONTRTFL == "Y" | .data$ADTM <= .data$TRTSDTM) &
          is.na(.data$DTYPE)
      ) %>%
      {
        if (worst_obs == TRUE) {
          dplyr::arrange(., .data$AVALC) %>% dplyr::filter(., ifelse(
            .data$PARAMCD == "ECGINTP",
            ifelse(.data$AVALC == "ABNORMAL", .data$AVALC == "ABNORMAL", .data$AVALC == "NORMAL"),
            .data$AVAL == min(.data$AVAL)
          ))
        } else {
          dplyr::filter(., ifelse(
            .data$PARAMCD == "ECGINTP",
            .data$AVALC == "ABNORMAL" | .data$AVALC == "NORMAL",
            .data$AVAL == min(.data$AVAL)
          ))
        }
      } %>%
      dplyr::slice(1) %>%
      {
        if (worst_obs == TRUE) {
          dplyr::mutate(., new_var = dplyr::case_when(
            (.data$AVALC == "ABNORMAL" | .data$AVALC == "NORMAL") ~ "Y",
            (!is.na(.data$AVAL) & is.na(.data$DTYPE)) ~ "Y",
            TRUE ~ ""
          ))
        } else {
          dplyr::mutate(., new_var = dplyr::case_when(
            (.data$AVALC == "ABNORMAL" | .data$AVALC == "NORMAL") ~ "Y",
            (!is.na(.data$AVAL) & is.na(.data$DTYPE)) ~ "Y",
            TRUE ~ ""
          ))
        }
      } %>%
      dplyr::ungroup()

    data_compare$new_var <- ifelse(data_compare$row_check %in% data$row_check, "Y", "")
    data_compare <- data_compare[, -which(names(data_compare) %in% c("row_check"))]

    data_compare
  }
  adeg <- flag_variables(adeg, FALSE) %>% dplyr::rename(WORS01FL = "new_var")
  adeg <- flag_variables(adeg, TRUE) %>% dplyr::rename(WORS02FL = "new_var")

  tmc_ex_adeg <- adeg %>%
    dplyr::group_by(.data$USUBJID, .data$PARAMCD, .data$BASETYPE) %>%
    dplyr::mutate(BASEC = ifelse(
      .data$PARAMCD == "ECGINTP",
      .data$AVALC[.data$AVISIT == "BASELINE"],
      as.character(.data$BASE)
    )) %>%
    dplyr::ungroup() %>%
    col_relabel(BASEC = "Baseline Character Value")

  i_lbls <- sapply(
    names(col_labels(tmc_ex_adeg)[is.na(col_labels(tmc_ex_adeg))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_adeg[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_adeg, file = "data/tmc_ex_adeg.rda", compress = "xz")
}
```

## `ADEX`

``` r

generate_adex <- function(adsl = tmc_ex_adsl,
                          n_assessments = 3L,
                          n_days = 3L,
                          max_n_exs = 3L) {
  set.seed(1)
  param <- c(
    "Dose administered during constant dosing interval",
    "Number of doses administered during constant dosing interval",
    "Total dose administered",
    "Total number of doses administered"
  )
  paramcd <- c("DOSE", "NDOSE", "TDOSE", "TNDOSE")
  paramu <- c("mg", " ", "mg", " ")
  parcat1 <- c("INDIVIDUAL", "OVERALL")
  parcat2 <- c("Drug A", "Drug B")
  visit_format <- "WEEK"

  param_init_list <- relvar_init(param, paramcd)
  unit_init_list <- relvar_init(param, paramu)

  adex <- expand.grid(
    STUDYID = unique(adsl$STUDYID),
    USUBJID = adsl$USUBJID,
    PARAM = c(
      rep(
        param_init_list$relvar1[1],
        length(levels(visit_schedule(visit_format = visit_format, n_assessments = n_assessments, n_days = n_days)))
      ),
      rep(
        param_init_list$relvar1[2],
        length(levels(visit_schedule(visit_format = visit_format, n_assessments = n_assessments, n_days = n_days)))
      ),
      param_init_list$relvar1[3:4]
    ),
    stringsAsFactors = FALSE
  )

  adex$PARAMCD <- as.factor(rel_var(
    df = adex,
    var_name = "PARAMCD",
    var_values = param_init_list$relvar2,
    related_var = "PARAM"
  ))

  adex$AVALU <- as.factor(rel_var(
    df = adex,
    var_name = "AVALU",
    var_values = unit_init_list$relvar2,
    related_var = "PARAM"
  ))

  adex <- adex %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(PARCAT_ind = sample(c(1, 2), size = 1)) %>%
    dplyr::mutate(PARCAT2 = ifelse(.data$PARCAT_ind == 1, parcat2[1], parcat2[2])) %>%
    dplyr::select(-"PARCAT_ind")

  adex <- adex %>% dplyr::mutate(PARCAT1 = dplyr::case_when(
    (.data$PARAMCD == "TNDOSE" | .data$PARAMCD == "TDOSE") ~ "OVERALL",
    .data$PARAMCD == "DOSE" | .data$PARAMCD == "NDOSE" ~ "INDIVIDUAL"
  ))

  adex_visit <- adex %>%
    dplyr::filter(.data$PARAMCD == "DOSE" | .data$PARAMCD == "NDOSE") %>%
    dplyr::mutate(
      AVISIT = rep(visit_schedule(visit_format = visit_format, n_assessments = n_assessments, n_days = n_days), 2)
    )

  adex <- dplyr::left_join(
    adex %>%
      dplyr::group_by(
        .data$USUBJID,
        .data$STUDYID,
        .data$PARAM,
        .data$PARAMCD,
        .data$AVALU,
        .data$PARCAT1,
        .data$PARCAT2
      ) %>%
      dplyr::mutate(id = dplyr::row_number()),
    adex_visit %>%
      dplyr::group_by(
        .data$USUBJID,
        .data$STUDYID,
        .data$PARAM,
        .data$PARAMCD,
        .data$AVALU,
        .data$PARCAT1,
        .data$PARCAT2
      ) %>%
      dplyr::mutate(id = dplyr::row_number()),
    by = c("USUBJID", "STUDYID", "PARCAT1", "PARCAT2", "id", "PARAMCD", "PARAM", "AVALU")
  ) %>%
    dplyr::select(-"id")

  adex <- adex %>% dplyr::mutate(AVISITN = dplyr::case_when(
    AVISIT == "SCREENING" ~ -1,
    AVISIT == "BASELINE" ~ 0,
    (grepl("^WEEK", AVISIT) | grepl("^CYCLE", AVISIT)) ~ as.numeric(AVISIT) - 2,
    TRUE ~ 999000
  ))

  adex2 <- split(adex, adex$USUBJID) %>%
    lapply(function(pinfo) {
      pinfo %>%
        dplyr::filter(.data$PARAMCD == "DOSE") %>%
        dplyr::group_by(.data$USUBJID, .data$PARCAT2, .data$AVISIT) %>%
        dplyr::mutate(changeind = dplyr::case_when(
          .data$AVISIT == "SCREENING" ~ 0,
          .data$AVISIT != "SCREENING" ~ sample(c(-1, 0, 1),
            size = 1,
            prob = c(0.25, 0.5, 0.25),
            replace = TRUE
          )
        )) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$USUBJID, .data$PARCAT2) %>%
        dplyr::mutate(
          csum = cumsum(.data$changeind),
          changeind = dplyr::case_when(
            .data$csum <= -3 ~ sample(c(0, 1), size = 1, prob = c(0.5, 0.5)),
            .data$csum >= 3 ~ sample(c(0, -1), size = 1, prob = c(0.5, 0.5)),
            TRUE ~ .data$changeind
          )
        ) %>%
        dplyr::mutate(csum = cumsum(.data$changeind)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$USUBJID, .data$PARCAT2, .data$AVISIT) %>%
        dplyr::mutate(AVAL = dplyr::case_when(
          .data$csum == -2 ~ 480,
          .data$csum == -1 ~ 720,
          .data$csum == 0 ~ 960,
          .data$csum == 1 ~ 1200,
          .data$csum == 2 ~ 1440
        )) %>%
        dplyr::select(-c("csum", "changeind")) %>%
        dplyr::ungroup()
    }) %>%
    Reduce(rbind, .)

  adextmp <- dplyr::full_join(adex2, adex, by = names(adex))
  adex <- adextmp %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(AVAL = ifelse(.data$PARAMCD == "NDOSE", 1, .data$AVAL)) %>%
    dplyr::mutate(AVAL = ifelse(
      .data$PARAMCD == "TNDOSE",
      sum(.data$AVAL[.data$PARAMCD == "NDOSE"]),
      .data$AVAL
    )) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$USUBJID, .data$STUDYID, .data$PARCAT2) %>%
    dplyr::mutate(AVAL = ifelse(
      .data$PARAMCD == "TDOSE",
      sum(.data$AVAL[.data$PARAMCD == "DOSE"]),
      .data$AVAL
    ))

  adex <- dplyr::inner_join(adsl, adex, by = c("STUDYID", "USUBJID"), multiple = "all") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TRTENDT = lubridate::date(dplyr::case_when(
      is.na(TRTEDTM) ~ lubridate::floor_date(lubridate::date(TRTSDTM) + study_duration_secs, unit = "day"),
      TRUE ~ TRTEDTM
    ))) %>%
    dplyr::mutate(ASTDTM = sample(
      seq(lubridate::as_datetime(TRTSDTM), lubridate::as_datetime(TRTENDT), by = "day"),
      size = 1
    )) %>%
    dplyr::select(-TRTENDT) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ASTDTM)

  adex <- adex %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(EXSEQ = seq_len(dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$PARAMCD,
      .data$ASTDTM,
      .data$AVISITN
    ) %>%
    col_relabel(
      PARCAT1 = "Parameter Category (Individual/Overall)",
      PARCAT2 = "Parameter Category (Drug A/Drug B)",
      EXSEQ = "Analysis Sequence Number"
    )

  visit_levels <- str_extract(levels(adex$AVISIT), pattern = "[0-9]+")
  vl_extracted <- vapply(visit_levels, function(x) as.numeric(x[2]), numeric(1))
  vl_extracted <- c(-1, 1, vl_extracted[!is.na(vl_extracted)])

  tmc_ex_adex <- adex %>%
    dplyr::mutate(VISITDY = as.numeric(as.character(factor(AVISIT, labels = vl_extracted)))) %>%
    dplyr::mutate(ASTDTM = lubridate::as_datetime(TRTSDTM) + lubridate::days(VISITDY)) %>%
    dplyr::distinct(USUBJID, .keep_all = TRUE)

  i_lbls <- sapply(
    names(col_labels(tmc_ex_adex)[is.na(col_labels(tmc_ex_adex))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_adex[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_adex, file = "data/tmc_ex_adex.rda", compress = "xz")
}
```

## `ADLB`

``` r

generate_adlb <- function(adsl = tmc_ex_adsl,
                          n_assessments = 3L,
                          n_days = 3L,
                          max_n_lbs = 3L) {
  set.seed(1)
  lbcat <- c("CHEMISTRY", "CHEMISTRY", "IMMUNOLOGY")
  param <- c(
    "Alanine Aminotransferase Measurement",
    "C-Reactive Protein Measurement",
    "Immunoglobulin A Measurement"
  )
  paramcd <- c("ALT", "CRP", "IGA")
  paramu <- c("U/L", "mg/L", "g/L")
  aval_mean <- c(20, 1, 2)
  visit_format <- "WEEK"

  # validate and initialize related variables
  lbcat_init_list <- relvar_init(param, lbcat)
  param_init_list <- relvar_init(param, paramcd)
  unit_init_list <- relvar_init(param, paramu)

  adlb <- expand.grid(
    STUDYID = unique(adsl$STUDYID),
    USUBJID = adsl$USUBJID,
    PARAM = as.factor(param_init_list$relvar1),
    AVISIT = visit_schedule(visit_format = visit_format, n_assessments = n_assessments, n_days = n_days),
    stringsAsFactors = FALSE
  )

  # assign AVAL based on different test
  adlb <- adlb %>%
    dplyr::mutate(AVAL = stats::rnorm(nrow(adlb), mean = 1, sd = 0.2)) %>%
    dplyr::left_join(data.frame(PARAM = param, ADJUST = aval_mean), by = "PARAM") %>%
    dplyr::mutate(AVAL = .data$AVAL * .data$ADJUST) %>%
    dplyr::select(-"ADJUST")

  # assign related variable values: PARAMxLBCAT are related
  adlb$LBCAT <- as.factor(rel_var(
    df = adlb,
    var_name = "LBCAT",
    var_values = lbcat_init_list$relvar2,
    related_var = "PARAM"
  ))

  # assign related variable values: PARAMxPARAMCD are related
  adlb$PARAMCD <- as.factor(rel_var(
    df = adlb,
    var_name = "PARAMCD",
    var_values = param_init_list$relvar2,
    related_var = "PARAM"
  ))

  adlb$AVALU <- as.factor(rel_var(
    df = adlb,
    var_name = "AVALU",
    var_values = unit_init_list$relvar2,
    related_var = "PARAM"
  ))

  adlb <- adlb %>% dplyr::mutate(AVISITN = dplyr::case_when(
    AVISIT == "SCREENING" ~ -1,
    AVISIT == "BASELINE" ~ 0,
    (grepl("^WEEK", AVISIT) | grepl("^CYCLE", AVISIT)) ~ as.numeric(AVISIT) - 2,
    TRUE ~ NA_real_
  ))

  adlb <- adlb %>%
    dplyr::mutate(AVISITN = dplyr::case_when(
      AVISIT == "SCREENING" ~ -1,
      AVISIT == "BASELINE" ~ 0,
      (grepl("^WEEK", AVISIT) | grepl("^CYCLE", AVISIT)) ~ as.numeric(AVISIT) - 2,
      TRUE ~ NA_real_
    ))

  # order to prepare for change from screening and baseline values
  adlb <- adlb[order(adlb$STUDYID, adlb$USUBJID, adlb$PARAMCD, adlb$AVISITN), ]

  adlb <- Reduce(rbind, lapply(split(adlb, adlb$USUBJID), function(x) {
    x$STUDYID <- adsl$STUDYID[which(adsl$USUBJID == x$USUBJID[1])]
    x$ABLFL2 <- ifelse(x$AVISIT == "SCREENING", "Y", "")
    x$ABLFL <- ifelse(toupper(visit_format) == "WEEK" & x$AVISIT == "BASELINE",
      "Y",
      ifelse(toupper(visit_format) == "CYCLE" & x$AVISIT == "CYCLE 1 DAY 1", "Y", "")
    )
    x
  }))

  adlb$BASE <- ifelse(adlb$ABLFL2 != "Y", retain(adlb, adlb$AVAL, adlb$ABLFL == "Y"), NA)
  anrind_choices <- c("HIGH", "LOW", "NORMAL")
  adlb <- adlb %>%
    dplyr::mutate(BASETYPE = "LAST") %>%
    dplyr::mutate(ANRIND = sample_fct(anrind_choices, nrow(adlb), prob = c(0.1, 0.1, 0.8))) %>%
    dplyr::mutate(ANRLO = dplyr::case_when(
      .data$PARAMCD == "ALT" ~ 7,
      .data$PARAMCD == "CRP" ~ 8,
      .data$PARAMCD == "IGA" ~ 0.8
    )) %>%
    dplyr::mutate(ANRHI = dplyr::case_when(
      .data$PARAMCD == "ALT" ~ 55,
      .data$PARAMCD == "CRP" ~ 10,
      .data$PARAMCD == "IGA" ~ 3
    )) %>%
    dplyr::mutate(DTYPE = NA) %>%
    dplyr::mutate(
      ATOXGR = factor(dplyr::case_when(
        .data$ANRIND == "LOW" ~ sample(
          c("-1", "-2", "-3", "-4", "-5"),
          nrow(adlb),
          replace = TRUE,
          prob = c(0.30, 0.25, 0.20, 0.15, 0)
        ),
        .data$ANRIND == "HIGH" ~ sample(
          c("1", "2", "3", "4", "5"),
          nrow(adlb),
          replace = TRUE,
          prob = c(0.30, 0.25, 0.20, 0.15, 0)
        ),
        .data$ANRIND == "NORMAL" ~ "0"
      )) %>% with_label("Analysis Toxicity Grade")
    ) %>%
    dplyr::group_by(.data$USUBJID, .data$PARAMCD, .data$BASETYPE) %>%
    dplyr::mutate(BTOXGR = .data$ATOXGR[.data$ABLFL == "Y"]) %>%
    dplyr::ungroup() %>%
    col_relabel(BTOXGR = "Baseline Toxicity Grade")

  # High and low descriptions of the different PARAMCD values
  # This is currently hard coded as the GDSR does not have these descriptions yet
  grade_lookup <- tibble::tribble(
    ~PARAMCD, ~ATOXDSCL, ~ATOXDSCH,
    "ALB", "Hypoalbuminemia", NA_character_,
    "ALKPH", NA_character_, "Alkaline phosphatase increased",
    "ALT", NA_character_, "Alanine aminotransferase increased",
    "AST", NA_character_, "Aspartate aminotransferase increased",
    "BILI", NA_character_, "Blood bilirubin increased",
    "CA", "Hypocalcemia", "Hypercalcemia",
    "CHOLES", NA_character_, "Cholesterol high",
    "CK", NA_character_, "CPK increased",
    "CREAT", NA_character_, "Creatinine increased",
    "CRP", NA_character_, "C reactive protein increased",
    "GGT", NA_character_, "GGT increased",
    "GLUC", "Hypoglycemia", "Hyperglycemia",
    "HGB", "Anemia", "Hemoglobin increased",
    "IGA", NA_character_, "Immunoglobulin A increased",
    "POTAS", "Hypokalemia", "Hyperkalemia",
    "LYMPH", "CD4 lymphocytes decreased", NA_character_,
    "PHOS", "Hypophosphatemia", NA_character_,
    "PLAT", "Platelet count decreased", NA_character_,
    "SODIUM", "Hyponatremia", "Hypernatremia",
    "WBC", "White blood cell decreased", "Leukocytosis",
  )

  # merge grade_lookup onto adlb
  adlb <- dplyr::left_join(adlb, grade_lookup, by = "PARAMCD")

  # merge adsl to be able to add LB date and study day variables
  adlb <- dplyr::inner_join(
    adsl,
    adlb,
    by = c("STUDYID", "USUBJID"),
    multiple = "all"
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TRTENDT = lubridate::date(dplyr::case_when(
      is.na(TRTEDTM) ~ lubridate::floor_date(lubridate::date(TRTSDTM) + study_duration_secs, unit = "day"),
      TRUE ~ TRTEDTM
    ))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::arrange(USUBJID, AVISITN) %>%
    dplyr::mutate(ADTM = rep(
      sort(sample(
        seq(lubridate::as_datetime(TRTSDTM[1]), lubridate::as_datetime(TRTENDT[1]), by = "day"),
        size = nlevels(AVISIT)
      )),
      each = n() / nlevels(AVISIT)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TRTENDT) %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ADTM)

  adlb <- adlb %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(LBSEQ = seq_len(dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$PARAMCD,
      .data$BASETYPE,
      .data$AVISITN,
      .data$DTYPE,
      .data$ADTM,
      .data$LBSEQ
    ) %>%
    col_relabel(LBSEQ = "Lab Test or Examination Sequence Number")

  adlb <- adlb %>% dplyr::mutate(ONTRTFL = factor(dplyr::case_when(
    is.na(.data$TRTSDTM) ~ "",
    is.na(.data$ADTM) ~ "Y",
    (.data$ADTM < .data$TRTSDTM) ~ "",
    (.data$ADTM > .data$TRTEDTM) ~ "",
    TRUE ~ "Y"
  )))

  flag_variables <- function(data,
                             apply_grouping,
                             apply_filter,
                             apply_mutate) {
    data_compare <- data %>% dplyr::mutate(row_check = seq_len(nrow(data)))
    data <- data_compare %>%
      {
        if (apply_grouping == TRUE) {
          dplyr::group_by(., .data$USUBJID, .data$PARAMCD, .data$BASETYPE, .data$AVISIT)
        } else {
          dplyr::group_by(., .data$USUBJID, .data$PARAMCD, .data$BASETYPE)
        }
      } %>%
      dplyr::arrange(.data$ADTM, .data$LBSEQ) %>%
      {
        if (apply_filter == TRUE) {
          dplyr::filter(
            .,
            (.data$AVISIT != "BASELINE" & .data$AVISIT != "SCREENING") &
              (.data$ONTRTFL == "Y" | .data$ADTM <= .data$TRTSDTM)
          ) %>%
            dplyr::filter(.data$ATOXGR == max(as.numeric(as.character(.data$ATOXGR))))
        } else if (apply_filter == FALSE) {
          dplyr::filter(
            .,
            (.data$AVISIT != "BASELINE" & .data$AVISIT != "SCREENING") &
              (.data$ONTRTFL == "Y" | .data$ADTM <= .data$TRTSDTM)
          ) %>%
            dplyr::filter(.data$ATOXGR == min(as.numeric(as.character(.data$ATOXGR))))
        } else {
          dplyr::filter(
            .,
            .data$AVAL == min(.data$AVAL) &
              (.data$AVISIT != "BASELINE" & .data$AVISIT != "SCREENING") &
              (.data$ONTRTFL == "Y" | .data$ADTM <= .data$TRTSDTM)
          )
        }
      } %>%
      dplyr::slice(1) %>%
      {
        if (apply_mutate == TRUE) {
          dplyr::mutate(., new_var = ifelse(is.na(.data$DTYPE), "Y", ""))
        } else {
          dplyr::mutate(., new_var = ifelse(is.na(.data$AVAL) == FALSE & is.na(.data$DTYPE), "Y", ""))
        }
      } %>%
      dplyr::ungroup()

    data_compare$new_var <- ifelse(data_compare$row_check %in% data$row_check, "Y", "")
    data_compare <- data_compare[, -which(names(data_compare) %in% c("row_check"))]
    data_compare
  }
  adlb <- flag_variables(adlb, TRUE, "ELSE", FALSE) %>% dplyr::rename(WORS01FL = "new_var")
  adlb <- flag_variables(adlb, FALSE, TRUE, TRUE) %>% dplyr::rename(WGRHIFL = "new_var")
  adlb <- flag_variables(adlb, FALSE, FALSE, TRUE) %>% dplyr::rename(WGRLOFL = "new_var")
  adlb <- flag_variables(adlb, TRUE, TRUE, TRUE) %>% dplyr::rename(WGRHIVFL = "new_var")
  adlb <- flag_variables(adlb, TRUE, FALSE, TRUE) %>% dplyr::rename(WGRLOVFL = "new_var")

  tmc_ex_adlb <- adlb %>% dplyr::mutate(
    ANL01FL = ifelse(
      (.data$ABLFL == "Y" | (.data$WORS01FL == "Y" & is.na(.data$DTYPE))) &
        (.data$AVISIT != "SCREENING"),
      "Y",
      ""
    ) %>% with_label("Analysis Flag 01 Baseline Post-Baseline"),
    PARAM = as.factor(.data$PARAM)
  )

  tmc_ex_adlb <- tmc_ex_adlb %>%
    group_by(.data$USUBJID, .data$PARAMCD, .data$BASETYPE) %>%
    mutate(BNRIND = .data$ANRIND[.data$ABLFL == "Y"]) %>%
    ungroup() %>%
    dplyr::mutate(ADY = ceiling(as.numeric(difftime(.data$ADTM, .data$TRTSDTM, units = "days"))))

  tmc_ex_adlb$PARAMCD <- as.factor(tmc_ex_adlb$PARAMCD)
  tmc_ex_adlb <- tmc_ex_adlb %>%
    dplyr::mutate(CHG = .data$AVAL - .data$BASE) %>%
    dplyr::mutate(PCHG = 100 * (.data$CHG / .data$BASE)) %>%
    col_relabel(
      LBCAT = "Category for Lab Test",
      ATOXDSCL = "Analysis Toxicity Description Low",
      ATOXDSCH = "Analysis Toxicity Description High",
      WGRHIFL = "Worst High Grade per Patient",
      WGRLOFL = "Worst Low Grade per Patient",
      WGRHIVFL = "Worst High Grade per Patient per Visit",
      WGRLOVFL = "Worst Low Grade per Patient per Visit"
    )

  i_lbls <- sapply(
    names(col_labels(tmc_ex_adlb)[is.na(col_labels(tmc_ex_adlb))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_adlb[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_adlb, file = "data/tmc_ex_adlb.rda", compress = "xz")
}
```

## `ADMH`

``` r

generate_admh <- function(adsl = tmc_ex_adsl,
                          max_n_mhs = 10L) {
  set.seed(1)
  lookup_mh <- tibble::tribble(
    ~MHBODSYS, ~MHDECOD, ~MHSOC,
    "cl A", "trm A_1/2", "cl A",
    "cl A", "trm A_2/2", "cl A",
    "cl B", "trm B_1/3", "cl B",
    "cl B", "trm B_2/3", "cl B",
    "cl B", "trm B_3/3", "cl B",
    "cl C", "trm C_1/2", "cl C",
    "cl C", "trm C_2/2", "cl C",
    "cl D", "trm D_1/3", "cl D",
    "cl D", "trm D_2/3", "cl D",
    "cl D", "trm D_3/3", "cl D"
  )

  admh <- Map(
    function(id, sid) {
      n_mhs <- sample(0:max_n_mhs, 1)
      i <- sample(seq_len(nrow(lookup_mh)), n_mhs, TRUE)
      dplyr::mutate(
        lookup_mh[i, ],
        USUBJID = id,
        STUDYID = sid
      )
    },
    adsl$USUBJID,
    adsl$STUDYID
  ) %>%
    Reduce(rbind, .) %>%
    `[`(c(4, 5, 1, 2, 3)) %>%
    dplyr::mutate(MHTERM = .data$MHDECOD %>% with_label("Reported Term for the Medical History"))

  admh <- dplyr::inner_join(
    adsl,
    admh,
    by = c("STUDYID", "USUBJID"),
    multiple = "all"
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TRTENDT = lubridate::date(dplyr::case_when(
      is.na(TRTEDTM) ~ lubridate::floor_date(lubridate::date(TRTSDTM) + study_duration_secs, unit = "day"),
      TRUE ~ TRTEDTM
    ))) %>%
    dplyr::mutate(ASTDTM = sample(
      seq(lubridate::as_datetime(TRTSDTM), lubridate::as_datetime(TRTENDT), by = "day"),
      size = 1
    )) %>%
    select(-TRTENDT) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ASTDTM, .data$MHTERM) %>%
    dplyr::mutate(MHDISTAT = sample(
      x = c("Resolved", "Ongoing with treatment", "Ongoing without treatment"),
      prob = c(0.6, 0.2, 0.2),
      size = dplyr::n(),
      replace = TRUE
    ) %>% with_label("Status of Disease"))

  tmc_ex_admh <- admh %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(MHSEQ = seq_len(dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ASTDTM) %>%
    col_relabel(
      MHBODSYS = "Body System or Organ Class",
      MHDECOD = "Dictionary-Derived Term",
      MHSOC = "Primary System Organ Class",
      MHSEQ = "Sponsor-Defined Identifier"
    )

  i_lbls <- sapply(
    names(col_labels(tmc_ex_admh)[is.na(col_labels(tmc_ex_admh))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_admh[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_admh, file = "data/tmc_ex_admh.rda", compress = "xz")
}
```

## `ADQS`

``` r

generate_adqs <- function(adsl = tmc_ex_adsl,
                          n_assessments = 5L,
                          n_days = 5L) {
  set.seed(1)
  param <- c(
    "BFI All Questions",
    "Fatigue Interference",
    "Function/Well-Being (GF1,GF3,GF7)",
    "Treatment Side Effects (GP2,C5,GP5)",
    "FKSI-19 All Questions"
  )
  paramcd <- c("BFIALL", "FATIGI", "FKSI-FWB", "FKSI-TSE", "FKSIALL")
  visit_format <- "WEEK"

  param_init_list <- relvar_init(param, paramcd)

  adqs <- expand.grid(
    STUDYID = unique(adsl$STUDYID),
    USUBJID = adsl$USUBJID,
    PARAM = param_init_list$relvar1,
    AVISIT = visit_schedule(visit_format = visit_format, n_assessments = n_assessments, n_days = n_days),
    stringsAsFactors = FALSE
  )

  adqs <- dplyr::mutate(
    adqs,
    AVISITN = dplyr::case_when(
      AVISIT == "SCREENING" ~ -1,
      AVISIT == "BASELINE" ~ 0,
      (grepl("^WEEK", AVISIT) | grepl("^CYCLE", AVISIT)) ~ as.numeric(AVISIT) - 2,
      TRUE ~ NA_real_
    )
  )

  adqs$PARAMCD <- rel_var(df = adqs, var_name = "PARAMCD", var_values = param_init_list$relvar2, related_var = "PARAM")
  adqs$AVAL <- stats::rnorm(nrow(adqs), mean = 50, sd = 8) + adqs$AVISITN * stats::rnorm(nrow(adqs), mean = 5, sd = 2)
  adqs <- adqs[order(adqs$STUDYID, adqs$USUBJID, adqs$PARAMCD, adqs$AVISITN), ]

  adqs <- Reduce(
    rbind,
    lapply(
      split(adqs, adqs$USUBJID),
      function(x) {
        x$STUDYID <- adsl$STUDYID[which(adsl$USUBJID == x$USUBJID[1])]
        x$ABLFL2 <- ifelse(x$AVISIT == "SCREENING", "Y", "")
        x$ABLFL <- ifelse(
          toupper(visit_format) == "WEEK" & x$AVISIT == "BASELINE",
          "Y",
          ifelse(
            toupper(visit_format) == "CYCLE" & x$AVISIT == "CYCLE 1 DAY 1",
            "Y",
            ""
          )
        )
        x
      }
    )
  )

  adqs$BASE <- ifelse(adqs$ABLFL2 != "Y", retain(adqs, adqs$AVAL, adqs$ABLFL == "Y"), NA)
  adqs <- adqs %>% dplyr::mutate(CHG = .data$AVAL - .data$BASE)

  adqs <- dplyr::inner_join(
    adsl,
    adqs,
    by = c("STUDYID", "USUBJID"),
    multiple = "all"
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TRTENDT = lubridate::date(dplyr::case_when(
      is.na(TRTEDTM) ~ lubridate::floor_date(lubridate::date(TRTSDTM) + study_duration_secs, unit = "day"),
      TRUE ~ TRTEDTM
    ))) %>%
    ungroup() %>%
    group_by(USUBJID) %>%
    arrange(USUBJID, AVISITN) %>%
    dplyr::mutate(ADTM = rep(
      sort(sample(
        seq(lubridate::as_datetime(TRTSDTM[1]), lubridate::as_datetime(TRTENDT[1]), by = "day"),
        size = nlevels(AVISIT)
      )),
      each = n() / nlevels(AVISIT)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TRTENDT) %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ADTM)

  tmc_ex_adqs <- adqs %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$PARAMCD,
      .data$AVISITN,
      .data$ADTM
    )

  i_lbls <- sapply(
    names(col_labels(tmc_ex_adqs)[is.na(col_labels(tmc_ex_adqs))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_adqs[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_adqs, file = "data/tmc_ex_adqs.rda", compress = "xz")
}
```

## `ADRS`

``` r

generate_adrs <- function(adsl = tmc_ex_adsl) {
  set.seed(1)
  param_codes <- stats::setNames(1:5, c("CR", "PR", "SD", "PD", "NE"))

  lookup_ars <- expand.grid(
    ARM = c("A: Drug X", "B: Placebo", "C: Combination"),
    AVALC = names(param_codes)
  ) %>% dplyr::mutate(
    AVAL = param_codes[.data$AVALC],
    p_scr = c(rep(0, 3), rep(0, 3), c(1, 1, 1), c(0, 0, 0), c(0, 0, 0)),
    p_bsl = c(rep(0, 3), rep(0, 3), c(1, 1, 1), c(0, 0, 0), c(0, 0, 0)),
    p_cycle = c(c(.35, .25, .4), c(.30, .20, .20), c(.2, .25, .3), c(.14, 0.20, 0.18), c(.01, 0.1, 0.02)),
    p_eoi = c(c(.35, .25, .4), c(.30, .20, .20), c(.2, .25, .3), c(.14, 0.20, 0.18), c(.01, 0.1, 0.02)),
    p_fu = c(c(.25, .15, .3), c(.15, .05, .25), c(.3, .25, .3), c(.3, .55, .25), rep(0, 3))
  )

  adrs <- split(adsl, adsl$USUBJID) %>%
    lapply(function(pinfo) {
      probs <- dplyr::filter(lookup_ars, .data$ARM == as.character(pinfo$ACTARM))
      # screening
      rsp_screen <- sample(probs$AVALC, 1, prob = probs$p_scr) %>% as.character()
      # baseline
      rsp_bsl <- sample(probs$AVALC, 1, prob = probs$p_bsl) %>% as.character()
      # cycle
      rsp_c2d1 <- sample(probs$AVALC, 1, prob = probs$p_cycle) %>% as.character()
      rsp_c4d1 <- sample(probs$AVALC, 1, prob = probs$p_cycle) %>% as.character()
      # end of induction
      rsp_eoi <- sample(probs$AVALC, 1, prob = probs$p_eoi) %>% as.character()
      # follow up
      rsp_fu <- sample(probs$AVALC, 1, prob = probs$p_fu) %>% as.character()

      best_rsp <- min(param_codes[c(rsp_screen, rsp_bsl, rsp_eoi, rsp_fu, rsp_c2d1, rsp_c4d1)])
      best_rsp_i <- which.min(param_codes[c(rsp_screen, rsp_bsl, rsp_eoi, rsp_fu, rsp_c2d1, rsp_c4d1)])

      avisit <- c("SCREENING", "BASELINE", "CYCLE 2 DAY 1", "CYCLE 4 DAY 1", "END OF INDUCTION", "FOLLOW UP")

      # meaningful date information
      TRTSTDT <- lubridate::date(pinfo$TRTSDTM) 
      TRTENDT <- lubridate::date(dplyr::if_else( 
        !is.na(pinfo$TRTEDTM), pinfo$TRTEDTM,
        lubridate::floor_date(TRTSTDT + study_duration_secs, unit = "day")
      ))
      scr_date <- TRTSTDT - lubridate::days(100)
      bs_date <- TRTSTDT
      flu_date <- sample(seq(lubridate::as_datetime(TRTSTDT), lubridate::as_datetime(TRTENDT), by = "day"), size = 1)
      eoi_date <- sample(seq(lubridate::as_datetime(TRTSTDT), lubridate::as_datetime(TRTENDT), by = "day"), size = 1)
      c2d1_date <- sample(seq(lubridate::as_datetime(TRTSTDT), lubridate::as_datetime(TRTENDT), by = "day"), size = 1)
      c4d1_date <- min(lubridate::date(c2d1_date + lubridate::days(60)), TRTENDT)

      tibble::tibble(
        STUDYID = pinfo$STUDYID,
        USUBJID = pinfo$USUBJID,
        PARAMCD = as.factor(c(rep("OVRINV", 6), "BESRSPI", "INVET")),
        PARAM = as.factor(dplyr::recode(
          .data$PARAMCD,
          OVRINV = "Overall Response by Investigator - by visit",
          OVRSPI = "Best Overall Response by Investigator (no confirmation required)",
          BESRSPI = "Best Confirmed Overall Response by Investigator",
          INVET = "Investigator End Of Induction Response"
        )),
        AVALC = c(
          rsp_screen, rsp_bsl, rsp_c2d1, rsp_c4d1, rsp_eoi, rsp_fu,
          names(param_codes)[best_rsp],
          rsp_eoi
        ),
        AVAL = param_codes[.data$AVALC],
        AVISIT = factor(c(avisit, avisit[best_rsp_i], avisit[5]), levels = avisit)
      ) %>%
        merge(
          tibble::tibble(
            AVISIT = avisit,
            ADTM = c(scr_date, bs_date, c2d1_date, c4d1_date, eoi_date, flu_date),
            AVISITN = c(-1, 0, 2, 4, 999, 999),
            TRTSDTM = pinfo$TRTSDTM
          ) %>%
            dplyr::select(-"TRTSDTM"),
          by = "AVISIT"
        )
    }) %>%
    Reduce(rbind, .) %>%
    dplyr::mutate(
      AVALC = factor(.data$AVALC, levels = names(param_codes)),
      DTHFL = factor(sample(c("Y", "N"), nrow(.), replace = TRUE, prob = c(1, 0.8))) %>%
        with_label("Death Flag")
    )

  # merge ADSL to be able to add RS date and study day variables
  adrs <- dplyr::inner_join(
    adsl,
    adrs,
    by = c("STUDYID", "USUBJID"),
    multiple = "all"
  )

  tmc_ex_adrs <- adrs %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$PARAMCD,
      .data$AVISITN,
      .data$ADTM
    )

  i_lbls <- sapply(
    names(col_labels(tmc_ex_adrs)[is.na(col_labels(tmc_ex_adrs))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_adrs[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_adrs, file = "data/tmc_ex_adrs.rda", compress = "xz")
}
```

## `ADTTE`

``` r

generate_adtte <- function(adsl = tmc_ex_adsl) {
  set.seed(1)
  lookup_tte <- tibble::tribble(
    ~ARM, ~PARAMCD, ~PARAM, ~LAMBDA, ~CNSR_P,
    "ARM A", "OS", "Overall Survival", log(2) / 610, 0.4,
    "ARM B", "OS", "Overall Survival", log(2) / 490, 0.3,
    "ARM C", "OS", "Overall Survival", log(2) / 365, 0.2,
    "ARM A", "PFS", "Progression Free Survival", log(2) / 365, 0.4,
    "ARM B", "PFS", "Progression Free Survival", log(2) / 305, 0.3,
    "ARM C", "PFS", "Progression Free Survival", log(2) / 243, 0.2,
    "ARM A", "EFS", "Event Free Survival", log(2) / 365, 0.4,
    "ARM B", "EFS", "Event Free Survival", log(2) / 305, 0.3,
    "ARM C", "EFS", "Event Free Survival", log(2) / 243, 0.2,
    "ARM A", "CRSD", "Duration of Confirmed Response", log(2) / 305, 0.4,
    "ARM B", "CRSD", "Duration of Confirmed Response", log(2) / 243, 0.3,
    "ARM C", "CRSD", "Duration of Confirmed Response", log(2) / 182, 0.2
  )

  evntdescr_sel <- c(
    "Death",
    "Disease Progression",
    "Last Tumor Assessment",
    "Adverse Event",
    "Last Date Known To Be Alive"
  )

  cnsdtdscr_sel <- c(
    "Preferred Term",
    "Clinical Cut Off",
    "Completion or Discontinuation",
    "End of AE Reporting Period"
  )

  adtte <- split(adsl, adsl$USUBJID) %>%
    lapply(FUN = function(pinfo) {
      lookup_tte %>%
        dplyr::filter(.data$ARM == as.character(pinfo$ACTARMCD)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          STUDYID = pinfo$STUDYID,
          USUBJID = pinfo$USUBJID,
          CNSR = sample(c(0, 1), 1, prob = c(1 - .data$CNSR_P, .data$CNSR_P)),
          AVAL = stats::rexp(1, .data$LAMBDA),
          AVALU = "DAYS",
          EVNTDESC = if (.data$CNSR == 1) {
            sample(evntdescr_sel[-c(1:2)], 1)
          } else {
            ifelse(.data$PARAMCD == "OS",
              sample(evntdescr_sel[1], 1),
              sample(evntdescr_sel[c(1:2)], 1)
            )
          }
        ) %>%
        dplyr::select(-"LAMBDA", -"CNSR_P")
    }) %>%
    Reduce(rbind, .)

  # merge ADSL to be able to add TTE date and study day variables
  adtte <- dplyr::inner_join(
    adsl,
    dplyr::select(adtte, -"ARM"),
    by = c("STUDYID", "USUBJID"),
    multiple = "all"
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TRTENDT = lubridate::date(dplyr::case_when(
      is.na(TRTEDTM) ~ lubridate::floor_date(lubridate::date(TRTSDTM) + study_duration_secs, unit = "day"),
      TRUE ~ TRTEDTM
    ))) %>%
    dplyr::mutate(ADTM = sample(
      seq(lubridate::as_datetime(TRTSDTM), lubridate::as_datetime(TRTENDT), by = "day"),
      size = 1
    )) %>%
    dplyr::select(-TRTENDT) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ADTM)

  adtte <- adtte %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(PARAM = as.factor(.data$PARAM)) %>%
    dplyr::mutate(PARAMCD = as.factor(.data$PARAMCD)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$PARAMCD,
      .data$ADTM
    )
  lbls <- col_labels(adtte)

  # adding adverse event counts and log follow-up time
  tmc_ex_adtte <- dplyr::bind_rows(
    adtte,
    data.frame(adtte %>%
      dplyr::group_by(.data$USUBJID) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::mutate(
        PARAMCD = "TNE",
        PARAM = "Total Number of Exacerbations",
        AVAL = stats::rpois(1, 3),
        AVALU = "COUNT",
        lgTMATRSK = log(stats::rexp(1, rate = 3)),
        dplyr::across(c("ADTM", "EVNTDESC"), ~NA)
      ))
  ) %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$PARAMCD,
      .data$ADTM
    )
  col_labels(tmc_ex_adtte) <- c(lbls, lgTMATRSK = "Log Time At Risk")

  i_lbls <- sapply(
    names(col_labels(tmc_ex_adtte)[is.na(col_labels(tmc_ex_adtte))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_adtte[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_adtte, file = "data/tmc_ex_adtte.rda", compress = "xz")
}
```

## `ADVS`

``` r

generate_advs <- function(adsl = tmc_ex_adsl,
                          n_assessments = 5L,
                          n_days = 5L) {
  set.seed(1)
  param <- c(
    "Diastolic Blood Pressure",
    "Pulse Rate",
    "Respiratory Rate",
    "Systolic Blood Pressure",
    "Temperature", "Weight"
  )
  paramcd <- c("DIABP", "PULSE", "RESP", "SYSBP", "TEMP", "WEIGHT")
  paramu <- c("Pa", "beats/min", "breaths/min", "Pa", "C", "Kg")
  visit_format <- "WEEK"

  param_init_list <- relvar_init(param, paramcd)
  unit_init_list <- relvar_init(param, paramu)

  advs <- expand.grid(
    STUDYID = unique(adsl$STUDYID),
    USUBJID = adsl$USUBJID,
    PARAM = as.factor(param_init_list$relvar1),
    AVISIT = visit_schedule(visit_format = visit_format, n_assessments = n_assessments),
    stringsAsFactors = FALSE
  )

  advs <- dplyr::mutate(
    advs,
    AVISITN = dplyr::case_when(
      AVISIT == "SCREENING" ~ -1,
      AVISIT == "BASELINE" ~ 0,
      (grepl("^WEEK", AVISIT) | grepl("^CYCLE", AVISIT)) ~ as.numeric(AVISIT) - 2,
      TRUE ~ NA_real_
    )
  )

  advs$PARAMCD <- as.factor(rel_var(
    df = advs,
    var_name = "PARAMCD",
    var_values = param_init_list$relvar2,
    related_var = "PARAM"
  ))
  advs$AVALU <- as.factor(rel_var(
    df = advs,
    var_name = "AVALU",
    var_values = unit_init_list$relvar2,
    related_var = "PARAM"
  ))

  advs$AVAL <- stats::rnorm(nrow(advs), mean = 50, sd = 8)
  advs <- advs[order(advs$STUDYID, advs$USUBJID, advs$PARAMCD, advs$AVISITN), ]

  advs <- dplyr::inner_join(
    adsl,
    advs,
    by = c("STUDYID", "USUBJID"),
    multiple = "all"
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(TRTENDT = lubridate::date(dplyr::case_when(
      is.na(TRTEDTM) ~ lubridate::floor_date(lubridate::date(TRTSDTM) + study_duration_secs, unit = "day"),
      TRUE ~ TRTEDTM
    ))) %>%
    dplyr::mutate(ADTM = sample(
      seq(lubridate::as_datetime(TRTSDTM), lubridate::as_datetime(TRTENDT), by = "day"),
      size = 1
    )) %>%
    dplyr::mutate(ADY = ceiling(difftime(ADTM, TRTSDTM, units = "days"))) %>%
    dplyr::select(-TRTENDT) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$ADTM)

  tmc_ex_advs <- advs %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$STUDYID,
      .data$USUBJID,
      .data$PARAMCD,
      .data$AVISITN,
      .data$ADTM
    )

  i_lbls <- sapply(
    names(col_labels(tmc_ex_advs)[is.na(col_labels(tmc_ex_advs))]), function(x) which(names(common_var_labels) == x)
  )
  col_labels(tmc_ex_advs[names(i_lbls)]) <- common_var_labels[i_lbls]

  save(tmc_ex_advs, file = "data/tmc_ex_advs.rda", compress = "xz")
}
```

## Generate Data

``` r

# Generate & load adsl
tmp_fol <- getwd()
setwd(dirname(tmp_fol))
generate_adsl()
load("data/tmc_ex_adsl.rda")

# Generate other datasets
generate_adae()
generate_adaette()
generate_adcm()
generate_adeg()
generate_adex()
generate_adlb()
generate_admh()
generate_adqs()
generate_adrs()
generate_adtte()
generate_advs()

setwd(tmp_fol)
```
