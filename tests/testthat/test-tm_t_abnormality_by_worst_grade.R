library(scda)
test_that("template_abnormality_by_worst_grade generates correct expressions with default arguments", {

  result <- template_abnormality_by_worst_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARMCD",
    id_var = "USUBJID",
    paramcd = "PARAMCD",
    anrind_var = "ANRIND",
    atoxgr_var = "ATOXGR",
    worst_high_flag_var = "WGRHIFL",
    worst_low_flag_var = "WGRLOFL",
    worst_flag_indicator = "Y",
    add_total = FALSE,
    drop_arm_levels = TRUE
    )

  expected <- list(
    data = quote({
      anl_labels <- var_labels(adlb)
      anl <- adlb %>% mutate(
        WGRLOFL = case_when(WGRLOFL == "Y" ~ TRUE, TRUE ~ FALSE),
        WGRHIFL = case_when(WGRHIFL == "Y" ~ TRUE, TRUE ~ FALSE),
        GRADE_DIR = factor(
          case_when(
            as.numeric(as.character(ATOXGR)) < 0 ~ "LOW",
            ATOXGR == "0" ~ "ZERO",
            as.numeric(as.character(ATOXGR)) > 0 ~ "HIGH"
          ),
          levels = c("LOW", "ZERO", "HIGH")
        ),
        GRADE_ANL = factor(abs(as.numeric(as.character(ATOXGR))))
      ) %>%
        filter(WGRLOFL == TRUE | WGRHIFL == TRUE) %>%
        droplevels()
      var_labels(anl) <- c(anl_labels, "GRADE_DIR", "GRADE_ANL")
      anl <- anl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
      arm_levels <- levels(anl[["ARMCD"]])
      adsl <- adsl %>% dplyr::filter(ARMCD %in% arm_levels)
      adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
    }),
    layout_prep = quote({
      map <- unique(
        anl[anl[["GRADE_DIR"]] != "ZERO", c("PARAMCD", "GRADE_DIR", "GRADE_ANL")]
        ) %>%
        lapply(as.character) %>%
        as.data.frame() %>%
        arrange("PARAMCD", desc(GRADE_DIR), GRADE_ANL)
    }),
    layout = quote(lyt <- basic_table() %>% split_cols_by(var = "ARMCD") %>%
      add_colcounts() %>% split_rows_by(
        "PARAMCD",
        label_pos = "topleft",
        split_label = obj_label(anl[["PARAMCD"]])
      ) %>% summarize_num_patients(
        var = "USUBJID",
        required = "GRADE_ANL",
        .stats = "unique_count"
      ) %>% split_rows_by(
        "GRADE_DIR",
        label_pos = "topleft",
        split_fun = trim_levels_to_map(map = map),
        split_label = "Direction of Abnormality"
      ) %>% count_abnormal_by_worst_grade(
        var = "GRADE_ANL",
        variables = list(id = "USUBJID", param = "PARAMCD", anrind = "GRADE_DIR")
      ) %>%
      append_topleft("    Highest NCI CTCAE Grade")),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_abnormality_by_worst_grade generates correct expressions with custom arguments", {
  result <- template_abnormality_by_worst_grade(
    parentname = "myadsl",
    dataname = "myadlb",
    arm_var = "ARMCD",
    id_var = "USUBJID",
    paramcd = "myPARAMCD",
    anrind_var = "ANRIND",
    atoxgr_var = "ATOXGR",
    worst_high_flag_var = "WGRHIFL",
    worst_low_flag_var = "WGRLOFL",
    worst_flag_indicator = "Y",
    add_total = FALSE,
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl_labels <- var_labels(myadlb)
      anl <- myadlb %>% mutate(
        WGRLOFL = case_when(WGRLOFL == "Y" ~ TRUE, TRUE ~ FALSE),
        WGRHIFL = case_when(WGRHIFL == "Y" ~ TRUE, TRUE ~ FALSE),
        GRADE_DIR = factor(
          case_when(
            as.numeric(as.character(ATOXGR)) < 0 ~ "LOW",
            ATOXGR == "0" ~ "ZERO",
            as.numeric(as.character(ATOXGR)) > 0 ~ "HIGH"
          ),
          levels = c("LOW", "ZERO", "HIGH")
        ),
        GRADE_ANL = factor(abs(as.numeric(as.character(ATOXGR))))
      ) %>%
        filter(WGRLOFL == TRUE | WGRHIFL == TRUE) %>%
        droplevels()
      var_labels(anl) <- c(anl_labels, "GRADE_DIR", "GRADE_ANL")
      anl <- anl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
      arm_levels <- levels(anl[["ARMCD"]])
      myadsl <- myadsl %>% dplyr::filter(ARMCD %in% arm_levels)
      myadsl <- myadsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
    }),
    layout_prep = quote({
      map <- unique(
        anl[anl[["GRADE_DIR"]] != "ZERO", c("myPARAMCD", "GRADE_DIR", "GRADE_ANL")]
      ) %>%
        lapply(as.character) %>%
        as.data.frame() %>%
        arrange("myPARAMCD", desc(GRADE_DIR), GRADE_ANL)
    }),
    layout = quote(lyt <- basic_table() %>% split_cols_by(var = "ARMCD") %>%
                     add_colcounts() %>% split_rows_by(
                       "myPARAMCD",
                       label_pos = "topleft",
                       split_label = obj_label(anl[["myPARAMCD"]])
                     ) %>% summarize_num_patients(
                       var = "USUBJID",
                       required = "GRADE_ANL",
                       .stats = "unique_count"
                     ) %>% split_rows_by(
                       "GRADE_DIR",
                       label_pos = "topleft",
                       split_fun = trim_levels_to_map(map = map),
                       split_label = "Direction of Abnormality"
                     ) %>% count_abnormal_by_worst_grade(
                       var = "GRADE_ANL",
                       variables = list(id = "USUBJID", param = "myPARAMCD", anrind = "GRADE_DIR")
                     ) %>%
                     append_topleft("    Highest NCI CTCAE Grade")),
    table = quote({
      result <- build_table(lyt = lyt, df = anl, alt_counts_df = myadsl)
      result
    })
  )
  expect_equal(result, expected)
})

test_that("template_abnormality_by_worst_grade throws an error when ATOXGR contains NA values", {
  adsl <- synthetic_cdisc_data("rcd_2021_07_07")$adsl
  adlb <- synthetic_cdisc_data("rcd_2021_07_07")$adlb

  adlb$ATOXGR[1:2000] <- NA

  template <- template_abnormality_by_worst_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARMCD",
    id_var = "USUBJID",
    paramcd = "PARAMCD",
    anrind_var = "ANRIND",
    atoxgr_var = "ATOXGR",
    worst_high_flag_var = "WGRHIFL",
    worst_low_flag_var = "WGRLOFL",
    worst_flag_indicator = "Y",
    add_total = FALSE,
    drop_arm_levels = TRUE
  )

  expect_error(mapply(eval, template))

})
