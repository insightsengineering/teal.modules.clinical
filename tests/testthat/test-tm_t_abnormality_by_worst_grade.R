library(scda)
testthat::test_that("template_abnormality_by_worst_grade generates correct expressions with default arguments", {
  result <- template_abnormality_by_worst_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARMCD",
    id_var = "USUBJID",
    paramcd = "PARAMCD",
    atoxgr_var = "ATOXGR",
    worst_high_flag_var = "WGRHIFL",
    worst_low_flag_var = "WGRLOFL",
    worst_flag_indicator = "Y",
    add_total = FALSE,
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl_labels <- formatters::var_labels(adlb, fill = FALSE)
      anl <- adlb %>%
        dplyr::mutate(
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
        dplyr::filter(WGRLOFL == "Y" | WGRHIFL == "Y") %>%
        droplevels()
      formatters::var_labels(anl) <- c(anl_labels, "Direction of Abnormality", "Highest Grade")
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
        dplyr::arrange("PARAMCD", desc(GRADE_DIR), GRADE_ANL)
    }),
    layout = quote(
      lyt <- rtables::basic_table() %>%
        rtables::split_cols_by(var = "ARMCD") %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          "PARAMCD",
          label_pos = "topleft",
          split_label = obj_label(anl[["PARAMCD"]])
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          required = "GRADE_ANL",
          .stats = "unique_count"
        ) %>%
        rtables::split_rows_by(
          "GRADE_DIR",
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map),
          split_label = obj_label(anl$GRADE_DIR)
        ) %>%
        count_abnormal_by_worst_grade(
          var = "GRADE_ANL",
          variables = list(
            id = "USUBJID",
            param = "PARAMCD",
            grade_dir = "GRADE_DIR"
          ),
          .indent_mods = 4L
        ) %>%
        rtables::append_topleft("    Highest Grade")
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_abnormality_by_worst_grade generates correct expressions with custom arguments", {
  result <- template_abnormality_by_worst_grade(
    parentname = "myadsl",
    dataname = "myadlb",
    arm_var = "ARMCD",
    id_var = "USUBJID",
    paramcd = "myPARAMCD",
    atoxgr_var = "ATOXGR",
    worst_high_flag_var = "WGRHIFL",
    worst_low_flag_var = "WGRLOFL",
    worst_flag_indicator = "Y",
    add_total = FALSE,
    drop_arm_levels = TRUE
  )

  expected <- list(
    data = quote({
      anl_labels <- formatters::var_labels(myadlb, fill = FALSE)
      anl <- myadlb %>%
        dplyr::mutate(
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
        dplyr::filter(WGRLOFL == "Y" | WGRHIFL == "Y") %>%
        droplevels()
      formatters::var_labels(anl) <- c(anl_labels, "Direction of Abnormality", "Highest Grade")
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
        dplyr::arrange("myPARAMCD", desc(GRADE_DIR), GRADE_ANL)
    }),
    layout = quote(
      lyt <- rtables::basic_table() %>%
        rtables::split_cols_by(var = "ARMCD") %>%
        rtables::add_colcounts() %>%
        rtables::split_rows_by(
          "myPARAMCD",
          label_pos = "topleft",
          split_label = obj_label(anl[["myPARAMCD"]])
        ) %>%
        summarize_num_patients(
          var = "USUBJID",
          required = "GRADE_ANL",
          .stats = "unique_count"
        ) %>%
        rtables::split_rows_by(
          "GRADE_DIR",
          label_pos = "topleft",
          split_fun = trim_levels_to_map(map = map),
          split_label = obj_label(anl$GRADE_DIR)
        ) %>%
        count_abnormal_by_worst_grade(
          var = "GRADE_ANL",
          variables = list(
            id = "USUBJID",
            param = "myPARAMCD",
            grade_dir = "GRADE_DIR"
          ),
          .indent_mods = 4L
        ) %>%
        rtables::append_topleft("    Highest Grade")
    ),
    table = quote({
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = myadsl)
      result
    })
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("template_abnormality_by_worst_grade throws an error when ATOXGR contains NA values", {
  adsl <- synthetic_cdisc_data("rcd_2021_07_07")$adsl
  adlb <- synthetic_cdisc_data("rcd_2021_07_07")$adlb

  adlb$ATOXGR[1:100] <- NA

  template <- template_abnormality_by_worst_grade(
    parentname = "adsl",
    dataname = "adlb",
    arm_var = "ARMCD",
    id_var = "USUBJID",
    paramcd = "PARAMCD",
    atoxgr_var = "ATOXGR",
    worst_high_flag_var = "WGRHIFL",
    worst_low_flag_var = "WGRLOFL",
    worst_flag_indicator = "Y",
    add_total = FALSE,
    drop_arm_levels = TRUE
  )

  testthat::expect_error(mapply(eval, template))
})
