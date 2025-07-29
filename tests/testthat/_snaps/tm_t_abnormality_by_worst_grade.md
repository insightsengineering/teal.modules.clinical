# template_abnormality_by_worst_grade generates correct expressions with default arguments

    Code
      res
    Output
      $data
      {
          anl_labels <- teal.data::col_labels(adlb, fill = FALSE)
          anl <- adlb %>% dplyr::mutate(GRADE_DIR = factor(dplyr::case_when(as.numeric(as.character(ATOXGR)) < 
              0 ~ "LOW", ATOXGR == "0" ~ "ZERO", as.numeric(as.character(ATOXGR)) > 
              0 ~ "HIGH"), levels = c("LOW", "ZERO", "HIGH")), GRADE_ANL = factor(abs(as.numeric(as.character(ATOXGR))))) %>% 
              dplyr::filter(WGRLOFL == "Y" | WGRHIFL == "Y") %>% droplevels()
          teal.data::col_labels(anl) <- c(anl_labels, GRADE_DIR = "   Direction of Abnormality", 
              GRADE_ANL = "Highest Grade")
          anl <- anl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          arm_levels <- levels(anl[["ARMCD"]])
          adsl <- adsl %>% dplyr::filter(ARMCD %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          if (is.null(formatters::obj_label(anl[["PARAMCD"]]))) {
              stop("Please specify label for ", "PARAMCD")
          }
      }
      
      $layout_prep
      {
          map <- expand.grid(PARAM = levels(anl[["PARAMCD"]]), GRADE_DIR = c("LOW", 
              "HIGH"), GRADE_ANL = as.character(1:4), stringsAsFactors = FALSE) %>% 
              dplyr::arrange("PARAMCD", desc(GRADE_DIR), GRADE_ANL)
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE) %>% rtables::split_cols_by(var = "ARMCD") %>% 
          rtables::split_rows_by("PARAMCD", label_pos = "topleft", 
              split_label = formatters::obj_label(anl[["PARAMCD"]])) %>% 
          tern::summarize_num_patients(var = "USUBJID", required = "GRADE_ANL", 
              .stats = "unique_count") %>% rtables::split_rows_by("GRADE_DIR", 
          label_pos = "topleft", split_fun = rtables::trim_levels_to_map(map = map), 
          split_label = formatters::obj_label(anl$GRADE_DIR)) %>% count_abnormal_by_worst_grade(var = "GRADE_ANL", 
          variables = list(id = "USUBJID", param = "PARAMCD", grade_dir = "GRADE_DIR"), 
          .indent_mods = 4L) %>% rtables::append_topleft("                                  Highest Grade")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_abnormality_by_worst_grade generates correct expressions with custom arguments

    Code
      res
    Output
      $data
      {
          anl_labels <- teal.data::col_labels(myadlb, fill = FALSE)
          anl <- myadlb %>% dplyr::mutate(GRADE_DIR = factor(dplyr::case_when(as.numeric(as.character(ATOXGR)) < 
              0 ~ "LOW", ATOXGR == "0" ~ "ZERO", as.numeric(as.character(ATOXGR)) > 
              0 ~ "HIGH"), levels = c("LOW", "ZERO", "HIGH")), GRADE_ANL = factor(abs(as.numeric(as.character(ATOXGR))))) %>% 
              dplyr::filter(WGRLOFL == "Y" | WGRHIFL == "Y") %>% droplevels()
          teal.data::col_labels(anl) <- c(anl_labels, GRADE_DIR = "   Direction of Abnormality", 
              GRADE_ANL = "Highest Grade")
          anl <- anl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          arm_levels <- levels(anl[["ARMCD"]])
          myadsl <- myadsl %>% dplyr::filter(ARMCD %in% arm_levels)
          myadsl <- myadsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          if (is.null(formatters::obj_label(anl[["myPARAMCD"]]))) {
              stop("Please specify label for ", "myPARAMCD")
          }
      }
      
      $layout_prep
      {
          map <- expand.grid(PARAM = levels(anl[["myPARAMCD"]]), GRADE_DIR = c("LOW", 
              "HIGH"), GRADE_ANL = as.character(1:4), stringsAsFactors = FALSE) %>% 
              dplyr::arrange("myPARAMCD", desc(GRADE_DIR), GRADE_ANL)
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE) %>% rtables::split_cols_by(var = "ARMCD") %>% 
          rtables::split_rows_by("myPARAMCD", label_pos = "topleft", 
              split_label = formatters::obj_label(anl[["myPARAMCD"]])) %>% 
          tern::summarize_num_patients(var = "USUBJID", required = "GRADE_ANL", 
              .stats = "unique_count") %>% rtables::split_rows_by("GRADE_DIR", 
          label_pos = "topleft", split_fun = rtables::trim_levels_to_map(map = map), 
          split_label = formatters::obj_label(anl$GRADE_DIR)) %>% count_abnormal_by_worst_grade(var = "GRADE_ANL", 
          variables = list(id = "USUBJID", param = "myPARAMCD", grade_dir = "GRADE_DIR"), 
          .indent_mods = 4L) %>% rtables::append_topleft("                                  Highest Grade")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = myadsl)
      }
      

