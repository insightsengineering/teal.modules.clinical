# template_shift_by_arm generates correct expressions with default arguments

    Code
      res
    Output
      $data
      {
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>% 
              dplyr::filter(ONTRTFL == "Y", WORS02FL == "Y") %>% dplyr::mutate(postbaseline_label = "Post-Baseline")
          attr(adeg$BASEC, "label") <- "Baseline Assessment"
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Shift by Arm by Worst Table") %>% 
          rtables::split_cols_by("postbaseline_label", split_fun = drop_split_levels) %>% 
          rtables::split_cols_by("AVALC") %>% rtables::split_rows_by("ARM", 
          split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adeg$ARM)) %>% 
          add_rowcounts() %>% summarize_vars("BASEC", denom = "N_row", 
          na_level = "<Missing>", na.rm = FALSE, .stats = "count_fraction") %>% 
          append_varlabels(adeg, "BASEC", indent = 1L)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adeg)
          result
      }
      

# template_shift_by_arm generates correct expressions with add_total being TRUE

    Code
      res
    Output
      $data
      {
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>% 
              dplyr::filter(ONTRTFL == "Y", WORS02FL == "Y") %>% dplyr::mutate(postbaseline_label = "Post-Baseline")
          attr(adeg$BASEC, "label") <- "Baseline Assessment"
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Shift by Arm by Worst Table") %>% 
          rtables::split_cols_by("postbaseline_label", split_fun = drop_split_levels) %>% 
          rtables::split_cols_by("AVALC") %>% rtables::split_rows_by("ARM", 
          split_fun = add_overall_level("All Patients", first = FALSE), 
          label_pos = "topleft", split_label = obj_label(adeg$ARM)) %>% 
          add_rowcounts() %>% summarize_vars("BASEC", denom = "N_row", 
          na_level = "<Missing>", na.rm = FALSE, .stats = "count_fraction") %>% 
          append_varlabels(adeg, "BASEC", indent = 1L)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adeg)
          result
      }
      

# template_shift_by_arm generates correct expressions with na.rm being TRUE

    Code
      res
    Output
      $data
      {
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>% 
              dplyr::filter(ONTRTFL == "Y", WORS02FL == "Y") %>% dplyr::mutate(postbaseline_label = "Post-Baseline")
          attr(adeg$BASEC, "label") <- "Baseline Assessment"
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Shift by Arm by Worst Table") %>% 
          rtables::split_cols_by("postbaseline_label", split_fun = drop_split_levels) %>% 
          rtables::split_cols_by("AVALC") %>% rtables::split_rows_by("ARM", 
          split_fun = drop_split_levels, label_pos = "topleft", split_label = obj_label(adeg$ARM)) %>% 
          add_rowcounts() %>% summarize_vars("BASEC", denom = "N_row", 
          na_level = "<Missing>", na.rm = TRUE, .stats = "count_fraction") %>% 
          append_varlabels(adeg, "BASEC", indent = 1L)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adeg)
          result
      }
      

