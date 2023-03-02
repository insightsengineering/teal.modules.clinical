# template_shift_by_arm generates correct expressions with default arguments

    Code
      res
    Output
      $data
      {
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          adeg <- df_explicit_na(adeg, na_level = "<Missing>") %>% 
              dplyr::filter(ONTRTFL == "Y")
          attr(adeg$BNRIND, "label") <- "Baseline Assessment"
      }
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by("AVISIT", 
          split_fun = drop_split_levels) %>% rtables::split_cols_by("ANRIND") %>% 
          rtables::split_rows_by("ARM", split_fun = drop_split_levels, 
              label_pos = "topleft", split_label = obj_label(adeg$ARM)) %>% 
          add_rowcounts() %>% summarize_vars("BNRIND", denom = "N_row", 
          na_level = "<Missing>", na.rm = FALSE, .stats = "count_fraction") %>% 
          append_varlabels(adeg, "BNRIND", indent = 1L)
      
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
              dplyr::filter(ONTRTFL == "Y")
          attr(adeg$BNRIND, "label") <- "Baseline Assessment"
      }
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by("AVISIT", 
          split_fun = drop_split_levels) %>% rtables::split_cols_by("ANRIND") %>% 
          rtables::split_rows_by("ARM", split_fun = add_overall_level("All Patients", 
              first = FALSE), label_pos = "topleft", split_label = obj_label(adeg$ARM)) %>% 
          add_rowcounts() %>% summarize_vars("BNRIND", denom = "N_row", 
          na_level = "<Missing>", na.rm = FALSE, .stats = "count_fraction") %>% 
          append_varlabels(adeg, "BNRIND", indent = 1L)
      
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
              dplyr::filter(ONTRTFL == "Y")
          attr(adeg$BNRIND, "label") <- "Baseline Assessment"
      }
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by("AVISIT", 
          split_fun = drop_split_levels) %>% rtables::split_cols_by("ANRIND") %>% 
          rtables::split_rows_by("ARM", split_fun = drop_split_levels, 
              label_pos = "topleft", split_label = obj_label(adeg$ARM)) %>% 
          add_rowcounts() %>% summarize_vars("BNRIND", denom = "N_row", 
          na_level = "<Missing>", na.rm = TRUE, .stats = "count_fraction") %>% 
          append_varlabels(adeg, "BNRIND", indent = 1L)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adeg)
          result
      }
      

