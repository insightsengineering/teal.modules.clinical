# template_shift_by_arm generates correct expressions with default arguments

    Code
      res
    Output
      $data
      {
          adsl <- tern::df_explicit_na(adsl, na_level = "<Missing>")
          adeg <- tern::df_explicit_na(adeg, na_level = "<Missing>") %>% 
              dplyr::filter(ONTRTFL == "Y", WORS02FL == "Y") %>% dplyr::mutate(postbaseline_label = "Post-Baseline")
          attr(adeg$BASEC, "label") <- "Baseline Assessment"
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Shift by Arm by Worst Table") %>% 
          rtables::split_cols_by("postbaseline_label", split_fun = rtables::drop_split_levels) %>% 
          rtables::split_cols_by("AVALC") %>% rtables::split_rows_by("ARM", 
          split_fun = rtables::drop_split_levels, label_pos = "topleft", 
          split_label = formatters::obj_label(adeg$ARM)) %>% tern::add_rowcounts() %>% 
          tern::analyze_vars("BASEC", denom = "N_row", na_str = "<Missing>", 
              na.rm = FALSE, .stats = "count_fraction") %>% tern::append_varlabels(adeg, 
          "BASEC", indent = 1L)
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = adeg)
      }
      

# template_shift_by_arm generates correct expressions with add_total being TRUE

    Code
      res
    Output
      $data
      {
          adsl <- tern::df_explicit_na(adsl, na_level = "<Missing>")
          adeg <- tern::df_explicit_na(adeg, na_level = "<Missing>") %>% 
              dplyr::filter(ONTRTFL == "Y", WORS02FL == "Y") %>% dplyr::mutate(postbaseline_label = "Post-Baseline")
          attr(adeg$BASEC, "label") <- "Baseline Assessment"
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Shift by Arm by Worst Table") %>% 
          rtables::split_cols_by("postbaseline_label", split_fun = rtables::drop_split_levels) %>% 
          rtables::split_cols_by("AVALC") %>% rtables::split_rows_by("ARM", 
          split_fun = rtables::add_overall_level("All Patients", first = FALSE), 
          label_pos = "topleft", split_label = formatters::obj_label(adeg$ARM)) %>% 
          tern::add_rowcounts() %>% tern::analyze_vars("BASEC", denom = "N_row", 
          na_str = "<Missing>", na.rm = FALSE, .stats = "count_fraction") %>% 
          tern::append_varlabels(adeg, "BASEC", indent = 1L)
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = adeg)
      }
      

# template_shift_by_arm generates correct expressions with na.rm being TRUE

    Code
      res
    Output
      $data
      {
          adsl <- tern::df_explicit_na(adsl, na_level = "<Missing>")
          adeg <- tern::df_explicit_na(adeg, na_level = "<Missing>") %>% 
              dplyr::filter(ONTRTFL == "Y", WORS02FL == "Y") %>% dplyr::mutate(postbaseline_label = "Post-Baseline")
          attr(adeg$BASEC, "label") <- "Baseline Assessment"
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Shift by Arm by Worst Table") %>% 
          rtables::split_cols_by("postbaseline_label", split_fun = rtables::drop_split_levels) %>% 
          rtables::split_cols_by("AVALC") %>% rtables::split_rows_by("ARM", 
          split_fun = rtables::drop_split_levels, label_pos = "topleft", 
          split_label = formatters::obj_label(adeg$ARM)) %>% tern::add_rowcounts() %>% 
          tern::analyze_vars("BASEC", denom = "N_row", na_str = "<Missing>", 
              na.rm = TRUE, .stats = "count_fraction") %>% tern::append_varlabels(adeg, 
          "BASEC", indent = 1L)
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = adeg)
      }
      

