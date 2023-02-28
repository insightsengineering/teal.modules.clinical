# template_mult_events generates correct expressions with 1 HLT parameter

    Code
      res
    Output
      $data
      {
          anl <- adcm
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("ATC1", "CMDECOD")))
          anl <- anl %>% dplyr::mutate(ASEQ = as.factor(ASEQ))
          adsl <- df_explicit_na(adsl, na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARM") %>% 
          rtables::add_colcounts() %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% rtables::split_rows_by("ATC1", 
          child_labels = "visible", nested = FALSE, indent_mod = -1L, 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC1"])) %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% count_occurrences(vars = "CMDECOD", 
          .indent_mods = -1L) %>% append_varlabels(adcm, "CMDECOD", 
          indent = 1L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $table_sorted
      {
          sorted_result <- result %>% sort_at_path(path = c("ATC1", 
              "*", "CMDECOD"), scorefun = score_occurrences)
      }
      
      $final_table
      {
          result <- sorted_result
          result
      }
      

# template_mult_events generates correct expressions with 2 HLT parameters and drop_arm_levels = FALSE

    Code
      res
    Output
      $data
      {
          anl <- adcm
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(adsl[["ARM"]])
          anl <- anl %>% dplyr::mutate(ARM = factor(ARM, levels = arm_levels))
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("ATC1", "ATC2", "CMDECOD")))
          anl <- anl %>% dplyr::mutate(ASEQ = as.factor(ASEQ))
          adsl <- df_explicit_na(adsl, na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARM") %>% 
          rtables::add_colcounts() %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% rtables::split_rows_by("ATC1", 
          child_labels = "visible", nested = FALSE, indent_mod = -1L, 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC1"])) %>% 
          rtables::split_rows_by("ATC2", child_labels = "visible", 
              nested = TRUE, indent_mod = 0L, split_fun = split_fun, 
              label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC2"])) %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% count_occurrences(vars = "CMDECOD", 
          .indent_mods = -1L) %>% append_varlabels(adcm, "CMDECOD", 
          indent = 2L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $table_sorted
      {
          sorted_result <- result %>% sort_at_path(path = c("ATC1", 
              "*", "ATC2", "*", "CMDECOD"), scorefun = score_occurrences)
      }
      
      $final_table
      {
          result <- sorted_result
          result
      }
      

# template_mult_events generates correct expressions with 3 HLT parameters

    Code
      res
    Output
      $data
      {
          anl <- adcm
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("ATC1", "ATC2", "ATC3", "CMDECOD")))
          anl <- anl %>% dplyr::mutate(ASEQ = as.factor(ASEQ))
          adsl <- df_explicit_na(adsl, na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARM") %>% 
          rtables::add_colcounts() %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% rtables::split_rows_by("ATC1", 
          child_labels = "visible", nested = FALSE, indent_mod = -1L, 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC1"])) %>% 
          rtables::split_rows_by("ATC2", child_labels = "visible", 
              nested = TRUE, indent_mod = 0L, split_fun = split_fun, 
              label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC2"])) %>% 
          rtables::split_rows_by("ATC3", child_labels = "visible", 
              nested = TRUE, indent_mod = 0L, split_fun = split_fun, 
              label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC3"])) %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% count_occurrences(vars = "CMDECOD", 
          .indent_mods = -1L) %>% append_varlabels(adcm, "CMDECOD", 
          indent = 3L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $table_sorted
      {
          sorted_result <- result %>% sort_at_path(path = c("ATC1", 
              "*", "ATC2", "*", "ATC3", "*", "CMDECOD"), scorefun = score_occurrences)
      }
      
      $final_table
      {
          result <- sorted_result
          result
      }
      

# template_mult_events generates correct expressions with 4 HLT parameters

    Code
      res
    Output
      $data
      {
          anl <- adcm
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("ATC1", "ATC2", "ATC3", "ATC4", "CMDECOD")))
          anl <- anl %>% dplyr::mutate(ASEQ = as.factor(ASEQ))
          adsl <- df_explicit_na(adsl, na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARM") %>% 
          rtables::add_colcounts() %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% rtables::split_rows_by("ATC1", 
          child_labels = "visible", nested = FALSE, indent_mod = -1L, 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC1"])) %>% 
          rtables::split_rows_by("ATC2", child_labels = "visible", 
              nested = TRUE, indent_mod = 0L, split_fun = split_fun, 
              label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC2"])) %>% 
          rtables::split_rows_by("ATC3", child_labels = "visible", 
              nested = TRUE, indent_mod = 0L, split_fun = split_fun, 
              label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC3"])) %>% 
          rtables::split_rows_by("ATC4", child_labels = "visible", 
              nested = TRUE, indent_mod = 0L, split_fun = split_fun, 
              label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC4"])) %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% count_occurrences(vars = "CMDECOD", 
          .indent_mods = -1L) %>% append_varlabels(adcm, "CMDECOD", 
          indent = 4L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $table_sorted
      {
          sorted_result <- result %>% sort_at_path(path = c("ATC1", 
              "*", "ATC2", "*", "ATC3", "*", "ATC4", "*", "CMDECOD"), 
              scorefun = score_occurrences)
      }
      
      $final_table
      {
          result <- sorted_result
          result
      }
      

# template_mult_events generates correct expressions with no HLT parameters

    Code
      res
    Output
      $data
      {
          anl <- adcm
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              "CMDECOD"))
          anl <- anl %>% dplyr::mutate(ASEQ = as.factor(ASEQ))
          adsl <- df_explicit_na(adsl, na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARM") %>% 
          rtables::add_colcounts() %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% count_occurrences(vars = "CMDECOD", 
          .indent_mods = -1L) %>% append_varlabels(adcm, "CMDECOD", 
          indent = 0L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $table_sorted
      {
          sorted_result <- result %>% sort_at_path(path = "CMDECOD", 
              scorefun = score_occurrences)
      }
      
      $final_table
      {
          result <- sorted_result
          result
      }
      

# template_mult_events generates correct expressions with 1 HLT parameter and without 'All Patients' column

    Code
      res
    Output
      $data
      {
          anl <- adcm
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("ATC1", "CMDECOD")))
          anl <- anl %>% dplyr::mutate(ASEQ = as.factor(ASEQ))
          adsl <- df_explicit_na(adsl, na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARM") %>% 
          rtables::add_colcounts() %>% summarize_num_patients(var = "USUBJID", 
          count_by = "ASEQ", .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
              nonunique = "Total number of treatments")) %>% rtables::split_rows_by("ATC1", 
          child_labels = "visible", nested = FALSE, indent_mod = -1L, 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adcm["ATC1"])) %>% 
          summarize_num_patients(var = "USUBJID", count_by = "ASEQ", 
              .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
                  nonunique = "Total number of treatments")) %>% count_occurrences(vars = "CMDECOD", 
          .indent_mods = -1L) %>% append_varlabels(adcm, "CMDECOD", 
          indent = 1L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $table_sorted
      {
          sorted_result <- result %>% sort_at_path(path = c("ATC1", 
              "*", "CMDECOD"), scorefun = score_occurrences)
      }
      
      $final_table
      {
          result <- sorted_result
          result
      }
      

