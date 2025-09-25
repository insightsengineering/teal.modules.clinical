# template_events_summary generates minimal table

    Code
      res
    Output
      $data
      {
          anl <- adae
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(adsl[["ARM"]])
          anl <- anl %>% dplyr::mutate(ARM = factor(ARM, levels = arm_levels))
          study_id <- unique(anl[["STUDYID"]])
          anl$tmp_aefl <- "Y"
          anl[["AEDECOD"]] <- as.character(anl[["AEDECOD"]])
          anl <- anl %>% dplyr::mutate(USUBJID_AESEQ = paste(USUBJID, 
              AESEQ, sep = "@@"))
          anl <- tern::df_explicit_na(anl, na_level = "<Missing>")
          adsl <- tern::df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout_parent
      lyt_parent <- rtables::basic_table(show_colcounts = TRUE) %>% 
          rtables::split_cols_by(var = "ARM") %>% rtables::add_overall_col(label = "All Patients") %>% 
          tern::count_values("DTHFL", values = "Y", .labels = c(count_fraction = "Total number of deaths"), 
              .formats = c(count_fraction = tern::format_count_fraction), 
              denom = "N_col") %>% tern::count_values("DCSREAS", values = "ADVERSE EVENT", 
          .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"), 
          .formats = c(count_fraction = tern::format_count_fraction), 
          denom = "N_col")
      
      $table_parent
      table_parent <- rtables::build_table(lyt = lyt_parent, df = adsl, 
          alt_counts_df = adsl)
      
      $layout_anl
      lyt_anl <- rtables::basic_table(show_colcounts = TRUE) %>% rtables::split_cols_by(var = "ARM") %>% 
          rtables::add_overall_col(label = "All Patients") %>% tern::count_patients_with_event(vars = "USUBJID", 
          filters = c(tmp_aefl = "Y"), denom = "N_col", .stats = "count_fraction", 
          .labels = c(count_fraction = "Total number of patients with at least one adverse event"), 
          .indent_mods = c(count_fraction = 0L), table_names = "total_pts_at_least_one") %>% 
          tern::count_values("STUDYID", values = study_id, .stats = "count", 
              .labels = c(count = "Total AEs"), table_names = "total_aes")
      
      $table_anl
      table_anl <- rtables::build_table(lyt = lyt_anl, df = anl, alt_counts_df = adsl)
      
      $table
      {
          rtables::col_info(table_parent) <- rtables::col_info(table_anl)
          table <- rtables::rbind(table_anl, table_parent)
      }
      

# template_events_summary generates table with multiple flags

    Code
      res
    Output
      $data
      {
          anl <- adae
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(adsl[["ARM"]])
          anl <- anl %>% dplyr::mutate(ARM = factor(ARM, levels = arm_levels))
          study_id <- unique(anl[["STUDYID"]])
          anl$tmp_aefl <- "Y"
          anl[["AEDECOD"]] <- as.character(anl[["AEDECOD"]])
          anl <- anl %>% dplyr::mutate(USUBJID_AESEQ = paste(USUBJID, 
              AESEQ, sep = "@@"))
          flag_var_anl_label <- teal.data::col_labels(anl[, c("A", 
              "B", "C")], fill = FALSE)
          flag_var_aesi_label <- teal.data::col_labels(anl[, c("X", 
              "Y")], fill = FALSE)
          anl <- tern::df_explicit_na(anl, na_level = "<Missing>")
          adsl <- tern::df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout_parent
      lyt_parent <- rtables::basic_table(show_colcounts = TRUE) %>% 
          rtables::split_cols_by(var = "ARM") %>% rtables::add_overall_col(label = "All Patients")
      
      $table_parent
      table_parent <- rtables::build_table(lyt = lyt_parent, df = adsl, 
          alt_counts_df = adsl)
      
      $layout_anl
      lyt_anl <- rtables::basic_table(show_colcounts = TRUE) %>% rtables::split_cols_by(var = "ARM") %>% 
          rtables::add_overall_col(label = "All Patients") %>% tern::count_patients_with_event(vars = "USUBJID", 
          filters = c(tmp_aefl = "Y"), denom = "N_col", .stats = "count_fraction", 
          .labels = c(count_fraction = "Total number of patients with at least one adverse event"), 
          .indent_mods = c(count_fraction = 0L), table_names = "total_pts_at_least_one") %>% 
          tern::count_values("STUDYID", values = study_id, .stats = "count", 
              .labels = c(count = "Total AEs"), table_names = "total_aes") %>% 
          tern::count_patients_with_flags(var = "USUBJID", flag_variables = flag_var_anl_label, 
              table_names = "count_subj_anl", denom = "N_col", var_labels = "Total number of patients with at least one", 
              show_labels = "visible") %>% tern::count_patients_with_flags(var = "AEDECOD", 
          flag_variables = flag_var_anl_label, table_names = "count_pt_anl", 
          .stats = "count", .formats = c(count = "xx"), denom = "N_col", 
          var_labels = "Total number of unique preferred terms which are", 
          show_labels = "visible") %>% tern::count_patients_with_flags(var = "USUBJID_AESEQ", 
          flag_variables = flag_var_anl_label, table_names = "count_events_anl", 
          .stats = "count", .formats = c(count = "xx"), denom = "N_col", 
          var_labels = "Total number of adverse events which are", 
          show_labels = "visible") %>% tern::count_patients_with_flags(var = "USUBJID", 
          flag_variables = flag_var_aesi_label, table_names = "count_subj_aesi", 
          denom = "N_col", var_labels = "Medical concepts: number of patients with", 
          show_labels = "visible") %>% tern::count_patients_with_flags(var = "AEDECOD", 
          flag_variables = flag_var_aesi_label, table_names = "count_pt_aesi", 
          .stats = "count", .formats = c(count = "xx"), denom = "N_col", 
          var_labels = "Medical concepts: number of unique preferred terms which are part of", 
          show_labels = "visible") %>% tern::count_patients_with_flags(var = "USUBJID_AESEQ", 
          flag_variables = flag_var_aesi_label, table_names = "count_events_aesi", 
          .stats = "count", .formats = c(count = "xx"), denom = "N_col", 
          var_labels = "Medical concepts: number of adverse events which are part of", 
          show_labels = "visible")
      
      $table_anl
      table_anl <- rtables::build_table(lyt = lyt_anl, df = anl, alt_counts_df = adsl)
      
      $table
      {
          rtables::col_info(table_parent) <- rtables::col_info(table_anl)
          table <- rtables::rbind(table_anl[1:2, ], table_anl[3:nrow(table_anl), 
              ])
      }
      

