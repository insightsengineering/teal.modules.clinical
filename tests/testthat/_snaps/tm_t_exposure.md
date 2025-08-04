# template_exposure generates correct expressions with default arguments

    Code
      res
    Output
      $data
      {
          anl <- adex
          anl <- tern::df_explicit_na(anl, na_level = "<Missing>")
      }
      
      $layout_prep
      split_fun <- rtables::drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, main_footer = "* Patient time is the sum of TDURD") %>% 
          rtables::split_cols_by("SEX") %>% tern::analyze_patients_exposure_in_cols(var = "RACE", 
          ex_var = "AVAL", col_split = TRUE, add_total_level = TRUE, 
          na_str = "<Missing>", .labels = c(n_patients = "Number of Patients", 
              sum_exposure = ifelse("Days" == " ", paste("Sum of", 
                  "TDURD"), paste("Sum of", "TDURD", sprintf("(%s)", 
                  "Days")))), custom_label = "Total number of patients and patient time*") %>% 
          tern::analyze_patients_exposure_in_cols(var = "RACE", col_split = FALSE, 
              na_str = "<Missing>") %>% rtables::append_topleft(c(teal.data::col_labels(adex["RACE"], 
          fill = TRUE)))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          table <- rtables::prune_table(table)
      }
      

# template_exposure generates correct expressions with custom arguments

    Code
      res
    Output
      $data
      {
          anl <- myadex
          anl <- tern::df_explicit_na(anl, na_level = "<myMissing>")
      }
      
      $layout_prep
      split_fun <- rtables::drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, main_footer = "* Patient time is the sum of myTDURD") %>% 
          rtables::split_cols_by("SEX") %>% tern::analyze_patients_exposure_in_cols(var = "myRACE", 
          ex_var = "myAVAL", col_split = TRUE, add_total_level = TRUE, 
          na_str = "<myMissing>", .labels = c(n_patients = "Number of Patients", 
              sum_exposure = ifelse("Days" == " ", paste("Sum of", 
                  "myTDURD"), paste("Sum of", "myTDURD", sprintf("(%s)", 
                  "Days")))), custom_label = "Total number of patients and patient time*") %>% 
          tern::analyze_patients_exposure_in_cols(var = "myRACE", col_split = FALSE, 
              na_str = "<myMissing>") %>% rtables::append_topleft(c(teal.data::col_labels(myadex["myRACE"], 
          fill = TRUE)))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = myadsl)
          table <- rtables::prune_table(table)
      }
      

# template_exposure generates correct expressions with paramcd_label

    Code
      res
    Output
      $data
      {
          anl <- adex
          anl <- tern::df_explicit_na(anl, na_level = "<Missing>")
      }
      
      $layout_prep
      split_fun <- rtables::drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, main_footer = "* Patient time is the sum of Total Duration (Days)") %>% 
          rtables::split_cols_by("SEX") %>% tern::analyze_patients_exposure_in_cols(var = "RACE", 
          ex_var = "AVAL", col_split = TRUE, add_total_level = TRUE, 
          na_str = "<Missing>", .labels = c(n_patients = "Number of Patients", 
              sum_exposure = ifelse("Days" == " ", paste("Sum of", 
                  "TDURD"), paste("Sum of", "TDURD", sprintf("(%s)", 
                  "Days")))), custom_label = "Total number of patients and patient time*") %>% 
          tern::analyze_patients_exposure_in_cols(var = "RACE", col_split = FALSE, 
              na_str = "<Missing>") %>% rtables::append_topleft(c(teal.data::col_labels(adex["RACE"], 
          fill = TRUE)))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          table <- rtables::prune_table(table)
      }
      

