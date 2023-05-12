# template_exposure generates correct expressions with default arguments

    Code
      res
    Output
      $data
      {
          anl <- adex
          anl <- df_explicit_na(anl, na_level = "<Missing>")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(main_footer = "* Patient Time is the sum of TDURD") %>% 
          rtables::split_cols_by("SEX") %>% rtables::add_colcounts() %>% 
          summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE, 
              .labels = c(n_patients = "Number of Patients", sum_exposure = ifelse("Days" == 
                  " ", paste("Sum of", "TDURD"), paste("Sum of", "TDURD", 
                  sprintf("(%s)", "Days")))), custom_label = "Total Number of Patients and Patient Time*") %>% 
          analyze_patients_exposure_in_cols(var = "RACE", col_split = FALSE) %>% 
          append_topleft(c(formatters::var_labels(adex["RACE"], fill = TRUE)))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          rtables::prune_table(result)
      }
      

# template_exposure generates correct expressions with custom arguments

    Code
      res
    Output
      $data
      {
          anl <- myadex
          anl <- df_explicit_na(anl, na_level = "<myMissing>")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(main_footer = "* Patient Time is the sum of myTDURD") %>% 
          rtables::split_cols_by("SEX") %>% rtables::add_colcounts() %>% 
          summarize_patients_exposure_in_cols(var = "myAVAL", col_split = TRUE, 
              .labels = c(n_patients = "Number of Patients", sum_exposure = ifelse("Days" == 
                  " ", paste("Sum of", "myTDURD"), paste("Sum of", 
                  "myTDURD", sprintf("(%s)", "Days")))), custom_label = "Total Number of Patients and Patient Time*") %>% 
          analyze_patients_exposure_in_cols(var = "myRACE", col_split = FALSE) %>% 
          append_topleft(c(formatters::var_labels(myadex["myRACE"], 
              fill = TRUE)))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = myadsl)
          rtables::prune_table(result)
      }
      

# template_exposure generates correct expressions with paramcd_label

    Code
      res
    Output
      $data
      {
          anl <- adex
          anl <- df_explicit_na(anl, na_level = "<Missing>")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(main_footer = "* Patient Time is the sum of Total Duration (Days)") %>% 
          rtables::split_cols_by("SEX") %>% rtables::add_colcounts() %>% 
          summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE, 
              .labels = c(n_patients = "Number of Patients", sum_exposure = ifelse("Days" == 
                  " ", paste("Sum of", "TDURD"), paste("Sum of", "TDURD", 
                  sprintf("(%s)", "Days")))), custom_label = "Total Number of Patients and Patient Time*") %>% 
          analyze_patients_exposure_in_cols(var = "RACE", col_split = FALSE) %>% 
          append_topleft(c(formatters::var_labels(adex["RACE"], fill = TRUE)))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          rtables::prune_table(result)
      }
      

