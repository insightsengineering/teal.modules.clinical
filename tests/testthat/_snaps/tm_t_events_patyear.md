# template_events_patyear generates standard expressions

    Code
      res
    Output
      $data
      {
          anl <- adaette
          anl <- anl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          arm_levels <- levels(anl[["ARMCD"]])
          adsl <- adsl %>% dplyr::filter(ARMCD %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          anl <- df_explicit_na(anl, na_level = "")
          adsl <- df_explicit_na(adsl, na_level = "")
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Event Rates Adjusted for Patient-Years by Time to First Occurrence of any Adverse Event", 
          main_footer = "CI Method: Normal (rate)") %>% rtables::split_cols_by(var = "ARMCD") %>% 
          rtables::add_colcounts() %>% rtables::add_overall_col(label = "All Patients") %>% 
          estimate_incidence_rate(vars = "AVAL", n_events = "n_events", 
              control = control_incidence_rate(conf_level = 0.95, conf_type = "normal", 
                  time_unit_input = "year", time_unit_output = 1))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          result
      }
      

# template_events_patyear generates right expressions with non-default

    Code
      res
    Output
      $data
      {
          anl <- adaette
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(adsl[["ARM"]])
          anl <- anl %>% dplyr::mutate(ARM = factor(ARM, levels = arm_levels))
          anl <- df_explicit_na(anl, na_level = "")
          adsl <- df_explicit_na(adsl, na_level = "")
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Event Rates Adjusted for Patient-Years by Time to First Occurrence of any Adverse Event", 
          main_footer = "CI Method: Normal (rate)") %>% rtables::split_cols_by(var = "ARM") %>% 
          rtables::add_colcounts() %>% estimate_incidence_rate(vars = "AVAL", 
          n_events = "n_events", control = control_incidence_rate(conf_level = 0.95, 
              conf_type = "normal", time_unit_input = "year", time_unit_output = 1))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          result
      }
      

# template_events_patyear generates right expressions with non-default controls

    Code
      res
    Output
      $data
      {
          anl <- adaette
          anl <- anl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          arm_levels <- levels(anl[["ARMCD"]])
          adsl <- adsl %>% dplyr::filter(ARMCD %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          anl <- df_explicit_na(anl, na_level = "")
          adsl <- df_explicit_na(adsl, na_level = "")
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Event Rates Adjusted for Patient-Years by Time to First Occurrence of any Adverse Event", 
          main_footer = "CI Method: Exact") %>% rtables::split_cols_by(var = "ARMCD") %>% 
          rtables::add_colcounts() %>% rtables::add_overall_col(label = "All Patients") %>% 
          estimate_incidence_rate(vars = "AVAL", n_events = "n_events", 
              control = control_incidence_rate(conf_level = 0.9, conf_type = "exact", 
                  time_unit_input = "month", time_unit_output = 100))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          result
      }
      

