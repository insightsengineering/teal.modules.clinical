# template_summary generates correct expressions

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% df_explicit_na(omit_columns = setdiff(names(adrs), 
              c(c("RACE", "COUNTRY", "AGE"))), na_level = "<Missing>")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, main_footer = "n represents the number of unique subject IDs such that the variable has non-NA values.") %>% 
          rtables::split_cols_by("ARM", split_fun = drop_split_levels) %>% 
          analyze_vars(vars = c("RACE", "COUNTRY", "AGE"), show_labels = "visible", 
              na.rm = FALSE, na_str = "<Missing>", denom = "N_col", 
              .stats = c("n", "mean_sd", "mean_ci", "median", "median_ci", 
                  "quantiles", "range", "geom_mean", "count_fraction"))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_summary can generate customized table

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% df_explicit_na(omit_columns = setdiff(names(adrs), 
              c("RACE")), na_level = "<Missing>")
          adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          arm_levels <- levels(adsl[["ARMCD"]])
          anl <- anl %>% dplyr::mutate(ARMCD = factor(ARMCD, levels = arm_levels))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, main_footer = "n represents the number of unique subject IDs such that the variable has non-NA values.") %>% 
          rtables::split_cols_by("ARMCD") %>% rtables::add_overall_col("All Patients") %>% 
          analyze_vars(vars = "RACE", var_labels = c(RACE = "Race"), 
              show_labels = "visible", na.rm = TRUE, na_str = "<Missing>", 
              denom = "N_col", .stats = c("n", "mean_sd", "mean_ci", 
                  "median", "median_ci", "quantiles", "range", "geom_mean", 
                  "count"))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_summary generates correct expressions for multiple grouping variables

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% df_explicit_na(omit_columns = setdiff(names(adrs), 
              c(c("RACE", "COUNTRY", "AGE"))), na_level = "<Missing>")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- anl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
          arm_levels <- levels(anl[["STRATA1"]])
          adsl <- adsl %>% dplyr::filter(STRATA1 %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, main_footer = "n represents the number of unique subject IDs such that the variable has non-NA values.") %>% 
          rtables::split_cols_by("ARM", split_fun = drop_split_levels) %>% 
          rtables::split_cols_by("STRATA1", split_fun = drop_split_levels) %>% 
          analyze_vars(vars = c("RACE", "COUNTRY", "AGE"), show_labels = "visible", 
              na.rm = FALSE, na_str = "<Missing>", denom = "N_col", 
              .stats = c("n", "mean_sd", "mean_ci", "median", "median_ci", 
                  "quantiles", "range", "geom_mean", "count_fraction"))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_summary generates correct expressions for multiple grouping variables and all patients

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% df_explicit_na(omit_columns = setdiff(names(adrs), 
              c(c("RACE", "COUNTRY", "AGE"))), na_level = "<Missing>")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- anl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
          arm_levels <- levels(anl[["STRATA1"]])
          adsl <- adsl %>% dplyr::filter(STRATA1 %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, main_footer = "n represents the number of unique subject IDs such that the variable has non-NA values.") %>% 
          rtables::split_cols_by("ARM", split_fun = drop_split_levels) %>% 
          rtables::split_cols_by("STRATA1", split_fun = drop_split_levels) %>% 
          rtables::add_overall_col("All Patients") %>% analyze_vars(vars = c("RACE", 
          "COUNTRY", "AGE"), show_labels = "visible", na.rm = FALSE, 
          na_str = "<Missing>", denom = "N_col", .stats = c("n", "mean_sd", 
              "mean_ci", "median", "median_ci", "quantiles", "range", 
              "geom_mean", "count_fraction"))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_summary generates correct expressions for customized numeric statistics

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% df_explicit_na(omit_columns = setdiff(names(adrs), 
              c(c("RACE", "COUNTRY", "AGE"))), na_level = "<Missing>")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- anl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
          arm_levels <- levels(anl[["STRATA1"]])
          adsl <- adsl %>% dplyr::filter(STRATA1 %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(STRATA1 = droplevels(STRATA1))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, main_footer = "n represents the number of unique subject IDs such that the variable has non-NA values.") %>% 
          rtables::split_cols_by("ARM", split_fun = drop_split_levels) %>% 
          rtables::split_cols_by("STRATA1", split_fun = drop_split_levels) %>% 
          analyze_vars(vars = c("RACE", "COUNTRY", "AGE"), show_labels = "visible", 
              na.rm = FALSE, na_str = "<Missing>", denom = "N_col", 
              .stats = c("n", "count_fraction"))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_summary generates correct expressions when arm variable labels are added

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% df_explicit_na(omit_columns = setdiff(names(adrs), 
              c(c("RACE", "COUNTRY", "AGE"))), na_level = "<Missing>")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          anl <- anl %>% dplyr::mutate(SEX = droplevels(SEX))
          arm_levels <- levels(anl[["SEX"]])
          adsl <- adsl %>% dplyr::filter(SEX %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(SEX = droplevels(SEX))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, main_footer = "n represents the number of unique subject IDs such that the variable has non-NA values.") %>% 
          rtables::split_cols_by("ARM", split_fun = drop_split_levels) %>% 
          rtables::split_cols_by("SEX", split_fun = drop_split_levels) %>% 
          rtables::add_overall_col("All Patients") %>% analyze_vars(vars = c("RACE", 
          "COUNTRY", "AGE"), show_labels = "visible", na.rm = FALSE, 
          na_str = "<Missing>", denom = "N_col", .stats = c("n", "mean_sd", 
              "mean_ci", "median", "median_ci", "quantiles", "range", 
              "geom_mean", "count_fraction")) %>% append_topleft(c("Arm", 
          "Sex", ""))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

