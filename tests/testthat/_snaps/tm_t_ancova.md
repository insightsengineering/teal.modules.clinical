# template_ancova generates expressions with multiple endpoints

    Code
      res
    Output
      $data
      {
          adqs <- adqs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for Function/Well-Being (GF1,GF3,GF7) and BFI All Questions at WEEK 1 DAY 8 for Absolute Change from Baseline") %>% 
          rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>% 
          rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adqs["AVISIT"], 
              fill = TRUE)) %>% rtables::split_rows_by("PARAMCD", split_fun = split_fun, 
          label_pos = "topleft", split_label = formatters::var_labels(adqs["PARAMCD"], 
              fill = TRUE)) %>% summarize_ancova(vars = "CHG", variables = list(arm = "ARMCD", 
          covariates = c("BASE", "STRATA1")), conf_level = 0.95, var_labels = "Adjusted mean", 
          show_labels = "hidden", .labels = NULL)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
          result
      }
      

---

    Code
      res
    Output
      $data
      {
          adqs <- adqs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for Function/Well-Being (GF1,GF3,GF7) and BFI All Questions at WEEK 1 DAY 8 for Absolute Change from Baseline") %>% 
          rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>% 
          rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adqs["AVISIT"], 
              fill = TRUE)) %>% rtables::split_rows_by("PARAMCD", split_fun = split_fun, 
          label_pos = "topleft", split_label = formatters::var_labels(adqs["PARAMCD"], 
              fill = TRUE)) %>% rtables::append_topleft(paste0("    Interaction Variable: ", 
          "SEX")) %>% summarize_ancova(vars = "CHG", variables = list(arm = "ARMCD", 
          covariates = c("BASE", "STRATA1", "ARMCD*SEX")), conf_level = 0.95, 
          var_labels = paste("Interaction Level:", "M"), show_labels = if (FALSE) "hidden" else "visible", 
          interaction_y = "M", interaction_item = "SEX")
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
          result
      }
      

# template_ancova generates expressions with multiple endpoints with combined comparison arms

    Code
      res
    Output
      $data
      {
          adqs <- adqs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% dplyr::mutate(ARMCD = combine_levels(ARMCD, 
              levels = c("ARM B", "ARM C"))) %>% df_explicit_na(na_level = "")
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% dplyr::mutate(ARMCD = combine_levels(ARMCD, 
              levels = c("ARM B", "ARM C"))) %>% df_explicit_na(na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for A and B at WEEK 1 DAY 8 for Absolute Change from Baseline") %>% 
          rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>% 
          rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adqs["AVISIT"], 
              fill = TRUE)) %>% rtables::split_rows_by("PARAMCD", split_fun = split_fun, 
          label_pos = "topleft", split_label = formatters::var_labels(adqs["PARAMCD"], 
              fill = TRUE)) %>% summarize_ancova(vars = "CHG", variables = list(arm = "ARMCD", 
          covariates = c("BASE", "STRATA1")), conf_level = 0.95, var_labels = "Adjusted mean", 
          show_labels = "hidden", .labels = NULL)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
          result
      }
      

# template_ancova generates expressions with multiple endpoints with combined reference arms

    Code
      res
    Output
      $data
      {
          adqs <- adqs %>% dplyr::filter(ARMCD %in% c("ARM B", "ARM C", 
              "ARM A")) %>% dplyr::mutate(ARMCD = combine_levels(ARMCD, 
              levels = c("ARM B", "ARM C"), new_level = "ARM B/ARM C")) %>% 
              dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM B/ARM C")) %>% 
              droplevels() %>% df_explicit_na(na_level = "")
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM B", "ARM C", 
              "ARM A")) %>% dplyr::mutate(ARMCD = combine_levels(ARMCD, 
              levels = c("ARM B", "ARM C"), new_level = "ARM B/ARM C")) %>% 
              dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM B/ARM C")) %>% 
              droplevels() %>% df_explicit_na(na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for A and B at WEEK 2 DAY 1 for Absolute Change from Baseline") %>% 
          rtables::split_cols_by(var = "ARMCD", ref_group = "ARM B/ARM C") %>% 
          rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adqs["AVISIT"], 
              fill = TRUE)) %>% rtables::split_rows_by("PARAMCD", split_fun = split_fun, 
          label_pos = "topleft", split_label = formatters::var_labels(adqs["PARAMCD"], 
              fill = TRUE)) %>% summarize_ancova(vars = "CHG", variables = list(arm = "ARMCD", 
          covariates = c("BASE", "STRATA1")), conf_level = 0.95, var_labels = "Adjusted mean", 
          show_labels = "hidden", .labels = NULL)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
          result
      }
      

# template_ancova generates expressions with single endpoint

    Code
      res
    Output
      $data
      {
          adqs <- adqs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for MYFAVORITE at  for Absolute Change from Baseline") %>% 
          rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>% 
          rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adqs["AVISIT"], 
              fill = TRUE)) %>% rtables::append_topleft(paste0("  ", 
          "MYFAVORITE")) %>% summarize_ancova(vars = "CHG", variables = list(arm = "ARMCD", 
          covariates = NULL), conf_level = 0.95, var_labels = "Unadjusted comparison", 
          .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"), 
          table_names = "unadjusted_comparison") %>% summarize_ancova(vars = "CHG", 
          variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")), 
          conf_level = 0.95, var_labels = paste0("Adjusted comparison (", 
              paste(c("BASE", "STRATA1"), collapse = " + "), ")"), 
          table_names = "adjusted_comparison")
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
          result
      }
      

# template_ancova generates expressions with discrete interaction variable

    Code
      res
    Output
      $data
      {
          adqs <- adqs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for Function/Well-Being (GF1,GF3,GF7) and BFI All Questions at WEEK 1 DAY 8 for Absolute Change from Baseline") %>% 
          rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>% 
          rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adqs["AVISIT"], 
              fill = TRUE)) %>% rtables::split_rows_by("PARAMCD", split_fun = split_fun, 
          label_pos = "topleft", split_label = formatters::var_labels(adqs["PARAMCD"], 
              fill = TRUE)) %>% rtables::append_topleft(paste0("    Interaction Variable: ", 
          "SEX")) %>% summarize_ancova(vars = "CHG", variables = list(arm = "ARMCD", 
          covariates = c("BASE", "STRATA1", "ARMCD*SEX")), conf_level = 0.95, 
          var_labels = paste("Interaction Level:", "M"), show_labels = if (FALSE) "hidden" else "visible", 
          interaction_y = "M", interaction_item = "SEX")
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
          result
      }
      

# template_ancova generates expressions with continuous interaction variable

    Code
      res
    Output
      $data
      {
          adqs <- adqs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% droplevels() %>% df_explicit_na(na_level = "")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(title = "Summary of Analysis of Variance for Function/Well-Being (GF1,GF3,GF7) and BFI All Questions at WEEK 1 DAY 8 for Absolute Change from Baseline") %>% 
          rtables::split_cols_by(var = "ARMCD", ref_group = "ARM A") %>% 
          rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_fun = split_fun, label_pos = "topleft", split_label = formatters::var_labels(adqs["AVISIT"], 
              fill = TRUE)) %>% rtables::split_rows_by("PARAMCD", split_fun = split_fun, 
          label_pos = "topleft", split_label = formatters::var_labels(adqs["PARAMCD"], 
              fill = TRUE)) %>% rtables::append_topleft(paste0("    Interaction Variable: ", 
          "BASE")) %>% summarize_ancova(vars = "CHG", variables = list(arm = "ARMCD", 
          covariates = c("BASE", "STRATA1", "ARMCD*BASE")), conf_level = 0.95, 
          var_labels = paste("Interaction Level:", FALSE), show_labels = if (TRUE) "hidden" else "visible", 
          interaction_y = FALSE, interaction_item = "BASE")
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = adqs, alt_counts_df = adsl)
          result
      }
      

