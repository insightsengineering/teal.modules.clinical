# template_summary_by generates correct expressions

    Code
      res
    Output
      $data
      {
          anl <- adlb %>% df_explicit_na(omit_columns = setdiff(names(adlb), 
              c("AVISIT", "AVAL")), na_level = "<Missing>")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Summary Table for AVAL by AVISIT") %>% 
          rtables::split_cols_by("ARM", split_fun = drop_split_levels) %>% 
          rtables::add_overall_col("All Patients") %>% rtables::split_rows_by("AVISIT", 
          split_label = teal.data::col_labels(adlb, fill = FALSE)[["AVISIT"]], 
          split_fun = split_fun, label_pos = "topleft") %>% analyze_vars(vars = "AVAL", 
          na.rm = FALSE, na_str = "<Missing>", denom = "N_col", .stats = c("n", 
              "mean_sd", "mean_ci", "median", "median_ci", "quantiles", 
              "range", "count_fraction"))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_summary_by generates correct expressions when `parallel_vars` is true

    Code
      res
    Output
      $data
      {
          anl <- adlb %>% df_explicit_na(omit_columns = setdiff(names(adlb), 
              c("AVISIT", c("AVAL", "CHG"))), na_level = "<Missing>")
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(adsl[["ARM"]])
          anl <- anl %>% dplyr::mutate(ARM = factor(ARM, levels = arm_levels))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Summary Table for AVAL, CHG by AVISIT") %>% 
          rtables::split_cols_by("ARM") %>% rtables::split_rows_by("AVISIT", 
          split_label = teal.data::col_labels(adlb, fill = FALSE)[["AVISIT"]], 
          split_fun = split_fun, label_pos = "topleft") %>% split_cols_by_multivar(vars = c("AVAL", 
          "CHG")) %>% summarize_colvars(vars = c("AVAL", "CHG"), na.rm = FALSE, 
          denom = "N_col", .stats = c("n", "mean_sd", "mean_ci", "median", 
              "median_ci", "quantiles", "range", "count_fraction"), 
          na_str = "<Missing>")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_summary_by generates correct expressions when `row_groups` is true

    Code
      res
    Output
      $data
      {
          anl <- adsl %>% df_explicit_na(omit_columns = setdiff(names(adsl), 
              c(c("SEX", "COUNTRY"), "AVAL")), na_level = "<Missing>")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout_cfun
      cfun_unique <- function(x, labelstr = "", .N_col) {
          y <- length(unique(x))
          rcell(c(y, y/.N_col), label = labelstr)
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Summary Table for AVAL by SEX, COUNTRY") %>% 
          rtables::split_cols_by("ARM", split_fun = drop_split_levels) %>% 
          rtables::split_rows_by("SEX", split_label = teal.data::col_labels(adsl, 
              fill = FALSE)[["SEX"]], split_fun = split_fun, label_pos = "topleft") %>% 
          rtables::summarize_row_groups(var = "USUBJID", cfun = cfun_unique, 
              na_str = "<Missing>") %>% rtables::split_rows_by("COUNTRY", 
          split_label = teal.data::col_labels(adsl, fill = FALSE)[["COUNTRY"]], 
          split_fun = split_fun, label_pos = "topleft") %>% rtables::summarize_row_groups(var = "USUBJID", 
          cfun = cfun_unique, na_str = "<Missing>")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_summary_by generates correct expressions for customized numeric statistics

    Code
      res
    Output
      $data
      {
          anl <- adlb %>% df_explicit_na(omit_columns = setdiff(names(adlb), 
              c("AVISIT", "AVAL")), na_level = "<Missing>")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Summary Table for AVAL by AVISIT") %>% 
          rtables::split_cols_by("ARM", split_fun = drop_split_levels) %>% 
          rtables::add_overall_col("All Patients") %>% rtables::split_rows_by("AVISIT", 
          split_label = teal.data::col_labels(adlb, fill = FALSE)[["AVISIT"]], 
          split_fun = split_fun, label_pos = "topleft") %>% analyze_vars(vars = "AVAL", 
          na.rm = FALSE, na_str = "<Missing>", denom = "N_col", .stats = c("n", 
              "count_fraction"))
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_summary_by generates correct expressions for `drop_zero_levels` is true

    Code
      res
    Output
      $data
      {
          anl <- adlb %>% df_explicit_na(omit_columns = setdiff(names(adlb), 
              c("AVISIT", "AVAL")), na_level = "<Missing>")
          anl <- anl %>% dplyr::mutate(ARM = droplevels(ARM))
          arm_levels <- levels(anl[["ARM"]])
          adsl <- adsl %>% dplyr::filter(ARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ARM = droplevels(ARM))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Summary Table for AVAL by AVISIT") %>% 
          rtables::split_cols_by("ARM", split_fun = drop_split_levels) %>% 
          rtables::add_overall_col("All Patients") %>% rtables::split_rows_by("AVISIT", 
          split_label = teal.data::col_labels(adlb, fill = FALSE)[["AVISIT"]], 
          split_fun = split_fun, label_pos = "topleft") %>% analyze_vars(vars = "AVAL", 
          na.rm = FALSE, na_str = "<Missing>", denom = "N_col", .stats = c("n", 
              "mean_sd", "mean_ci", "median", "median_ci", "quantiles", 
              "range", "count_fraction"))
      
      $table
      {
          all_zero <- function(tr) {
              if (!inherits(tr, "TableRow") || inherits(tr, "LabelRow")) {
                  return(FALSE)
              }
              rvs <- unlist(unname(row_values(tr)))
              isTRUE(all(rvs == 0))
          }
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>% 
              rtables::trim_rows(criteria = all_zero)
      }
      

