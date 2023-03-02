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
      lyt <- rtables::basic_table(title = "Summary Table for AVAL by AVISIT") %>% 
          rtables::split_cols_by("ARM", split_fun = add_overall_level("All Patients", 
              first = FALSE)) %>% rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_label = formatters::var_labels(adlb, fill = FALSE)[["AVISIT"]], 
          split_fun = split_fun, label_pos = "topleft") %>% summarize_vars(vars = "AVAL", 
          na.rm = FALSE, na_level = "<Missing>", denom = "N_col", .stats = c("n", 
              "mean_sd", "mean_ci", "median", "median_ci", "quantiles", 
              "range", "count_fraction"))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          result
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
      lyt <- rtables::basic_table(title = "Summary Table for AVAL, CHG by AVISIT") %>% 
          rtables::split_cols_by("ARM", split_fun = add_overall_level("All Patients", 
              first = FALSE)) %>% rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_label = formatters::var_labels(adlb, fill = FALSE)[["AVISIT"]], 
          split_fun = split_fun, label_pos = "topleft") %>% split_cols_by_multivar(vars = c("AVAL", 
          "CHG")) %>% summarize_colvars(vars = c("AVAL", "CHG"), na.rm = FALSE, 
          denom = "N_col", .stats = c("n", "mean_sd", "mean_ci", "median", 
              "median_ci", "quantiles", "range", "count_fraction"))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          result
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
      lyt <- rtables::basic_table(title = "Summary Table for AVAL by SEX, COUNTRY") %>% 
          rtables::split_cols_by("ARM") %>% rtables::add_colcounts() %>% 
          rtables::split_rows_by("SEX", split_label = formatters::var_labels(adsl, 
              fill = FALSE)[["SEX"]], split_fun = split_fun, label_pos = "topleft") %>% 
          rtables::summarize_row_groups(var = "USUBJID", cfun = cfun_unique) %>% 
          rtables::split_rows_by("COUNTRY", split_label = formatters::var_labels(adsl, 
              fill = FALSE)[["COUNTRY"]], split_fun = split_fun, label_pos = "topleft") %>% 
          rtables::summarize_row_groups(var = "USUBJID", cfun = cfun_unique)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          result
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
      lyt <- rtables::basic_table(title = "Summary Table for AVAL by AVISIT") %>% 
          rtables::split_cols_by("ARM", split_fun = add_overall_level("All Patients", 
              first = FALSE)) %>% rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_label = formatters::var_labels(adlb, fill = FALSE)[["AVISIT"]], 
          split_fun = split_fun, label_pos = "topleft") %>% summarize_vars(vars = "AVAL", 
          na.rm = FALSE, na_level = "<Missing>", denom = "N_col", .stats = c("n", 
              "count_fraction"))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
          result
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
      lyt <- rtables::basic_table(title = "Summary Table for AVAL by AVISIT") %>% 
          rtables::split_cols_by("ARM", split_fun = add_overall_level("All Patients", 
              first = FALSE)) %>% rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT", 
          split_label = formatters::var_labels(adlb, fill = FALSE)[["AVISIT"]], 
          split_fun = split_fun, label_pos = "topleft") %>% summarize_vars(vars = "AVAL", 
          na.rm = FALSE, na_level = "<Missing>", denom = "N_col", .stats = c("n", 
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
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl) %>% 
              rtables::trim_rows(criteria = all_zero)
          result
      }
      

