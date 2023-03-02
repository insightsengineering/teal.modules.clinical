# template_smq generates correct expressions with default arguments

    Code
      res
    Output
      $data
      {
          anl <- adae
          adsl <- adsl %>% dplyr::mutate(ARMCD = droplevels(ARMCD))
          arm_levels <- levels(adsl[["ARMCD"]])
          anl <- anl %>% dplyr::mutate(ARMCD = factor(ARMCD, levels = arm_levels))
          adsl <- adsl %>% dplyr::mutate(SEX = droplevels(SEX))
          arm_levels <- levels(adsl[["SEX"]])
          anl <- anl %>% dplyr::mutate(SEX = factor(SEX, levels = arm_levels))
          anl <- h_stack_by_baskets(df = anl, baskets = c("SMQ01NAM", 
              "SMQ02NAM", "CQ01NAM"), smq_varlabel = "Standardized MedDRA Query", 
              keys = unique(c("STUDYID", "USUBJID", c("ARMCD", "SEX"), 
                  "AEDECOD")))
          if (nrow(anl) == 0) {
              stop("Analysis dataset contains only missing values")
          }
          anl <- df_explicit_na(anl, na_level = "<Missing>")
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARMCD") %>% 
          rtables::split_cols_by(var = "SEX") %>% rtables::add_colcounts() %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique"), 
              .labels = c(unique = "Total number of patients with at least one adverse event")) %>% 
          rtables::split_rows_by("SMQ", child_labels = "visible", nested = FALSE, 
              split_fun = trim_levels_in_group("AEDECOD", drop_outlevs = FALSE), 
              indent_mod = -1L, label_pos = "topleft", split_label = formatters::var_labels(anl, 
                  fill = FALSE)[["SMQ"]]) %>% summarize_num_patients(var = "USUBJID", 
          .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one adverse event", 
              nonunique = "Total number of events")) %>% count_occurrences(vars = "AEDECOD", 
          drop = FALSE) %>% append_varlabels(anl, "AEDECOD", indent = 1L)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      
      $sort
      {
          sorted_result <- result %>% sort_at_path(path = c("SMQ"), 
              scorefun = cont_n_allcols) %>% sort_at_path(path = c("SMQ", 
              "*", "AEDECOD"), scorefun = score_occurrences, na.pos = "last")
      }
      
      $sort_and_prune
      {
          all_zero <- function(tr) {
              !inherits(tr, "ContentRow") && rtables::all_zero_or_na(tr)
          }
          pruned_and_sorted_result <- sorted_result %>% rtables::trim_rows(criteria = all_zero)
          pruned_and_sorted_result
      }
      

# template_smq generates correct expressions with custom arguments

    Code
      res
    Output
      $data
      {
          anl <- myadae
          myadsl <- myadsl %>% dplyr::mutate(myARMCD = droplevels(myARMCD))
          arm_levels <- levels(myadsl[["myARMCD"]])
          anl <- anl %>% dplyr::mutate(myARMCD = factor(myARMCD, levels = arm_levels))
          anl <- h_stack_by_baskets(df = anl, baskets = "mybaskets", 
              smq_varlabel = "mylabel", keys = unique(c("STUDYID", 
                  "myUSUBJID", "myARMCD", "myAEDECOD")))
          if (nrow(anl) == 0) {
              stop("Analysis dataset contains only missing values")
          }
          anl <- df_explicit_na(anl, na_level = "<Missing>")
          myadsl <- df_explicit_na(myadsl, na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by(var = "myARMCD") %>% 
          rtables::add_colcounts() %>% summarize_num_patients(var = "myUSUBJID", 
          .stats = c("unique"), .labels = c(unique = "Total number of patients with at least one adverse event")) %>% 
          rtables::split_rows_by("SMQ", child_labels = "visible", nested = FALSE, 
              split_fun = trim_levels_in_group("myAEDECOD", drop_outlevs = FALSE), 
              indent_mod = -1L, label_pos = "topleft", split_label = formatters::var_labels(anl, 
                  fill = FALSE)[["SMQ"]]) %>% summarize_num_patients(var = "myUSUBJID", 
          .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one adverse event", 
              nonunique = "Total number of events")) %>% count_occurrences(vars = "myAEDECOD", 
          drop = FALSE) %>% append_varlabels(anl, "myAEDECOD", indent = 1L)
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = myadsl)
      }
      
      $sort
      {
          sorted_result <- result %>% sort_at_path(path = c("SMQ"), 
              scorefun = cont_n_allcols) %>% sort_at_path(path = c("SMQ", 
              "*", "myAEDECOD"), scorefun = score_occurrences, na.pos = "last")
      }
      
      $sort_and_prune
      {
          all_zero <- function(tr) {
              !inherits(tr, "ContentRow") && rtables::all_zero_or_na(tr)
          }
          pruned_and_sorted_result <- sorted_result %>% rtables::trim_rows(criteria = all_zero)
          pruned_and_sorted_result
      }
      

