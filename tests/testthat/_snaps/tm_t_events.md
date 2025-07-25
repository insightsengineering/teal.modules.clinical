# template_events generates correct expressions

    Code
      res
    Output
      $data
      {
          anl <- adae
          anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          arm_levels <- levels(anl[["ACTARM"]])
          adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("AEBODSYS", "AEDECOD")))
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Event Summary by Term : Body System and Adverse Event Code") %>% 
          rtables::split_cols_by(var = "ACTARM") %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          rtables::split_rows_by("AEBODSYS", child_labels = "visible", 
              nested = FALSE, indent_mod = -1L, split_fun = split_fun, 
              label_pos = "topleft", split_label = teal.data::col_labels(adae["AEBODSYS"])) %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L)) %>% 
          append_varlabels(adae, "AEDECOD", indent = 1L)
      
      $table
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- rtables::prune_table(table)
      }
      
      $sort
      {
          idx_split_col <- which(sapply(col_paths(table), tail, 1) == 
              "All Patients")
          pruned_and_sorted_result <- pruned_result %>% sort_at_path(path = c("AEBODSYS"), 
              scorefun = cont_n_onecol(idx_split_col)) %>% sort_at_path(path = c("AEBODSYS", 
              "*", "AEDECOD"), scorefun = score_occurrences_cols(col_indices = idx_split_col))
      }
      

# template_events generates correct expressions for nested columns

    Code
      res
    Output
      $data
      {
          anl <- adae
          anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          arm_levels <- levels(anl[["ACTARM"]])
          adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          anl <- anl %>% dplyr::mutate(ACTARMCD = droplevels(ACTARMCD))
          arm_levels <- levels(anl[["ACTARMCD"]])
          adsl <- adsl %>% dplyr::filter(ACTARMCD %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ACTARMCD = droplevels(ACTARMCD))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("AEBODSYS", "AEDECOD")))
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Event Summary by Term : Body System and Adverse Event Code") %>% 
          rtables::split_cols_by(var = "ACTARM") %>% rtables::split_cols_by("ACTARMCD", 
          split_fun = drop_split_levels) %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          rtables::split_rows_by("AEBODSYS", child_labels = "visible", 
              nested = FALSE, indent_mod = -1L, split_fun = split_fun, 
              label_pos = "topleft", split_label = teal.data::col_labels(adae["AEBODSYS"])) %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L)) %>% 
          append_varlabels(adae, "AEDECOD", indent = 1L)
      
      $table
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- rtables::prune_table(table)
      }
      
      $sort
      {
          idx_split_col <- which(sapply(col_paths(table), tail, 1) == 
              "All Patients")
          pruned_and_sorted_result <- pruned_result %>% sort_at_path(path = c("AEBODSYS"), 
              scorefun = cont_n_onecol(idx_split_col)) %>% sort_at_path(path = c("AEBODSYS", 
              "*", "AEDECOD"), scorefun = score_occurrences_cols(col_indices = idx_split_col))
      }
      

# template_events can generate customized table

    Code
      res
    Output
      $data
      {
          anl <- adcm
          adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          arm_levels <- levels(adsl[["ACTARM"]])
          anl <- anl %>% dplyr::mutate(ACTARM = factor(ACTARM, levels = arm_levels))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              "CMDECOD"))
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Event Summary by Term : Con Med Code") %>% 
          rtables::split_cols_by(var = "ACTARM") %>% summarize_num_patients(var = "USUBJID", 
          .stats = c("unique", "nonunique"), .labels = c(unique = "Total number of patients with at least one treatment", 
              nonunique = "Overall total number of treatments"), na_str = "<Missing>") %>% 
          count_occurrences(vars = "CMDECOD", .indent_mods = -1L) %>% 
          append_varlabels(adcm, "CMDECOD")
      
      $table
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- rtables::prune_table(table)
      }
      
      $sort
      {
          pruned_and_sorted_result <- pruned_result %>% sort_at_path(path = c("CMDECOD"), 
              scorefun = score_occurrences)
      }
      

# template_events can generate customized table with alphabetical sorting

    Code
      res
    Output
      $data
      {
          anl <- adae
          anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          arm_levels <- levels(anl[["ACTARM"]])
          adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          anl[["AEBODSYS"]] <- as.character(anl[["AEBODSYS"]])
          anl[["AEDECOD"]] <- as.character(anl[["AEDECOD"]])
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("AEBODSYS", "AEDECOD")))
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Event Summary by Term : Body System and Adverse Event Code") %>% 
          rtables::split_cols_by(var = "ACTARM") %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          rtables::split_rows_by("AEBODSYS", child_labels = "visible", 
              nested = FALSE, indent_mod = -1L, split_fun = split_fun, 
              label_pos = "topleft", split_label = teal.data::col_labels(adae["AEBODSYS"])) %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L)) %>% 
          append_varlabels(adae, "AEDECOD", indent = 1L)
      
      $table
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- rtables::prune_table(table)
      }
      
      $sort
      {
          pruned_and_sorted_result <- pruned_result
      }
      

# template_events can generate customized table with pruning

    Code
      res
    Output
      $data
      {
          anl <- adae
          anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          arm_levels <- levels(anl[["ACTARM"]])
          adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("AEBODSYS", "AEDECOD")))
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Event Summary by Term : Body System and Adverse Event Code") %>% 
          rtables::split_cols_by(var = "ACTARM") %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          rtables::split_rows_by("AEBODSYS", child_labels = "visible", 
              nested = FALSE, indent_mod = -1L, split_fun = split_fun, 
              label_pos = "topleft", split_label = teal.data::col_labels(adae["AEBODSYS"])) %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L)) %>% 
          append_varlabels(adae, "AEDECOD", indent = 1L)
      
      $table
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- rtables::prune_table(table)
          col_indices <- 1:(ncol(table) - TRUE)
          row_condition <- has_fraction_in_any_col(atleast = 0.4, col_indices = col_indices) & 
              has_fractions_difference(atleast = 0.1, col_indices = col_indices)
          pruned_result <- pruned_result %>% rtables::prune_table(keep_rows(row_condition))
      }
      
      $sort
      {
          idx_split_col <- which(sapply(col_paths(table), tail, 1) == 
              "All Patients")
          pruned_and_sorted_result <- pruned_result %>% sort_at_path(path = c("AEBODSYS"), 
              scorefun = cont_n_onecol(idx_split_col)) %>% sort_at_path(path = c("AEBODSYS", 
              "*", "AEDECOD"), scorefun = score_occurrences_cols(col_indices = idx_split_col))
          criteria_fun <- function(tr) {
              inherits(tr, "ContentRow")
          }
          pruned_and_sorted_result <- rtables::trim_rows(pruned_and_sorted_result, 
              criteria = criteria_fun)
      }
      

# template_events can generate customized table with pruning for nested column

    Code
      res
    Output
      $data
      {
          anl <- adae
          anl <- anl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          arm_levels <- levels(anl[["ACTARM"]])
          adsl <- adsl %>% dplyr::filter(ACTARM %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          anl <- anl %>% dplyr::mutate(ACTARMCD = droplevels(ACTARMCD))
          arm_levels <- levels(anl[["ACTARMCD"]])
          adsl <- adsl %>% dplyr::filter(ACTARMCD %in% arm_levels)
          adsl <- adsl %>% dplyr::mutate(ACTARMCD = droplevels(ACTARMCD))
          adsl <- df_explicit_na(adsl, na_level = "<Missing>")
          anl <- anl %>% df_explicit_na(omit_columns = setdiff(names(anl), 
              c("AEBODSYS", "AEDECOD")))
      }
      
      $layout_prep
      split_fun <- drop_split_levels
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Event Summary by Term : Body System and Adverse Event Code") %>% 
          rtables::split_cols_by(var = "ACTARM") %>% rtables::split_cols_by("ACTARMCD", 
          split_fun = drop_split_levels) %>% rtables::add_overall_col(label = "All Patients") %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          rtables::split_rows_by("AEBODSYS", child_labels = "visible", 
              nested = FALSE, indent_mod = -1L, split_fun = split_fun, 
              label_pos = "topleft", split_label = teal.data::col_labels(adae["AEBODSYS"])) %>% 
          summarize_num_patients(var = "USUBJID", .stats = c("unique", 
              "nonunique"), .labels = c(unique = "Total number of patients with at least one event", 
              nonunique = "Overall total number of events"), na_str = "<Missing>") %>% 
          count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L)) %>% 
          append_varlabels(adae, "AEDECOD", indent = 1L)
      
      $table
      table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- rtables::prune_table(table)
          col_indices <- 1:(ncol(table) - TRUE)
          row_condition <- has_fraction_in_any_col(atleast = 0.4, col_indices = col_indices) & 
              has_fractions_difference(atleast = 0.1, col_indices = col_indices)
          pruned_result <- pruned_result %>% rtables::prune_table(keep_rows(row_condition))
      }
      
      $sort
      {
          idx_split_col <- which(sapply(col_paths(table), tail, 1) == 
              "All Patients")
          pruned_and_sorted_result <- pruned_result %>% sort_at_path(path = c("AEBODSYS"), 
              scorefun = cont_n_onecol(idx_split_col)) %>% sort_at_path(path = c("AEBODSYS", 
              "*", "AEDECOD"), scorefun = score_occurrences_cols(col_indices = idx_split_col))
          criteria_fun <- function(tr) {
              inherits(tr, "ContentRow")
          }
          pruned_and_sorted_result <- rtables::trim_rows(pruned_and_sorted_result, 
              criteria = criteria_fun)
      }
      

