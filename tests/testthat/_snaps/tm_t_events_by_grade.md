# template_events_by_grade generates standard expressions

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
          adae <- tern::df_explicit_na(adae, na_level = "<Missing>")
          anl <- tern::df_explicit_na(anl, na_level = "<Missing>")
          adsl <- tern::df_explicit_na(adsl, na_level = "<Missing>")
          grade_groups <- list(`- Any Intensity -` = levels(adae$AESEV))
      }
      
      $layout_prep
      split_fun <- rtables::trim_levels_in_group
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Adverse Event summary by Analysis Toxicity Grade: Body System or Organ Class and Dictionary-Derived Term") %>% 
          rtables::split_cols_by("ACTARM") %>% rtables::add_overall_col(label = "All Patients") %>% 
          tern::summarize_occurrences_by_grade(var = "AESEV", grade_groups = grade_groups, 
              na_str = "<Missing>") %>% rtables::split_rows_by("AEBODSYS", 
          child_labels = "visible", nested = TRUE, indent_mod = -1L, 
          split_fun = split_fun("AESEV"), label_pos = "topleft", split_label = teal.data::col_labels(adae["AEBODSYS"])) %>% 
          tern::summarize_occurrences_by_grade(var = "AESEV", grade_groups = grade_groups, 
              na_str = "<Missing>") %>% rtables::split_rows_by("AEDECOD", 
          child_labels = "visible", nested = TRUE, indent_mod = -1L, 
          split_fun = split_fun("AESEV"), label_pos = "topleft", split_label = teal.data::col_labels(adae["AEDECOD"])) %>% 
          tern::summarize_num_patients(var = "", .stats = "unique", 
              .labels = c("- Any Intensity -"), na_str = "<Missing>") %>% 
          tern::count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L, 
              na_str = "<Missing>") %>% tern::append_varlabels(adae, 
          "AESEV", indent = 2L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- result
      }
      
      $sort
      {
          pruned_and_sorted_result <- pruned_result %>% rtables::sort_at_path(path = "AEBODSYS", 
              scorefun = rtables::cont_n_onecol(length(levels(adsl$ACTARM)) + 
                  1), decreasing = TRUE) %>% rtables::sort_at_path(path = c("AEBODSYS", 
              "*", "AEDECOD"), scorefun = rtables::cont_n_onecol(length(levels(adsl$ACTARM)) + 
              1), decreasing = TRUE)
      }
      

# template_events_by_grade generates standard expressions with pruning conditions

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
          adae <- tern::df_explicit_na(adae, na_level = "<Missing>")
          anl <- tern::df_explicit_na(anl, na_level = "<Missing>")
          adsl <- tern::df_explicit_na(adsl, na_level = "<Missing>")
          grade_groups <- list(`- Any Intensity -` = levels(adae$AESEV))
      }
      
      $layout_prep
      split_fun <- rtables::trim_levels_in_group
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Adverse Event summary by Severity/Intensity: Body System or Organ Class and Dictionary-Derived Term") %>% 
          rtables::split_cols_by("ACTARM") %>% rtables::add_overall_col(label = "All Patients") %>% 
          tern::summarize_occurrences_by_grade(var = "AESEV", grade_groups = grade_groups, 
              na_str = "<Missing>") %>% rtables::split_rows_by("AEBODSYS", 
          child_labels = "visible", nested = TRUE, indent_mod = -1L, 
          split_fun = split_fun("AESEV"), label_pos = "topleft", split_label = teal.data::col_labels(adae["AEBODSYS"])) %>% 
          tern::summarize_occurrences_by_grade(var = "AESEV", grade_groups = grade_groups, 
              na_str = "<Missing>") %>% rtables::split_rows_by("AEDECOD", 
          child_labels = "visible", nested = TRUE, indent_mod = -1L, 
          split_fun = split_fun("AESEV"), label_pos = "topleft", split_label = teal.data::col_labels(adae["AEDECOD"])) %>% 
          tern::summarize_num_patients(var = "", .stats = "unique", 
              .labels = c("- Any Intensity -"), na_str = "<Missing>") %>% 
          tern::count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L, 
              na_str = "<Missing>") %>% tern::append_varlabels(adae, 
          "AESEV", indent = 2L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- result
          col_indices <- 1:(ncol(result) - TRUE)
          row_condition <- has_fraction_in_any_col(atleast = 0.4, col_indices = col_indices) & 
              has_fractions_difference(atleast = 0.1, col_indices = col_indices)
          pruned_result <- pruned_result %>% rtables::prune_table(keep_content_rows(row_condition))
      }
      
      $sort
      {
          pruned_and_sorted_result <- pruned_result %>% rtables::sort_at_path(path = "AEBODSYS", 
              scorefun = rtables::cont_n_onecol(length(levels(adsl$ACTARM)) + 
                  1), decreasing = TRUE) %>% rtables::sort_at_path(path = c("AEBODSYS", 
              "*", "AEDECOD"), scorefun = rtables::cont_n_onecol(length(levels(adsl$ACTARM)) + 
              1), decreasing = TRUE)
      }
      

# template_events_by_grade without adding total column option works as expected

    Code
      res
    Output
      $data
      {
          anl <- adae
          adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          arm_levels <- levels(adsl[["ACTARM"]])
          anl <- anl %>% dplyr::mutate(ACTARM = factor(ACTARM, levels = arm_levels))
          adae <- tern::df_explicit_na(adae, na_level = "<Missing>")
          anl <- tern::df_explicit_na(anl, na_level = "<Missing>")
          adsl <- tern::df_explicit_na(adsl, na_level = "<Missing>")
          grade_groups <- list(`- Any Intensity -` = levels(adae$AESEV))
      }
      
      $layout_prep
      split_fun <- rtables::trim_levels_in_group
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Adverse Event summary by Severity/Intensity: Body System or Organ Class and Dictionary-Derived Term") %>% 
          rtables::split_cols_by("ACTARM") %>% tern::summarize_occurrences_by_grade(var = "AESEV", 
          grade_groups = grade_groups, na_str = "<Missing>") %>% rtables::split_rows_by("AEBODSYS", 
          child_labels = "visible", nested = TRUE, indent_mod = -1L, 
          split_fun = split_fun("AESEV"), label_pos = "topleft", split_label = teal.data::col_labels(adae["AEBODSYS"])) %>% 
          tern::summarize_occurrences_by_grade(var = "AESEV", grade_groups = grade_groups, 
              na_str = "<Missing>") %>% rtables::split_rows_by("AEDECOD", 
          child_labels = "visible", nested = TRUE, indent_mod = -1L, 
          split_fun = split_fun("AESEV"), label_pos = "topleft", split_label = teal.data::col_labels(adae["AEDECOD"])) %>% 
          tern::summarize_num_patients(var = "", .stats = "unique", 
              .labels = c("- Any Intensity -"), na_str = "<Missing>") %>% 
          tern::count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L, 
              na_str = "<Missing>") %>% tern::append_varlabels(adae, 
          "AESEV", indent = 2L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- result
      }
      
      $sort
      {
          pruned_and_sorted_result <- pruned_result %>% rtables::sort_at_path(path = "AEBODSYS", 
              scorefun = rtables::cont_n_allcols, decreasing = TRUE) %>% 
              rtables::sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), 
                  scorefun = rtables::cont_n_allcols, decreasing = TRUE)
      }
      

# template_events_by_grade with hlt only works

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
          adae <- tern::df_explicit_na(adae, na_level = "<Missing>")
          anl <- tern::df_explicit_na(anl, na_level = "<Missing>")
          adsl <- tern::df_explicit_na(adsl, na_level = "<Missing>")
          grade_groups <- list(`- Any Intensity -` = levels(adae$AESEV))
      }
      
      $layout_prep
      split_fun <- rtables::trim_levels_in_group
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Adverse Event summary by Severity/Intensity: Body System or Organ Class") %>% 
          rtables::split_cols_by("ACTARM") %>% rtables::add_overall_col(label = "All Patients") %>% 
          tern::summarize_occurrences_by_grade(var = "AESEV", grade_groups = grade_groups, 
              na_str = "<Missing>") %>% rtables::split_rows_by("AEBODSYS", 
          child_labels = "visible", nested = TRUE, indent_mod = -1L, 
          split_fun = split_fun("AESEV"), label_pos = "topleft", split_label = teal.data::col_labels(adae["AEBODSYS"])) %>% 
          tern::summarize_num_patients(var = "", .stats = "unique", 
              .labels = c("- Any Intensity -"), na_str = "<Missing>") %>% 
          tern::count_occurrences_by_grade(var = "AESEV", .indent_mods = -1L, 
              na_str = "<Missing>") %>% tern::append_varlabels(adae, 
          "AESEV", indent = 1L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      
      $prune
      {
          pruned_result <- result
      }
      
      $sort
      {
          pruned_and_sorted_result <- pruned_result %>% rtables::sort_at_path(path = "AEBODSYS", 
              scorefun = rtables::cont_n_onecol(length(levels(adsl$ACTARM)) + 
                  1), decreasing = TRUE)
      }
      

# template_events_col_by_grade generates standard expressions

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
          col_counts <- rep(c(table(adsl[["ACTARM"]]), nrow(adsl)), 
              each = length(list(`Any Grade (%)` = c("1", "2", "3", 
                  "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
                  "4"), `Grade 5 (%)` = "5")))
          anl <- anl %>% dplyr::group_by(USUBJID, ACTARM, AEBODSYS, 
              AEDECOD) %>% dplyr::summarize(MAXAETOXGR = factor(max(as.numeric(AETOXGR)))) %>% 
              dplyr::ungroup() %>% tern::df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Adverse Event summary by Analysis Toxicity Grade: Body System or Organ Class and Dictionary-Derived Term") %>% 
          rtables::split_cols_by(var = "ACTARM", split_fun = rtables::add_overall_level("All Patients", 
              first = FALSE)) %>% split_cols_by_groups("MAXAETOXGR", 
          groups = list(`Any Grade (%)` = c("1", "2", "3", "4", "5"), 
              `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
                  "4"), `Grade 5 (%)` = "5")) %>% rtables::split_rows_by("AEBODSYS", 
          child_labels = "visible", nested = FALSE, split_fun = rtables::trim_levels_in_group("AEDECOD")) %>% 
          tern::append_varlabels(df = anl, vars = "AEBODSYS") %>% tern::summarize_num_patients(var = "USUBJID", 
          .stats = "unique", .labels = "Total number of patients with at least one adverse event", 
          ) %>% tern::analyze_vars("AEDECOD", na.rm = FALSE, denom = "N_col", 
          .stats = "count_fraction", .formats = c(count_fraction = format_fraction_threshold(0.01))) %>% 
          tern::append_varlabels(df = anl, vars = "AEDECOD", indent = 1L)
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, col_counts = col_counts)
      
      $sort
      {
          lengths <- lapply(list(`Any Grade (%)` = c("1", "2", "3", 
              "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5"), length)
          start_index <- unname(which.max(lengths))
          col_indices <- seq(start_index, ncol(result), by = length(list(`Any Grade (%)` = c("1", 
              "2", "3", "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5")))
          scorefun_soc <- tern::score_occurrences_cont_cols(col_indices = col_indices)
          scorefun_term <- tern::score_occurrences_cols(col_indices = col_indices)
          sorted_result <- result %>% rtables::sort_at_path(path = c("AEBODSYS"), 
              scorefun = scorefun_soc, decreasing = TRUE) %>% rtables::sort_at_path(path = c("AEBODSYS", 
              "*", "AEDECOD"), scorefun = scorefun_term, decreasing = TRUE)
      }
      
      $prune
      {
          criteria_fun <- function(tr) {
              inherits(tr, "ContentRow")
          }
          at_least_percent_any <- has_fraction_in_any_col(atleast = 0.1, 
              col_indices = col_indices)
          pruned_and_sorted_result <- sorted_result %>% rtables::trim_rows(criteria = criteria_fun) %>% 
              rtables::prune_table(keep_rows(at_least_percent_any))
      }
      

# template_events_col_by_grade works with custom values

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
          col_counts <- rep(c(table(adsl[["ACTARM"]]), nrow(adsl)), 
              each = length(list(`Any Grade (%)` = c("1", "2", "3", 
                  "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
                  "4"), `Grade 5 (%)` = "5")))
          anl <- anl %>% dplyr::group_by(USUBJID, ACTARM, AEDECOD) %>% 
              dplyr::summarize(MAXAETOXGR = factor(max(as.numeric(AETOXGR)))) %>% 
              dplyr::ungroup() %>% tern::df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Adverse Event summary by Analysis Toxicity Grade: Dictionary-Derived Term") %>% 
          rtables::split_cols_by(var = "ACTARM", split_fun = rtables::add_overall_level("All Patients", 
              first = FALSE)) %>% split_cols_by_groups("MAXAETOXGR", 
          groups = list(`Any Grade (%)` = c("1", "2", "3", "4", "5"), 
              `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
                  "4"), `Grade 5 (%)` = "5")) %>% tern::analyze_vars("AEDECOD", 
          na.rm = FALSE, denom = "N_col", .stats = "count_fraction", 
          .formats = c(count_fraction = format_fraction_threshold(0.01))) %>% 
          tern::append_varlabels(df = anl, vars = "AEDECOD")
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, col_counts = col_counts)
      
      $sort
      {
          lengths <- lapply(list(`Any Grade (%)` = c("1", "2", "3", 
              "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5"), length)
          start_index <- unname(which.max(lengths))
          col_indices <- seq(start_index, ncol(result), by = length(list(`Any Grade (%)` = c("1", 
              "2", "3", "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5")))
          scorefun_term <- tern::score_occurrences_cols(col_indices = col_indices)
          sorted_result <- result %>% rtables::sort_at_path(path = c("AEDECOD"), 
              scorefun = scorefun_term, decreasing = TRUE)
      }
      
      $prune
      {
          criteria_fun <- function(tr) {
              inherits(tr, "ContentRow")
          }
          at_least_percent_any <- has_fraction_in_any_col(atleast = 0.1, 
              col_indices = col_indices)
          pruned_and_sorted_result <- sorted_result %>% rtables::trim_rows(criteria = criteria_fun) %>% 
              rtables::prune_table(keep_rows(at_least_percent_any))
      }
      

# template_events_col_by_grade without adding total column option works as expected

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
          col_counts <- rep(table(adsl[["ACTARM"]]), each = length(list(`Any Grade (%)` = c("1", 
              "2", "3", "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5")))
          anl <- anl %>% dplyr::group_by(USUBJID, ACTARM, AEDECOD) %>% 
              dplyr::summarize(MAXAETOXGR = factor(max(as.numeric(AETOXGR)))) %>% 
              dplyr::ungroup() %>% tern::df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Adverse Event summary by Analysis Toxicity Grade: Dictionary-Derived Term") %>% 
          rtables::split_cols_by(var = "ACTARM") %>% split_cols_by_groups("MAXAETOXGR", 
          groups = list(`Any Grade (%)` = c("1", "2", "3", "4", "5"), 
              `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
                  "4"), `Grade 5 (%)` = "5")) %>% tern::analyze_vars("AEDECOD", 
          na.rm = FALSE, denom = "N_col", .stats = "count_fraction", 
          .formats = c(count_fraction = format_fraction_threshold(0.01))) %>% 
          tern::append_varlabels(df = anl, vars = "AEDECOD")
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, col_counts = col_counts)
      
      $sort
      {
          lengths <- lapply(list(`Any Grade (%)` = c("1", "2", "3", 
              "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5"), length)
          start_index <- unname(which.max(lengths))
          col_indices <- seq(start_index, ncol(result), by = length(list(`Any Grade (%)` = c("1", 
              "2", "3", "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5")))
          scorefun_term <- tern::score_occurrences_cols(col_indices = col_indices)
          sorted_result <- result %>% rtables::sort_at_path(path = c("AEDECOD"), 
              scorefun = scorefun_term, decreasing = TRUE)
      }
      
      $prune
      {
          criteria_fun <- function(tr) {
              inherits(tr, "ContentRow")
          }
          at_least_percent_any <- has_fraction_in_any_col(atleast = 0.1, 
              col_indices = col_indices)
          pruned_and_sorted_result <- sorted_result %>% rtables::trim_rows(criteria = criteria_fun) %>% 
              rtables::prune_table(keep_rows(at_least_percent_any))
      }
      

# template_events_col_by_grade without dropping arm levels option works as expected

    Code
      res
    Output
      $data
      {
          anl <- adae
          adsl <- adsl %>% dplyr::mutate(ACTARM = droplevels(ACTARM))
          arm_levels <- levels(adsl[["ACTARM"]])
          anl <- anl %>% dplyr::mutate(ACTARM = factor(ACTARM, levels = arm_levels))
          col_counts <- rep(table(adsl[["ACTARM"]]), each = length(list(`Any Grade (%)` = c("1", 
              "2", "3", "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5")))
          anl <- anl %>% dplyr::group_by(USUBJID, ACTARM, AEDECOD) %>% 
              dplyr::summarize(MAXAETOXGR = factor(max(as.numeric(AETOXGR)))) %>% 
              dplyr::ungroup() %>% tern::df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Adverse Event summary by Analysis Toxicity Grade: Dictionary-Derived Term") %>% 
          rtables::split_cols_by(var = "ACTARM") %>% split_cols_by_groups("MAXAETOXGR", 
          groups = list(`Any Grade (%)` = c("1", "2", "3", "4", "5"), 
              `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
                  "4"), `Grade 5 (%)` = "5")) %>% tern::analyze_vars("AEDECOD", 
          na.rm = FALSE, denom = "N_col", .stats = "count_fraction", 
          .formats = c(count_fraction = format_fraction_threshold(0.01))) %>% 
          tern::append_varlabels(df = anl, vars = "AEDECOD")
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl, col_counts = col_counts)
      
      $sort
      {
          lengths <- lapply(list(`Any Grade (%)` = c("1", "2", "3", 
              "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5"), length)
          start_index <- unname(which.max(lengths))
          col_indices <- seq(start_index, ncol(result), by = length(list(`Any Grade (%)` = c("1", 
              "2", "3", "4", "5"), `Grade 1-2 (%)` = c("1", "2"), `Grade 3-4 (%)` = c("3", 
              "4"), `Grade 5 (%)` = "5")))
          scorefun_term <- tern::score_occurrences_cols(col_indices = col_indices)
          sorted_result <- result %>% rtables::sort_at_path(path = c("AEDECOD"), 
              scorefun = scorefun_term, decreasing = TRUE)
      }
      
      $prune
      {
          criteria_fun <- function(tr) {
              inherits(tr, "ContentRow")
          }
          at_least_percent_any <- has_fraction_in_any_col(atleast = 0.1, 
              col_indices = col_indices)
          pruned_and_sorted_result <- sorted_result %>% rtables::trim_rows(criteria = criteria_fun) %>% 
              rtables::prune_table(keep_rows(at_least_percent_any))
      }
      

