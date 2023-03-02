# template_tte produces healthy standard output

    Code
      res
    Output
      $data
      {
          anl <- ANL %>% dplyr::mutate(ARM = droplevels(ARM)) %>% dplyr::mutate(is_event = CNSR == 
              0, is_not_event = CNSR == 1, EVNT1 = factor(dplyr::case_when(is_event == 
              TRUE ~ "Patients with event (%)", is_event == FALSE ~ 
              "Patients without event (%)"), levels = c("Patients with event (%)", 
              "Patients without event (%)")), EVNTDESC = factor(EVNTDESC)) %>% 
              df_explicit_na()
          ANL_ADSL <- ANL_ADSL %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              df_explicit_na()
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Time-To-Event Table for OS", 
          main_footer = "Confidence Level Type for Survfit: plain") %>% 
          rtables::split_cols_by(var = "ARM") %>% rtables::add_colcounts() %>% 
          summarize_vars("is_event", .stats = "count_fraction", .labels = c(count_fraction = "Patients with event (%)")) %>% 
          rtables::split_rows_by("EVNT1", split_label = "Earliest contributing event", 
              split_fun = keep_split_levels("Patients with event (%)"), 
              label_pos = "visible", child_labels = "hidden", indent_mod = 1L, 
              ) %>% rtables::split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>% 
          rtables::summarize_row_groups(format = "xx") %>% summarize_vars("is_not_event", 
          .stats = "count_fraction", .labels = c(count_fraction = "Patients without event (%)"), 
          nested = FALSE, show_labels = "hidden") %>% surv_time(vars = "AVAL", 
          var_labels = paste0("Time to Event (", as.character(anl$AVALU[1]), 
              ")"), is_event = "is_event", control = list(conf_level = 0.95, 
              conf_type = "plain", quantiles = c(0.25, 0.75)), table_names = "time_to_event") %>% 
          surv_timepoint(vars = "AVAL", var_labels = as.character(anl$AVALU[1]), 
              is_event = "is_event", time_point = c(183, 365, 548), 
              method = "surv", control = control_surv_timepoint(conf_level = 0.95, 
                  conf_type = "plain"), .indent_mods = NULL, table_names = "time_points")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ANL_ADSL)
          table
      }
      

# template_tte produces correct data expression when not comparing arms

    Code
      res
    Output
      $data
      {
          anl <- ANL %>% dplyr::mutate(ARM = droplevels(ARM)) %>% dplyr::mutate(is_event = CNSR == 
              0, is_not_event = CNSR == 1, EVNT1 = factor(dplyr::case_when(is_event == 
              TRUE ~ "Patients with event (%)", is_event == FALSE ~ 
              "Patients without event (%)"), levels = c("Patients with event (%)", 
              "Patients without event (%)")), EVNTDESC = factor(EVNTDESC)) %>% 
              df_explicit_na()
          ANL_ADSL <- ANL_ADSL %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              df_explicit_na()
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Time-To-Event Table for OS", 
          main_footer = "Confidence Level Type for Survfit: plain") %>% 
          rtables::split_cols_by(var = "ARM") %>% rtables::add_colcounts() %>% 
          summarize_vars("is_event", .stats = "count_fraction", .labels = c(count_fraction = "Patients with event (%)")) %>% 
          rtables::split_rows_by("EVNT1", split_label = "Earliest contributing event", 
              split_fun = keep_split_levels("Patients with event (%)"), 
              label_pos = "visible", child_labels = "hidden", indent_mod = 1L, 
              ) %>% rtables::split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>% 
          rtables::summarize_row_groups(format = "xx") %>% summarize_vars("is_not_event", 
          .stats = "count_fraction", .labels = c(count_fraction = "Patients without event (%)"), 
          nested = FALSE, show_labels = "hidden") %>% surv_time(vars = "AVAL", 
          var_labels = paste0("Time to Event (", as.character(anl$AVALU[1]), 
              ")"), is_event = "is_event", control = list(conf_level = 0.95, 
              conf_type = "plain", quantiles = c(0.25, 0.75)), table_names = "time_to_event") %>% 
          surv_timepoint(vars = "AVAL", var_labels = as.character(anl$AVALU[1]), 
              is_event = "is_event", time_point = c(183, 365, 548), 
              method = "surv", control = control_surv_timepoint(conf_level = 0.95, 
                  conf_type = "plain"), .indent_mods = NULL, table_names = "time_points")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ANL_ADSL)
          table
      }
      

# template_tte produces correct data expression when comparing and combining arms

    Code
      res
    Output
      $data
      {
          anl <- ANL %>% dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo", 
              "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, 
              ref = "")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              dplyr::mutate(is_event = CNSR == 0, is_not_event = CNSR == 
                  1, EVNT1 = factor(dplyr::case_when(is_event == TRUE ~ 
                  "Patients with event (%)", is_event == FALSE ~ "Patients without event (%)"), 
                  levels = c("Patients with event (%)", "Patients without event (%)")), 
                  EVNTDESC = factor(EVNTDESC)) %>% df_explicit_na()
          ANL_ADSL <- ANL_ADSL %>% dplyr::filter(ARM %in% c("A: Drug X", 
              "B: Placebo", "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, 
              ref = "")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              df_explicit_na()
      }
      
      $combine_comp_arms
      groups <- combine_groups(fct = ANL_ADSL[["ARM"]], ref = "")
      
      $layout
      lyt <- rtables::basic_table(title = "Time-To-Event Table for OS", 
          main_footer = c("p-value method for Coxph (Hazard Ratio): log-rank", 
              "Ties for Coxph (Hazard Ratio): efron", "Confidence Level Type for Survfit: plain")) %>% 
          split_cols_by_groups(var = "ARM", groups_list = groups, ref_group = names(groups)[1]) %>% 
          rtables::add_colcounts() %>% summarize_vars("is_event", .stats = "count_fraction", 
          .labels = c(count_fraction = "Patients with event (%)")) %>% 
          rtables::split_rows_by("EVNT1", split_label = "Earliest contributing event", 
              split_fun = keep_split_levels("Patients with event (%)"), 
              label_pos = "visible", child_labels = "hidden", indent_mod = 1L, 
              ) %>% rtables::split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>% 
          rtables::summarize_row_groups(format = "xx") %>% summarize_vars("is_not_event", 
          .stats = "count_fraction", .labels = c(count_fraction = "Patients without event (%)"), 
          nested = FALSE, show_labels = "hidden") %>% surv_time(vars = "AVAL", 
          var_labels = paste0("Time to Event (", as.character(anl$AVALU[1]), 
              ")"), is_event = "is_event", control = list(conf_level = 0.95, 
              conf_type = "plain", quantiles = c(0.25, 0.75)), table_names = "time_to_event") %>% 
          coxph_pairwise(vars = "AVAL", is_event = "is_event", var_labels = c("Unstratified Analysis"), 
              control = list(pval_method = "log-rank", ties = "efron", 
                  conf_level = 0.95), table_names = "unstratified") %>% 
          surv_timepoint(vars = "AVAL", var_labels = as.character(anl$AVALU[1]), 
              is_event = "is_event", time_point = c(183, 365, 548), 
              method = "both", control = control_surv_timepoint(conf_level = 0.95, 
                  conf_type = "plain"), .indent_mods = c(pt_at_risk = 0L, 
                  event_free_rate = 0L, rate_ci = 0L, rate_diff = 1L, 
                  rate_diff_ci = 1L, ztest_pval = 1L), table_names = "time_points")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ANL_ADSL)
          table
      }
      

# template_tte produces correct data expression when comparing arms

    Code
      res
    Output
      $data
      {
          anl <- ANL %>% dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo", 
              "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, 
              ref = "")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              dplyr::mutate(is_event = CNSR == 0, is_not_event = CNSR == 
                  1, EVNT1 = factor(dplyr::case_when(is_event == TRUE ~ 
                  "Patients with event (%)", is_event == FALSE ~ "Patients without event (%)"), 
                  levels = c("Patients with event (%)", "Patients without event (%)")), 
                  EVNTDESC = factor(EVNTDESC)) %>% df_explicit_na()
          ANL_ADSL <- ANL_ADSL %>% dplyr::filter(ARM %in% c("A: Drug X", 
              "B: Placebo", "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, 
              ref = "")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              df_explicit_na()
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Time-To-Event Table for OS", 
          main_footer = c("p-value method for Coxph (Hazard Ratio): log-rank", 
              "Ties for Coxph (Hazard Ratio): efron", "Confidence Level Type for Survfit: plain")) %>% 
          rtables::split_cols_by(var = "ARM", ref_group = "") %>% rtables::add_colcounts() %>% 
          summarize_vars("is_event", .stats = "count_fraction", .labels = c(count_fraction = "Patients with event (%)")) %>% 
          rtables::split_rows_by("EVNT1", split_label = "Earliest contributing event", 
              split_fun = keep_split_levels("Patients with event (%)"), 
              label_pos = "visible", child_labels = "hidden", indent_mod = 1L, 
              ) %>% rtables::split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>% 
          rtables::summarize_row_groups(format = "xx") %>% summarize_vars("is_not_event", 
          .stats = "count_fraction", .labels = c(count_fraction = "Patients without event (%)"), 
          nested = FALSE, show_labels = "hidden") %>% surv_time(vars = "AVAL", 
          var_labels = paste0("Time to Event (", as.character(anl$AVALU[1]), 
              ")"), is_event = "is_event", control = list(conf_level = 0.95, 
              conf_type = "plain", quantiles = c(0.25, 0.75)), table_names = "time_to_event") %>% 
          coxph_pairwise(vars = "AVAL", is_event = "is_event", var_labels = c("Unstratified Analysis"), 
              control = list(pval_method = "log-rank", ties = "efron", 
                  conf_level = 0.95), table_names = "unstratified") %>% 
          surv_timepoint(vars = "AVAL", var_labels = as.character(anl$AVALU[1]), 
              is_event = "is_event", time_point = c(183, 365, 548), 
              method = "both", control = control_surv_timepoint(conf_level = 0.95, 
                  conf_type = "plain"), .indent_mods = c(pt_at_risk = 0L, 
                  event_free_rate = 0L, rate_ci = 0L, rate_diff = 1L, 
                  rate_diff_ci = 1L, ztest_pval = 1L), table_names = "time_points")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ANL_ADSL)
          table
      }
      

