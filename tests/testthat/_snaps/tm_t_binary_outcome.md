# template_binary_outcome generates standard expressions

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(is_rsp = dplyr::if_else(!is.na(AVALC), 
                  AVALC %in% c("Complete Response (CR)", "Partial Response (PR)"), 
                  NA)) %>% dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", 
              "Partial Response (PR)")))
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders", 
          subtitles = "") %>% rtables::split_cols_by(var = "ARMCD", 
          ref_group = "ARM A") %>% estimate_proportion(vars = "is_rsp", 
          conf_level = 0.95, method = "waldcc", table_names = "prop_est", 
          denom = "N_col") %>% estimate_proportion_diff(vars = "is_rsp", 
          show_labels = "visible", var_labels = "Unstratified Analysis", 
          conf_level = 0.95, method = "waldcc", table_names = "u_prop_diff") %>% 
          test_proportion_diff(vars = "is_rsp", method = "schouten", 
              table_names = "u_test_diff") %>% estimate_odds_ratio(vars = "is_rsp", 
          conf_level = 0.95, table_names = "u_est_or") %>% estimate_multinomial_response(var = "AVALC", 
          conf_level = 0.95, method = "waldcc")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

# template_binary_outcome generates right expressions with non-default

    Code
      res
    Output
      $data
      {
          anl <- ADRS %>% dplyr::filter(ARM %in% c("B: Placebo", "A: Drug X", 
              "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, 
              ref = "B: Placebo")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              dplyr::mutate(is_rsp = dplyr::if_else(!is.na(AVALC), 
                  AVALC %in% c("PR", "SD"), NA)) %>% dplyr::mutate(AVALC = factor(AVALC, 
              levels = c("PR", "SD")))
          ADSL <- ADSL %>% dplyr::filter(ARM %in% c("B: Placebo", "A: Drug X", 
              "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, 
              ref = "B: Placebo")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Table of BESRSPI for PR and SD Responders", 
          subtitles = "") %>% rtables::split_cols_by(var = "ARM", ref_group = "B: Placebo") %>% 
          estimate_proportion(vars = "is_rsp", conf_level = 0.95, method = "waldcc", 
              table_names = "prop_est", denom = "N_col") %>% estimate_proportion_diff(vars = "is_rsp", 
          show_labels = "visible", var_labels = "Unstratified Analysis", 
          conf_level = 0.95, method = "waldcc", table_names = "u_prop_diff") %>% 
          test_proportion_diff(vars = "is_rsp", method = "schouten", 
              table_names = "u_test_diff") %>% estimate_odds_ratio(vars = "is_rsp", 
          conf_level = 0.95, table_names = "u_est_or")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      }
      

# template_binary_outcome generates expression without arm comparison

    Code
      res
    Output
      $data
      {
          anl <- ADRS %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              dplyr::mutate(is_rsp = dplyr::if_else(!is.na(AVALC), 
                  AVALC %in% c("Complete Response (CR)", "Partial Response (PR)"), 
                  NA)) %>% dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", 
              "Partial Response (PR)")))
          ADSL <- ADSL %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders", 
          subtitles = "") %>% rtables::split_cols_by(var = "ARM") %>% 
          estimate_proportion(vars = "is_rsp", conf_level = 0.95, method = "waldcc", 
              table_names = "prop_est", denom = "N_col")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      }
      

# template_binary_outcome generates expression with non-default controls and strata.

    Code
      res
    Output
      $data
      {
          anl <- ADRS %>% dplyr::filter(ARM %in% c("B: Placebo", "A: Drug X", 
              "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, 
              ref = "B: Placebo")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              dplyr::mutate(is_rsp = dplyr::if_else(!is.na(AVALC), 
                  AVALC %in% c("Complete Response (CR)", "Partial Response (PR)"), 
                  NA)) %>% dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", 
              "Partial Response (PR)")))
          ADSL <- ADSL %>% dplyr::filter(ARM %in% c("B: Placebo", "A: Drug X", 
              "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, 
              ref = "B: Placebo")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders", 
          subtitles = "Stratified by SEX") %>% rtables::split_cols_by(var = "ARM", 
          ref_group = "B: Placebo") %>% estimate_proportion(vars = "is_rsp", 
          conf_level = 0.8, method = "jeffreys", table_names = "prop_est", 
          denom = "N_col") %>% estimate_proportion_diff(vars = "is_rsp", 
          show_labels = "visible", var_labels = "Unstratified Analysis", 
          conf_level = 0.8, method = "ha", table_names = "u_prop_diff") %>% 
          test_proportion_diff(vars = "is_rsp", method = "chisq", table_names = "u_test_diff") %>% 
          estimate_odds_ratio(vars = "is_rsp", conf_level = 0.8, table_names = "u_est_or") %>% 
          estimate_proportion_diff(vars = "is_rsp", show_labels = "visible", 
              var_labels = "Stratified Analysis", variables = list(strata = "SEX"), 
              conf_level = 0.8, method = "cmh", table_names = "s_prop_diff") %>% 
          test_proportion_diff(vars = "is_rsp", method = "cmh", variables = list(strata = "SEX"), 
              table_names = "s_test_diff") %>% estimate_odds_ratio(vars = "is_rsp", 
          variables = list(arm = "ARM", strata = "SEX"), conf_level = 0.8, 
          table_names = "s_est_or") %>% estimate_multinomial_response(var = "AVALC", 
          conf_level = 0.8, method = "jeffreys")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      }
      

# template_binary_outcome can combine comparison arms

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(is_rsp = dplyr::if_else(!is.na(AVALC), 
                  AVALC %in% c("Complete Response (CR)", "Partial Response (PR)"), 
                  NA)) %>% dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", 
              "Partial Response (PR)")))
          ADSL <- ADSL %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              df_explicit_na(na_level = "<Missing>")
      }
      
      $combine_comp_arms
      groups <- combine_groups(fct = ADSL[["ARMCD"]], ref = "ARM A")
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders", 
          subtitles = "") %>% split_cols_by_groups(var = "ARMCD", groups_list = groups, 
          ref_group = names(groups)[1]) %>% estimate_proportion(vars = "is_rsp", 
          conf_level = 0.95, method = "waldcc", table_names = "prop_est", 
          denom = "N_col") %>% estimate_proportion_diff(vars = "is_rsp", 
          show_labels = "visible", var_labels = "Unstratified Analysis", 
          conf_level = 0.95, method = "waldcc", table_names = "u_prop_diff") %>% 
          test_proportion_diff(vars = "is_rsp", method = "schouten", 
              table_names = "u_test_diff") %>% estimate_odds_ratio(vars = "is_rsp", 
          conf_level = 0.95, table_names = "u_est_or") %>% estimate_multinomial_response(var = "AVALC", 
          conf_level = 0.95, method = "waldcc")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      }
      

# template_binary_outcome can combine comparison arms when compare arms is FALSE

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(is_rsp = dplyr::if_else(!is.na(AVALC), 
                  AVALC %in% c("Complete Response (CR)", "Partial Response (PR)"), 
                  NA)) %>% dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", 
              "Partial Response (PR)")))
          ADSL <- ADSL %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders", 
          subtitles = "") %>% rtables::split_cols_by(var = "ARMCD") %>% 
          estimate_proportion(vars = "is_rsp", conf_level = 0.95, method = "waldcc", 
              table_names = "prop_est", denom = "N_col") %>% estimate_multinomial_response(var = "AVALC", 
          conf_level = 0.95, method = "waldcc")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = ADSL)
      }
      

# split_col_expr prepare the right four possible expressions

    Code
      res
    Output
      [[1]]
      split_cols_by_groups(var = "ARMCD", groups_list = groups, ref_group = names(groups)[1])
      
      [[2]]
      rtables::split_cols_by(var = "ARMCD", ref_group = "ARM C")
      
      [[3]]
      rtables::split_cols_by(var = "ARMCD")
      
      [[4]]
      rtables::split_cols_by(var = "ARMCD")
      

# template_binary_outcome can combine refs

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = combine_levels(ARMCD, 
              levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>% 
              dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A/ARM B")) %>% 
              dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% dplyr::mutate(is_rsp = dplyr::if_else(!is.na(AVALC), 
              AVALC %in% c("Complete Response (CR)", "Partial Response (PR)"), 
              NA)) %>% dplyr::mutate(AVALC = factor(AVALC, levels = c("Complete Response (CR)", 
              "Partial Response (PR)")))
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = combine_levels(ARMCD, 
              levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>% 
              dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A/ARM B")) %>% 
              dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% df_explicit_na(na_level = "<Missing>")
      }
      
      $layout
      lyt <- rtables::basic_table(show_colcounts = TRUE, title = "Table of BESRSPI for Complete Response (CR) and Partial Response (PR) Responders", 
          subtitles = "") %>% rtables::split_cols_by(var = "ARMCD", 
          ref_group = "ARM A/ARM B") %>% estimate_proportion(vars = "is_rsp", 
          conf_level = 0.95, method = "waldcc", table_names = "prop_est", 
          denom = "N_col") %>% estimate_proportion_diff(vars = "is_rsp", 
          show_labels = "visible", var_labels = "Unstratified Analysis", 
          conf_level = 0.95, method = "waldcc", table_names = "u_prop_diff") %>% 
          test_proportion_diff(vars = "is_rsp", method = "schouten", 
              table_names = "u_test_diff") %>% estimate_odds_ratio(vars = "is_rsp", 
          conf_level = 0.95, table_names = "u_est_or") %>% estimate_multinomial_response(var = "AVALC", 
          conf_level = 0.95, method = "waldcc")
      
      $table
      {
          table <- rtables::build_table(lyt = lyt, df = anl, alt_counts_df = adsl)
      }
      

