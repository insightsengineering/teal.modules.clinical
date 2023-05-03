# template_coxreg generates correct univariate cox regression expressions

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(event = 1 - CNSR) %>% df_explicit_na(na_level = "")
          control <- list(pval_method = "wald", ties = "efron", conf_level = 0.95, 
              interaction = FALSE)
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Multi-Variable Cox Regression for OS", 
          main_footer = c("p-value method for Coxph (Hazard Ratio): wald", 
              "Ties for Coxph (Hazard Ratio): efron")) %>% rtables::append_topleft("OS") %>% 
          summarize_coxreg(variables = list(time = "AVAL", event = "event", 
              arm = "ARMCD", covariates = NULL, strata = "STRATA1"), 
              control = list(pval_method = "wald", ties = "efron", 
                  conf_level = 0.95, interaction = FALSE), at = list(AGE = c(35, 
                  45)), multivar = FALSE, .stats = c("n", "hr", "ci", 
                  "pval"))
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl)
      

# template_coxreg generates correct univariate cox regression expressions with interactions

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(event = 1 - CNSR) %>% df_explicit_na(na_level = "")
          control <- list(pval_method = "wald", ties = "efron", conf_level = 0.95, 
              interaction = TRUE)
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Multi-Variable Cox Regression for OS", 
          main_footer = c("p-value method for Coxph (Hazard Ratio): wald", 
              "Ties for Coxph (Hazard Ratio): efron")) %>% rtables::append_topleft("OS") %>% 
          summarize_coxreg(variables = list(time = "AVAL", event = "event", 
              arm = "ARMCD", covariates = NULL, strata = "STRATA1"), 
              control = list(pval_method = "wald", ties = "efron", 
                  conf_level = 0.95, interaction = TRUE), at = list(AGE = c(35, 
                  45)), multivar = FALSE, .stats = c("n", "hr", "ci", 
                  "pval", "pval_inter"))
      
      $table
      result <- rtables::build_table(lyt = lyt, df = anl)
      

# template_coxreg generates correct multivariate cox regression expressions

    Code
      res
    Output
      $data
      {
          anl <- adrs %>% dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo", 
              "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, 
              ref = "A: Drug X")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% 
              dplyr::mutate(ARM = combine_levels(x = ARM, levels = c("B: Placebo", 
                  "C: Combination"))) %>% dplyr::mutate(event = 1 - 
              CNSR) %>% df_explicit_na(na_level = "")
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Cox Regression for OS", 
          main_footer = c("p-value method for Coxph (Hazard Ratio): wald", 
              "Ties for Coxph (Hazard Ratio): exact")) %>% rtables::append_topleft("OS") %>% 
          summarize_coxreg(variables = list(time = "AVAL", event = "event", 
              arm = "ARM", covariates = c("AGE", "SEX")), control = list(pval_method = "wald", 
              ties = "exact", conf_level = 0.95, interaction = FALSE), 
              multivar = TRUE, .stats = c("hr", "ci", "pval"))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = anl)
          result
      }
      

