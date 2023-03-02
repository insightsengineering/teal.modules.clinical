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
          variables <- list(time = "AVAL", event = "event", arm = "ARMCD", 
              covariates = NULL)
          variables$strata <- "STRATA1"
          model <- fit_coxreg_univar(variables = variables, data = anl, 
              control = list(pval_method = "wald", ties = "efron", 
                  conf_level = 0.95, interaction = FALSE), at = list(AGE = c(35, 
                  45)))
          df <- broom::tidy(model)
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Multi-Variable Cox Regression for OS", 
          main_footer = c("p-value method for Coxph (Hazard Ratio): wald", 
              "Ties for Coxph (Hazard Ratio): efron")) %>% rtables::split_rows_by("effect") %>% 
          rtables::append_topleft("OS") %>% rtables::split_rows_by("term", 
          child_labels = "hidden") %>% summarize_coxreg(multivar = FALSE, 
          conf_level = 0.95, vars = c("n", "hr", "ci", "pval"))
      
      $table
      result <- rtables::build_table(lyt = lyt, df = df)
      

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
          variables <- list(time = "AVAL", event = "event", arm = "ARMCD", 
              covariates = NULL)
          variables$strata <- "STRATA1"
          model <- fit_coxreg_univar(variables = variables, data = anl, 
              control = list(pval_method = "wald", ties = "efron", 
                  conf_level = 0.95, interaction = TRUE), at = list(AGE = c(35, 
                  45)))
          df <- broom::tidy(model)
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Multi-Variable Cox Regression for OS", 
          main_footer = c("p-value method for Coxph (Hazard Ratio): wald", 
              "Ties for Coxph (Hazard Ratio): efron")) %>% rtables::split_rows_by("effect") %>% 
          rtables::append_topleft("OS") %>% rtables::split_rows_by("term", 
          child_labels = "hidden") %>% summarize_coxreg(multivar = FALSE, 
          conf_level = 0.95, vars = c("n", "hr", "ci", "pval", "pval_inter"))
      
      $table
      result <- rtables::build_table(lyt = lyt, df = df)
      

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
          variables <- list(time = "AVAL", event = "event", arm = "ARM", 
              covariates = c("AGE", "SEX"))
          model <- fit_coxreg_multivar(variables = variables, data = anl, 
              control = list(pval_method = "wald", ties = "exact", 
                  conf_level = 0.95, interaction = FALSE))
          df <- broom::tidy(model)
      }
      
      $layout
      lyt <- rtables::basic_table(title = "Cox Regression for OS", 
          main_footer = c("p-value method for Coxph (Hazard Ratio): wald", 
              "Ties for Coxph (Hazard Ratio): exact")) %>% rtables::append_topleft("OS") %>% 
          rtables::split_rows_by("term", child_labels = "hidden") %>% 
          summarize_coxreg(multivar = TRUE, conf_level = 0.95, vars = c("n", 
              "hr", "ci", "pval"))
      
      $table
      {
          result <- rtables::build_table(lyt = lyt, df = df)
          result
      }
      

