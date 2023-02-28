# template_logistic generates correct expressions

    Code
      res
    Output
      $arm_lab
      arm_var_lab <- formatters::var_labels(ANL["ARMCD"], fill = FALSE)
      
      $data
      {
          ANL <- ANL %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = combine_levels(ARMCD, 
              levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>% 
              dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A/ARM B")) %>% 
              dplyr::mutate(ARMCD = droplevels(ARMCD))
          ANL <- ANL %>% dplyr::mutate(Response = AVALC %in% "CR") %>% 
              df_explicit_na(na_level = "_NA_")
      }
      
      $relabel
      formatters::var_labels(ANL["ARMCD"]) <- arm_var_lab
      
      $model
      mod <- fit_logistic(ANL, variables = list(response = "Response", 
          arm = "ARMCD", covariates = c("AGE", "SEX"), interaction = "AGE")) %>% 
          broom::tidy(conf_level = 0.95, at = c(30, 40)) %>% df_explicit_na(na_level = "_NA_")
      
      $table
      {
          result <- rtables::basic_table(title = "Summary of Logistic Regression Analysis for Best Confirmed Overall Response by Investigator for CR Responders") %>% 
              summarize_logistic(conf_level = 0.95, drop_and_remove_str = "_NA_") %>% 
              rtables::append_topleft("BESRSPI") %>% rtables::build_table(df = mod)
          result
      }
      

---

    Code
      res
    Output
      $arm_lab
      arm_var_lab <- formatters::var_labels(ANL["ARMCD"], fill = FALSE)
      
      $data
      {
          ANL <- ANL %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = combine_levels(ARMCD, 
              levels = c("ARM A", "ARM B"), new_level = "ARM A/ARM B")) %>% 
              dplyr::mutate(ARMCD = stats::relevel(ARMCD, ref = "ARM A/ARM B")) %>% 
              dplyr::mutate(ARMCD = droplevels(ARMCD))
          ANL <- ANL %>% dplyr::mutate(Response = AVALC %in% "CR") %>% 
              df_explicit_na(na_level = "_NA_")
      }
      
      $relabel
      formatters::var_labels(ANL["ARMCD"]) <- arm_var_lab
      
      $model
      mod <- fit_logistic(ANL, variables = list(response = "Response", 
          arm = "ARMCD", covariates = c("AGE", "SEX"), interaction = "AGE")) %>% 
          broom::tidy(conf_level = 0.95, at = c(30, 40)) %>% df_explicit_na(na_level = "_NA_")
      
      $table
      {
          result <- rtables::basic_table(title = "Summary of Logistic Regression Analysis for Best Confirmed Overall Response by Investigator for CR Responders") %>% 
              summarize_logistic(conf_level = 0.95, drop_and_remove_str = "_NA_") %>% 
              rtables::append_topleft("BESRSPI") %>% rtables::build_table(df = mod)
          result
      }
      

# template_logistic generates correct expressions for no arm variable

    Code
      res
    Output
      $data
      {
          ANL <- ANL %>% dplyr::mutate(Response = AVALC %in% "CR") %>% 
              df_explicit_na(na_level = "_NA_")
      }
      
      $model
      mod <- fit_logistic(ANL, variables = list(response = "Response", 
          arm = NULL, covariates = c("AGE", "SEX"), interaction = "AGE")) %>% 
          broom::tidy(conf_level = 0.95, at = c(30, 40)) %>% df_explicit_na(na_level = "_NA_")
      
      $table
      {
          result <- rtables::basic_table(title = "Summary of Logistic Regression Analysis for Best Confirmed Overall Response by Investigator for CR Responders") %>% 
              summarize_logistic(conf_level = 0.95, drop_and_remove_str = "_NA_") %>% 
              rtables::append_topleft("BESRSPI") %>% rtables::build_table(df = mod)
          result
      }
      

