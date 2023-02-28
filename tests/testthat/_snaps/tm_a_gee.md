# template_a_gee t_gee_cov table works as expected with default input

    Code
      res
    Output
      $model
      {
          model_fit <- tern.gee::fit_gee(vars = tern.gee::vars_gee(response = as.vector("AVAL"), 
              covariates = as.vector(NULL), id = as.vector("USUBJID"), 
              arm = as.vector("ARMCD"), visit = as.vector("AVISIT")), 
              data = ANL, regression = "logistic", cor_struct = "unstructured")
      }
      
      $table
      {
          result_table <- tern.gee::as.rtable(model_fit, type = "cov")
          subtitles(result_table) <- NULL
          main_footer(result_table) <- NULL
      }
      

# template_a_gee t_gee_coef table works as expected with default input

    Code
      res
    Output
      $model
      {
          model_fit <- tern.gee::fit_gee(vars = tern.gee::vars_gee(response = as.vector("AVAL"), 
              covariates = as.vector(NULL), id = as.vector("USUBJID"), 
              arm = as.vector("ARMCD"), visit = as.vector("AVISIT")), 
              data = ANL, regression = "logistic", cor_struct = "unstructured")
      }
      
      $table
      {
          result_table <- tern.gee::as.rtable(data.frame(Coefficient = model_fit$coefficients))
          subtitles(result_table) <- NULL
          main_footer(result_table) <- NULL
      }
      

# template_a_gee works as expected with non-default reference arm

    Code
      res
    Output
      $model
      {
          model_fit <- tern.gee::fit_gee(vars = tern.gee::vars_gee(response = as.vector("AVAL"), 
              covariates = as.vector(NULL), id = as.vector("USUBJID"), 
              arm = as.vector("ARMCD"), visit = as.vector("AVISIT")), 
              data = ANL, regression = "logistic", cor_struct = "unstructured")
      }
      
      $table
      {
          lsmeans_fit_model <- tern.gee::lsmeans(model_fit, 0.95)
          result_table <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARM", 
              ref_group = model_fit$ref_level) %>% rtables::add_colcounts() %>% 
              tern.gee::summarize_gee_logistic() %>% rtables::build_table(df = lsmeans_fit_model, 
              alt_counts_df = ANL_ADSL)
          subtitles(result_table) <- NULL
          main_footer(result_table) <- NULL
          result_table
      }
      

# template_a_gee works as expected when arm is not considered in the model

    Code
      res
    Output
      $model
      {
          model_fit <- tern.gee::fit_gee(vars = tern.gee::vars_gee(response = as.vector("AVAL"), 
              covariates = as.vector(NULL), id = as.vector("USUBJID"), 
              arm = as.vector(NULL), visit = as.vector("AVISIT")), 
              data = ANL, regression = "logistic", cor_struct = "unstructured")
      }
      
      $table
      {
          lsmeans_fit_model <- tern.gee::lsmeans(model_fit, 0.95)
          result_table <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARM", 
              ref_group = model_fit$ref_level) %>% rtables::add_colcounts() %>% 
              tern.gee::summarize_gee_logistic() %>% rtables::build_table(df = lsmeans_fit_model, 
              alt_counts_df = ANL_ADSL)
          subtitles(result_table) <- NULL
          main_footer(result_table) <- NULL
          result_table
      }
      

