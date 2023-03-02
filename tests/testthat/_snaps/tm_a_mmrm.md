# template_fit_mmrm works as expected when not combining comparison arms

    Code
      res
    Output
      $data
      {
          anl <- adqs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              df_explicit_na(na_level = "")
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              df_explicit_na(na_level = "")
      }
      
      $fit
      fit <- tern.mmrm::fit_mmrm(vars = list(response = "AVAL", covariates = NULL, 
          id = "USUBJID", arm = "ARMCD", visit = "AVISIT"), data = anl, 
          conf_level = 0.95, method = "Satterthwaite", cor_struct = "unstructured", 
          weights_emmeans = "proportional", parallel = FALSE)
      

# template_fit_mmrm works as expected when combining combination arms

    Code
      res
    Output
      $data
      {
          anl <- adqs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", 
                  "ARM C"))) %>% df_explicit_na(na_level = "")
          adsl <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", 
                  "ARM C"))) %>% df_explicit_na(na_level = "")
      }
      
      $fit
      fit <- tern.mmrm::fit_mmrm(vars = list(response = "AVAL", covariates = c("SEX", 
      "BASE", "AVISIT"), id = "USUBJID", arm = "ARMCD", visit = "AVISIT"), 
          data = anl, conf_level = 0.95, method = "Satterthwaite", 
          cor_struct = "unstructured", weights_emmeans = "proportional", 
          parallel = TRUE)
      

# template_mmrm_tables works as expected

    Code
      res
    Output
      $layout
      lyt <- rtables::basic_table() %>% rtables::split_cols_by(var = "ARMCD", 
          ref_group = "ARM A") %>% rtables::add_colcounts() %>% rtables::split_rows_by("AVISIT") %>% 
          append_varlabels(ANL, "AVISIT") %>% tern.mmrm::summarize_lsmeans(show_relative = "increase") %>% 
          rtables::append_topleft(paste0("  ", "ALBUMIN"))
      
      $cov_matrix
      {
          cov_matrix <- tern.mmrm::as.rtable(fit_mmrm, type = "cov")
          subtitles(cov_matrix) <- NULL
          cov_matrix
      }
      

# template_mmrm_tables works as expected when arm is not considered in the model

    Code
      res
    Output
      $layout
      lyt <- rtables::basic_table() %>% rtables::add_overall_col("All Patients") %>% 
          rtables::split_rows_by("AVISIT") %>% tern.mmrm::summarize_lsmeans(arms = FALSE) %>% 
          rtables::append_topleft(paste0("  ", "ALBUMIN"))
      
      $cov_matrix
      {
          cov_matrix <- tern.mmrm::as.rtable(fit_mmrm, type = "cov")
          subtitles(cov_matrix) <- NULL
          cov_matrix
      }
      

# template_mmrm_plots works as expected

    Code
      res
    Output
      $lsmeans_plot
      {
          lsmeans_plot <- tern.mmrm::g_mmrm_lsmeans(fit_mmrm, select = c("estimates", 
          "contrasts"), width = 0.6, show_pval = FALSE, titles = if (is.null(fit_mmrm$vars$arm)) {
              c(estimates = paste("Adjusted mean of", fit_mmrm$labels$response, 
                  " at visits"), contrasts = " ")
          }
          else {
              c(estimates = paste("Adjusted mean of", fit_mmrm$labels$response, 
                  "by treatment at visits"), contrasts = paste0("Differences of ", 
                  fit_mmrm$labels$response, " adjusted means vs. control ('", 
                  fit_mmrm$ref_level, "')"))
          })
          lsmeans_plot
      }
      
      $diagnostic_plot
      {
          diagnostic_plot <- tern.mmrm::g_mmrm_diagnostic(fit_mmrm, 
              type = "fit-residual", z_threshold = NULL)
          diagnostic_plot
      }
      

