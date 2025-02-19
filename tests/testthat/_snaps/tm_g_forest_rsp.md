# template_forest_rsp generates correct expressions

    Code
      res
    Output
      $data
      {
          adrs <- adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(is_rsp = AVALC %in% c("CR", "PR")) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", 
                  "ARM C")))
          parent <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", 
                  "ARM C")))
      }
      
      $summary
      {
          df <- extract_rsp_subgroups(variables = list(rsp = "is_rsp", 
              arm = "ARMCD", subgroups = c("SEX", "STRATA2"), strata = NULL), 
              data = adrs, conf_level = 0.95)
      }
      
      $table
      result <- rtables::basic_table() %>% tabulate_rsp_subgroups(df, 
          vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"), riskdiff = NULL)
      
      $plot
      $plot[[1]]
      f <- g_forest(tbl = result, col_symbol_size = NULL, font_size = 15, 
          as_list = TRUE)
      
      $plot[[2]]
      plot <- cowplot::plot_grid(f[["table"]] + ggplot2::labs(title = "Forest Plot of Best Overall Response for "), 
          f[["plot"]] + ggplot2::labs(caption = ""), align = "h", axis = "tblr", 
          rel_widths = c(1 - 0.25, 0.25))
      
      

# template_forest_rsp works with risk difference column added

    Code
      res
    Output
      $data
      {
          adrs <- adrs %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(is_rsp = AVALC %in% c("CR", "PR")) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", 
                  "ARM C")))
          parent <- adsl %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, levels = c("ARM B", 
                  "ARM C")))
      }
      
      $summary
      {
          df <- extract_rsp_subgroups(variables = list(rsp = "is_rsp", 
              arm = "ARMCD", subgroups = c("SEX", "STRATA2"), strata = NULL), 
              data = adrs, conf_level = 0.95)
      }
      
      $table
      result <- rtables::basic_table() %>% tabulate_rsp_subgroups(df, 
          vars = c("n_tot", "or", "ci"), riskdiff = list(arm_x = NULL, 
              arm_y = NULL, format = "xx.x (xx.x - xx.x)", col_label = "Prop. Diff", 
              pct = TRUE))
      
      $plot
      $plot[[1]]
      f <- g_forest(tbl = result, col_symbol_size = NULL, font_size = 15, 
          as_list = TRUE)
      
      $plot[[2]]
      plot <- cowplot::plot_grid(f[["table"]] + ggplot2::labs(title = "Forest Plot of Best Overall Response for "), 
          f[["plot"]] + ggplot2::labs(caption = ""), align = "h", axis = "tblr", 
          rel_widths = c(1 - 0.25, 0.25))
      
      

