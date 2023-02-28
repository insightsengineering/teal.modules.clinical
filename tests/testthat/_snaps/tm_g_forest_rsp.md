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
              arm = "ARMCD", subgroups = c("SEX", "STRATA2"), strat = NULL), 
              data = adrs, conf_level = 0.95)
      }
      
      $table
      result <- rtables::basic_table() %>% tabulate_rsp_subgroups(df, 
          vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))
      
      $plot
      {
          p <- decorate_grob(g_forest(tbl = result, col_symbol_size = NULL), 
              titles = "Forest plot of best overall response for ", 
              footnotes = "", gp_footnotes = grid::gpar(fontsize = 12))
          grid::grid.newpage()
          grid::grid.draw(p)
      }
      

