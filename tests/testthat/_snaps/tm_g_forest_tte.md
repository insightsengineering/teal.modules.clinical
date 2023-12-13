# template_forest_tte generates correct expressions

    Code
      res
    Output
      $data
      {
          anl <- adtte %>% dplyr::filter(ARMCD %in% c("ARM A", "ARM B", 
              "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, c("ARM B", 
                  "ARM C"))) %>% dplyr::mutate(is_event = CNSR == 0)
          parent <- ANL_ADSL %>% dplyr::filter(ARMCD %in% c("ARM A", 
              "ARM B", "ARM C")) %>% dplyr::mutate(ARMCD = stats::relevel(ARMCD, 
              ref = "ARM A")) %>% dplyr::mutate(ARMCD = droplevels(ARMCD)) %>% 
              dplyr::mutate(ARMCD = combine_levels(ARMCD, c("ARM B", 
                  "ARM C")))
      }
      
      $summary
      {
          df <- extract_survival_subgroups(variables = list(tte = "AVAL", 
              is_event = "is_event", arm = "ARMCD", subgroups = c("SEX", 
                  "BMRKR2"), strat = "STRATA2"), control = control_coxph(conf_level = 0.9), 
              data = anl)
      }
      
      $table
      {
          result <- rtables::basic_table() %>% tabulate_survival_subgroups(df, 
              vars = c("n_tot", "n_tot_events", "n", "n_events", "median", 
                  "hr", "ci"), time_unit = as.character(anl$AVALU[1]))
      }
      
      $plot
      $plot[[1]]
      f <- g_forest(tbl = result, col_symbol_size = NULL, font_size = 15, 
          as_list = TRUE)
      
      $plot[[2]]
      p <- cowplot::plot_grid(f[["table"]] + ggplot2::labs(title = "Forest Plot of Survival Duration for \nStratified by STRATA2", 
          subtitle = NULL), f[["plot"]] + ggplot2::labs(caption = ""), 
          align = "h", axis = "tblr", rel_widths = c(1 - 0.25, 0.25))
      
      

