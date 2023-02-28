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
      {
          p <- decorate_grob(g_forest(tbl = result, col_symbol_size = NULL), 
              titles = c("Forest Plot of Survival Duration for ", "Stratified by STRATA2"
              ), footnotes = "", gp_footnotes = grid::gpar(fontsize = 12))
          grid::grid.newpage()
          grid::grid.draw(p)
      }
      

