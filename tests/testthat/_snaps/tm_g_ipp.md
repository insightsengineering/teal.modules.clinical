# template_g_ipp works as expected with default arguments

    Code
      res
    Output
      $data
      anl <- ANL %>% droplevels()
      
      $graph
      {
          plot <- h_g_ipp(df = anl, xvar = "AVISIT", yvar = "AVAL", 
              xlab = "Visit", ylab = "e (d)", title = "Individual Patient Plot for e Values (d) over Time", 
              subtitle = "a, b, c", id_var = "USUBJID", add_baseline_hline = FALSE, 
              yvar_baseline = "BASE")
          grid::grid.newpage()
          grid::grid.draw(plot)
      }
      

# template_g_ipp works as expected with non-default arguments

    Code
      res
    Output
      $data
      anl <- adlb %>% droplevels()
      
      $graph
      {
          plot <- h_g_ipp(df = anl, xvar = "AVISIT", yvar = "AVAL", 
              xlab = "Visit", ylab = "e (d)", title = "Individual Patient Plot for e Values (d) over Time", 
              subtitle = "a, b, c", id_var = "SUBJID", add_baseline_hline = TRUE, 
              yvar_baseline = "BASE")
          plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(SUBJID))
          grid::grid.newpage()
          grid::grid.draw(plot)
      }
      

