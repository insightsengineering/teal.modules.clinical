# template_g_lineplot works as expected with default arguments

    Code
      res
    Output
      $data
      {
          anl <- ANL %>% dplyr::mutate(ARM = droplevels(ARM))
      }
      
      $variables
      variables <- control_lineplot_vars(x = "AVISIT", y = "AVAL", 
          strata = "ARM", paramcd = "PARAMCD", y_unit = "AVALU")
      
      $graph
      {
          grid::grid.newpage()
          plot <- g_lineplot(df = anl, variables = variables, interval = "mean_ci", 
              mid = "mean", whiskers = c("mean_ci_lwr", "mean_ci_upr"), 
              table = c("n", "mean_sd", "median", "range"), mid_type = "pl", 
              mid_point_size = 2, table_font_size = 4, newpage = FALSE, 
              title = "Plot of Mean and 95% Mean Confidence Interval of AVAL by Visit", 
              subtitle = "", caption = NULL, y_lab = "AVAL Mean Values for", 
              legend_title = NULL, ggtheme = ggplot2::theme_minimal(), 
              control = control_summarize_vars(conf_level = 0.95), 
              subtitle_add_paramcd = FALSE, subtitle_add_unit = FALSE)
          plot
      }
      

# template_g_lineplot gives correct data expression with custom arguments

    Code
      res
    Output
      $data
      {
          anl <- ANL %>% dplyr::filter(AVISIT != "SCREENING") %>% dplyr::mutate(AVISIT = factor(AVISIT)) %>% 
              dplyr::mutate(ARMCD = droplevels(ARMCD))
      }
      
      $variables
      variables <- control_lineplot_vars(x = "AVISIT", y = "CHG", strata = "ARMCD", 
          paramcd = "PARAMCD", y_unit = "AVALU")
      
      $graph
      {
          grid::grid.newpage()
          plot <- g_lineplot(df = anl, variables = variables, interval = "median_ci", 
              mid = "median", whiskers = "median_ci_upr", table = c("mean_sd", 
                  "median", "median_ci"), mid_type = "l", mid_point_size = 2, 
              table_font_size = 4, newpage = FALSE, title = "Plot of Median and 90% Median Confidence Interval of CHG by Visit", 
              subtitle = "", caption = NULL, y_lab = "CHG Median Values for", 
              legend_title = NULL, ggtheme = ggplot2::theme_minimal(), 
              control = control_summarize_vars(conf_level = 0.9), subtitle_add_paramcd = FALSE, 
              subtitle_add_unit = FALSE)
          plot
      }
      

