# template_g_km works as expected with default arguments

    Code
      res
    Output
      $data
      {
          anl <- ANL %>% dplyr::mutate(ARM = droplevels(ARM)) %>% dplyr::mutate(is_event = CNSR == 
              0)
      }
      
      $variables
      variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARM")
      
      $graph
      {
          facets <- droplevels(anl$SEX)
          anl <- split(anl, f = facets)
          g_km_counter_generator <- function() {
              plot_number <- 0L
              function(x) {
                  plot_number <<- plot_number + 1L
                  g_km(x, variables = variables, control_surv = control_surv_timepoint(conf_level = 0.95), 
                      xticks = NULL, xlab = sprintf("%s (%s)", "Survival time", 
                        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", 
                          tolower(x$AVALU[1]), perl = TRUE)), yval = "Survival", 
                      title = sprintf("%s%s", sprintf("%s%s", "KM Plot", 
                        if (!is.null(facets)) {
                          sprintf(", %s = %s", as.character(quote(SEX)), 
                            unique(x[[as.character(quote(SEX))]]))
                        }
                        else {
                          ""
                        }), if (length(NULL) != 0) {
                        sprintf("\nStratified by %s", toString(NULL))
                      }
                      else {
                        ""
                      }), footnotes = if (TRUE) {
                        paste("Ties for Coxph (Hazard Ratio):", "efron", 
                          "\n", "p-value Method for Coxph (Hazard Ratio):", 
                          "log-rank")
                      }, font_size = 11, ci_ribbon = FALSE, annot_surv_med = TRUE, 
                      annot_coxph = TRUE, control_coxph_pw = control_coxph(conf_level = 0.95, 
                        pval_method = "log-rank", ties = "efron"), 
                      control_annot_surv_med = list(x = 0.8, y = 0.85, 
                        w = 0.32, h = 0.16, fill = TRUE), control_annot_coxph = list(x = 0.27, 
                        y = 0.35, w = 0.3, h = 0.125, fill = TRUE, 
                        ref_lbls = FALSE), legend_pos = NULL, rel_height_plot = 0.8)
              }
          }
          g_km_counter <- g_km_counter_generator()
          plot_list <- lapply(anl, g_km_counter)
          plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)
          plot
      }
      

# template_g_km gives correct data expression when we want to compare versus reference arms

    Code
      res
    Output
      $data
      {
          anl <- ANL %>% dplyr::filter(ARM %in% c("ARM C", "ARM D", 
              "ARM A", "ARM B")) %>% dplyr::mutate(ARM = combine_levels(ARM, 
              levels = c("ARM C", "ARM D"), new_level = "ARM C/ARM D")) %>% 
              dplyr::mutate(ARM = stats::relevel(ARM, ref = "ARM C/ARM D")) %>% 
              dplyr::mutate(ARM = droplevels(ARM)) %>% dplyr::mutate(is_event = CNSR == 
              0)
      }
      
      $variables
      variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARM")
      
      $graph
      {
          facets <- droplevels(anl$SEX)
          anl <- split(anl, f = facets)
          g_km_counter_generator <- function() {
              plot_number <- 0L
              function(x) {
                  plot_number <<- plot_number + 1L
                  g_km(x, variables = variables, control_surv = control_surv_timepoint(conf_level = 0.95), 
                      xticks = NULL, xlab = sprintf("%s (%s)", "Survival time", 
                        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", 
                          tolower(x$AVALU[1]), perl = TRUE)), yval = "Survival", 
                      title = sprintf("%s%s", sprintf("%s%s", "KM Plot", 
                        if (!is.null(facets)) {
                          sprintf(", %s = %s", as.character(quote(SEX)), 
                            unique(x[[as.character(quote(SEX))]]))
                        }
                        else {
                          ""
                        }), if (length(NULL) != 0) {
                        sprintf("\nStratified by %s", toString(NULL))
                      }
                      else {
                        ""
                      }), footnotes = if (TRUE) {
                        paste("Ties for Coxph (Hazard Ratio):", "efron", 
                          "\n", "p-value Method for Coxph (Hazard Ratio):", 
                          "log-rank")
                      }, font_size = 11, ci_ribbon = FALSE, annot_surv_med = TRUE, 
                      annot_coxph = TRUE, control_coxph_pw = control_coxph(conf_level = 0.95, 
                        pval_method = "log-rank", ties = "efron"), 
                      control_annot_surv_med = list(x = 0.8, y = 0.85, 
                        w = 0.32, h = 0.16, fill = TRUE), control_annot_coxph = list(x = 0.27, 
                        y = 0.35, w = 0.3, h = 0.125, fill = TRUE, 
                        ref_lbls = FALSE), legend_pos = NULL, rel_height_plot = 0.8)
              }
          }
          g_km_counter <- g_km_counter_generator()
          plot_list <- lapply(anl, g_km_counter)
          plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)
          plot
      }
      

# template_g_km gives correct data expression when we want to combine comparison arms

    Code
      res
    Output
      $data
      {
          anl <- ANL %>% dplyr::filter(ARM %in% c("ARM C", "ARM D", 
              "ARM A", "ARM B")) %>% dplyr::mutate(ARM = combine_levels(ARM, 
              levels = c("ARM C", "ARM D"), new_level = "ARM C/ARM D")) %>% 
              dplyr::mutate(ARM = stats::relevel(ARM, ref = "ARM C/ARM D")) %>% 
              dplyr::mutate(ARM = droplevels(ARM)) %>% dplyr::mutate(is_event = CNSR == 
              0) %>% dplyr::mutate(ARM = combine_levels(ARM, levels = c("ARM A", 
              "ARM B"), new_level = "ARM A/ARM B"))
      }
      
      $variables
      variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARM")
      
      $graph
      {
          facets <- droplevels(anl$SEX)
          anl <- split(anl, f = facets)
          g_km_counter_generator <- function() {
              plot_number <- 0L
              function(x) {
                  plot_number <<- plot_number + 1L
                  g_km(x, variables = variables, control_surv = control_surv_timepoint(conf_level = 0.95), 
                      xticks = NULL, xlab = sprintf("%s (%s)", "Survival time", 
                        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", 
                          tolower(x$AVALU[1]), perl = TRUE)), yval = "Survival", 
                      title = sprintf("%s%s", sprintf("%s%s", "KM Plot", 
                        if (!is.null(facets)) {
                          sprintf(", %s = %s", as.character(quote(SEX)), 
                            unique(x[[as.character(quote(SEX))]]))
                        }
                        else {
                          ""
                        }), if (length(NULL) != 0) {
                        sprintf("\nStratified by %s", toString(NULL))
                      }
                      else {
                        ""
                      }), footnotes = if (TRUE) {
                        paste("Ties for Coxph (Hazard Ratio):", "efron", 
                          "\n", "p-value Method for Coxph (Hazard Ratio):", 
                          "log-rank")
                      }, font_size = 11, ci_ribbon = FALSE, annot_surv_med = TRUE, 
                      annot_coxph = TRUE, control_coxph_pw = control_coxph(conf_level = 0.95, 
                        pval_method = "log-rank", ties = "efron"), 
                      control_annot_surv_med = list(x = 0.8, y = 0.85, 
                        w = 0.32, h = 0.16, fill = TRUE), control_annot_coxph = list(x = 0.27, 
                        y = 0.35, w = 0.3, h = 0.125, fill = TRUE, 
                        ref_lbls = FALSE), legend_pos = NULL, rel_height_plot = 0.8)
              }
          }
          g_km_counter <- g_km_counter_generator()
          plot_list <- lapply(anl, g_km_counter)
          plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)
          plot
      }
      

