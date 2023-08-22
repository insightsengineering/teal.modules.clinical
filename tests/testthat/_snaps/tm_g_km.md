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
          grid::grid.newpage()
          lyt <- grid::grid.layout(nrow = length(anl), ncol = 1) %>% 
              grid::viewport(layout = .) %>% grid::pushViewport()
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
                      }, newpage = FALSE, vp = grid::viewport(layout.pos.row = plot_number, 
                        layout.pos.col = 1), font_size = 10, ci_ribbon = FALSE, 
                      ggtheme = ggplot2::theme_minimal(), annot_surv_med = TRUE, 
                      annot_coxph = TRUE, control_coxph_pw = control_coxph(conf_level = 0.95, 
                        pval_method = "log-rank", ties = "efron"), 
                      position_coxph = c(-0.04, 0.02), width_annots = list(surv_med = structure(0.45, 
                        unit = 0L, class = c("simpleUnit", "unit", 
                          "unit_v2")), coxph = structure(0.6, unit = 0L, 
                        class = c("simpleUnit", "unit", "unit_v2"))))
              }
          }
          g_km_counter <- g_km_counter_generator()
          plot_list <- lapply(anl, g_km_counter)
          plot <- tern::stack_grobs(grobs = plot_list)
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
          grid::grid.newpage()
          lyt <- grid::grid.layout(nrow = length(anl), ncol = 1) %>% 
              grid::viewport(layout = .) %>% grid::pushViewport()
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
                      }, newpage = FALSE, vp = grid::viewport(layout.pos.row = plot_number, 
                        layout.pos.col = 1), font_size = 10, ci_ribbon = FALSE, 
                      ggtheme = ggplot2::theme_minimal(), annot_surv_med = TRUE, 
                      annot_coxph = TRUE, control_coxph_pw = control_coxph(conf_level = 0.95, 
                        pval_method = "log-rank", ties = "efron"), 
                      position_coxph = c(-0.04, 0.02), width_annots = list(surv_med = structure(0.45, 
                        unit = 0L, class = c("simpleUnit", "unit", 
                          "unit_v2")), coxph = structure(0.6, unit = 0L, 
                        class = c("simpleUnit", "unit", "unit_v2"))))
              }
          }
          g_km_counter <- g_km_counter_generator()
          plot_list <- lapply(anl, g_km_counter)
          plot <- tern::stack_grobs(grobs = plot_list)
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
          grid::grid.newpage()
          lyt <- grid::grid.layout(nrow = length(anl), ncol = 1) %>% 
              grid::viewport(layout = .) %>% grid::pushViewport()
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
                      }, newpage = FALSE, vp = grid::viewport(layout.pos.row = plot_number, 
                        layout.pos.col = 1), font_size = 10, ci_ribbon = FALSE, 
                      ggtheme = ggplot2::theme_minimal(), annot_surv_med = TRUE, 
                      annot_coxph = TRUE, control_coxph_pw = control_coxph(conf_level = 0.95, 
                        pval_method = "log-rank", ties = "efron"), 
                      position_coxph = c(-0.04, 0.02), width_annots = list(surv_med = structure(0.45, 
                        unit = 0L, class = c("simpleUnit", "unit", 
                          "unit_v2")), coxph = structure(0.6, unit = 0L, 
                        class = c("simpleUnit", "unit", "unit_v2"))))
              }
          }
          g_km_counter <- g_km_counter_generator()
          plot_list <- lapply(anl, g_km_counter)
          plot <- tern::stack_grobs(grobs = plot_list)
          plot
      }
      

