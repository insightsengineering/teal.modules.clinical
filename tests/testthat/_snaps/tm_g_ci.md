# 1. and 2. Mean and 95% CIs for mean

    Code
      res
    Output
      {
          gg <- ggplot2::ggplot(data = ANL, mapping = ggplot2::aes(x = ARMCD, 
              y = AVAL, color = SEX, lty = SEX, shape = SEX)) + ggplot2::stat_summary(fun.data = stat_mean_ci, 
              geom = "errorbar", width = 0.1, position = ggplot2::position_dodge(width = 0.5)) + 
              ggplot2::stat_summary(fun = mean, geom = "point", position = ggplot2::position_dodge(width = 0.5)) + 
              ggplot2::labs(title = "Confidence Interval Plot by Treatment Group", 
                  caption = "Mean and 95% CIs for mean are displayed.", 
                  x = "Treatment Group", y = "Value", color = "", lty = "", 
                  shape = "")
          print(gg)
      }

# 3. Confidence Interval Plot (using different stratification variable)

    Code
      res
    Output
      {
          gg <- ggplot2::ggplot(data = ANL, mapping = ggplot2::aes(x = ARMCD, 
              y = AVAL, color = STRATA2, lty = STRATA2, shape = STRATA2)) + 
              ggplot2::stat_summary(fun.data = stat_mean_ci, geom = "errorbar", 
                  width = 0.1, position = ggplot2::position_dodge(width = 0.5)) + 
              ggplot2::stat_summary(fun = mean, geom = "point", position = ggplot2::position_dodge(width = 0.5)) + 
              ggplot2::labs(title = "Confidence Interval Plot by Treatment Group", 
                  caption = "Mean and 95% CIs for mean are displayed.", 
                  x = "Treatment Group", y = "Value", color = "", lty = "", 
                  shape = "")
          print(gg)
      }

# 4. Median and 95% CIs for median

    Code
      res
    Output
      {
          gg <- ggplot2::ggplot(data = ANL, mapping = ggplot2::aes(x = ARMCD, 
              y = AVAL, color = STRATA1, lty = STRATA1, shape = STRATA1)) + 
              ggplot2::stat_summary(fun.data = stat_median_ci, geom = "errorbar", 
                  width = 0.1, position = ggplot2::position_dodge(width = 0.5)) + 
              ggplot2::stat_summary(fun = median, geom = "point", position = ggplot2::position_dodge(width = 0.5)) + 
              ggplot2::labs(title = "Confidence Interval Plot by Treatment Group", 
                  caption = "Median and 95% CIs for median are displayed.", 
                  x = "Treatment Group", y = "Value", color = "", lty = "", 
                  shape = "")
          print(gg)
      }

# 5. Using different alpha level

    Code
      res
    Output
      {
          gg <- ggplot2::ggplot(data = ANL, mapping = ggplot2::aes(x = ARMCD, 
              y = AVAL, color = SEX, lty = SEX, shape = SEX)) + ggplot2::stat_summary(fun.data = function(x) stat_mean_ci(x, 
              conf_level = 0.9), geom = "errorbar", width = 0.1, position = ggplot2::position_dodge(width = 0.5)) + 
              ggplot2::stat_summary(fun = mean, geom = "point", position = ggplot2::position_dodge(width = 0.5)) + 
              ggplot2::labs(title = "Confidence Interval Plot by Treatment Group", 
                  caption = "Mean and 90% CIs for mean are displayed.", 
                  x = "Treatment Group", y = "Value", color = "", lty = "", 
                  shape = "")
          print(gg)
      }

