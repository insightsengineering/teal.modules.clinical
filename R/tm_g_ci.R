
#' Template: Confidence Interval Plot
#'
#' Writes the expressions to filter data and draw confidence interval
#' estimation.
#'
#' @inheritParams argument_convention
#' @param x_var (`string`)\cr treatment variable corresponding to the x axis.
#' @param y_var (`string`)\cr response variable corresponding to the y axis.
#' @param grp_var (`string`)\cr group variable corresponding to the colors
#'  point shape and line type.
#' @param stat (`string`)\cr either `mean` or `median`.
#' @param unit_var (`string`)\cr variable name in `dataname` where the unit is
#'  read.
#'
template_g_ci <- function(dataname, # nousage # nolint
                          x_var,
                          y_var,
                          grp_var,
                          paramcd,
                          avisit,
                          stat = c("mean", "median"),
                          conf_level = 0.95,
                          unit_var = "AVALU") {
  stat <- match.arg(stat)
  y <- list()
  y$data <- substitute(
    expr = anl <- dataname %>%
      filter(PARAMCD == paramcd, AVISIT == avisit),
    env = list(
      dataname = as.name(dataname),
      paramcd = paramcd,
      avisit = avisit
    )
  )

  graph_list <- list()
  graph_list <- add_expr(
    expr_ls = graph_list,
    new_expr = {
      substitute(
        expr = ggplot(
          data = anl,
          mapping = aes(
            x = x_var,
            y = y_var,
            color = grp_var,
            lty = grp_var,
            shape = grp_var
          )
        ),
        env = list(
          x_var = as.name(x_var),
          y_var = as.name(y_var),
          grp_var = as.name(grp_var)
        )
      )
    }
  )

  graph_list <- if (conf_level == 0.95) {
    add_expr(
      expr_ls = graph_list,
      new_expr = substitute(
        expr = stat_summary(
          fun.data = fun,
          geom = "errorbar",
          width = .1,
          position = position_dodge(width = .5)
        ),
        env = list(
          fun = switch(
            stat,
            mean = substitute(stat_mean_ci),
            median = substitute(stat_median_ci)
          )
        )
      )
    )
  } else {
    add_expr(
      expr_ls = graph_list,
      new_expr = substitute(
        expr = stat_summary(
          fun.data = fun,
          geom = "errorbar",
          width = .1,
          position = position_dodge(width = .5)
        ),
        env = list(
          fun = switch(
            stat,
            mean = substitute(
              expr = function(x) stat_mean_ci(x, conf_level = conf_level),
              env = list(conf_level = conf_level)
            ),
            median = substitute(
              expr = function(x) stat_median_ci(x, conf_level = conf_level),
              env = list(conf_level = conf_level)
            )
          )
        )
      )
    )
  }

  graph_list <- add_expr(
    expr_ls = graph_list,
    new_expr = substitute(
      expr = stat_summary(
        fun = fun,
        geom = "point",
        position = position_dodge(width = .5)
      ) + labs(
        title = title,
        subtitle = subtitle,
        caption = caption,
        x = "Treatment Group",
        y = paste0(paramcd, " (", unique(anl$unit_var), ")")
      ),
      env = list(
        fun = switch(
          stat,
          mean = substitute(mean),
          median = substitute(median)
        ),
        title = paste(
          "Confidence Interval Plot for",
          paramcd,
          "by Treatment Group"
        ),
        subtitle = paste("Visit:", avisit),
        caption = paste0(
          switch(stat, mean = "Mean", median = "Median"),
          " and ", 100 * conf_level, "% CIs for ",
          stat,
          " are displayed."
        ),
        unit_var = unit_var,
        paramcd = paramcd
      )
    )
  )

  y$graph <- pipe_expr(graph_list, pipe_str = "+")
  y
}
