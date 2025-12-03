# Template: Confidence Interval Plot

Creates a valid expression to generate a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
confidence interval plot.

## Usage

``` r
template_g_ci(
  dataname,
  x_var,
  y_var,
  grp_var = NULL,
  stat = c("mean", "median"),
  conf_level = 0.95,
  unit_var = "AVALU",
  ggplot2_args = teal.widgets::ggplot2_args()
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- x_var:

  (`character`)  
  name of the treatment variable to put on the x-axis.

- y_var:

  (`character`)  
  name of the response variable to put on the y-axis.

- grp_var:

  (`character`)  
  name of the group variable used to determine the plot colors, point
  shapes, and line types.

- stat:

  (`character`)  
  statistic to plot. Options are `"mean"` and `"median"`.

- conf_level:

  (`numeric`)  
  value for the confidence level within the range of (0, 1).

- unit_var:

  (`character`)  
  name of the unit variable.

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with option
  `teal.ggplot2_args` and with default module arguments (hard coded in
  the module body). For more details, see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_g_ci()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_ci.md)
