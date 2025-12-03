# Template: Line Plot

Creates a valid expression to generate a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
line plot.

## Usage

``` r
template_g_lineplot(
  dataname = "ANL",
  group_var = "ARM",
  x = "AVISIT",
  y = "AVAL",
  y_unit = "AVALU",
  paramcd = "PARAMCD",
  param = "ALT",
  mid = "mean",
  interval = "mean_ci",
  whiskers = c("mean_ci_lwr", "mean_ci_upr"),
  table = c("n", "mean_sd", "median", "range"),
  mid_type = "pl",
  conf_level = 0.95,
  incl_screen = TRUE,
  mid_point_size = 2,
  table_font_size = 4,
  title = "Line Plot",
  y_lab = "",
  ggplot2_args = teal.widgets::ggplot2_args()
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- group_var:

  (`string` or `NA`)  
  group variable name.

- x:

  (`string`)  
  x-variable name.

- y:

  (`string`)  
  y-variable name.

- y_unit:

  (`string` or `NA`)  
  y-axis unit variable name.

- paramcd:

  (`string` or `NA`)  
  parameter code variable name.

- param:

  (`character`)  
  parameter to filter the data by.

- mid:

  (`character` or `NULL`)  
  names of the statistics that will be plotted as midpoints. All the
  statistics indicated in `mid` variable must be present in the object
  returned by `sfun`, and be of a `double` or `numeric` type vector of
  length one.

- interval:

  (`character` or `NULL`)  
  names of the statistics that will be plotted as intervals. All the
  statistics indicated in `interval` variable must be present in the
  object returned by `sfun`, and be of a `double` or `numeric` type
  vector of length two. Set `interval = NULL` if intervals should not be
  added to the plot.

- whiskers:

  (`character`)  
  names of the interval whiskers that will be plotted. Names must match
  names of the list element `interval` that will be returned by `sfun`
  (e.g. `mean_ci_lwr` element of `sfun(x)[["mean_ci"]]`). It is possible
  to specify one whisker only, or to suppress all whiskers by setting
  `interval = NULL`.

- table:

  (`character` or `NULL`)  
  names of the statistics that will be displayed in the table below the
  plot. All the statistics indicated in `table` variable must be present
  in the object returned by `sfun`.

- mid_type:

  (`string`)  
  controls the type of the `mid` plot, it can be point (`"p"`), line
  (`"l"`), or point and line (`"pl"`).

- conf_level:

  (`numeric`)  
  value for the confidence level within the range of (0, 1).

- incl_screen:

  (`logical`)  
  whether the screening visit should be included.

- mid_point_size:

  (`numeric(1)`)  
  font size of the `mid` plot points.

- table_font_size:

  (`numeric(1)`)  
  font size of the text in the table.

- title:

  (`string`)  
  plot title.

- y_lab:

  (`string` or `NULL`)  
  y-axis label. If `NULL` then no label will be added.

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. For this module, this argument will
  only accept `ggplot2_args` object with `labs` list of following child
  elements: `title`, `subtitle`, `caption`, `y`, `lty`. No other
  elements would be taken into account. The argument is merged with
  option `teal.ggplot2_args` and with default module arguments (hard
  coded in the module body).

  For more details, see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_g_lineplot()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_lineplot.md)
