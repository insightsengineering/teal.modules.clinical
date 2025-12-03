# `ggplot2` call to generate simple bar chart

`ggplot2` call to generate simple bar chart

## Usage

``` r
make_barchart_simple_call(
  y_name,
  x_name = NULL,
  fill_name = NULL,
  x_facet_name = NULL,
  y_facet_name = NULL,
  label_bars = TRUE,
  barlayout = c("side_by_side", "stacked"),
  flip_axis = FALSE,
  rotate_bar_labels = FALSE,
  rotate_x_label = FALSE,
  rotate_y_label = FALSE,
  expand_y_range = 0,
  facet_scales = "free_x",
  ggplot2_args = teal.widgets::ggplot2_args()
)
```

## Arguments

- y_name:

  (`character` or `NULL`)  
  name of the y-axis variable.

- x_name:

  (`character` or `NULL`)  
  name of the x-axis variable. Defaults to `NULL` because it is
  dependent on extract input which can be empty.

- fill_name:

  (`character` or `NULL`)  
  name of the variable to determine the bar fill color.

- x_facet_name:

  (`character` or `NULL`)  
  name of the variable to use for horizontal plot faceting.

- y_facet_name:

  (`character` or `NULL`)  
  name of the variable to use for vertical plot faceting.

- label_bars:

  (`logical` or `NULL`)  
  whether bars should be labeled. If `TRUE`, label bar numbers would
  also be drawn as text.

- barlayout:

  (`character` or `NULL`)  
  type of the bar layout. Options are `"stacked"` (default) or
  `"side_by_side"`.

- flip_axis:

  (`character` or `NULL`)  
  whether to flip the plot axis.

- rotate_bar_labels:

  (`logical` or `NULL`)  
  whether bar labels should be rotated by 45 degrees.

- rotate_x_label:

  (`logical` or `NULL`)  
  whether x-axis labels should be rotated by 45 degrees.

- rotate_y_label:

  (`logical` or `NULL`)  
  whether y-axis labels should be rotated by 45 degrees.

- expand_y_range:

  (`numeric` or `NULL`)  
  fraction of y-axis range to further expand by.

- facet_scales:

  (`character`)  
  value passed to `scales` argument of
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
  Options are `fixed`, `free_x`, `free_y`, and `free`.

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with option
  `teal.ggplot2_args` and with default module arguments (hard coded in
  the module body). For more details, see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

## Value

`call` to produce a `ggplot` object.
