# Template: Individual Patient Plots

Creates a valid expression to generate
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
plots of individual patients.

## Usage

``` r
template_g_ipp(
  dataname = "ANL",
  paramcd,
  arm_var,
  arm_levels,
  avalu_first,
  paramcd_first,
  aval_var = "AVAL",
  avalu_var = "AVALU",
  id_var = "USUBJID",
  visit_var = "AVISIT",
  baseline_var = "BASE",
  add_baseline_hline = FALSE,
  separate_by_obs = FALSE,
  ggplot2_args = teal.widgets::ggplot2_args(),
  suppress_legend = FALSE,
  add_avalu = TRUE
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- paramcd:

  (`character`)  
  name of the parameter code variable.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- arm_levels:

  (`character`)  
  vector of all levels of `arm_var`.

- avalu_first:

  (`character`)  
  `avalu_var` text to append to the plot title and y-axis label if
  `add_avalu` is `TRUE`.

- paramcd_first:

  (`character`)  
  `paramcd` text to append to the plot title and y-axis label.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- avalu_var:

  (`character`)  
  name of the analysis value unit variable.

- id_var:

  (`character`)  
  the variable name for subject id.

- visit_var:

  (`character`)  
  name of the variable for visit timepoints.

- baseline_var:

  (`character`)  
  name of the variable for baseline values of the analysis variable.

- add_baseline_hline:

  (`logical`)  
  whether a horizontal line should be added to the plot at baseline
  y-value.

- separate_by_obs:

  (`logical`)  
  whether to create multi-panel plots.

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. For this module, this argument will
  only accept `ggplot2_args` object with `labs` list of the following
  child elements: `title`, `subtitle`, `x`, `y`. No other elements are
  taken into account. The argument is merged with option
  `teal.ggplot2_args` and with default module arguments (hard coded in
  the module body).

  For more details, see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

- suppress_legend:

  (`logical`)  
  whether to suppress the plot legend.

- add_avalu:

  (`logical`)  
  whether `avalu_first` text should be appended to the plot title and
  y-axis label.

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_g_ipp()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_ipp.md)
