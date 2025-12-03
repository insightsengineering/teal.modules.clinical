# Template: Response Forest Plot

Creates a valid expression to generate a response forest plot.

## Usage

``` r
template_forest_rsp(
  dataname = "ANL",
  parentname = "ADSL",
  arm_var,
  ref_arm = NULL,
  comp_arm = NULL,
  obj_var_name = "",
  aval_var = "AVALC",
  responders = c("CR", "PR"),
  subgroup_var,
  strata_var = NULL,
  stats = c("n_tot", "n", "n_rsp", "prop", "or", "ci"),
  riskdiff = NULL,
  conf_level = 0.95,
  col_symbol_size = NULL,
  font_size = 15,
  ggplot2_args = teal.widgets::ggplot2_args()
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- parentname:

  (`character`)  
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- ref_arm:

  (`character`)  
  the level of reference arm in case of arm comparison.

- comp_arm:

  (`character`)  
  the level of comparison arm in case of arm comparison.

- obj_var_name:

  (`character`)  
  additional text to append to the table title.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- responders:

  (`character`)  
  values of `aval_var` that are considered to be responders.

- subgroup_var:

  (`character`)  
  with variable names that can be used as subgroups.

- strata_var:

  (`character`)  
  names of the variables for stratified analysis.

- stats:

  (`character`)  
  the names of statistics to be reported among:

  - `n`: Total number of observations per group.

  - `n_rsp`: Number of responders per group.

  - `prop`: Proportion of responders.

  - `n_tot`: Total number of observations.

  - `or`: Odds ratio.

  - `ci` : Confidence interval of odds ratio.

  - `pval`: p-value of the effect. Note, the statistics `n_tot`, `or`,
    and `ci` are required.

- riskdiff:

  (`list`)  
  if a risk (proportion) difference column should be added, a list of
  settings to apply within the column. See
  [`tern::control_riskdiff()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_riskdiff.html)
  for details. If `NULL`, no risk difference column will be added.

- conf_level:

  (`numeric`)  
  value for the confidence level within the range of (0, 1).

- col_symbol_size:

  (`integer` or `NULL`)  
  column index to be used to determine relative size for estimator plot
  symbol. Typically, the symbol size is proportional to the sample size
  used to calculate the estimator. If `NULL`, the same symbol size is
  used for all subgroups.

- font_size:

  (`numeric(1)`)  
  font size.

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. For this module, this argument will
  only accept `ggplot2_args` object with `labs` list of following child
  elements: `title`, `caption`. No other elements would be taken into
  account. The argument is merged with option `teal.ggplot2_args` and
  with default module arguments (hard coded in the module body).

  For more details, see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_g_forest_rsp()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_forest_rsp.md)
