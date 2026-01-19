# Template: Survival Forest Plot

Creates a valid expression to generate a survival forest plot.

## Usage

``` r
template_forest_tte(
  dataname = "ANL",
  parentname = "ANL_ADSL",
  arm_var,
  ref_arm = NULL,
  comp_arm = NULL,
  obj_var_name = "",
  aval_var = "AVAL",
  cnsr_var = "CNSR",
  subgroup_var,
  strata_var = NULL,
  stats = c("n_tot_events", "n_events", "median", "hr", "ci"),
  riskdiff = NULL,
  conf_level = 0.95,
  col_symbol_size = NULL,
  time_unit_var = "AVALU",
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

- cnsr_var:

  (`character`)  
  name of the censoring variable.

- subgroup_var:

  (`character`)  
  with variable names that can be used as subgroups.

- strata_var:

  (`character`)  
  names of the variables for stratified analysis.

- stats:

  (`character`)  
  the names of statistics to be reported among:

  - `n_tot_events`: Total number of events per group.

  - `n_events`: Number of events per group.

  - `n_tot`: Total number of observations per group.

  - `n`: Number of observations per group.

  - `median`: Median survival time.

  - `hr`: Hazard ratio.

  - `ci`: Confidence interval of hazard ratio.

  - `pval`: p-value of the effect. Note, one of the statistics `n_tot`
    and `n_tot_events`, as well as both `hr` and `ci` are required.

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

- time_unit_var:

  (`character`)  
  name of the variable representing time units.

- font_size:

  (`numeric`)  
  font size value.

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

[`tm_g_forest_tte()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_g_forest_tte.md)
