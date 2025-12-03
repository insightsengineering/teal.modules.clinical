# Template: Summary of Variables

Creates a valid expression to generate a table to summarize variables.

## Usage

``` r
template_summary(
  dataname,
  parentname,
  arm_var,
  sum_vars,
  add_total = TRUE,
  total_label = default_total_label(),
  var_labels = character(),
  arm_var_labels = NULL,
  na.rm = FALSE,
  na_level = tern::default_na_str(),
  numeric_stats = c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles",
    "range", "geom_mean"),
  denominator = c("N", "n", "omit"),
  drop_arm_levels = TRUE,
  basic_table_args = teal.widgets::basic_table_args()
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

- sum_vars:

  (`character`)  
  names of the variables that should be summarized.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- var_labels:

  (named `character`) optional  
  variable labels for relabeling the analysis variables.

- arm_var_labels:

  (`character` or `NULL`)  
  vector of column variable labels to display, of the same length as
  `arm_var`. If `NULL`, no labels will be displayed.

- na.rm:

  (`logical`)  
  whether `NA` values should be removed prior to analysis.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- numeric_stats:

  (`character`)  
  names of statistics to display for numeric summary variables.
  Available statistics are `n`, `mean_sd`, `mean_ci`, `median`,
  `median_ci`, `quantiles`, `range`, and `geom_mean`.

- denominator:

  (`character`)  
  chooses how percentages are calculated. With option `N`, the reference
  population from the column total is used as the denominator. With
  option `n`, the number of non-missing records in this row and column
  intersection is used as the denominator. If `omit` is chosen, then the
  percentage is omitted.

- drop_arm_levels:

  (`logical`)  
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

- basic_table_args:

  (`basic_table_args`) optional  
  object created by
  [`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
  with settings for the module table. The argument is merged with option
  `teal.basic_table_args` and with default module arguments (hard coded
  in the module body). For more details, see the vignette:
  [`vignette("custom-basic-table-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-basic-table-arguments.html).

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_t_summary()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_summary.md)
