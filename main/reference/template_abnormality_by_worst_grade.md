# Template: Laboratory test results with highest grade post-baseline

Creates a valid expression to generate a table to summarize abnormality
by grade.

## Usage

``` r
template_abnormality_by_worst_grade(
  parentname,
  dataname,
  arm_var,
  id_var = "USUBJID",
  paramcd = "PARAMCD",
  atoxgr_var = "ATOXGR",
  worst_high_flag_var = "WGRHIFL",
  worst_low_flag_var = "WGRLOFL",
  worst_flag_indicator = "Y",
  add_total = FALSE,
  total_label = default_total_label(),
  drop_arm_levels = TRUE,
  basic_table_args = teal.widgets::basic_table_args()
)
```

## Arguments

- parentname:

  (`character`)  
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- dataname:

  (`character`)  
  analysis data used in teal module.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- id_var:

  (`character`)  
  the variable name for subject id.

- paramcd:

  (`character`)  
  name of the parameter code variable.

- atoxgr_var:

  (`character`)  
  name of the variable indicating Analysis Toxicity Grade.

- worst_high_flag_var:

  (`character`)  
  name of the variable indicating Worst High Grade flag

- worst_low_flag_var:

  (`character`)  
  name of the variable indicating Worst Low Grade flag

- worst_flag_indicator:

  (`character`)  
  flag value indicating the worst grade.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

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

[`tm_t_abnormality_by_worst_grade()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_abnormality_by_worst_grade.md)
