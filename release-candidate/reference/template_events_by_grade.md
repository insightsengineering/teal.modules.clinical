# Template: Events by Grade

Creates a valid expression to generate a table to summarize events by
grade.

## Usage

``` r
template_events_by_grade(
  dataname,
  parentname,
  arm_var,
  id = "",
  hlt,
  llt,
  label_hlt = NULL,
  label_llt = NULL,
  grade,
  label_grade = NULL,
  prune_freq = 0,
  prune_diff = 0,
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
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

- id:

  (`character`)  
  unique identifier of patients in datasets, default to `"USUBJID"`.

- hlt:

  (`character`)  
  name of the variable with high level term for events.

- llt:

  (`character`)  
  name of the variable with low level term for events.

- label_hlt:

  (`string`)  
  label of the `hlt` variable from `dataname`. The label will be
  extracted from the module.

- label_llt:

  (`string`)  
  label of the `llt` variable from `dataname`. The label will be
  extracted from the module.

- grade:

  (`character`)  
  name of the severity level variable.

- label_grade:

  (`string`)  
  label of the `grade` variable from `dataname`. The label will be
  extracted from the module.

- prune_freq:

  (`number`)  
  threshold to use for trimming table using event incidence rate in any
  column.

- prune_diff:

  (`number`)  
  threshold to use for trimming table using as criteria difference in
  rates between any two columns.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

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

[`tm_t_events_by_grade()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_events_by_grade.md)
