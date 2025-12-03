# Template: Grade Summary Table

Creates a valid expression to generate a grade summary table.

## Usage

``` r
template_shift_by_grade(
  parentname,
  dataname,
  arm_var = "ARM",
  id_var = "USUBJID",
  visit_var = "AVISIT",
  worst_flag_var = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"),
  worst_flag_indicator = "Y",
  anl_toxgrade_var = "ATOXGR",
  base_toxgrade_var = "BTOXGR",
  paramcd = "PARAMCD",
  drop_arm_levels = TRUE,
  add_total = FALSE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  code_missing_baseline = FALSE,
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

- visit_var:

  (`character`)  
  variable names that can be used as `visit` variable. Must be a factor
  in `dataname`.

- worst_flag_var:

  (`character`)  
  name of the worst flag variable.

- worst_flag_indicator:

  (`character`)  
  value indicating worst grade.

- anl_toxgrade_var:

  (`character`)  
  name of the variable indicating the analysis toxicity grade.

- base_toxgrade_var:

  (`character`)  
  name of the variable indicating the baseline toxicity grade.

- paramcd:

  (`character`)  
  name of the parameter code variable.

- drop_arm_levels:

  (`logical`)  
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

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

- code_missing_baseline:

  (`logical`)  
  whether missing baseline grades should be counted as grade 0.

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

[`tm_t_shift_by_grade()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_shift_by_grade.md)
