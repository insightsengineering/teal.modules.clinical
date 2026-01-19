# Template: Shift by Arm by Worst Analysis Indicator Level

Creates a valid expression to generate a summary table of worst analysis
indicator variable level per subject by arm.

## Usage

``` r
template_shift_by_arm_by_worst(
  dataname,
  parentname,
  arm_var = "ARM",
  paramcd = "PARAMCD",
  worst_flag_var = "WORS02FL",
  worst_flag = "Y",
  treatment_flag_var = "ONTRTFL",
  treatment_flag = "Y",
  aval_var = "ANRIND",
  baseline_var = "BNRIND",
  na.rm = FALSE,
  na_level = tern::default_na_str(),
  add_total = FALSE,
  total_label = default_total_label(),
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

- paramcd:

  (`character`)  
  name of the parameter code variable.

- worst_flag_var:

  (`character`)  
  name of the worst flag variable.

- worst_flag:

  (`character`)  
  value indicating worst analysis indicator level.

- treatment_flag_var:

  (`character`)  
  name of the on treatment flag variable.

- treatment_flag:

  (`character`)  
  name of the value indicating on treatment records in
  `treatment_flag_var`.

- aval_var:

  (`character`)  
  name of the analysis reference range indicator variable.

- baseline_var:

  (`character`)  
  name of the baseline reference range indicator variable.

- na.rm:

  (`logical`)  
  whether `NA` values should be removed prior to analysis.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- add_total:

  (`logical`)  
  whether to include row with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

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

[`tm_t_shift_by_arm()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_shift_by_arm.md)
