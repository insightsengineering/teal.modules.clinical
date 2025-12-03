# Template: Abnormality Summary Table

Creates a valid expression to generate a table to summarize abnormality.

## Usage

``` r
template_abnormality(
  parentname,
  dataname,
  arm_var,
  id_var = "USUBJID",
  by_vars,
  abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
  grade = "ANRIND",
  baseline_var = "BNRIND",
  treatment_flag_var = "ONTRTFL",
  treatment_flag = "Y",
  add_total = FALSE,
  total_label = default_total_label(),
  exclude_base_abn = FALSE,
  drop_arm_levels = TRUE,
  na_level = tern::default_na_str(),
  basic_table_args = teal.widgets::basic_table_args(),
  tbl_title
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

- by_vars:

  (`character`)  
  variable names used to split the summary by rows.

- abnormal:

  (`named list`)  
  indicating abnormality direction and grades.

- grade:

  (`character`)  
  name of the variable used to specify the abnormality grade. Variable
  must be factor.

- baseline_var:

  (`character`)  
  name of the variable specifying baseline abnormality grade.

- treatment_flag_var:

  (`character`)  
  name of the on treatment flag variable.

- treatment_flag:

  (`character`)  
  name of the value indicating on treatment records in
  `treatment_flag_var`.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- exclude_base_abn:

  (`logical`)  
  whether to exclude patients who had abnormal values at baseline.

- drop_arm_levels:

  (`logical`)  
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

- na_level:

  (`character`)  
  the NA level in the input dataset, defaults to `"<Missing>"`.

- basic_table_args:

  (`basic_table_args`) optional  
  object created by
  [`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
  with settings for the module table. The argument is merged with option
  `teal.basic_table_args` and with default module arguments (hard coded
  in the module body). For more details, see the vignette:
  [`vignette("custom-basic-table-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-basic-table-arguments.html).

- tbl_title:

  (`character`)  
  Title with label of variables from by bars

## Value

a `list` of expressions to generate a table or plot object.

## See also

[`tm_t_abnormality()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_abnormality.md)
