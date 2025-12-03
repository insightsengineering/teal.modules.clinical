# Template: Exposure Table for Risk management plan

Creates a valid expression to generate exposure table for risk
management plan.

## Usage

``` r
template_exposure(
  parentname,
  dataname,
  id_var,
  paramcd,
  paramcd_label = NULL,
  row_by_var,
  col_by_var = NULL,
  add_total = FALSE,
  total_label = "Total",
  add_total_row = TRUE,
  total_row_label = "Total number of patients and patient time*",
  drop_levels = TRUE,
  na_level = tern::default_na_str(),
  aval_var,
  avalu_var,
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

- id_var:

  (`character`)  
  the variable name for subject id.

- paramcd:

  (`character`)  
  name of the parameter code variable.

- paramcd_label:

  (`character`)  
  the column from the `dataname` dataset where the value will be used to
  label the argument `paramcd`.

- row_by_var:

  (`character`)  
  variable name used to split the values by rows.

- col_by_var:

  (`character`)  
  variable name used to split the values by columns.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- add_total_row:

  (`flag`)  
  whether a "total" level should be added after the others which
  includes all the levels that constitute the split. A custom label can
  be set for this level via the `total_row_label` argument.

- total_row_label:

  (`character`)  
  string to display as total row label if row is enabled (see
  `add_total_row`).

- drop_levels:

  (`flag`)  
  whether empty rows should be removed from the table.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- avalu_var:

  (`character`)  
  name of the analysis value unit variable.

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

[`tm_t_exposure()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_exposure.md)
