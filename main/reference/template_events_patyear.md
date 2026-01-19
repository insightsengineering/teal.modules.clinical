# Template: Event Rates Adjusted for Patient-Years

Creates a valid expression to generate a table of event rates adjusted
for patient-years.

## Usage

``` r
template_events_patyear(
  dataname,
  parentname,
  arm_var,
  events_var,
  label_paramcd,
  aval_var = "AVAL",
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  control = tern::control_incidence_rate(),
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

- events_var:

  (`character`)  
  name of the variable for number of observed events.

- label_paramcd:

  (`character`)  
  `paramcd` variable text to use in the table title.

- aval_var:

  (`character`)  
  name of the analysis value variable.

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

- control:

  (`list`)  
  list of settings for the analysis.

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

[`tm_t_events_patyear()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_events_patyear.md)
