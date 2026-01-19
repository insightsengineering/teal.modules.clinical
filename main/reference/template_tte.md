# Template: Time-To-Event

Creates a valid expression to generate a time-to-event analysis.

## Usage

``` r
template_tte(
  dataname = "ANL",
  parentname = "ADSL",
  arm_var = "ARM",
  paramcd,
  ref_arm = NULL,
  comp_arm = NULL,
  compare_arm = FALSE,
  combine_comp_arms = FALSE,
  aval_var = "AVAL",
  cnsr_var = "CNSR",
  strata_var = NULL,
  time_points = NULL,
  time_unit_var = "AVALU",
  event_desc_var = "EVNTDESC",
  control = control_tte(),
  add_total = FALSE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
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
  endpoint parameter value to use in the table title.

- ref_arm:

  (`character`)  
  the level of reference arm in case of arm comparison.

- comp_arm:

  (`character`)  
  the level of comparison arm in case of arm comparison.

- compare_arm:

  (`logical`)  
  triggers the comparison between study arms.

- combine_comp_arms:

  (`logical`)  
  triggers the combination of comparison arms.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- cnsr_var:

  (`character`)  
  name of the censoring variable.

- strata_var:

  (`character`)  
  names of the variables for stratified analysis.

- time_points:

  (`character`)  
  time points that can be used in
  [`tern::surv_timepoint()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_timepoint.html).

- time_unit_var:

  (`character`)  
  name of the variable representing time units.

- event_desc_var:

  (`character`)  
  name of the variable with events description.

- control:

  (`list`)  
  list of settings for the analysis. See
  [`control_tte()`](https://insightsengineering.github.io/teal.modules.clinical/reference/control_tte.md)
  for details.

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

[`control_tte()`](https://insightsengineering.github.io/teal.modules.clinical/reference/control_tte.md),
[`tm_t_tte()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_tte.md)
