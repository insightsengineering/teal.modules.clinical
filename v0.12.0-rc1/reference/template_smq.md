# Template: Adverse Events Table by Standardized MedDRA Query

Creates a valid expression to generate an adverse events table by
Standardized MedDRA Query.

## Usage

``` r
template_smq(
  dataname,
  parentname,
  arm_var,
  llt = "AEDECOD",
  add_total = TRUE,
  total_label = default_total_label(),
  sort_criteria = c("freq_desc", "alpha"),
  drop_arm_levels = TRUE,
  na_level = tern::default_na_str(),
  smq_varlabel = "Standardized MedDRA Query",
  baskets = c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"),
  id_var = "USUBJID",
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

- llt:

  (`character`)  
  name of the variable with low level term for events.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- sort_criteria:

  (`character`)  
  how to sort the final table. Default option `freq_desc` sorts on
  column `sort_freq_col` by decreasing number of patients with event.
  Alternative option `alpha` sorts events alphabetically.

- drop_arm_levels:

  (`logical`)  
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- smq_varlabel:

  (`character`)  
  label to use for new column `SMQ` created by
  [`tern::h_stack_by_baskets()`](https://insightsengineering.github.io/tern/latest-tag/reference/h_stack_by_baskets.html).

- baskets:

  (`character`)  
  names of the selected standardized/customized queries variables.

- id_var:

  (`character`)  
  the variable name for subject id.

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

[`tm_t_smq()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_smq.md)
