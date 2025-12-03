# Template: Binary Outcome

Creates a valid expression to generate a binary outcome analysis.

## Usage

``` r
template_binary_outcome(
  dataname,
  parentname,
  arm_var,
  paramcd,
  ref_arm = NULL,
  comp_arm = NULL,
  compare_arm = FALSE,
  combine_comp_arms = FALSE,
  aval_var = "AVALC",
  show_rsp_cat = TRUE,
  responder_val = c("Complete Response (CR)", "Partial Response (PR)"),
  responder_val_levels = responder_val,
  control = list(global = list(method = "waldcc", conf_level = 0.95), unstrat =
    list(method_ci = "waldcc", method_test = "schouten", odds = TRUE), strat =
    list(method_ci = "cmh", method_test = "cmh", strat = NULL)),
  add_total = FALSE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  denom = c("N_col", "n", "N_row"),
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
  response parameter value to use in the table title.

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

- show_rsp_cat:

  (`logical`)  
  display the multinomial response estimations.

- responder_val:

  (`character`)  
  the short label for observations to translate `AVALC` into
  responder/non-responder.

- responder_val_levels:

  (`character`)  
  the levels of responses that will be shown in the multinomial response
  estimations.

- control:

  (`list`)  
  list of settings for the analysis.

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

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `N_col`: total number of patients in this column across rows.

  - `n`: number of patients with any occurrences.

  - `N_row`: total number of patients in this row across columns.

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

[`tm_t_binary_outcome()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_binary_outcome.md)
