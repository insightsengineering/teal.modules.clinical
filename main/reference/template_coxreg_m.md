# Template: Multi-Variable Cox Regression

Creates a valid expression to generate a multi-variable Cox regression
analysis.

## Usage

``` r
template_coxreg_m(
  dataname,
  cov_var,
  arm_var,
  cnsr_var,
  aval_var,
  ref_arm,
  comp_arm,
  paramcd,
  at = list(),
  strata_var = NULL,
  combine_comp_arms = FALSE,
  control = tern::control_coxreg(),
  na_level = tern::default_na_str(),
  basic_table_args = teal.widgets::basic_table_args()
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- cov_var:

  (`character`)  
  names of the covariates variables.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- cnsr_var:

  (`character`)  
  name of the censoring variable.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- ref_arm:

  (`character`)  
  the level of reference arm in case of arm comparison.

- comp_arm:

  (`character`)  
  the level of comparison arm in case of arm comparison.

- paramcd:

  (`character`)  
  name of the parameter code variable.

- at:

  (`list` of `numeric`)  
  when the candidate covariate is a `numeric` type variable, use `at` to
  specify the value of the covariate at which the effect should be
  estimated.

- strata_var:

  (`character`)  
  names of the variables for stratified analysis.

- combine_comp_arms:

  (`logical`)  
  triggers the combination of comparison arms.

- control:

  (`list`)  
  list of settings for the analysis (see
  [`tern::control_coxreg()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_coxreg.html)).

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

[`template_coxreg_u()`](https://insightsengineering.github.io/teal.modules.clinical/reference/template_coxreg_u.md),
[`tm_t_coxreg()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_coxreg.md)
