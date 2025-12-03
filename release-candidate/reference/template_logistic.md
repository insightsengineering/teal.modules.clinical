# Template: Logistic Regression

Creates a valid expression to generate a logistic regression table.

## Usage

``` r
template_logistic(
  dataname,
  arm_var,
  aval_var,
  label_paramcd,
  cov_var,
  interaction_var,
  ref_arm,
  comp_arm,
  topleft = "Logistic Regression",
  conf_level = 0.95,
  combine_comp_arms = FALSE,
  responder_val = c("CR", "PR"),
  at = NULL,
  basic_table_args = teal.widgets::basic_table_args()
)
```

## Arguments

- dataname:

  (`character`)  
  analysis data used in teal module.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`. To fit a logistic model
  with no arm/treatment variable, set to `NULL`.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- label_paramcd:

  (`character`)  
  label of response parameter value to print in the table title.

- cov_var:

  (`character`)  
  names of the covariates variables.

- interaction_var:

  (`character`)  
  names of the variables that can be used for interaction variable
  selection.

- ref_arm:

  (`character`)  
  the level of reference arm in case of arm comparison.

- comp_arm:

  (`character`)  
  the level of comparison arm in case of arm comparison.

- topleft:

  (`character`)  
  text to use as top-left annotation in the table.

- conf_level:

  (`numeric`)  
  value for the confidence level within the range of (0, 1).

- combine_comp_arms:

  (`logical`)  
  triggers the combination of comparison arms.

- responder_val:

  (`character`)  
  values of the responder variable corresponding with a successful
  response.

- at:

  (`numeric` or `NULL`)  
  optional values for the interaction variable. Otherwise the median is
  used.

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

[`tm_t_logistic()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_logistic.md)
