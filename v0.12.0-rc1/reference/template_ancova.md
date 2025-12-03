# Template: ANCOVA Summary

Creates a valid expression to generate an analysis of variance summary
table.

## Usage

``` r
template_ancova(
  dataname = "ANL",
  parentname = "ADSL",
  arm_var,
  ref_arm = NULL,
  comp_arm = NULL,
  combine_comp_arms = FALSE,
  aval_var,
  label_aval = NULL,
  cov_var,
  include_interact = FALSE,
  interact_var = NULL,
  interact_y = FALSE,
  paramcd_levels = "",
  paramcd_var = "PARAMCD",
  label_paramcd = NULL,
  visit_levels = "",
  visit_var = "AVISIT",
  conf_level = 0.95,
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

- ref_arm:

  (`character`)  
  the level of reference arm in case of arm comparison.

- comp_arm:

  (`character`)  
  the level of comparison arm in case of arm comparison.

- combine_comp_arms:

  (`logical`)  
  triggers the combination of comparison arms.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- label_aval:

  (`character`)  
  label of value variable used for title rendering.

- cov_var:

  (`character`)  
  names of the covariates variables.

- include_interact:

  (`logical`)  
  whether an interaction term should be included in the model.

- interact_var:

  (`character`)  
  name of the variable that should have interactions with arm. If the
  interaction is not needed, the default option is `NULL`.

- interact_y:

  (`character`)  
  a selected item from the `interact_var` column which will be used to
  select the specific ANCOVA results. If the interaction is not needed,
  the default option is `FALSE`.

- paramcd_levels:

  (`character`)  
  variable levels for the studied parameter.

- paramcd_var:

  (`character`)  
  variable name for the studied parameter.

- label_paramcd:

  (`character`)  
  variable label used for title rendering.

- visit_levels:

  (`character`)  
  variable levels for studied visits.

- visit_var:

  (`character`)  
  variable names that can be used as `visit` variable. Must be a factor
  in `dataname`.

- conf_level:

  (`numeric`)  
  value for the confidence level within the range of (0, 1).

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

[`tm_t_ancova()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_t_ancova.md)
