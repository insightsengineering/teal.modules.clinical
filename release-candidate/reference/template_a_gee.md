# Template for Generalized Estimating Equations (GEE) analysis module

Creates a valid expression to generate an analysis table using
Generalized Estimating Equations (GEE).

## Usage

``` r
template_a_gee(
  output_table,
  data_model_fit = "ANL",
  dataname_lsmeans = "ANL_ADSL",
  input_arm_var = "ARM",
  ref_group = "A: Drug X",
  aval_var,
  id_var,
  arm_var,
  visit_var,
  split_covariates,
  cor_struct,
  conf_level = 0.95,
  basic_table_args = teal.widgets::basic_table_args()
)
```

## Arguments

- output_table:

  (`character`)  
  type of output table (`"t_gee_cov", "t_gee_coef", "t_gee_lsmeans"`).

- data_model_fit:

  (`character`)  
  dataset used to fit the model by
  [`tern.gee::fit_gee()`](https://insightsengineering.github.io/tern.gee/latest-tag/reference/fit_gee.html).

- dataname_lsmeans:

  (`character`)  
  dataset used for `alt_counts_df` argument of
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html).

- aval_var:

  (`character`)  
  name of the analysis value variable.

- id_var:

  (`character`)  
  the variable name for subject id.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- visit_var:

  (`character`)  
  variable names that can be used as `visit` variable. Must be a factor
  in `dataname`.

- split_covariates:

  (`character`)  
  vector of names of variables to use as covariates in
  [`tern.gee::vars_gee()`](https://insightsengineering.github.io/tern.gee/latest-tag/reference/vars_gee.html).

- cor_struct:

  (`character`)  
  assumed correlation structure in
  [`tern.gee::fit_gee`](https://insightsengineering.github.io/tern.gee/latest-tag/reference/fit_gee.html).

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

[`tm_a_gee()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_a_gee.md)
