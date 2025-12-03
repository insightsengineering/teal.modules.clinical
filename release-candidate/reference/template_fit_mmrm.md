# Template: Mixed Model Repeated Measurements (MMRM) Analysis

Creates a valid expression to generate analysis tables and plots for
Mixed Model Repeated Measurements.

## Usage

``` r
template_fit_mmrm(
  parentname,
  dataname,
  aval_var,
  arm_var,
  ref_arm,
  comp_arm = NULL,
  combine_comp_arms = FALSE,
  id_var,
  visit_var,
  cov_var,
  conf_level = 0.95,
  method = "Satterthwaite",
  cor_struct = "unstructured",
  weights_emmeans = "proportional",
  parallel = FALSE
)

template_mmrm_tables(
  parentname,
  dataname,
  fit_name,
  arm_var,
  ref_arm,
  visit_var,
  paramcd,
  show_relative = c("increase", "reduction", "none"),
  table_type = "t_mmrm_cov",
  total_label = default_total_label(),
  basic_table_args = teal.widgets::basic_table_args()
)

template_mmrm_plots(
  fit_name,
  lsmeans_plot = list(select = c("estimates", "contrasts"), width = 0.6, show_pval =
    FALSE),
  diagnostic_plot = list(type = "fit-residual", z_threshold = NULL),
  ggplot2_args = teal.widgets::ggplot2_args()
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

- aval_var:

  (`character`)  
  name of the analysis value variable.

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

- id_var:

  (`character`)  
  the variable name for subject id.

- visit_var:

  (`character`)  
  variable names that can be used as `visit` variable. Must be a factor
  in `dataname`.

- cov_var:

  (`character`)  
  names of the covariates variables.

- conf_level:

  (`numeric`)  
  value for the confidence level within the range of (0, 1).

- method:

  (`string`)  
  a string specifying the adjustment method.

- cor_struct:

  (`string`)  
  a string specifying the correlation structure, defaults to
  `"unstructured"`. See
  [`tern.mmrm::build_formula()`](https://insightsengineering.github.io/tern.mmrm/latest-tag/reference/build_formula.html)
  for more options.

- weights_emmeans:

  argument from
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
  "proportional" by default.

- parallel:

  (`flag`)  
  flag that controls whether optimizer search can use available free
  cores on the machine (not default).

- fit_name:

  (`string`)  
  name of fitted MMRM object.

- paramcd:

  (`character`)  
  name of the parameter code variable.

- show_relative:

  (`string`)  
  should the "reduction" (`control - treatment`, default) or the
  "increase" (`treatment - control`) be shown for the relative change
  from baseline.

- table_type:

  (`string`)  
  type of table to output.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- basic_table_args:

  (`basic_table_args`) optional  
  object created by
  [`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
  with settings for the module table. The argument is merged with option
  `teal.basic_table_args` and with default module arguments (hard coded
  in the module body). For more details, see the vignette:
  [`vignette("custom-basic-table-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-basic-table-arguments.html).

- lsmeans_plot:

  (named `list`)  
  a `list` of controls for LS means plot. See more
  [`tern.mmrm::g_mmrm_lsmeans()`](https://insightsengineering.github.io/tern.mmrm/latest-tag/reference/g_mmrm_lsmeans.html).

- diagnostic_plot:

  (named `list`)  
  a `list` of controls for diagnostic_plot. See more
  [`tern.mmrm::g_mmrm_diagnostic()`](https://insightsengineering.github.io/tern.mmrm/latest-tag/reference/g_mmrm_diagnostic.html).

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with option
  `teal.ggplot2_args` and with default module arguments (hard coded in
  the module body). For more details, see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

## Value

a `list` of expressions to generate a table or plot object.

## Functions

- `template_mmrm_tables()`: Creates valid expressions to generate MMRM
  LS means, covariance matrix, fixed effects, and diagnostic tables.

- `template_mmrm_plots()`: Creates valid expressions to generate MMRM LS
  means and diagnostic plots.

## See also

[`tm_a_mmrm()`](https://insightsengineering.github.io/teal.modules.clinical/reference/tm_a_mmrm.md)
