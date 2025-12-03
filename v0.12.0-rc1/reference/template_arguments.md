# Standard Template Arguments

The documentation to this function lists all the arguments in teal
module templates that are used repeatedly to express an analysis.

## Arguments

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- anl_name:

  (`character`)  
  analysis data used in teal module.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- atirel:

  (`character`)  
  name of time relation of medication variable.

- aval:

  **\[deprecated\]** Please use the `aval_var` argument instead.

- avalu:

  **\[deprecated\]** Please use the `avalu_var` argument instead.

- avalu_var:

  (`character`)  
  name of the analysis value unit variable.

- aval_var:

  (`character`)  
  name of the analysis value variable.

- baseline_var:

  (`character`)  
  name of the variable for baseline values of the analysis variable.

- base_var:

  **\[deprecated\]** Please use the `baseline_var` argument instead.

- basic_table_args:

  (`basic_table_args`) optional  
  object created by
  [`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
  with settings for the module table. The argument is merged with option
  `teal.basic_table_args` and with default module arguments (hard coded
  in the module body). For more details, see the vignette:
  [`vignette("custom-basic-table-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-basic-table-arguments.html).

- by_vars:

  (`character`)  
  variable names used to split the summary by rows.

- cmdecod:

  (`character`)  
  name of standardized medication name variable.

- cmindc:

  (`character`)  
  name of indication variable.

- cmstdy:

  (`character`)  
  name of study relative day of start of medication variable.

- cnsr_var:

  (`character`)  
  name of the censoring variable.

- combine_comp_arms:

  (`logical`)  
  triggers the combination of comparison arms.

- compare_arm:

  (`logical`)  
  triggers the comparison between study arms.

- comp_arm:

  (`character`)  
  the level of comparison arm in case of arm comparison.

- conf_level:

  (`numeric`)  
  value for the confidence level within the range of (0, 1).

- control:

  (`list`)  
  list of settings for the analysis.

- cov_var:

  (`character`)  
  names of the covariates variables.

- dataname:

  (`character`)  
  analysis data used in teal module.

- denominator:

  (`character`)  
  chooses how percentages are calculated. With option `N`, the reference
  population from the column total is used as the denominator. With
  option `n`, the number of non-missing records in this row and column
  intersection is used as the denominator. If `omit` is chosen, then the
  percentage is omitted.

- drop_arm_levels:

  (`logical`)  
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

- event_type:

  (`character`)  
  type of event that is summarized (e.g. adverse event, treatment).
  Default is `"event"`.

- font_size:

  (`numeric`)  
  font size value.

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with option
  `teal.ggplot2_args` and with default module arguments (hard coded in
  the module body). For more details, see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

- hlt:

  (`character`)  
  name of the variable with high level term for events.

- id_var:

  (`character`)  
  the variable name for subject id.

- include_interact:

  (`logical`)  
  whether an interaction term should be included in the model.

- label_hlt:

  (`string`)  
  label of the `hlt` variable from `dataname`. The label will be
  extracted from the module.

- label_llt:

  (`string`)  
  label of the `llt` variable from `dataname`. The label will be
  extracted from the module.

- llt:

  (`character`)  
  name of the variable with low level term for events.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- na.rm:

  (`logical`)  
  whether `NA` values should be removed prior to analysis.

- numeric_stats:

  (`character`)  
  names of statistics to display for numeric summary variables.
  Available statistics are `n`, `mean_sd`, `mean_ci`, `median`,
  `median_ci`, `quantiles`, `range`, and `geom_mean`.

- paramcd:

  (`character`)  
  name of the parameter code variable.

- parentname:

  (`character`)  
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- patient_id:

  (`character`)  
  patient ID.

- prune_diff:

  (`number`)  
  threshold to use for trimming table using as criteria difference in
  rates between any two columns.

- prune_freq:

  (`number`)  
  threshold to use for trimming table using event incidence rate in any
  column.

- ref_arm:

  (`character`)  
  the level of reference arm in case of arm comparison.

- sort_criteria:

  (`character`)  
  how to sort the final table. Default option `freq_desc` sorts on
  column `sort_freq_col` by decreasing number of patients with event.
  Alternative option `alpha` sorts events alphabetically.

- strata_var:

  (`character`)  
  names of the variables for stratified analysis.

- subgroup_var:

  (`character`)  
  with variable names that can be used as subgroups.

- sum_vars:

  (`character`)  
  names of the variables that should be summarized.

- time_points:

  (`character`)  
  time points that can be used in
  [`tern::surv_timepoint()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_timepoint.html).

- time_unit_var:

  (`character`)  
  name of the variable representing time units.

- title:

  (`character`)  
  title of the output.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- treatment_flag:

  (`character`)  
  name of the value indicating on treatment records in
  `treatment_flag_var`.

- treatment_flag_var:

  (`character`)  
  name of the on treatment flag variable.

- useNA:

  (`character`)  
  whether missing data (`NA`) should be displayed as a level.

- var_labels:

  (named `character`) optional  
  variable labels for relabeling the analysis variables.

- visit_var:

  (`character`)  
  variable names that can be used as `visit` variable. Must be a factor
  in `dataname`.

- worst_flag_indicator:

  (`character`)  
  value indicating worst grade.

- worst_flag_var:

  (`character`)  
  name of the worst flag variable.

## Value

a `list` of expressions to generate a table or plot object.

## Details

Although this function just returns `NULL` it has two uses, for the teal
module users it provides a documentation of arguments that are commonly
and consistently used in the framework. For the developer it adds a
single reference point to import the `roxygen` argument description
with: `@inheritParams template_arguments`
