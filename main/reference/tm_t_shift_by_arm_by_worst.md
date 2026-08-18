# teal Module: Shift by Arm by Worst Analysis Indicator Level

This module produces a summary table of worst analysis indicator
variable level per subject by arm.

## Usage

``` r
tm_t_shift_by_arm_by_worst(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  paramcd,
  aval_var,
  base_var = lifecycle::deprecated(),
  baseline_var,
  worst_flag_var,
  worst_flag,
  treatment_flag_var = teal.picks::variables("ONTRTFL", "ONTRTFL"),
  treatment_flag = teal.picks::values("Y", "Y", fixed = TRUE, multiple = FALSE),
  useNA = c("ifany", "no"),
  na_level = tern::default_na_str(),
  add_total = FALSE,
  total_label = default_total_label(),
  pre_output = NULL,
  post_output = NULL,
  basic_table_args = teal.widgets::basic_table_args(),
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character`)\
  menu item label of the module in the teal app.

- dataname:

  (`character`)\
  analysis data used in teal module.

- parentname:

  (`character`)\
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- arm_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. It defines the grouping variable
  in the results table.

- paramcd:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  variable for lab parameter code. The `values()` element is added
  internally to allow users to filter the parameter values
  interactively.

- aval_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and pre-selected option for the
  analysis variable.

- base_var:

  **\[deprecated\]** Please use the `baseline_var` argument instead.

- baseline_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  values that can be used as `baseline_var`.

- worst_flag_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as worst flag variable.

- worst_flag:

  (`character`)\
  value indicating worst analysis indicator level.

- treatment_flag_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  on treatment flag variable.

- treatment_flag:

  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  is deprecated but still accepted)\
  value matching `treatment_flag_var` for on-treatment records (default
  `"Y"`).

- useNA:

  (`character`)\
  whether missing data (`NA`) should be displayed as a level.

- na_level:

  (`string`)\
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- add_total:

  (`logical`)\
  whether to include column with total number of patients.

- total_label:

  (`string`)\
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- pre_output:

  (`shiny.tag`) optional,\
  with text placed before the output to put the output into context. For
  example a title.

- post_output:

  (`shiny.tag`) optional,\
  with text placed after the output to put the output into context. For
  example the
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements are useful.

- basic_table_args:

  (`basic_table_args`) optional\
  object created by
  [`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
  with settings for the module table. The argument is merged with option
  `teal.basic_table_args` and with default module arguments (hard coded
  in the module body). For more details, see the vignette:
  [`vignette("custom-basic-table-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-basic-table-arguments.html).

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

- decorators:

  **\[experimental\]** (named `list` of lists of
  `teal_transform_module`) optional, decorator for tables or plots
  included in the module output reported. The decorators are applied to
  the respective output objects.

  See section "Decorating Module" below for more details.

## Value

a `teal_module` object.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://rdrr.io/pkg/rtables/man/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_shift_by_arm_by_worst(
       ..., # arguments for module
       decorators = list(
         table = teal_transform_module(...) # applied only to `table` output
       )
    )

For additional details and examples of decorators, refer to the vignette
[`vignette("decorate-module-output", package = "teal.modules.clinical")`](https://insightsengineering.github.io/teal.modules.clinical/articles/decorate-module-output.md).

To learn more please refer to the vignette
[`vignette("transform-module-output", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html)
or the
[`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)
documentation.

## Reporting

This module returns an object of class `teal_module`, that contains a
`server` function. Since the server function returns a `teal_report`
object, this makes this module reportable, which means that the
reporting functionality will be turned on automatically by the `teal`
framework.

For more information on reporting in `teal`, see the vignettes:

- [`vignette("reportable-shiny-application", package = "teal.reporter")`](https://insightsengineering.github.io/teal.reporter/latest-tag/articles/reportable-shiny-application.html)

- `vignette("adding-support-for-reporting-to-custom-modules", package = "teal")`

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOynAHEikrLK6v44AHMlAF9FCAArIni-AGs4VlFE0Jtw7uMoYVI-An5aUTLR8amZ4Gh4WaS5AF1XaHQi+Oj2BqSdAF4dJNwGviERUWedT+ExPcIBkMiU-GtRHFjGt6KxqowYH5YX4IkRGKJSECQSDqFB6HBqL8FGBclDSDpYVkEXY8SJie9gdjXqEjroXsSci16Q1sSxEZoWL8BYxaLTAQQYmMCGJfgR7mBMlgALL0nQc5UAYWyxLkcgZTJ0qBYsA2QpYovo33YEqlMvZYAACorMkqtTr9UzUei1sZcT0-MKzSKxbMeUybTI7To5cSAOoAeSwuQADABGABi+VVccTKYATJn3WHsaICXB1HB+ESwAmk8mC1mwMWdHrm16MX5fVAembqIJAcSAJrZsDDvAmWgVSu-ezOVuM3kC-yBl7Ci1WiPSn4vGMKgBqmXyGpHmQAclgAJKn7VgPU6UsiCtV+1ny-XosLkH0KAP+JwAOCqu5ohtakqRtu0bygAQpkuROMe47ElB55Xjed4PuW5DPmqYDIW+aEetigilqemTVrQywQKwxJhkMGRDEMFE6Ow8TkGoGjaNYNhpIykLxKwmToOwaCoAAJIItApCJomlow2iMEM-RKGA-RnEAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADEG <- tmc_ex_adeg
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_t_shift_by_arm_by_worst(
      label = "Shift by Arm Table",
      dataname = "ADEG",
      arm_var = variables(choices = c("ARM", "ARMCD")),
      paramcd = variables(choices = "PARAMCD"),
      worst_flag_var = variables(
        choices = c("WORS01FL", "WORS02FL"),
        selected = "WORS02FL"
      ),
      worst_flag = values("Y", "Y", fixed = TRUE),
      aval_var = variables(choices = c("AVALC", "ANRIND"), selected = "ANRIND"),
      baseline_var = variables(choices = c("BASEC", "BNRIND"), selected = "BNRIND"),
      useNA = "ifany"
    )
  )
)
#> Initializing tm_t_shift_by_arm_by_worst
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
