# teal Module: Summarize Variables by Row Groups

This module produces a table to summarize variables by row groups.

## Usage

``` r
tm_t_summary_by(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  by_vars,
  summarize_vars,
  id_var = teal.picks::variables("USUBJID", "USUBJID", fixed = TRUE),
  paramcd = NULL,
  add_total = TRUE,
  total_label = default_total_label(),
  parallel_vars = FALSE,
  row_groups = FALSE,
  useNA = c("ifany", "no"),
  na_level = tern::default_na_str(),
  numeric_stats = c("n", "mean_sd", "median", "range"),
  categorical_stats = c("n", "count"),
  denominator = teal.picks::values(c("n", "N", "omit"), "omit", fixed = TRUE),
  drop_arm_levels = TRUE,
  drop_zero_levels = TRUE,
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
  If there are two elements selected for `arm_var`, the second variable
  is nested under the first.

- by_vars:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names used to split the summary by rows.

- summarize_vars:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  names of the variables that should be summarized.

- id_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object specifying the variable name for subject id.

- paramcd:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  optional variable for parameter code filter. When provided, a
  `values()` selector is added with `multiple = FALSE`, so one parameter
  level is selected by default.

- add_total:

  (`logical`)\
  whether to include column with total number of patients.

- total_label:

  (`string`)\
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- parallel_vars:

  (`logical`)\
  whether summarized variables should be arranged in columns. Can only
  be set to `TRUE` if all chosen analysis variables are numeric.

- row_groups:

  (`logical`)\
  whether summarized variables should be arranged in row groups.

- useNA:

  (`character`)\
  whether missing data (`NA`) should be displayed as a level.

- na_level:

  (`string`)\
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- numeric_stats:

  (`character`)\
  names of statistics to display for numeric summary variables.
  Available statistics are `n`, `mean_sd`, `mean_ci`, `median`,
  `median_ci`, `quantiles`, `range`, and `geom_mean`.

- categorical_stats:

  (`character`)\
  names of statistics to display for non-numeric summary variables.
  Available statistics are `n`, `count`, `count_fraction`,
  `count_fraction_fixed_dp`, `fraction` and `n_blq`.

- denominator:

  (`character`)\
  chooses how percentages are calculated. With option `N`, the reference
  population from the column total is used as the denominator. With
  option `n`, the number of non-missing records in this row and column
  intersection is used as the denominator. If `omit` is chosen, then the
  percentage is omitted.

- drop_arm_levels:

  (`logical`)\
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

- drop_zero_levels:

  (`logical`)\
  whether rows with zero counts in all columns should be removed from
  the table.

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

    tm_t_summary_by(
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

## See also

The [TLG
Catalog](https://insightsengineering.github.io/tlg-catalog/stable/)
where additional example apps implementing this module can be found.

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOz8gCEikrLK6v5qeiUAX0UIACsieL8AazhWUUTQm3D+OGMoYVI-An5aUTKRscnp4Gh4GaS5AF1XaHQi+Oj2BqSdAF4dJNwGviERUWedT+ExPcIBkMiU-GtRIIYDAWKw-PQOA0QTpqFB6HBqL8FGBclCYWwdAidFgiBEdABxRhEQSoH62NEibHvYHI16hI66F7YnItJlIkEsGB+TQsX4ixi0BmAggxUYEMS-Aj3MCZLAAWSZOm56oAwtlsXIUjBVrRUCJfvZnIb+RkavxwURPJiXpanMzWYS4eKfi9xZL6N92DK5QqXkrsQAFVWZDV4LUqgBqAElcknbAaUqIMXB1HB+FjEym05rjdQNGbOXZHE5rSzkZDoSxaAAvODClg+nR+qUzYMyUM6cOJzL5TXYnUACXJGZ0WZEufzXOHo7jpfL5pd1drHsEWYAcpkC7RlhBWHy6yDUCxYBtfqgZOMZjaQd2A9LZf3O5Ho2q9RnnxkIrUIIgJzjm5CLvGI7pquJoVhaW7uh6g4xDm4wBKEWakL8ABiI65C4F4ZIMyIkTogyDMeOjsPE5BqBo2jWDYaQsqIcSnpk6DsGgqAACSCLQKQ8bxWaMNojCDH0ShgH0ZxAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_t_summary_by(
      label = "Summary by Row Groups Table",
      dataname = "ADLB",
      arm_var = variables(choices = c("ARM", "ARMCD"), multiple = TRUE),
      add_total = TRUE,
      by_vars = variables(choices = c("PARAM", "AVISIT"), selected = "AVISIT", multiple = TRUE),
      summarize_vars = variables(choices = c("AVAL", "CHG"), selected = "AVAL", multiple = TRUE),
      useNA = "ifany",
      paramcd = picks(
        variables(choices = "PARAMCD"),
        values(selected = "ALT", multiple = TRUE),
        check_dataset = FALSE
      )
    )
  )
)
#> Initializing tm_t_summary_by
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
