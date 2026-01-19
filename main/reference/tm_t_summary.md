# teal Module: Summary of Variables

This module produces a table to summarize variables.

## Usage

``` r
tm_t_summary(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  summarize_vars,
  add_total = TRUE,
  total_label = default_total_label(),
  show_arm_var_labels = TRUE,
  useNA = c("ifany", "no"),
  na_level = tern::default_na_str(),
  numeric_stats = c("n", "mean_sd", "mean_ci", "median", "median_ci", "quantiles",
    "range", "geom_mean"),
  denominator = c("N", "n", "omit"),
  drop_arm_levels = TRUE,
  pre_output = NULL,
  post_output = NULL,
  basic_table_args = teal.widgets::basic_table_args(),
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character`)  
  menu item label of the module in the teal app.

- dataname:

  (`character`)  
  analysis data used in teal module.

- parentname:

  (`character`)  
  parent analysis data used in teal module, usually this refers to
  `ADSL`.

- arm_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. It defines the grouping
  variable(s) in the results table. If there are two elements selected
  for `arm_var`, second variable will be nested under the first
  variable.

- summarize_vars:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  names of the variables that should be summarized.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- show_arm_var_labels:

  (`flag`)  
  whether arm variable label(s) should be displayed. Defaults to `TRUE`.

- useNA:

  (`character`)  
  whether missing data (`NA`) should be displayed as a level.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- numeric_stats:

  (`character`)  
  names of statistics to display for numeric summary variables.
  Available statistics are `n`, `mean_sd`, `mean_ci`, `median`,
  `median_ci`, `quantiles`, `range`, and `geom_mean`.

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

- pre_output:

  (`shiny.tag`) optional,  
  with text placed before the output to put the output into context. For
  example a title.

- post_output:

  (`shiny.tag`) optional,  
  with text placed after the output to put the output into context. For
  example the
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements are useful.

- basic_table_args:

  (`basic_table_args`) optional  
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
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_summary(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpQGIdABUZxULKBokOkTGOqQAFrrkoqQ6sqK6RoIJOgAGTgDyAMoAIgCaqTpQEPxpOQDCWVhOAIJZhZostFD0IqJhRDr8cHwQMczkOjC0oqK0EADmXQFQGEr8M-pGXNQA+gukUNbzi4Y6AO60EePsG1C4OiBKOjo1OVkAMkthMASrcAAeq1D8otTXt3uDwAJJlcnlgABGAC6zwAcjVVuNyBNVKslABfRQQABWRHGqwA1nBWKJTjMbHtusYoMJSKsCPwRm88QTiaTgNB4GSznJoa4IHdHs8zsBgAowEKHhLofyIEo0KhnuMjuwAWcdABeaabXAAvhCNpaoYCYRiNUQG43UgwVb00SCGAwFgcAFWnTUFpwajGiU5HpECbMVDhGR2FoiCV6y3unXFWC6bUSqVRt1Wli2xqMY0EcL4ghiVYJETqOD8dgENWSrAAWSjOmTtfKOQlcgujbrYDbaZuP34dqImx92vszmjsZ0DqdTQAXnBVln2trc-nC8W4KXyz2rZWJVknAANesSrA1couPANsAAIRrWAA0lgAEzHsBg-KvipVWpZV81ADiF7djGE67mA+5HpeJ5nkB27AROyRwAivpgLQNIQKwEpptiNzYtiaE6OwyKqFAFjaNYNhXDGoihhhNToOwirAoItAXExCSMNojDYhiShgBi0JAA)

## Examples

``` r
# Preparation of the test case - use `EOSDY` and `DCSREAS` variables to demonstrate missing data.
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADSL$EOSDY[1] <- NA_integer_
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
      add_total = TRUE,
      summarize_vars = choices_selected(
        c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE"),
        c("SEX", "RACE")
      ),
      useNA = "ifany"
    )
  )
)
#> Initializing tm_t_summary
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
