# teal Module: Summarize Variables by Row Groups

This module produces a table to summarize variables by row groups.

## Usage

``` r
tm_t_summary_by(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  by_vars,
  summarize_vars,
  id_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    subset = "USUBJID"), selected = "USUBJID", fixed = TRUE),
  paramcd = NULL,
  add_total = TRUE,
  total_label = default_total_label(),
  parallel_vars = FALSE,
  row_groups = FALSE,
  useNA = c("ifany", "no"),
  na_level = tern::default_na_str(),
  numeric_stats = c("n", "mean_sd", "median", "range"),
  denominator = teal.transform::choices_selected(c("n", "N", "omit"), "omit", fixed =
    TRUE),
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

- by_vars:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names used to split the summary by rows.

- summarize_vars:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  names of the variables that should be summarized.

- id_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object specifying the variable name for subject id.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- parallel_vars:

  (`logical`)  
  whether summarized variables should be arranged in columns. Can only
  be set to `TRUE` if all chosen analysis variables are numeric.

- row_groups:

  (`logical`)  
  whether summarized variables should be arranged in row groups.

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

- drop_zero_levels:

  (`logical`)  
  whether rows with zero counts in all columns should be removed from
  the table.

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOz8gCEikrLK6v5qeiUAX0UIACsieL8AazhWUUTQm3D+OGMoYVI-An5aUTKRscnp4Gh4GaS5AF1XCByCoqTgYAUwa-zHs4urptaF0PvHnJbXu8lGhUEV4tF2A0kjoALw6JK4Bp8IQiUSwnTI4RiSEQDIZEp+NaiQQwGAsVh+egcBp4nTUKD0ODUdGPXIkslsHRUnRYIgRHQAcUYREEqDRtgZIkeiNxtPhoSOujhf0+0ppeJYMD8mhY6IIMVGBDEflETLg6jg-Bxcrx+sNYnROsYtElcHWBpk2OeKQIkKeWAAstKdH9AwBhbKPORyGU2jKmkQW-gs-1BsDqjIxjM6Gr8QlETzMuH2ZyxuVU7UsNFwu2e0Qms1J61x2tG6s6J0u+gid32mb-Zo+v0ABUyWEyaZSfwAagBJXKz2xRrOym0J83kZM1v2ZOcLpdgQY2ldr9ksWgALzdTvbreN66b2Yyd-bnddvbr7AHQ5nmReeBDMAwwACQFZcyzXRtNz1Hdpz-KNsxPOVBFNAA5TIU1oZYIFYNVVzxVAWFgDY9Q9NsG0TTdmxtF9HW4QQ3RfL9PinMBR3HAMI2DR52InKMILlB9oOVJ58gPRD1SPHRBkGLCdHYeJyDUDRtGsGw0llUQ4hwzJ0HYEEABJBFoFJDNNRhtEYQY+iUMA+jOIA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADLB <- data[["ADLB"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_summary_by(
      label = "Summary by Row Groups Table",
      dataname = "ADLB",
      arm_var = choices_selected(
        choices = variable_choices(ADSL, c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      add_total = TRUE,
      by_vars = choices_selected(
        choices = variable_choices(ADLB, c("PARAM", "AVISIT")),
        selected = c("AVISIT")
      ),
      summarize_vars = choices_selected(
        choices = variable_choices(ADLB, c("AVAL", "CHG")),
        selected = c("AVAL")
      ),
      useNA = "ifany",
      paramcd = choices_selected(
        choices = value_choices(ADLB, "PARAMCD", "PARAM"),
        selected = "ALT"
      )
    )
  )
)
#> Initializing tm_t_summary_by
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
