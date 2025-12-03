# teal Module: Grade Summary Table

This module produces a summary table of worst grades per subject by
visit and parameter.

## Usage

``` r
tm_t_shift_by_grade(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  visit_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    subset = "AVISIT"), selected = "AVISIT", fixed = TRUE),
  paramcd,
  worst_flag_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, subset =
    c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL")), selected = "WGRLOVFL"),
  worst_flag_indicator =
    teal.transform::choices_selected(teal.transform::value_choices(dataname, "WGRLOVFL"),
    selected = "Y", fixed = TRUE),
  anl_toxgrade_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, subset =
    c("ATOXGR")), selected = c("ATOXGR"), fixed = TRUE),
  base_toxgrade_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, subset =
    c("BTOXGR")), selected = c("BTOXGR"), fixed = TRUE),
  id_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    subset = "USUBJID"), selected = "USUBJID", fixed = TRUE),
  add_total = FALSE,
  total_label = default_total_label(),
  drop_arm_levels = TRUE,
  pre_output = NULL,
  post_output = NULL,
  na_level = tern::default_na_str(),
  code_missing_baseline = FALSE,
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
  names that can be used as `arm_var`. It defines the grouping variable
  in the results table.

- visit_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as `visit` variable. Must be a factor in
  `dataname`.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- worst_flag_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as worst flag variable.

- worst_flag_indicator:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  value indicating worst grade.

- anl_toxgrade_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  variable for analysis toxicity grade.

- base_toxgrade_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  variable for baseline toxicity grade.

- id_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object specifying the variable name for subject id.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

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

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- code_missing_baseline:

  (`logical`)  
  whether missing baseline grades should be counted as grade 0.

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

    tm_t_shift_by_grade(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOz8gCEikrLK6v5qeiUAX0UIACsieL8AazhWUUTQm3D+OGMoYVI-An5aUTKRscnp4Gh4GaS5AF1XCByCoqTgYAUwa-zHs4urptaF0PvHnJbXu8lGhUEV4tF2A0kjoALw6JK4Bp8IQiUSwnTI4RiSEQDIZEp+NaiOLGNb0Vh+ADmzEWOLxeOoUHocGo6MeAHEabp8kyiMxSHzWFl6BA+TBuNEhbYmSJHojcfT4aEjro4X9PnKGvSWDA-JoWOiCDFRgQxH5RCy4Oo4Pw6YqMkaTWJ0frGLQZXB1saZNjnilRIJ6BbSIbIU8sABZOU6P6RgDC2Uecjk8vtGQtImt-DZ4ajYC1eJTBYyqBYsA2hu9ptE5stWbt9sdPrRcP11EEnqb1fY-2aKUeAAVMlhMhGE9HB8PR0nU2mM1byNm1U98rZHsWdEWFfSInzRGtjIzKXqDXCu2b5-WNw6q87Wyx3fQRF6nTNe-7A8HQ48AOrsrD5AA8gAagAYi8eAxmAf4AYB4ETtB-4ABIAJJgRB-aIVgqHwWAyazval6Lt+WFAehSYblu9q7ow+5+IeUDHvEmyyAKjCVq+taZouDaKm2HYvs2PafJhMFkbhVFznWxHLgAmtGxi0BUNrovYziUQReJQBA-gChU1I1J6rocc2XELjavH0ueLY6K6j7PtZwktB+QZwCGZ5hpktiAQAGv+SaSYR0kqR5fzeX5WAzteJhKSFdiOC426FppGT0FAFqEkQ+lcie7FnreNZERZ0XWS6D4eoJ3bvjoAaue5OgEGGzThf5eGBYqRVLg1TUtZFeEpXiinKV1amJfa7UZDU-CZZ4rJwqBmT5LkY2Fg0kmKdQ5B5cUcDcOadDdr4+0+p5eQYVBuSZKBuH+sFXWPPJbVKIMtDGDo7DxFtUAWNo1g2GkCrEvErCZOg7AggAJIItApJDFqMNojCDH0ShgH0ZxAA)

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
    tm_t_shift_by_grade(
      label = "Grade Laboratory Abnormality Table",
      dataname = "ADLB",
      arm_var = choices_selected(
        choices = variable_choices(ADSL, subset = c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      paramcd = choices_selected(
        choices = value_choices(ADLB, "PARAMCD", "PARAM"),
        selected = "ALT"
      ),
      worst_flag_var = choices_selected(
        choices = variable_choices(ADLB, subset = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL")),
        selected = c("WGRLOVFL")
      ),
      worst_flag_indicator = choices_selected(
        value_choices(ADLB, "WGRLOVFL"),
        selected = "Y", fixed = TRUE
      ),
      anl_toxgrade_var = choices_selected(
        choices = variable_choices(ADLB, subset = c("ATOXGR")),
        selected = c("ATOXGR"),
        fixed = TRUE
      ),
      base_toxgrade_var = choices_selected(
        choices = variable_choices(ADLB, subset = c("BTOXGR")),
        selected = c("BTOXGR"),
        fixed = TRUE
      ),
      add_total = FALSE
    )
  ),
  filter = teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y"))
)
#> Initializing tm_t_shift_by_grade
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
