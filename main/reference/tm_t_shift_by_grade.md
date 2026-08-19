# teal Module: Grade Summary Table

This module produces a summary table of worst grades per subject by
visit and parameter.

## Usage

``` r
tm_t_shift_by_grade(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  visit_var = teal.picks::variables("AVISIT", "AVISIT", fixed = TRUE),
  paramcd,
  worst_flag_var = teal.picks::variables(c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"),
    "WGRLOVFL"),
  worst_flag_indicator = teal.picks::values("Y", "Y", fixed = TRUE, multiple = FALSE),
  anl_toxgrade_var = teal.picks::variables("ATOXGR", "ATOXGR", fixed = TRUE),
  base_toxgrade_var = teal.picks::variables("BTOXGR", "BTOXGR", fixed = TRUE),
  id_var = teal.picks::variables("USUBJID", "USUBJID", fixed = TRUE),
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

- visit_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as `visit` variable. Must be a factor in
  `dataname`.

- paramcd:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  variable for lab parameter code. The `values()` element is added
  internally to allow users to filter the parameter values
  interactively.

- worst_flag_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  variable for worst grade flag.

- worst_flag_indicator:

  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  is deprecated but still accepted)\
  value(s) indicating worst grade records.

- anl_toxgrade_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)
  variable for analysis toxicity grade.

- base_toxgrade_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)
  variable for baseline toxicity grade.

- id_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object specifying the variable name for subject id.

- add_total:

  (`logical`)\
  whether to include column with total number of patients.

- total_label:

  (`string`)\
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- drop_arm_levels:

  (`logical`)\
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

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

- na_level:

  (`string`)\
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- code_missing_baseline:

  (`logical`)\
  whether missing baseline grades should be counted as grade 0.

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOz8gCEikrLK6v5qeiUAX0UIACsieL8AazhWUUTQm3D+OGMoYVI-An5aUTKRscnp4Gh4GaS5AF1XaHQi+Oj2BqSdAF4dJNwGviERUWedT+ExPcIBkMiU-GtRHFjGt6Kw-ABzZiLIEgkHUKD0ODUX4KMAAcSRunyGKIzFIpNYWXoEFJMG40UptgxIlx72BqNeoSOuheuJyLVZDVRLBgfk0LF+4sYtGZgII9zAmSwAFlWTo+SqAMLZXFyORsjk6VAsWAbX6oGTjGZSmX0b4KgAKSsyyu1upS4uogkBoixcHUcH4OMV+Vs7p0BBi-vGAVCvtIvwAYpl8rknPqhSCIqTRGtjOj4WKJS8bbKZpnUZHRgQxL95biAOp4rD5ADyADVE-k1Y3m22uz2wE2sAAJACSne7eHVQ+b44HYAz7MNvpEAaDvNnLY7C4rOiXhpq-HBRE82JeydTLmX+4aB5MtGo5EYv18fjqMkBb4-NYVOQKg65JkiYLikq7+uQG4zgAmrqgyDLQxg6Ow8TPlAFjaNYNhpOykLxKwmToOwaCoAAJIItApCRpG+ow2iMIMfRKGAfRnEAA)

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
    tm_t_shift_by_grade(
      label = "Grade Laboratory Abnormality Table",
      dataname = "ADLB",
      arm_var = variables(c("ARM", "ARMCD")),
      paramcd = picks(variables("PARAMCD"), values(selected = "ALT"), check_dataset = FALSE),
      worst_flag_var = variables(
        choices = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"),
        selected = "WGRLOVFL"
      ),
      add_total = FALSE
    )
  ),
  filter = teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y"))
)
#> Initializing tm_t_shift_by_grade
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
