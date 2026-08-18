# teal Module: Time-To-Event Table

This module produces a time-to-event analysis summary table, consistent
with the TLG Catalog template for `TTET01` available
[here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/ttet01.html).

## Usage

``` r
tm_t_tte(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  arm_ref_comp = NULL,
  paramcd,
  strata_var,
  aval_var = teal.picks::variables("AVAL", "AVAL", fixed = TRUE),
  cnsr_var = teal.picks::variables("CNSR", "CNSR", fixed = TRUE),
  conf_level_coxph = teal.picks::values(c(0.95, 0.9, 0.8), 0.95),
  conf_level_survfit = teal.picks::values(c(0.95, 0.9, 0.8), 0.95),
  time_points,
  time_unit_var = teal.picks::variables("AVALU", "AVALU", fixed = TRUE),
  event_desc_var = teal.picks::variables("EVNTDESC", "EVNTDESC", fixed = TRUE),
  add_total = FALSE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
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

- arm_ref_comp:

  (`list`) optional,\
  if specified it must be a named list with each element corresponding
  to an arm variable in `ADSL` and the element must be another list
  (possibly with delayed
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::variable_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/variable_choices.html)
  and
  [`teal.transform::value_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/value_choices.html)
  are deprecated but still accepted) with the elements named `ref` and
  `comp` that define the default reference and comparison arms when the
  arm variable is changed.

- paramcd:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- strata_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  names of the variables for stratified analysis.

- aval_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and pre-selected option for the
  analysis variable.

- cnsr_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  censoring variable.

- conf_level_coxph:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and pre-selected option for
  confidence level, each within range of (0, 1).

- conf_level_survfit:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and pre-selected option for
  confidence level, each within range of (0, 1).

- time_points:

  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  is deprecated but still accepted)\
  object with all available choices and preselected option for time
  points that can be used in
  [`tern::surv_timepoint()`](https://rdrr.io/pkg/tern/man/survival_timepoint.html).

- time_unit_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and pre-selected option for the time
  unit variable.

- event_desc_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  objects are deprecated but still accepted\
  variable for event description.

- add_total:

  (`logical`)\
  whether to include column with total number of patients.

- total_label:

  (`string`)\
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- na_level:

  (`string`)\
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

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

## Details

- The core functionality of this module is based on
  [`tern::coxph_pairwise()`](https://rdrr.io/pkg/tern/man/survival_coxph_pairwise.html),
  [`tern::surv_timepoint()`](https://rdrr.io/pkg/tern/man/survival_timepoint.html),
  and
  [`tern::surv_time()`](https://rdrr.io/pkg/tern/man/survival_time.html)
  from the `tern` package.

- The arm and stratification variables are taken from the `parentname`
  data.

- The following variables are used in the module:

  - `AVAL`: time to event

  - `CNSR`: 1 if record in `AVAL` is censored, 0 otherwise

  - `PARAMCD`: variable used to filter for endpoint (e.g. OS). After
    filtering for `PARAMCD` one observation per patient is expected

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://rdrr.io/pkg/rtables/man/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_tte(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOzbWycikrLK6v5SciUAX0UIACsieL8AazhWUUTQm3D+OGMoYVI-An5aUTKRscnp4Gh4GaS5AF1XaEYYP0Yl9aIYVCK6UVJ2BsyAYVtMrABZL7ZHQAXh0r3eDQyd2MoJ0CjAf3+OgAQgjcFCdMQnnCCB9EQCsuj4QTkV8EYMMnIMRAMki4RCPrSMjoYXCESjEDoAArUKAEOD0IjozHY55gvEIzJc7KMQQAcx0AA1iQivlyvo96PEPJYIBSGoNBko0M9wvFokyMkk4UkaRk+EIRKI4Y7hGIrSySn41j04J6WeCoPQ4NR2WBbLR4HYiDonNoyHZgyIRczA0kjrowVKmi1U4GMiwbpoWHCS4xaMmPZLSarSUC699fgCG2A5NTMYXrrd7mK4UWe8YHk97QXUCxYBsyyxK-Rnewa9y-plAdk60usCuKR20yy3sxPH5y9OK1WZjXck4VXgSSj-lgANJYABMFJSCMv153BeKUbgflQUYyBdMFfAwVAZHGUREEQEtqEEat2AARgADmfFJnwAFgAZmpHRUOfSkWSIo0lFoWF2Hicg1A0bRrBsNJmVEOIIFYTJ0HYU0ABJBFoFJuNEVRtEYQY+iUMA+jOIA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADTTE <- tmc_ex_adtte
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

arm_ref_comp <- list(
  ACTARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  ),
  ARM = list(
    ref = "B: Placebo",
    comp = c("A: Drug X", "C: Combination")
  )
)

app <- init(
  data = data,
  modules = modules(
    tm_t_tte(
      label = "Time To Event Table",
      dataname = "ADTTE",
      arm_var = variables(c("ARM", "ARMCD", "ACTARMCD")),
      arm_ref_comp = arm_ref_comp,
      paramcd = variables(c("PARAMCD", "PARAM")),
      strata_var = variables(c("SEX", "BMRKR2"), "SEX"),
      time_points = teal.picks::values(c(182, 243), 182)
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
