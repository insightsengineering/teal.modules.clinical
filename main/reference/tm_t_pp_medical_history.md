# teal Module: Patient Profile Medical History

This module produces a patient profile medical history report using ADaM
datasets.

## Usage

``` r
tm_t_pp_medical_history(
  label,
  dataname = "ADMH",
  parentname = "ADSL",
  patient_col = "USUBJID",
  mhterm = NULL,
  mhbodsys = NULL,
  mhdistat = NULL,
  pre_output = NULL,
  post_output = NULL,
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

- patient_col:

  (`character`)\
  name of patient ID variable.

- mhterm:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `MHTERM` variable from `dataname`.

- mhbodsys:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `MHBODSYS` variable from `dataname`.

- mhdistat:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `MHDISTAT` variable from `dataname`.

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

    tm_t_pp_medical_history(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOyAWQAJIpKyyur+GBilAF9FCAArIni-AGs4VlFE0Jtw-jhjKGFSPwJ+WlEy0fGpmeBoeFmkuQBdVwgcgqKk4GAFMBv8p-PL6+a2xdCHp5zWm8Pko0KgivFouwGkkdABeHRJXANPhCESiOE6FHCMRQiAZDIlPzrdB+eBbWT+OKiUhENi4-H46hQehwagYp5NODk7g6FrbGlsJ5IvEMhGhY66eH-L5ChoM1AsCikCXs555V54OX4hUaJUbIhsqVgBy5BwAIQAUgBJbKykUM3rkRgwDGaFi0ZloqFgVq2JxYJpCky0CpcjH2ZxyYWizExegCUQzV3uz04jktM0AeTyAE1ckHjCGw-CI04o1qMr0ttSPMnGB76F709krblbJlbAWi-xw44y9H8UMMkMhrRjDp2PEnVALNprDY0iLRHEIKxMuh2KCACSCWgpbeiVTaRhDfpKMD9c5AA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADMH <- tmc_ex_admh
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADMH <- data[["ADMH"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_pp_medical_history(
      label = "Medical History",
      dataname = "ADMH",
      parentname = "ADSL",
      patient_col = "USUBJID",
      mhterm = variables("MHTERM", fixed = TRUE),
      mhbodsys = variables("MHBODSYS", fixed = TRUE),
      mhdistat = variables("MHDISTAT", fixed = TRUE),
    )
  )
)
#> Initializing tm_t_pp_medical_history
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
