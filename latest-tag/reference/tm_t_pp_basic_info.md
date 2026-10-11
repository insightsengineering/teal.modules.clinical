# teal Module: Patient Profile Basic Info

This module produces a patient profile basic info report using ADaM
datasets.

## Usage

``` r
tm_t_pp_basic_info(
  label,
  dataname = "ADSL",
  patient_col = "USUBJID",
  vars = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = lifecycle::deprecated()
)
```

## Arguments

- label:

  (`character`)\
  menu item label of the module in the teal app.

- dataname:

  (`character`)\
  analysis data used in teal module.

- patient_col:

  (`character`)\
  name of patient ID variable.

- vars:

  ([`teal.picks::variables`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))

  object specifying available choices and preselected variables from
  `dataname` to show in the table.

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWolAF9FCAArIni-AGs4VlFE0Jtw-jhjKGFSPwJ+WlEyto7u3uBoeD6kuQBdV2h0Ivjo9nSdJJ0AXmPQ3CO+IRFRM50b4TFDiAyMkr8J9D96KFEZH54sYiK93u9qFB6HBqA8FGAAEL-GQ6ACSEBB8Kub3BF08K1053hOQKWKO4NQHloFAmxFhRLADlyDgRAClUdkyTjwZoWPdzrzGLQoXcwbiMgRDmBMlgALJYnTEgDiLjwirAuScAA0FfCAMIAeQcADl7ABNXVgLCZPWqlLwpwG3K5Wy2eFybHijIwca0VAiB72Zzk97NcFhnTNZq0Yw6djxchqDTaaw2NI40RxCCsTLodhoVAAEkEtBSBcLolU2kYzQaSjADQ2QA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_t_pp_basic_info(
      label = "Basic Info",
      dataname = "ADSL",
      patient_col = "USUBJID",
      vars = variables(
        c("ARM", "AGE", "SEX", "COUNTRY", "RACE", "EOSSTT"),
        multiple = TRUE
      )
    )
  )
)
#> Initializing tm_t_pp_basic_info
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
