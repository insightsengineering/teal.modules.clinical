# teal Module: Patient Profile Vitals Plot

This module produces a patient profile vitals
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
type plot using ADaM datasets.

## Usage

``` r
tm_g_pp_vitals(
  label,
  dataname = "ADVS",
  parentname = "ADSL",
  patient_col = "USUBJID",
  paramcd = NULL,
  aval = lifecycle::deprecated(),
  aval_var = NULL,
  xaxis = NULL,
  font_size = c(12L, 12L, 25L),
  plot_height = c(700L, 200L, 2000L),
  plot_width = NULL,
  pre_output = NULL,
  post_output = NULL,
  ggplot2_args = teal.widgets::ggplot2_args(),
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

- patient_col:

  (`character`)  
  name of patient ID variable.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- aval:

  **\[deprecated\]** Please use the `aval_var` argument instead.

- aval_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  analysis variable.

- xaxis:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the time
  variable from `dataname` to be put on the plot x-axis.

- font_size:

  (`numeric`)  
  numeric vector of length 3 of current, minimum and maximum font size
  values.

- plot_height:

  (`numeric`) optional  
  vector of length three with `c(value, min, max)`. Specifies the height
  of the main plot and renders a slider on the plot to interactively
  adjust the plot height.

- plot_width:

  (`numeric`) optional  
  vector of length three with `c(value, min, max)`. Specifies the width
  of the main plot and renders a slider on the plot to interactively
  adjust the plot width.

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

- ggplot2_args:

  (`ggplot2_args`) optional  
  object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with option
  `teal.ggplot2_args` and with default module arguments (hard coded in
  the module body). For more details, see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

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

This plot supports horizontal lines for the following 6 `PARAMCD` levels
when they are present in `dataname`: `"SYSBP"`, `"DIABP"`, `"TEMP"`,
`"RESP"`, `"OXYSAT"`.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_pp_vitals(
       ..., # arguments for module
       decorators = list(
         plot = teal_transform_module(...) # applied only to `plot` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo6AIIAIgDKADKROqQwBDFwAB4xUPyi1KUVlQBq1Q1NLe2d-JqiSgC+QQBWRLkxANZwrKL5yTap-HDGUMKkMQT8tKIti8trG8DQ8JsFcgC6rhBVdQ0FwMAKYB+1P7PV7vQbDXbJH5-KpDIEgpRoVANXKZdh9Ao6AC8OgKuD6fCEIlEWJ0BOEYjREDKZSaMQA5jF0DFNJluJs+tSdNQoPQ4NQSX8BqzqNM8BzqQV7rpsdCwX88VTOTpUCwKKQpQL-jVAWLFZyVRo1SciPyZWAHNUHAAhABSAElKvLxWVUP5jlk4LQ6VlSCSCOwAGwABiDtSKACYQ2GdJGo3IFUrlSxYKc-VklgQxDFRHy4Oo4PxKYmygR0zIxCTNCxaDyRCcy5nNjDqkU-gAFcpYcoAWQAwo6wPHndScyJ8-xNR2u32B8Oh3rqW0oG1zmmM1nR3nyIXhyWGxXsVXGDX6HXS+um2DW1qAJp-efFnSb8eaqp3sBzhNKqBV2JHtflqI2a5uORbFuegGVtWtZwPWF7sM217lAM5Q6g+xbPtur4oTqc7ikEZRBEEtDGDo7C5OQagaNo1g2CUiqiDkECsOU6DsIiAAkgi0EUnE5ow2iBLMShgDMzxAA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADVS <- tmc_ex_advs
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADVS <- data[["ADVS"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_pp_vitals(
      label = "Vitals",
      dataname = "ADVS",
      parentname = "ADSL",
      patient_col = "USUBJID",
      plot_height = c(600L, 200L, 2000L),
      paramcd = choices_selected(
        choices = variable_choices(ADVS, "PARAMCD"),
        selected = "PARAMCD"
      ),
      xaxis = choices_selected(
        choices = variable_choices(ADVS, "ADY"),
        selected = "ADY"
      ),
      aval_var = choices_selected(
        choices = variable_choices(ADVS, "AVAL"),
        selected = "AVAL"
      )
    )
  )
)
#> Initializing tm_g_pp_vitals
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
