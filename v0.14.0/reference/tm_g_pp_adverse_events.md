# teal Module: Patient Profile Adverse Events Table and Plot

This module produces an adverse events table and
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
type plot using ADaM datasets.

## Usage

``` r
tm_g_pp_adverse_events(
  label,
  dataname = "ADAE",
  parentname = "ADSL",
  patient_col = "USUBJID",
  aeterm = NULL,
  tox_grade = NULL,
  causality = NULL,
  outcome = NULL,
  action = NULL,
  time = NULL,
  decod = NULL,
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

- aeterm:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `AETERM` variable from `dataname`.

- tox_grade:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `AETOXGR` variable from `dataname`.

- causality:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `AEREL` variable from `dataname`.

- outcome:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `AEOUT` variable from `dataname`.

- action:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `AEACN` variable from `dataname`.

- time:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `ASTDY` variable from `dataname`.

- decod:

  ([teal.picks::variables](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))\
  object with all available choices and preselected option for the
  `AEDECOD` variable from `dataname`.

- font_size:

  (`numeric`)\
  numeric vector of length 3 of current, minimum and maximum font size
  values.

- plot_height:

  (`numeric`) optional\
  vector of length three with `c(value, min, max)`. Specifies the height
  of the main plot and renders a slider on the plot to interactively
  adjust the plot height.

- plot_width:

  (`numeric`) optional\
  vector of length three with `c(value, min, max)`. Specifies the width
  of the main plot and renders a slider on the plot to interactively
  adjust the plot width.

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

- ggplot2_args:

  (`ggplot2_args`) optional\
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

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators::

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_pp_adverse_events(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo6AIIAIuVOkTrRvALCYhJSMtyIiKQwBDFwAB4xUKFwpRWVAMoAMnX8qNSsjJ3GtNTkjJxw3I1CIuKSue3Und29A0P8otRFDhMOAEIAUgCSlToApLnv4zUAJLcPF6VIIAXyCACsiLkYgBrOCsUT5ZI2VL8ODGKDCUgxAj8WiiXqQ6FwhHAaDwREFOQAXVcECq01myWAwAUYAZUzZ1Np9OqtVRzNZ7L5XJ5SjQqDqh1I7DGBR0AF4dAVcGM+LsxIqdOrmoixmVujEAOYxdAXbSMURwPraMh6iBlR06ahQehwahatnlfgWq06Jy20iiNmqh1O5XJcm6JVekV4fWO1AsCikKOe4XTEMJspJjQpnFED0xsAAp6vLNhp3zIjYrJwWhGrKkLUEdgANgADB2pkUAExdns6fsDuSh8M6KBwdYwLWaFi0V17WXspy2JxYACybNH2fqREGRuYaNn88XYmXNVsAHkABoAcSw27H4dkglE3EyrBPjAX9CXXvXJxOTAHdK0dIhBF8Ih4G-X9-xXK8HFsJ9dygCwSFgs89RXcoAGEADkULAg1aBgpU5x-LCLwmWxKgATSI8c0WIfgtXwhwpk5MCgjKIIgloYwdHYXJ1jQjRtGsGwSjDUQcggVhynQdgJV+QRaCKFSrUYC1QSUMAQWpIA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  ADAE <- teal.modules.clinical::tmc_ex_adae
  ADSL <- dplyr::filter(teal.modules.clinical::tmc_ex_adsl, USUBJID %in% ADAE$USUBJID)
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_pp_adverse_events(
      label = "Adverse Events",
      dataname = "ADAE",
      parentname = "ADSL",
      patient_col = "USUBJID",
      plot_height = c(600L, 200L, 2000L),
      aeterm = variables("AETERM"),
      tox_grade = variables("AETOXGR"),
      causality = variables("AEREL"),
      outcome = variables("AEOUT"),
      action = variables("AEACN"),
      time = variables("ASTDY"),
      decod = NULL
    )
  )
)
#> Initializing tm_g_pp_adverse_events
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
