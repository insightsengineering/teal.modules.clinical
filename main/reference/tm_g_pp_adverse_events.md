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

- aeterm:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AETERM` variable from `dataname`.

- tox_grade:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AETOXGR` variable from `dataname`.

- causality:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AEREL` variable from `dataname`.

- outcome:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AEOUT` variable from `dataname`.

- action:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AEACN` variable from `dataname`.

- time:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `ASTDY` variable from `dataname`.

- decod:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  `AEDECOD` variable from `dataname`.

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxsnHDcvALCYhJSMvKl5QyVHPyo1KyBbQCCACIDTpE6pDAEMXAAHjFQoXCDQwDKADLjxrTU5IycUzPzi6LURQ6rDgBCAFIAkkM6AKS5TzrDowAkF9f3Q0EAXyCACsiLkYgBrOCsUT5ZI2VL8ODGKDCUgxAj8Wiiaag8FQmHAaDwWEFOQAXVcEGGG3GBWAwAUYBp6yZ5Mp1JGY0RyQZTI+LjA7KpaFQ41ymXYbQKOgAvDoCrg2nwhCJRHKdCqGrC2mVJjEAOYxdALfjaRiiOAzbRkHUQMoO8pQehwaga-lm1SWnROG2kURMpX2x0K5LE3Ty-lcwO6h2oFgUUjh93MtasvCxsrxjSJjFEN2RsA-W4PGPBx09IjorJwWgGrKkDUEdgANgADG31kUAEwdrs6Xt9uRBkM6KBwPYwJtZMEEMQxS0idRwfhS8shggzmRiDWaFi0Z0iDFbuewgVFflOWxOLAAWSZw8zDsXcGX-BTo2vd6ZT8f671RDzAazBItOs7zi+b5rqOZSbuB6rynujAHvQR5wduZ5chezJXgA8gAGgA4lgD4jjBkHkO+hafgRxE-v+Oh-qOsiCKI3CZKwYEYQurqvpR0Ewehp67vuh5WkJYjsOeOiXlgTjpkx5G8W+H43vJ9GjopjpEIIvhEPAXGnjxS78U+sEnjuiGiah4kWZhozYaMuEOLYpFmToFErqpzmuWAv5kY6UAWCQhkQcppkMeZ8EichYnHvBUlYTJOEDAAwgAcm5kUeeFXnUU4aWZX5DFaQ6GgGfKEmiMZfErgJzF2TFKFoXZiUOclAyrLYQwAJpZTBOUmXlHVdb1GkhqVZRIsQVE6OlDjrKy65BGUQRBLQxg6OwuR7EFGjaNYNglMGog5BArADOg7Cip8gi0EUN2Wow5qAkoYAAuSQA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(dplyr)
  ADAE <- tmc_ex_adae
  ADSL <- filter(tmc_ex_adsl, USUBJID %in% ADAE$USUBJID)
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
      aeterm = choices_selected(
        choices = variable_choices(ADAE, "AETERM"),
        selected = "AETERM"
      ),
      tox_grade = choices_selected(
        choices = variable_choices(ADAE, "AETOXGR"),
        selected = "AETOXGR"
      ),
      causality = choices_selected(
        choices = variable_choices(ADAE, "AEREL"),
        selected = "AEREL"
      ),
      outcome = choices_selected(
        choices = variable_choices(ADAE, "AEOUT"),
        selected = "AEOUT"
      ),
      action = choices_selected(
        choices = variable_choices(ADAE, "AEACN"),
        selected = "AEACN"
      ),
      time = choices_selected(
        choices = variable_choices(ADAE, "ASTDY"),
        selected = "ASTDY"
      ),
      decod = NULL
    )
  )
)
#> Initializing tm_g_pp_adverse_events
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
