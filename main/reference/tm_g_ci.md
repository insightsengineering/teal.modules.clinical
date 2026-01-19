# teal Module: Confidence Interval Plot

This module produces a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
type confidence interval plot consistent with the TLG Catalog template
`CIG01` available
[here](https://insightsengineering.github.io/tlg-catalog/stable/graphs/other/cig01.html).

## Usage

``` r
tm_g_ci(
  label,
  x_var,
  y_var,
  color,
  stat = c("mean", "median"),
  conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
    TRUE),
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

- x_var:

  (`character`)  
  name of the treatment variable to put on the x-axis.

- y_var:

  (`character`)  
  name of the response variable to put on the y-axis.

- color:

  (`data_extract_spec`)  
  the group variable used to determine the plot colors, shapes, and line
  types.

- stat:

  (`character`)  
  statistic to plot. Options are `"mean"` and `"median"`.

- conf_level:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  confidence level, each within range of (0, 1).

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
place using decorators:

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_ci(
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

## See also

The [TLG
Catalog](https://insightsengineering.github.io/tlg-catalog/stable/)
where additional example apps implementing this module can be found.

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxsnHDcvALCYhJSMvKl5QyVHPyo1KyBbQCCACIAygAykTqkMAQxcAAeMVD8otSDQ2MAQpPTswtL-NT0SgC+QQBWRLkxANZwrKL5yTap-HDGUMKkMQT8tKKzS7XO4PYDQeCPApyAC6rggw3GkwKwGACjACLGaOhsPhG22r2SKLRwy2WJxSjQqEmuUy7DaBR0AF4dAVcG0+EIRKImToOQ1Hm0ytMYgBzH60OkQMrS8pQehwag8tEAYRIxlob3MugAkipGJpuDoAAr+UhotlSmU6RYGxg8gpzeakNTfUSoOAESVW6UFcG6ZnE0aYvCCq2iBUe0g88MidQxN0er3e6UELJXAhiHme4lYACyyqG5p0aM2uawAGksAAmNFyC3J6UxyNwfhZunovMF2v1hu8r60Hr+nQAMQGYxGTh7DfV8xbPNH45clu9QRXU+lrBitvtyUdzqgcYT2eXVt9sCHgdJIZPMvV1HIduZdFEpCT09o99U8fdx970tt3IBmARoDFgAz5oW15-mUqbppmT5wNo1CPCSmwACQgWBEF1qGDZNuoc4IUhKF4hhoHgQWcjAAAjNC64NjA-aDvOY4TvRybUHKCpKmAIwRuosr0IgaK4VaOE3lad4Pt+iaid6AE8QMABq2ojNqtjmnJVqwTI8HlIhCokVsaHKap6nidBOj4eQrZEYZ7CoSZKlqbYVG0ex3qMfeA4iCxi4eVanHyoqQF8bGUaaP8mTCWAWllKuyYWcm1nRvxro-m+HFcSFxbotAvQAF5zkp3CCHAmkSTKOkZoBOjZuiSljkWKoABIAOLdnFVlpYRuXKU1UG9l5GjMcyC5sV1M69eNS4NglMpJVVRD+I+LK7gs+6HhlWlnvAilBhVeFpal4Uyb+vZBdxQGqitOj0KwOi2rQcoiIdvbVXp9UTgAGs1vH2AMtgDNRf0jADQM1mAi3JT1tl1e2YNgUDINQwFZTDT5Q4zWjJi0LOcMzVp83SsTCVBEEtDGDo7C5A+B4aNo1g2CUlqiDkECsAM6DsJSaGCLQRS8+G+qqEEJxKGAJzQkAA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(dplyr)
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADLB <- data[["ADLB"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_ci(
      label = "Confidence Interval Plot",
      x_var = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = c("ARMCD", "BMRKR2"),
          selected = c("ARMCD"),
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      y_var = data_extract_spec(
        dataname = "ADLB",
        filter = list(
          filter_spec(
            vars = "PARAMCD",
            choices = levels(ADLB$PARAMCD),
            selected = levels(ADLB$PARAMCD)[1],
            multiple = FALSE,
            label = "Select lab:"
          ),
          filter_spec(
            vars = "AVISIT",
            choices = levels(ADLB$AVISIT),
            selected = levels(ADLB$AVISIT)[1],
            multiple = FALSE,
            label = "Select visit:"
          )
        ),
        select = select_spec(
          label = "Analyzed Value",
          choices = c("AVAL", "CHG"),
          selected = "AVAL",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      color = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Color by variable",
          choices = c("SEX", "STRATA1", "STRATA2"),
          selected = c("STRATA1"),
          multiple = FALSE,
          fixed = FALSE
        )
      )
    )
  )
)
#> Initializing tm_g_ci
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
