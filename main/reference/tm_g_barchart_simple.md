# teal Module: Simple Bar Chart and Table of Counts per Category

This module produces a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
type bar chart and summary table of counts per category.

## Usage

``` r
tm_g_barchart_simple(
  x = NULL,
  fill = NULL,
  x_facet = NULL,
  y_facet = NULL,
  label = "Count Barchart",
  plot_options = NULL,
  plot_height = c(600L, 200L, 2000L),
  plot_width = NULL,
  pre_output = NULL,
  post_output = NULL,
  ggplot2_args = teal.widgets::ggplot2_args(),
  transformators = list(),
  decorators = list()
)
```

## Arguments

- x:

  (`data_extract_spec`)  
  variable on the x-axis.

- fill:

  (`data_extract_spec`)  
  grouping variable to determine bar colors.

- x_facet:

  (`data_extract_spec`)  
  row-wise faceting groups.

- y_facet:

  (`data_extract_spec`)  
  column-wise faceting groups.

- label:

  (`character`)  
  menu item label of the module in the teal app.

- plot_options:

  (`list`)  
  list of plot options.

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

Categories can be defined up to four levels deep and are defined through
the `x`, `fill`, `x_facet`, and `y_facet` parameters. Any parameters set
to `NULL` (default) are ignored.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_barchart_simple(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxsnHDcvALCYhJSMvKl5QyVHMYBMGHkjKJBZRWe+ajUrIFtAIIAIgDKADKROqQwBDFwAB4xUPyi1DoApAB8R21lMILh5OwAkra2AGLLALwmUOoB7ApgAJq-Gync4QMpg9KZLIxahQehwag-MB3FRkAy2Ihoxg1Ug6AAKRFQwjCllBTxhAHNAUMdHNpk4VmsNttdqFdMCLiZaNR+uwAITsdh02wAeQAGgBxLA6V7vACMNgAZDpBU55k4AGrS96-ACyd0Ws0BOiVgqwOq1Ol+00QOlmjEE5J0oqpQQAvkEAFZEXIxADWcFYonyyRsqX4cGMUGEpBiBH4tFEGy9Pv9geA0HgQYKcgAuq4IHMlisCsBgFaFotfjm8wXZnTi8lS+W6VWa0o0KgVrlMj9QToCtL+8lcG0+EIRKJB2OGkGOWsYuSYvQWAQsiwY6JaDBxnBe+DhrD4YPm-TptAJpvRL8R33wVtBwVNltSGoN6g4AQ9-uygUM7ptWAhaVngHLgqI8Ifji7zgSI6gxKI76fqB+6rt6BBiIOmgsLQsIiLGWRoWIX7fuCQE3iRKHERRYJWma16WoBADCtjTHReAMWqzogbe1FlL8WDTIxLjsb8DzPMBRS-PM0xPC89FSfY0wsQATL8yHftSFFyOR1EwZBcD8MeTEsWxOkUVc3K0Dug5PNMixqupOiaWC2nIcYXKHO8dCiKQVHgo+2wvp8b4fn5+6-rA-4MUB16OWUenqIOCUhUhPEUahMgYe8WGMDh9B4Rl6Gzml1FkXFlHld+tE6vJxmsTVIlgJxsUlbx-GCcJklIo8cmNdJskSRxikqWprUkc5WlmbpEHqAZRnNdxvGXNGVkiDZdkOWNZQTS5U3+ckT5BXBCGheVEXwEZtKdeVyVJTNKVhd+hVZToOV5QVBGZUGV1FEhgFOCKEpYLVdJquqIOqk4wNgHIrlbWCyVze8AByDiLIse0kRZGjWe8tn2S48M7dtyFw9+OyRuhUHtD5j1DuEh2vvBiF0z+yR-pddbXfDt3QfdzOnfDZTPZO2XYbhcD4YR31c79iJCmKkoQ2DytQ1SmPfojhkAQrQMtUtOjY6tUX45tvHE05Gv01AjPBQLqW8edUXlks+vTbB1PJfbrNgiLmHi-lksiz7pEVlbT0h-u1Ug8x9W1Qt4cke1Qm1WJvVdf16dDQJI1gJVLn52TS1a4OqPo4ny2WbjOim4T5uOTtRfgqwMSU3A1Peb5jkBc+TMnQ71FO5zLaLe7kF3R73v537Yu5RLUtfYKss6H9utK41oMaqr0OwxXOglzrkPg6PvFG9XtflRbTfhQdgV9yzZ3s5FnOuyfJG8-v-P95HM+vQHH3S0jmUMqQtfZAJooBUy0VY5QKkk4Lie9wTJ06gxNOg0pIySzgpHO0xVJ51ASTUB18KIHx0GXDG+cz5rTxhtOu1Er6kw5NSIIQRaDGGVLkfowUtC7lhsUNoogcgQFYNMdA7AOwABJBC0CKJI8CjBtBTAgK6JQYBXQ5iAA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(formatters)
  library(dplyr)
  ADSL <- tmc_ex_adsl %>%
    mutate(ITTFL = factor("Y") %>%
      with_label("Intent-To-Treat Population Flag"))
  ADAE <- tmc_ex_adae %>%
    filter(!((AETOXGR == 1) & (AESEV == "MILD") & (ARM == "A: Drug X")))
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_barchart_simple(
      label = "ADAE Analysis",
      x = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(
            ADSL,
            c(
              "ARM", "ACTARM", "SEX",
              "RACE", "ITTFL", "SAFFL", "STRATA2"
            )
          ),
          selected = "ACTARM",
          multiple = FALSE
        )
      ),
      fill = list(
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices(
              ADSL,
              c(
                "ARM", "ACTARM", "SEX",
                "RACE", "ITTFL", "SAFFL", "STRATA2"
              )
            ),
            selected = "SEX",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADAE",
          select = select_spec(
            choices = variable_choices(ADAE, c("AETOXGR", "AESEV", "AESER")),
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      x_facet = list(
        data_extract_spec(
          dataname = "ADAE",
          select = select_spec(
            choices = variable_choices(ADAE, c("AETOXGR", "AESEV", "AESER")),
            selected = "AETOXGR",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices(
              ADSL,
              c(
                "ARM", "ACTARM", "SEX",
                "RACE", "ITTFL", "SAFFL", "STRATA2"
              )
            ),
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      y_facet = list(
        data_extract_spec(
          dataname = "ADAE",
          select = select_spec(
            choices = variable_choices(ADAE, c("AETOXGR", "AESEV", "AESER")),
            selected = "AESEV",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices(
              ADSL,
              c(
                "ARM", "ACTARM", "SEX",
                "RACE", "ITTFL", "SAFFL", "STRATA2"
              )
            ),
            selected = NULL,
            multiple = FALSE
          )
        )
      )
    )
  )
)
#> Initializing tm_g_barchart_simple
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
