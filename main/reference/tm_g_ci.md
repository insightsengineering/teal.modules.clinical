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
  paramcd = NULL,
  avisit = NULL,
  color,
  stat = c("mean", "median"),
  conf_level = teal.picks::values(c(0.95, 0.9, 0.8), 0.95),
  plot_height = c(700L, 200L, 2000L),
  plot_width = NULL,
  pre_output = NULL,
  post_output = NULL,
  ggplot2_args = teal.widgets::ggplot2_args(),
  transformators = list(),
  decorators = list()
)

# S3 method for class 'data_extract_spec'
tm_g_ci(
  label,
  x_var,
  y_var,
  paramcd = NULL,
  avisit = NULL,
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

# Default S3 method
tm_g_ci(
  label,
  x_var,
  y_var,
  paramcd,
  avisit,
  color,
  stat = c("mean", "median"),
  conf_level = teal.picks::values(c(0.95, 0.9, 0.8), 0.95),
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

- x_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  or legacy `data_extract_spec`)\
  treatment-axis encoding.

- y_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  or legacy `data_extract_spec`)\
  analysis-value encoding.

- paramcd:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- avisit:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  value of analysis visit `AVISIT` of interest.

- color:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  or legacy `data_extract_spec`)\
  grouping variable for colors, shapes, and line types.

- stat:

  (`character`)\
  statistic to plot. Options are `"mean"` and `"median"`.

- conf_level:

  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  is deprecated but still accepted)\
  available confidence levels and default selection, each in the range
  (0, 1).

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

## Methods (by class)

- `tm_g_ci(data_extract_spec)`: Legacy encodings via `data_extract_spec`
  (merge-based UI).

- `tm_g_ci(default)`: teal.picks encodings via `picks` objects for
  `x_var`, `y_var`, and `color` (use `tm_g_ci()` to pass
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  objects; they are wrapped into `picks`).

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxs+ajUrIGlOgCCACIAygAykTqkMAQxcAAeMVD8otQNLe0AQl09fYPD-NT0SgC+QQBWRLkxANZwrKL5yTap-HDGUMKkMQT8tKJ9Wzv7h8DQ8EcFcgC6rtDoLq5TLsBoFHQAXh0BVwDT4QhEokhOnhwjEoIgZTKPRiAHNbrQMVisdQoPQ4NRkQowABhEjGWjncy6ACSKkYmm4OgACv5SNTYZjiTohpzGMjUDJdkcGsLoclRHBSDKwC0OgKdNS1e1qXJBXKymLaGTEewCKDVVgALI05oa6lTK1YADSWAATLqilrrbbdbKsXr-WVWDExRKpTKhXKCorlRbJlN7armtNPUGsUaTejzVqAGqNHV4TW0gASAHFPcXGvnC0E5YGo1i-AFwwRpUSDTGlSrtUne2AGwadJn6Kac2BWk4ABpJ1r2Rq2RoARln88XHoHXona+XfsbZUHwtQLFgd1b7fTZS7ca1KcTRdvqc3l+HLGNo-R1O5jSwjRtdofMBv1-f8033DNuEEdFFREdQ4H4KlVXaWwNRga5aBqXQoQAMQLSc62FQ9iSgTQHkyc9IyHa8ezvPtaOfcDDTfLMe1zFlWhZFDAOrdjOLAodOWoKCjhguA4IQqFqVaGksCcJwADkWXkisizQ6gNEw5FcPafD0wIg8GiCIJaGMHR2Fycg1A0bRrBsEohVEHIIFYRp0HYNBUAAEkEWgig8zzFQ5VQglWJQwFWH4gA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(dplyr)
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_ci(
      label = "Confidence Interval Plot",
      x_var = picks(
        datasets("ADSL", "ADSL"),
        variables(c("ARMCD", "BMRKR2"), "ARMCD")
      ),
      y_var = picks(
        datasets("ADLB", "ADLB"),
        variables(c("AVAL", "CHG"), "AVAL")
      ),
      color = picks(
        datasets("ADSL", "ADSL"),
        variables(c("SEX", "STRATA1", "STRATA2"), "STRATA1")
      ),
      paramcd = picks(
        datasets("ADLB", "ADLB"),
        variables("PARAMCD", "PARAMCD"),
        values(selected = "ALT", multiple = FALSE)
      ),
      avisit = picks(
        datasets("ADLB", "ADLB"),
        variables("AVISIT", "AVISIT"),
        values(selected = "SCREENING", multiple = FALSE)
      )
    )
  )
)
#> Initializing tm_g_ci
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
