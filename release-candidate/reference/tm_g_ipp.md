# teal Module: Individual Patient Plots

This module produces
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
type individual patient plots that display trends in parameter values
over time for each patient, using data with ADaM structure.

## Usage

``` r
tm_g_ipp(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  paramcd,
  id_var = teal.picks::variables("USUBJID", "USUBJID", fixed = TRUE),
  visit_var = teal.picks::variables("AVISIT", "AVISIT", fixed = TRUE),
  aval_var = teal.picks::variables("AVAL", "AVAL", fixed = TRUE),
  avalu_var = teal.picks::variables("AVALU", "AVALU", fixed = TRUE),
  baseline_var = teal.picks::variables("BASE", "BASE", fixed = TRUE),
  add_baseline_hline = FALSE,
  separate_by_obs = FALSE,
  suppress_legend = FALSE,
  add_avalu = TRUE,
  plot_height = c(1200L, 400L, 5000L),
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

- arm_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  or legacy `choices_selected`)\
  **Required** (no default, as on `main`). Arm encoding; resolved on
  `parentname` (typically `ADSL`). Legacy inputs are coerced with a
  deprecation warning.

- paramcd:

  ([`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy `choices_selected` from
  [`teal.transform::value_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/value_choices.html))\
  **Required** (no default, as on `main`). Parameter code filter: a
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  with
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  (typically `PARAMCD`) and
  [`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  for levels; legacy `choices_selected` inputs are coerced with a
  deprecation warning.

- id_var, :

  visit_var, aval_var, avalu_var, baseline_var
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html),
  or legacy `choices_selected`)\
  encodings on `dataname`. Legacy inputs are coerced with a deprecation
  warning.

- visit_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as `visit` variable. Must be a factor in
  `dataname`.

- aval_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and pre-selected option for the
  analysis variable.

- avalu_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  analysis unit variable.

- baseline_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  values that can be used as `baseline_var`.

- add_baseline_hline:

  (`logical`)\
  whether a horizontal line should be added to the plot at baseline
  y-value.

- separate_by_obs:

  (`logical`)\
  whether to create multi-panel plots.

- suppress_legend:

  (`logical`)\
  whether to suppress the plot legend.

- add_avalu:

  (`logical`)\
  whether `avalu_first` text should be appended to the plot title and
  y-axis label.

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
  with settings for the module plot. For this module, this argument will
  only accept `ggplot2_args` object with `labs` list of the following
  child elements: `title`, `subtitle`, `x`, `y`. No other elements are
  taken into account. The argument is merged with option
  `teal.ggplot2_args` and with default module arguments (hard coded in
  the module body).

  For more details, see the vignette:
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

    tm_g_ipp(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxsnHDcvALCYhJSMvKl5QyVHOSMEEFlFZ75qNSsgW0AggAiAMoAMpE6pDAEMXAAHjFQ-KLUOgCkAHx7bWU7MnDsAIyIAEwADDaHxxBlZfzGq2vDMpkx0IkvHRTWYAIQWSxW602-Go9H2RxOJlo1G67Ac0wcIIAUgBJSb7XJ7IEzWYAEnRmNxk0eCMBbw+62+BF+-xpz1eSJRqnY4wAajjpjjbDoAIQAXh0CjA0wAwlgnE4AHI4xUAcSlQQAvkEAFZEXIxADWcFYonyyRsqX4cGMUGEpBiBH4tFEKz1BuNpuA0HgZoKcgAuq4IFM5gsCsBgFLQ7MpQGgyHJqDw8lI9GkyC4wmlGhUAtcpl2G0CjoJQVcG0+EIRKJSzoqw0zYiljEAOYxWjoIt017UKD0OC7CVSnEQZ2aWhCbg6AAKYVoFFIs-8pClFZ7b2SPt0w7AwMzeERZVQLEX27r6bma6POhYMBimhYdcfjFo-Zr7AIRb3WAAsjLJjXSUf1-DUimjP8ALAm8T2YZZ+DrVAZENJsN1eApRDgUgmz3DMgPTUFoLQsoXzfegPylGdxiwcZ-0AvBgKomi6KIjkOUfahBDEdhMJEdQ4AQ3dxlmWwgJge1OxEOsADFhOmJw+jYuR1zYnRJwfJ8JVI99uKlClsTxfCwH0qlWLYidRF+F9nxYMiKL3flBVEhjo0coUzI5KAOI0xgbNfHSzS-VzhKMmUAAl1TAZTgL5EKopUtivO4QQfL8uzdIc4SHCM2LZmy+Kb3oKBeNyOBUq02yAu-EFxnkoyarqgq0K2fgYiKkrvBiLIpB3HRZNmeSEo5TDYLCMr6FYGIiHoWsJX6uqe0UoIgloYwdHYXJuigCxtGsGwSkBUQcggVhxi7XNSUEWgiguzDGG0MYIE1JQwE1AMgA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(tern)
  library(dplyr)
  ADSL <- tmc_ex_adsl %>%
    slice(1:20) %>%
    df_explicit_na()
  ADLB <- tmc_ex_adlb %>%
    filter(USUBJID %in% ADSL$USUBJID) %>%
    df_explicit_na() %>%
    filter(AVISIT != "SCREENING")
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADLB <- data[["ADLB"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_ipp(
      label = "Individual Patient Plot",
      dataname = "ADLB",
      parentname = "ADSL",
      arm_var = variables(c("ARMCD", "ARM"), "ARMCD"),
      paramcd = picks(
        datasets("ADLB", "ADLB"),
        variables("PARAMCD", "PARAMCD"),
        values(selected = "ALT", multiple = FALSE)
      ),
      id_var = variables("USUBJID", "USUBJID"),
      visit_var = variables("AVISIT", "AVISIT"),
      aval_var = variables(c("AVAL", "CHG"), "AVAL"),
      avalu_var = variables("AVALU", "AVALU"),
      baseline_var = variables("BASE", "BASE"),
      add_baseline_hline = FALSE,
      separate_by_obs = FALSE
    )
  )
)
#> Initializing tm_g_ipp
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
