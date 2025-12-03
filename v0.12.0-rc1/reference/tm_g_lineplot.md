# teal Module: Line Plot

This module produces a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
type line plot, with optional summary table, for standard ADaM data.

## Usage

``` r
tm_g_lineplot(
  label,
  dataname,
  parentname = NULL,
  strata = lifecycle::deprecated(),
  group_var =
    teal.transform::choices_selected(teal.transform::variable_choices(parentname,
    c("ARM", "ARMCD", "ACTARMCD")), "ARM"),
  x = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVISIT"), "AVISIT", fixed = TRUE),
  y = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    c("AVAL", "BASE", "CHG", "PCHG")), "AVAL"),
  y_unit = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVALU"), "AVALU", fixed = TRUE),
  paramcd = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "PARAMCD"), "PARAMCD", fixed = TRUE),
  param = teal.transform::choices_selected(teal.transform::value_choices(dataname,
    "PARAMCD", "PARAM"), "ALT"),
  conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
    TRUE),
  interval = "mean_ci",
  mid = "mean",
  whiskers = c("mean_ci_lwr", "mean_ci_upr"),
  table = c("n", "mean_sd", "median", "range"),
  mid_type = "pl",
  mid_point_size = c(2, 1, 5),
  table_font_size = c(4, 2, 6),
  plot_height = c(1000L, 200L, 4000L),
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

- strata:

  **\[deprecated\]** Please use the `group_var` argument instead.

- group_var:

  (`string` or `NA`)  
  group variable name.

- x:

  (`teal_module` or `teal_modules`) Object to format/print.

- y:

  (`string`)  
  y-variable name.

- y_unit:

  (`string` or `NA`)  
  y-axis unit variable name.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- param:

  (`character`)  
  parameter to filter the data by.

- conf_level:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  confidence level, each within range of (0, 1).

- interval:

  (`character` or `NULL`)  
  names of the statistics that will be plotted as intervals. All the
  statistics indicated in `interval` variable must be present in the
  object returned by `sfun`, and be of a `double` or `numeric` type
  vector of length two. Set `interval = NULL` if intervals should not be
  added to the plot.

- mid:

  (`character` or `NULL`)  
  names of the statistics that will be plotted as midpoints. All the
  statistics indicated in `mid` variable must be present in the object
  returned by `sfun`, and be of a `double` or `numeric` type vector of
  length one.

- whiskers:

  (`character`)  
  names of the interval whiskers that will be plotted. Names must match
  names of the list element `interval` that will be returned by `sfun`
  (e.g. `mean_ci_lwr` element of `sfun(x)[["mean_ci"]]`). It is possible
  to specify one whisker only, or to suppress all whiskers by setting
  `interval = NULL`.

- table:

  (`character` or `NULL`)  
  names of the statistics that will be displayed in the table below the
  plot. All the statistics indicated in `table` variable must be present
  in the object returned by `sfun`.

- mid_type:

  (`string`)  
  controls the type of the `mid` plot, it can be point (`"p"`), line
  (`"l"`), or point and line (`"pl"`).

- mid_point_size:

  (`numeric(1)`)  
  font size of the `mid` plot points.

- table_font_size:

  (`numeric(1)`)  
  font size of the text in the table.

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
  with settings for the module plot. For this module, this argument will
  only accept `ggplot2_args` object with `labs` list of following child
  elements: `title`, `subtitle`, `caption`, `y`, `lty`. No other
  elements would be taken into account. The argument is merged with
  option `teal.ggplot2_args` and with default module arguments (hard
  coded in the module body).

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

    tm_g_lineplot(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxsnHDcvALCYhJSMvKl5QyVHPyo1KyBbRWe7MYBsqSiQWUAggAiAMoAMpE6pDAEMXAAHjFQ-KLUbbMLAELLq+tbO-zU9DoApAB8d21lMILh5OxTAGoAknO-Ww6AC8wJM6hijDgAX4qi+fwBtiKP3+gIAckUYLk5EEAL5BABWRFyMQA1nBWKJ8skbKlYcYoMJSDECPxaKJ1kSSeTKcBoPAqQU5ABdVwQWaLZYFYDABRgCULOXC0XimYnKXJGVyo7HJUqpRoVDLXKZdhtAognQFXBtPhCESiS12hpUl4rGAxADmMSkcB6RFIZogZRD5Sg9Dg1EtcoWuV0AAV-KQ5Tbg6Grcl+bowdq1bq8G6yp7GERBKgYpoWJaCFliQQxDFRJG4Oo4Pwg+mQ5XGLRwyIWbWZGIvvMFkUCGb5VgALIpnTamcAYRmc+1i9sUyXK7AONTnbKC9nYELOjke-TrGrg-rokbzdb7ZPZW7vfo-ZrdeHOvHk5+U0VeDzmAxxTHMLiAXKi4ABIAOKrmA8bQXBO5nk+QF-gBJ6oWmoaoCwsBXp+t5NiID4dvulbUIIcADkRI4nEUcrxpuUzTsu8HMVgrFyth+7oQsthylhbqTKeShBLQxg6OwuTkGoGjaNYNglGmog5BArBTOg7CGgAJIItBFHpTaMNo-QQLiShgLiwpAA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(dplyr)
  library(forcats)
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb %>%
    mutate(AVISIT == fct_reorder(AVISIT, AVISITN, min))
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADLB <- data[["ADLB"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_lineplot(
      label = "Line Plot",
      dataname = "ADLB",
      group_var = choices_selected(
        variable_choices(ADSL, c("ARM", "ARMCD", "ACTARMCD")),
        "ARM"
      ),
      y = choices_selected(
        variable_choices(ADLB, c("AVAL", "BASE", "CHG", "PCHG")),
        "AVAL"
      ),
      param = choices_selected(
        value_choices(ADLB, "PARAMCD", "PARAM"),
        "ALT"
      )
    )
  )
)
#> Initializing tm_g_lineplot
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
