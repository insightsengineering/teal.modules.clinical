# teal Module: Forest Survival Plot

This module produces a grid-style forest plot for time-to-event data
with ADaM structure.

## Usage

``` r
tm_g_forest_tte(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  arm_ref_comp = NULL,
  subgroup_var,
  paramcd,
  strata_var,
  aval_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVAL"), "AVAL", fixed = TRUE),
  cnsr_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "CNSR"), "CNSR", fixed = TRUE),
  stats = c("n_tot_events", "n_events", "median", "hr", "ci"),
  riskdiff = NULL,
  conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
    TRUE),
  time_unit_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, "AVALU"),
    "AVALU", fixed = TRUE),
  fixed_symbol_size = TRUE,
  plot_height = c(500L, 200L, 2000L),
  plot_width = c(1500L, 800L, 3000L),
  rel_width_forest = c(25L, 0L, 100L),
  font_size = c(15L, 1L, 30L),
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

- arm_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. It defines the grouping variable
  in the results table.

- arm_ref_comp:

  (`list`) optional,  
  if specified it must be a named list with each element corresponding
  to an arm variable in `ADSL` and the element must be another list
  (possibly with delayed
  [`teal.transform::variable_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/variable_choices.html)
  or delayed
  [`teal.transform::value_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/value_choices.html)
  with the elements named `ref` and `comp` that the defined the default
  reference and comparison arms when the arm variable is changed.

- subgroup_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as the default subgroups.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- strata_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  names of the variables for stratified analysis.

- aval_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  analysis variable.

- cnsr_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  censoring variable.

- stats:

  (`character`)  
  the names of statistics to be reported among:

  - `n_tot_events`: Total number of events per group.

  - `n_events`: Number of events per group.

  - `n_tot`: Total number of observations per group.

  - `n`: Number of observations per group.

  - `median`: Median survival time.

  - `hr`: Hazard ratio.

  - `ci`: Confidence interval of hazard ratio.

  - `pval`: p-value of the effect. Note, one of the statistics `n_tot`
    and `n_tot_events`, as well as both `hr` and `ci` are required.

- riskdiff:

  (`list`)  
  if a risk (proportion) difference column should be added, a list of
  settings to apply within the column. See
  [`tern::control_riskdiff()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_riskdiff.html)
  for details. If `NULL`, no risk difference column will be added.

- conf_level:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  confidence level, each within range of (0, 1).

- time_unit_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the time
  unit variable.

- fixed_symbol_size:

  (`logical`)  
  When (`TRUE`), the same symbol size is used for plotting each
  estimate. Otherwise, the symbol size will be proportional to the
  sample size in each each subgroup.

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

- rel_width_forest:

  (`proportion`)  
  proportion of total width to allocate to the forest plot. Relative
  width of table is then `1 - rel_width_forest`. If `as_list = TRUE`,
  this parameter is ignored.

- font_size:

  (`numeric(1)`)  
  font size.

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

    tm_g_forest_tte(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxsnHDcvALCYhJSMvKl5QyVHMYBMGHkjKJBZRWe+ajUrIFtAIIAIgDKADKROqQwBDFwAB4xUPyi1DOztrZOK2sb27v8pORHSwAkWNMAwmep-IxE43Da1KLsOaPZ5vGwAUgAfGD0pksjFqFB6HBqOwFGAsFACHA0UEAL5BABWRFyMQA1nBWACCjYPnBjFBhKQYgR+LRRBsiSTyZTgNB4FTknIALquCBA5YfZLAYBo8VooUisXHU4rArS2XKlxgBWilgwGKMOnMogwVArOiiUioiBlaZYACyOgAvO1LdaymVDcZnTo0QAhRA6AAKCKx9CIaNwbTKxFNPoIqLA00Ds0YggA5joABqR31gF6Bl4m+i5MKWYJgIY6ORRm06O32l6zH0Wq3RnRen2yh06P2R9uxs0uhPdx3TXOjnQvHFtIJBJRoM2pXKZd06Ao+gq1sp8IQiUQ+3cNAHttYxdMxbqGy0xW5wNce8qI5FdsAAMQCPh080EjE0Wm4fs60fAo+V0F0NROLVt0fHQ9RiTQWHjLJiSxUQYlEZE4HUOB+AfWDEMYWhEREZkUJkMRAQWRYihHJMHQnejG1mHEa3bR9RybNF2OrGDH3gr1jTjF0BKNQc+I9VAWFgFlkNQsQMKwnC8J4spEOoQQ4DI+SATmKCijRIM7WmZjGKM557RxCSOLAAB5eZuOAj02KcspREEeh0y+QRUAQpDh3ItDFJEZT8MfQjiPoUiCECyjxSKMDdOouQXNgj06L9e0sAAaSwAAmRj5icHNKx41LYMtZhwj8xg5Io9DMJC8gVNcj0IpIrSYp0qilloxN5nsaZbGmABGQrBuGgrK3KtK0QG55JsctKqzKKs5yUWhvXYXJ+kxDRtGsGwSjrUQcggVhpnQdhFweQRaCKG7ML-VQ8SUMBcSFIA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(formatters)
  library(dplyr)
  ADSL <- tmc_ex_adsl
  ADTTE <- tmc_ex_adtte
  ADSL$RACE <- droplevels(ADSL$RACE) %>% with_label("Race")
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADTTE <- data[["ADTTE"]]

arm_ref_comp <- list(
  ARM = list(
    ref = "B: Placebo",
    comp = c("A: Drug X", "C: Combination")
  ),
  ARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  )
)

app <- init(
  data = data,
  modules = modules(
    tm_g_forest_tte(
      label = "Forest Survival",
      dataname = "ADTTE",
      arm_var = choices_selected(
        variable_choices(ADSL, c("ARM", "ARMCD")),
        "ARMCD"
      ),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        value_choices(ADTTE, "PARAMCD", "PARAM"),
        "OS"
      ),
      subgroup_var = choices_selected(
        variable_choices(ADSL, names(ADSL)),
        c("BMRKR2", "SEX")
      ),
      strata_var = choices_selected(
        variable_choices(ADSL, c("STRATA1", "STRATA2")),
        "STRATA2"
      )
    )
  )
)
#> Initializing tm_g_forest_tte
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
