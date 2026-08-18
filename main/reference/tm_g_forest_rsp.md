# teal Module: Forest Response Plot

This module produces a grid-style forest plot for response data with
ADaM structure.

## Usage

``` r
tm_g_forest_rsp(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  arm_ref_comp = NULL,
  paramcd,
  aval_var = teal.picks::variables("AVALC", "AVALC", fixed = TRUE),
  subgroup_var,
  strata_var,
  stats = c("n_tot", "n", "n_rsp", "prop", "or", "ci"),
  riskdiff = NULL,
  fixed_symbol_size = TRUE,
  conf_level = teal.picks::values(c(0.95, 0.9, 0.8), 0.95),
  default_responses = c("CR", "PR", "Y", "Complete Response (CR)",
    "Partial Response (PR)"),
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

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. It defines the grouping variable
  in the results table.

- arm_ref_comp:

  (`list`) optional,\
  if specified it must be a named list with each element corresponding
  to an arm variable in `ADSL` and the element must be another list
  (possibly with delayed
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::variable_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/variable_choices.html)
  and
  [`teal.transform::value_choices()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/value_choices.html)
  are deprecated but still accepted) with the elements named `ref` and
  `comp` that define the default reference and comparison arms when the
  arm variable is changed.

- paramcd:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- aval_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and pre-selected option for the
  analysis variable.

- subgroup_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for variable
  names that can be used as the default subgroups.

- strata_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  names of the variables for stratified analysis.

- stats:

  (`character`)\
  the names of statistics to be reported among:

  - `n`: Total number of observations per group.

  - `n_rsp`: Number of responders per group.

  - `prop`: Proportion of responders.

  - `n_tot`: Total number of observations.

  - `or`: Odds ratio.

  - `ci` : Confidence interval of odds ratio.

  - `pval`: p-value of the effect. Note, the statistics `n_tot`, `or`,
    and `ci` are required.

- riskdiff:

  (`list`)\
  if a risk (proportion) difference column should be added, a list of
  settings to apply within the column. See
  [`tern::control_riskdiff()`](https://rdrr.io/pkg/tern/man/control_riskdiff.html)
  for details. If `NULL`, no risk difference column will be added.

- fixed_symbol_size:

  (`logical`)\
  When (`TRUE`), the same symbol size is used for plotting each
  estimate. Otherwise, the symbol size will be proportional to the
  sample size in each each subgroup.

- conf_level:

  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  is deprecated but still accepted)\
  available confidence levels and default selection, each in the range
  (0, 1).

- default_responses:

  (`list` or `character`)\
  defines the default codes for the response variable in the module per
  value of `paramcd`. A passed vector is transmitted for all `paramcd`
  values. A passed `list` must be named and contain arrays, each name
  corresponding to a single value of `paramcd`. Each array may contain
  default response values or named arrays `rsp` of default selected
  response values and `levels` of default level choices.

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

- rel_width_forest:

  (`proportion`)\
  proportion of total width to allocate to the forest plot. Relative
  width of table is then `1 - rel_width_forest`. If `as_list = TRUE`,
  this parameter is ignored.

- font_size:

  (`numeric(1)`)\
  font size.

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
  only accept `ggplot2_args` object with `labs` list of following child
  elements: `title`, `caption`. No other elements would be taken into
  account. The argument is merged with option `teal.ggplot2_args` and
  with default module arguments (hard coded in the module body).

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

    tm_g_forest_rsp(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxs7MYBMGHkjKJBZRWe+ajUrIGlOgCCACIAygAykTrRvALCYhJSMtyIiKQwBDFwAB4xUPyi1D0DWINjE3xCIuKSufPUi8urG1v8jToApAB8Lz1lMILh5Oy9ABqvWGAGEdABeHT8GIkYgxRqoGLUKD0ODUAHAsE2d6fCBlAnpTJZZGo9HsBRgUFZFhQdSqHRYMTCUgAegAYrl+LkAOaUuQ4j5fEy0agNdgABV6WF6AFlQf0dABCKGUgDygKwAEkAHKAyk6AA+fUBWsGWtskNVYHZauGwzVAHUdA4JfylABfIIAKyIuRiAGs4KxRPlkjZUvw4MYoCyYgRuaJVr7-UGQ8BoPBQwU5ABdVzQRgwBHR+NEGCoMZ0USkCn4vpYWWQ8q0Gt1wmMaPNykAIUQOglKIIcHoREpuGFxArzYIFLAvX7-UYgh5OgAGuOdJTQf3QeX6LkwpZgmBmjo5BP69L5YqodXa8LO8Zu-PGzoe+PJ+XK1DZ5Tr30m7-m+oLuvWQRBEoaCVqkVwPvWBTNgUl7fFM5zNqc0yhsKywxDyMQ1J2NYIqIqDtoS5RktQL7sgEPiMmIqAkKIcCfvWhIFJmujWgcgxsRROgsMWmgsM2ImMLQqLnOwf6vrKQFyQq-JFMBN7KcKZRCSWxhltOUJaU+umoChFGoLSKz8M2qAyAG2HsRR4mSfQ0mUlKMpqXgW5gG5cpKaeJkCSJ1CCGI7AsSI9KWdauqAk4tibj8Yq0B03E6OyIKDE4F4aQSBBZHABABnEyQsaQzbpcMmU5dl9llKIgj0DyjBEIISLiWJLBOdJsk9rKWAANJYAATApmUbv5Xm9QNw0JSyyUiM29jODVAk1sw4QxO1UKOVJoWyYM9i9LYvQAIyjYdx0jRNlIHTKl2zUlKWLY4WUBQSHREKQMT5bQPJZGVv7sAAbAADCDwxFENYMQzoUPQytFFRjGcZEUxEAsaIzb3uRAk9k4gyHBKWpY628ECR2pEznOgzhM5uj9K2cBQCxOjsIM-SKJ5lI6p9OhOEFgi7azOpZeptUUSI2jUJjgM5QJ27fiI5AMaRzG6OwoJYJzKneSwGjcCraMs5KWujbTC0MyxzPq+z2tyxRrnNU1YiiFo9OM9brMShzCk82V-PcILdPC6LYD22UZ4CQjAkxXFJNtuHOiIlTCsVkruhMqr6Pq5r2teRKeuSdRmdG+rEqm-5ieS+iMs6H+4vy1SitwMrJdq6zue+7zAfBUL7Ai3nrmFwbbfZ17FdveT+dO0RrvaDoltM8b3uD2ANNC4vntsz7YcNwSkcUdHFEatqerx2TU9J5TsveTPLtuwvHvLzvOvr8Hm-G7bYuX+UcBS7XskJR31EHPd2Vtn6rzfhbJ+NsX5eT9nzAWfcB5gSngfCOwozwQSULQZ87BcgNDpBobQ1gbAlHrKIHIEBWC9HQOwaCAASQQtAiiMJYowbQ3QIAeiUGAD0uYgA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(formatters)
  library(dplyr)
  ADSL <- teal.modules.clinical::tmc_ex_adsl
  ADRS <- teal.modules.clinical::tmc_ex_adrs %>%
    mutate(AVALC = d_onco_rsp_label(AVALC) %>%
      with_label("Character Result/Finding")) %>%
    filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

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
    tm_g_forest_rsp(
      label = "Forest Response",
      dataname = "ADRS",
      arm_var = variables(c("ARM", "ARMCD"), "ARMCD"),
      arm_ref_comp = arm_ref_comp,
      paramcd = picks(
        variables("PARAMCD", "PARAMCD"),
        values(selected = "INVET", multiple = FALSE),
        check_dataset = FALSE
      ),
      subgroup_var = variables(c("BMRKR2", "SEX"), "BMRKR2", multiple = TRUE),
      strata_var = variables(c("STRATA1", "STRATA2"), "STRATA2", multiple = TRUE),
      plot_height = c(600L, 200L, 2000L),
      default_responses = list(
        BESRSPI = list(
          rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
          levels = c(
            "Complete Response (CR)", "Partial Response (PR)", "Stable Disease (SD)",
            "Progressive Disease (PD)", "Not Evaluable (NE)"
          )
        ),
        INVET = list(
          rsp = c("Complete Response (CR)", "Partial Response (PR)"),
          levels = c(
            "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
            "Progressive Disease (PD)", "Stable Disease (SD)"
          )
        ),
        OVRINV = list(
          rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
          levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
        )
      )
    )
  )
)
#> Initializing tm_g_forest_rsp
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
