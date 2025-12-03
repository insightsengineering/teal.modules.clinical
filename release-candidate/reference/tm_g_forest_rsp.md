# teal Module: Forest Response Plot

This module produces a grid-style forest plot for response data with
ADaM structure.

## Usage

``` r
tm_g_forest_rsp(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  arm_ref_comp = NULL,
  paramcd,
  aval_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVALC"), "AVALC", fixed = TRUE),
  subgroup_var,
  strata_var,
  stats = c("n_tot", "n", "n_rsp", "prop", "or", "ci"),
  riskdiff = NULL,
  fixed_symbol_size = TRUE,
  conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
    TRUE),
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

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- aval_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  analysis variable.

- subgroup_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as the default subgroups.

- strata_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  names of the variables for stratified analysis.

- stats:

  (`character`)  
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

  (`list`)  
  if a risk (proportion) difference column should be added, a list of
  settings to apply within the column. See
  [`tern::control_riskdiff()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_riskdiff.html)
  for details. If `NULL`, no risk difference column will be added.

- fixed_symbol_size:

  (`logical`)  
  When (`TRUE`), the same symbol size is used for plotting each
  estimate. Otherwise, the symbol size will be proportional to the
  sample size in each each subgroup.

- conf_level:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  confidence level, each within range of (0, 1).

- default_responses:

  (`list` or `character`)  
  defines the default codes for the response variable in the module per
  value of `paramcd`. A passed vector is transmitted for all `paramcd`
  values. A passed `list` must be named and contain arrays, each name
  corresponding to a single value of `paramcd`. Each array may contain
  default response values or named arrays `rsp` of default selected
  response values and `levels` of default level choices.

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo67sxsnHDcvALCYhJSMvKl5QyVHMYBMGHkjKJBZRWe+ajUrIFtAIIAIgDKADKROqQwBDFwAB4xUPyi1DOzWPMraxvbu-wDOgCkAHy3bWUwguHk7NMAatOLAMI6AC8On4MRIxBiA1QMWoUHocGonx+-xsDyeEDKmPSmSyMLhCPYCjAfyyLCg6lUOiwYmEpAA9AAxXL8XIAcyJclRj2eJlo1H67AACtMsNMALJ-WY6ACEwKJAHkvlgAJIAOS+RJ0AB8dN9lfNlbYgXKwAz5YtFvKAOo6ByCjlKAC+QQAVkRcjEANZwViifLJGypfhwYxQWkxAgs0QbN0e72+4DQeB+gpyAC6rggcyWKwKwGAROziyJaYzWeOpyDyXzhYrJbLShYMEhIYjRBgqBWdFEpEJGN1WDFQPaPb7WMYIeHRIAQogdILYQQ4PQiETcDziB3hwRCWBpnPZoxBKydAANNc6Il-Od-dv0XJhSzBMBDHRydf9kUSqXA7u9nkTsYU57oOOjTmuG7tp2wI7oWoHTBecFDn8Dr9kEQSNugKy5JkY4gskw4FB+Lz1CIojDnwQhkXhZRrDErIxN0E49pCoioDRmKwvC1DAQyAQ+FSYioCQohwBB-ZYgUSa6CacwnOJWJlE2MSaCw25ZO6S6iDEokiBS-AcViqmMLQcIiBGGkyGInwLIsRSwSBYqIY5kocu+PJYkhrlgB5b7EYpymAW2W7AoFrabqg-lYqgZLrPw6maWIOkInA+mGZiqnUIIcAWYlfpyfMRREsKorfs5JXihyUWKUSapfE4thEr57kSZioiCPQrKMEQgjQsZCVWdpumpeQBm+WUxmmfQ5kEJZWk2UsRTSfltmctVWIOdOYpYAA0lgABMznzE454vs1606D2zDhCpakwXNSXDWl406JNZk5bNeULXZOgOfM9jTLY0wAIxHQDQOHS+LWKZiRL-aKENNa1ZTQ4p4xEKQMRZHAtCslkpDbuwABsAAMJM-ftZMU2T5Oo5JIZhvyLZsSJYjDn+6VlNOTjzCcgrKuztCji9ZRQoTcPhNNuizELNSiTo7DzLMih4JeYCqhjOhOJlgjvQrqpOCrdMw+UcDaNQ5EwZzNXElBIjkIJLMQPL7B-FgKtFWAgosBo3CO8Jzu6EK7tHZLIg6DLolQC7SseyLsNe91XViKIWjS7L0dB4KyvORrBPa9wutS-rhtIybmKvjDxtYnVDWC8LyOKWLVtXnbcAO9STsu27Htq97jC+zxncBy7gohy+F1YiI5uW791uebbHb27ow+swrPe55rBdZXr7AG73xU+6ZQ9CWvwdx43MPFUnzGp9oEcZ6POeqxLeuR3LQex2X5dvi91eYoqFU6p67-kvpiZuc9r5EGTqIO+6co5PwPmAeYYd4EfwVl-Ce8dp4Ilng5QUN8U5pwfggrOz9PYoLfo-T+5C1Z5y1jrXe+9ULl0rhXHkr50JKFoEBdguR+jkg0NoawNgSj9lEDkCArBpjoHYGgVAAASQQtAijyIUaJRg2gpgQEdEoMAjo0xAA)

## Examples

``` r
library(nestcolor)

data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(formatters)
  library(dplyr)
  ADSL <- tmc_ex_adsl
  ADRS <- tmc_ex_adrs %>%
    mutate(AVALC = d_onco_rsp_label(AVALC) %>%
      with_label("Character Result/Finding")) %>%
    filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADRS <- data[["ADRS"]]

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
      arm_var = choices_selected(
        variable_choices(ADSL, c("ARM", "ARMCD")),
        "ARMCD"
      ),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        value_choices(ADRS, "PARAMCD", "PARAM"),
        "INVET"
      ),
      subgroup_var = choices_selected(
        variable_choices(ADSL, names(ADSL)),
        c("BMRKR2", "SEX")
      ),
      strata_var = choices_selected(
        variable_choices(ADSL, c("STRATA1", "STRATA2")),
        "STRATA2"
      ),
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
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
