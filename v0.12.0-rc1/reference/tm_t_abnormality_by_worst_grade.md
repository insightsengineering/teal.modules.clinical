# teal Module: Laboratory test results with highest grade post-baseline

This module produces a table to summarize laboratory test results with
highest grade post-baseline

## Usage

``` r
tm_t_abnormality_by_worst_grade(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  id_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    subset = "USUBJID"), selected = "USUBJID", fixed = TRUE),
  paramcd,
  atoxgr_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, subset =
    "ATOXGR"), selected = "ATOXGR", fixed = TRUE),
  worst_high_flag_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, subset =
    "WGRHIFL"), selected = "WGRHIFL", fixed = TRUE),
  worst_low_flag_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, subset =
    "WGRLOFL"), selected = "WGRLOFL", fixed = TRUE),
  worst_flag_indicator = teal.transform::choices_selected("Y"),
  add_total = TRUE,
  total_label = default_total_label(),
  drop_arm_levels = TRUE,
  pre_output = NULL,
  post_output = NULL,
  basic_table_args = teal.widgets::basic_table_args(),
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

- id_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object specifying the variable name for subject id.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- atoxgr_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as Analysis Toxicity Grade.

- worst_high_flag_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as Worst High Grade flag.

- worst_low_flag_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for variable
  names that can be used as Worst Low Grade flag.

- worst_flag_indicator:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  value indicating worst grade.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- drop_arm_levels:

  (`logical`)  
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

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

- basic_table_args:

  (`basic_table_args`) optional  
  object created by
  [`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
  with settings for the module table. The argument is merged with option
  `teal.basic_table_args` and with default module arguments (hard coded
  in the module body). For more details, see the vignette:
  [`vignette("custom-basic-table-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-basic-table-arguments.html).

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

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_abnormality_by_worst_grade(
       ..., # arguments for module
       decorators = list(
         table = teal_transform_module(...) # applied only to `table` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEEr8UKRQ+kZc1AD6waHWQSFhhjoA7rSkABa0EOyxULg6IEo6OgCCACIAygAy4TqkMARRcAAeUVD8otTFZeXVAEJ1DU2t7fzU9DoApAB8Uz0lxrTU5IzsAISlAGoAkpU7ttPZUzoE7ApglQDCWE5OAHI79wDiFwUX-aWVTtVPLmByAIAXwCACsiNkogBrOCsUS5RI2ZL8ODGKDCUhRAj8WiiJrgyEwuHAaDweF5OQAXVcEAqNTqeWAwAudOqF0p1NpfUGyMSTJZ3PZnKUaFQdWy6XOEBKeR0AF4dHlcD0+EIRKJ5TpVcIxFKSiUGlFMVB6BAiIwYNx0qwovQbSlzaJMQBzZgovX6krUE1waiai7VE3mkLm1h2MSkHRYMQYjVpTI6AAStGdGQjOmebt0AAUiE6DP0oKJfdk4G8Fvq8qTdAqBQNy9LPToWDAopoWJqCBkIQQxFFiyJ1HB+B6m6duzIxJr24xaCaRFiJ734ayCqJBPRi5GFWcWVgALJvHR7-dXcoXQHKxtjgdwIf8f1gUoHi4VkpyK9j1AsWDYztLvtb3vUcmy7HspwVdtqEEOBF3AlduXeMBs2fUpT3PPBj2Q1DDwBT8xx0IDyAfHdzifapbCPC4bmzKiwB2Z5SgvN8dA-FiOn4I0iFCP0FQAMVKapvgrAJ33wpYVlUTVIn7OhlxAmSuknMjWToypSj4vi2Uwojh0fABNC98INOBuFk5S636OiAHl7nsWwtKPXSSKwwyAR6AIAloYwdHYbJVigCxtGsGwikbUQsggVhSnQdhRQAEkEWgCgS4tGG0fwlCBJQwCBSkgA)

## Examples

``` r
library(dplyr)

data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb %>%
    filter(!AVISIT %in% c("SCREENING", "BASELINE"))
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADLB <- data[["ADLB"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_abnormality_by_worst_grade(
      label = "Laboratory Test Results with Highest Grade Post-Baseline",
      dataname = "ADLB",
      arm_var = choices_selected(
        choices = variable_choices(ADSL, subset = c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      paramcd = choices_selected(
        choices = value_choices(ADLB, "PARAMCD", "PARAM"),
        selected = c("ALT", "CRP", "IGA")
      ),
      add_total = FALSE
    )
  ),
  filter = teal_slices(
    teal_slice("ADSL", "SAFFL", selected = "Y"),
    teal_slice("ADLB", "ONTRTFL", selected = "Y")
  )
)
#> Initializing tm_t_abnormality_by_worst_grade
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
