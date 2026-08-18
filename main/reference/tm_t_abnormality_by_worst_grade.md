# teal Module: Laboratory test results with highest grade post-baseline

This module produces a table to summarize laboratory test results with
highest grade post-baseline

## Usage

``` r
tm_t_abnormality_by_worst_grade(
  label,
  dataname,
  parentname = "ADSL",
  arm_var,
  id_var = teal.picks::variables("USUBJID", "USUBJID", fixed = TRUE),
  paramcd,
  atoxgr_var = teal.picks::variables("ATOXGR", "ATOXGR", fixed = TRUE),
  worst_high_flag_var = teal.picks::variables("WGRHIFL", "WGRHIFL", fixed = TRUE),
  worst_low_flag_var = teal.picks::variables("WGRLOFL", "WGRLOFL", fixed = TRUE),
  worst_flag_indicator = teal.picks::values("Y", "Y", fixed = FALSE, multiple = FALSE),
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

- id_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object specifying the variable name for subject id.

- paramcd:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  variable used to filter the analysis dataset (typically `PARAMCD`).
  The `values()` element is added internally to allow users to pick
  laboratory parameter value(s) interactively.

- atoxgr_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the
  analysis toxicity grade variable.

- worst_high_flag_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the worst
  high grade flag variable.

- worst_low_flag_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object with all available choices and preselected option for the worst
  low grade flag variable.

- worst_flag_indicator:

  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  is deprecated but still accepted)\
  Value(s) matching the worst high/low flag variables (default `"Y"`).
  Uses explicit candidate levels including an empty string where needed.
  The UI shows the selected value as static text (not an interactive
  control).

- add_total:

  (`logical`)\
  whether to include column with total number of patients.

- total_label:

  (`string`)\
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- drop_arm_levels:

  (`logical`)\
  whether to drop unused levels of `arm_var`. If `TRUE`, `arm_var`
  levels are set to those used in the `dataname` dataset. If `FALSE`,
  `arm_var` levels are set to those used in the `parentname` dataset. If
  `dataname` and `parentname` are the same, then `drop_arm_levels` is
  set to `TRUE` and user input for this parameter is ignored.

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

- basic_table_args:

  (`basic_table_args`) optional\
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
  [`rtables::build_table()`](https://rdrr.io/pkg/rtables/man/build_table.html))

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEEr8UKRQ+kZc1AD6waHWQSFhhjoA7rSkABa0EOyxULg6IEo6OgCCACIAygAy4TqkMARRcAAeUVD8otTFZeXVAEJ1DU2t7fzU9DoApAB8Uz0lxrTU5IzsAISlAGoAkpU7ttPZUzoE7ApglQDCWE5OAHI79wDiFwUX-aWVTtVPLmByAIAXwCACsiNkogBrOCsUS5RI2ZL8ODGKDCUhRAj8WiiJrgyEwuHAaDweF5OQAXVcEAqNTqeWAwAudOqF0p1NpfUGyMSTJZ3PZnKUaFQdWy6XOEBKeR0AF4dHlcD0+EIRKJ5TpVcIxFKSiUGlFMVB6BAiIwYNx0qwovQbSlzaJMQBzZgovX6krUE1waiai7VE3mkLm1h2MSkHRYMQYjVpTI6AAStGdGQjOmebt0AAUiE6DP0oKJfdk4G8Fvq8qTdAqBQNy9LPToWDAopoWJr24xaCb1ewCBkIQQxJqziysABZN46ccTq7lC5yArFkTqOD8f1gUqTxfKxue1AsWDYzssHv0PsDocj2tgbPb0pzhcAvdN5v8fhGoihP0KgBipTVN8FYBCUS49EsKyqJqkRRF0Mi6hWsHwcO5xblUbJ4DOlylH+f6YcuvpwGuG63gAmruSFwNwcF0Khdb9NOFwAPL3PYtj4dOK7EeQpHYRRAI9AEAS0MYOjsNkqxQBY2jWDYRSNqIWQQKwpToOwooACSCLQBRacWjDaP4ShAkoYBApSQA)

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
      arm_var = variables(choices = c("ARM", "ARMCD"), selected = "ARM"),
      paramcd = variables(choices = "PARAMCD"),
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
