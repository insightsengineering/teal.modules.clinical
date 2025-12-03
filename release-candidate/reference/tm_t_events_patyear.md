# teal Module: Event Rates Adjusted for Patient-Years

This module produces a table of event rates adjusted for patient-years.

## Usage

``` r
tm_t_events_patyear(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  events_var,
  paramcd,
  aval_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVAL"), "AVAL", fixed = TRUE),
  avalu_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVALU"), "AVALU", fixed = TRUE),
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = tern::default_na_str(),
  conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
    TRUE),
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
  names that can be used as `arm_var`. It defines the grouping
  variable(s) in the results table. If there are two elements selected
  for `arm_var`, second variable will be nested under the first
  variable.

- events_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  variable with all event counts.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- aval_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  analysis variable.

- avalu_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  analysis unit variable.

- add_total:

  (`logical`)  
  whether to include column with total number of patients.

- total_label:

  (`string`)  
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- na_level:

  (`string`)  
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

- conf_level:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  confidence level, each within range of (0, 1).

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

- `table` (`ElementaryTable` as created from
  [`rtables::build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_events_patyear(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEEr8UKRQ+kZc1AD6waHWQSFhhjoA7rSkABa0EOyxULg6IEo6OgCCACIAygAy4TqkMARRcAAeUVD8otTFZeWlTra2TnUNTa3twXCk5DoApAB8sz0lxrTU5IzsAAqlWKUAsgDC5XPZszoE7Apg-YNOAIzXBde3QwBMTzovA0MAzNdyGwLJYQEolGCCULkdi0UTNbRkHQAXh0hwAcpUsMiUQAGIGLZY6CFQuDsCDwiikUTInRQcTZcgAc1UMLhcARpEBSgAvgEAFZEbJRADWcFYolyiRsyX4cGMUGEpCiBH4sKaAqFovFwGg8AleTkAF1XBAKjU6nlgMAXlVqtdDcbTX0fsMZYkrTbXi4wA6TUoAMQ6e4YHQAITpMh0ThasB8cBNaFQdWy6SuoJ0eRpeVwPT4QhE1JReeEYjTYPqMCiSvZlLhqBCrDgLDL5Z01Cg9Dg1Bp3x0WBCulK-D5glE5H4JiIjB0WxCtEpBgAmk3GNTSqQ+7DhXYOyInoSSnldboUZ6Xfv0+WWJXNCwaQQMoKCGIoqIu3B1HB+C3WxdHzIxBpW9GFoXc4GVf9nwlM1qgKS4XiwfZPgQo5ygBOQc0vVs3xET8J1PG5EOOa4Dx0DDSI6fgqyIUJuxRexnEw38azIOFgPvSCXxwj9xx-VsHyfQCUWA0D6BECDBOg507meSAKVYgEmN-HRuLwns5JYqkL2UkxaBaL8aQYlwsJKciTJ0etmEafC-0k193zwvjywEgDCx0W9qEEcCXKg9gKi9WSdj2VDkLAIKDkU0iSlU8d1K9R4wFIgJy2SsilACWhjB0GEVDUDRtGsGwinTUQsggVhSnQdhEwAEkEWgClqt9GG0fweSUMBuUNIA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEEr8UKRQ+kZc1AD6waHWQSFhhjoA7rSkABa0EOyxULg6IEo6OgCCACIAygAy4TqkMARRcAAeUVD8otTFZeWlTra2TnUNTa3twXCk5DoApAB8sz0lxrTU5IzsAAqlWKUAsgDC5XPZszoE7Apg-YNOAIzXBde3QwBMTzovA0MAzNdyGwLJYQEolGCCULkdi0UTNbRkHQAXh0hwAcpUsMiUQAGIGLZY6CFQuDsCDwiikUTInRQcTZcgAc1UMLhcARpEBSgAvgEAFZEbJRADWcFYolyiRsyX4cGMUGEpCiBH4sKaAqFovFwGg8AleTkAF1XBAKjU6nlgMAXlVqtdDcbTX0fsMZYkrTbXi4wA6TUoAMQ6N4YHROFqwHy6NKZepQegiHSiHzpHQkIO0xgwKKaFg6HOMWhxkSiE1oVB1bLpK6gnR5Gl5XA9PhCYs05vCMTVsH1LNK9mUuGoEKsOAsLvdnTUONwag0746LAhXSlfh8wSicj8ExERg6LYhWiUgwATVHjGppVIC9hwrsRbgT0JJTyut0KM9LsfNe7LCz+ZpBAZIKBBiFEogznA6hwPw44ThcQEyGINL5oW8ZwMqCEgRKZrVAUlwvFg+yfARRzlMRYCVE4AAaAJyI234TuBIhQVuKL4TchHkZRNFgAEcF0U+tL8PwUSkEQoSzii9jOPRcH9mQcL-mxmGgUxkGbrBE6AcBSEoih94YTp2HOnczyQBSCkArJcGJhBLFzuZ8lUl+NkmLQLTQTS0kuAxJQCb5OhDswjSsfBRlgXZGmCSU2mIdSencII6GxVh7AVF6Zk7HspHkVlBxWdFtnMZuDleo8YCCXxYJVQEAS0MYOgwioagaNo1g2EUNaiFkECsKU6DsGWAAkgi0AUw3gYw2j+DyShgNyhpAA)

## Examples

``` r
library(dplyr)

data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADAETTE <- tmc_ex_adaette %>%
    filter(PARAMCD %in% c("AETTE1", "AETTE2", "AETTE3")) %>%
    mutate(is_event = CNSR == 0) %>%
    mutate(n_events = as.integer(is_event))
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADAETTE <- data[["ADAETTE"]]

# 1. Basic Example

app <- init(
  data = data,
  modules = modules(
    tm_t_events_patyear(
      label = "AE Rate Adjusted for Patient-Years At Risk Table",
      dataname = "ADAETTE",
      arm_var = choices_selected(
        choices = variable_choices(ADSL, c("ARM", "ARMCD")),
        selected = "ARMCD"
      ),
      add_total = TRUE,
      events_var = choices_selected(
        choices = variable_choices(ADAETTE, "n_events"),
        selected = "n_events",
        fixed = TRUE
      ),
      paramcd = choices_selected(
        choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
        selected = "AETTE1"
      )
    )
  )
)
#> Initializing tm_t_events_patyear
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# 2. Example with table split on 2 arm_var variables

app <- init(
  data = data,
  modules = modules(
    tm_t_events_patyear(
      label = "AE Rate Adjusted for Patient-Years At Risk Table",
      dataname = "ADAETTE",
      arm_var = choices_selected(
        choices = variable_choices(ADSL, c("ARM", "ARMCD", "SEX")),
        selected = c("ARM", "SEX")
      ),
      add_total = TRUE,
      events_var = choices_selected(
        choices = variable_choices(ADAETTE, "n_events"),
        selected = "n_events",
        fixed = TRUE
      ),
      paramcd = choices_selected(
        choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
        selected = "AETTE1"
      )
    )
  )
)
#> Initializing tm_t_events_patyear
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
