# teal Module: Exposure Table for Risk management plan

The module produces an exposure table for risk management plan.

## Usage

``` r
tm_t_exposure(
  label,
  dataname,
  parentname = "ADSL",
  row_by_var,
  col_by_var,
  paramcd = teal.picks::variables("PARAMCD"),
  paramcd_label = "PARAM",
  id_var = teal.picks::variables("USUBJID", "USUBJID", fixed = TRUE),
  parcat,
  aval_var = teal.picks::variables("AVAL", "AVAL", fixed = TRUE),
  avalu_var = teal.picks::variables("AVALU", "AVALU", fixed = TRUE),
  add_total,
  total_label = default_total_label(),
  add_total_row = TRUE,
  total_row_label = "Total number of patients and patient time*",
  na_level = tern::default_na_str(),
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

- row_by_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  variable names that can be used to split rows (`dataname`).

- col_by_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  variable names that can be used to split columns (`parentname`).

- paramcd:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  variable used to filter by parameter (`dataname`); `values()` is added
  internally.

- paramcd_label:

  (`character`) the column from the dataset where the value will be used
  to label the argument `paramcd`.

- id_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  object specifying the variable name for subject id.

- parcat:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html);
  legacy `teal.transform` objects are deprecated but still accepted)\
  parameter category column on `dataname`; `values()` is added
  internally.

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

- add_total:

  (`logical`)\
  whether to include column with total number of patients.

- total_label:

  (`string`)\
  string to display as total column/row label if column/row is enabled
  (see `add_total`). Defaults to `"All Patients"`. To set a new default
  `total_label` to apply in all modules, run
  `set_default_total_label("new_default")`.

- add_total_row:

  (`flag`)\
  whether a "total" level should be added after the others which
  includes all the levels that constitute the split. A custom label can
  be set for this level via the `total_row_label` argument.

- total_row_label:

  (`character`)\
  string to display as total row label if row is enabled (see
  `add_total_row`).

- na_level:

  (`string`)\
  used to replace all `NA` or empty values in character or factor
  variables in the data. Defaults to `"<Missing>"`. To set a default
  `na_level` to apply in all modules, run
  `set_default_na_str("new_default")`.

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

- `table` (`ElementaryTable` as created from
  [`rtables::build_table`](https://rdrr.io/pkg/rtables/man/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_exposure(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEEr8UKRQ+kZc1AD6waHWQSFhhjoA7rSkABa0EOyxULg6IEo6OgCCACIAygAy4TqkMARRcAAeUVD8otTFZeVOABp1DU2t7fytrhAlonCkGDNw-OwAjAUA1tn8OgC8OgpgALKqMxAQcAa2aaLkjPsBJRjUUPRw1KJ1xNFPL2-sFQMFYy0ajUHY6ABipWqlSc916Azq-0GAFIAHzInolfi0a7ZdTsByVBwAIQAUgBJcoFDBrOBwVDtEFg+zOGxojFTEo6GCCULkdiYrkABVKWFKBwAwuUwftbOUHFhyvtcIKSiKxQcZWAAPLaZhMoTMDQkHS5KCsUSKPCqsoANShYNEsB8cHYLUdcAAjisCgAmAAM-rkBVEtAAXrpdjlgzpGPSngRI3ZHLCVZyuXaoQ4teVzaJ9oK2ejBfRNlFGEQUqI-n1+nDPlFvq9q0ibMlHs9m0oAL4BABWRGyUVpFrNoTbRnGxigwlIUQI2NETQHQ5HomA0Hg1bycgAupM0Kg6tl0gLOXkwXk0yU+EIRO9drfhGIzxmGlE561UERRII46+Mx0JtQV2fZyj-EJLCmIhjB0JwWm-X84zsZ4RGVG08k3JN9iRdD0y5VAWFgBcwU0FhaFQl8CAyQdEwfPYwHVcUpTua9AIbehWCiMjGFI8jKOrG0Smo2ixDBAgBTAGF+mVBjRQOVihJ0GYRHURYtWkgt8JKYMbQrFIok47iWD4xgKPoe8AMAnQRJkMTdgk-YxQlFw8AYrAnAAcXJbUADlllk-ZKnsUpbFKAK3KCgZFO0rkVLgNStlAsBnNcm1dNiwjGFkUgwVQGQ1mrHjzMs2y6K1dUJVC31WJ0MjqEEF8Y2ohK1hiRIZly3ZIWhVMbQ6fgPyIUIQIhKEYULHoMpKIFqBuMFIiiLo7OrRblsTSSKhqQKpNKcFwWqWT4sSrUAE07gCAJaFg9hshuKALG0awbCKTlRCyCBWFKdB2EPAASQRaAKf6ZkYPUAm7JQwG7XcgA)

## Examples

``` r
library(dplyr)

data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADEX <- tmc_ex_adex

  set.seed(1, kind = "Mersenne-Twister")
  .labels <- col_labels(ADEX, fill = FALSE)
  ADEX <- ADEX %>%
    distinct(USUBJID, .keep_all = TRUE) %>%
    mutate(
      PARAMCD = "TDURD",
      PARAM = "Overall duration (days)",
      AVAL = sample(x = seq(1, 200), size = n(), replace = TRUE),
      AVALU = "Days"
    ) %>%
    bind_rows(ADEX)
  col_labels(ADEX) <- .labels
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_t_exposure(
      label = "Duration of Exposure Table",
      dataname = "ADEX",
      paramcd = variables(choices = "PARAMCD"),
      col_by_var = variables(
        choices = c("SEX", "ARM"),
        selected = "SEX"
      ),
      row_by_var = variables(
        choices = c("RACE", "REGION1", "STRATA1", "SEX"),
        selected = "RACE"
      ),
      parcat = picks(variables(choices = "PARCAT2"), values(), check_dataset = FALSE),
      add_total = FALSE
    )
  ),
  filter = teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y"))
)
#> Initializing tm_t_exposure
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
