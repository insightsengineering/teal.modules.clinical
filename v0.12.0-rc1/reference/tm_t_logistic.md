# teal Module: Logistic Regression

This module produces a multi-variable logistic regression table
consistent with the TLG Catalog template `LGRT02` available
[here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/lgrt02.html).

## Usage

``` r
tm_t_logistic(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var = NULL,
  arm_ref_comp = NULL,
  paramcd,
  cov_var = NULL,
  avalc_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVALC"), "AVALC", fixed = TRUE),
  conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
    TRUE),
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

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  or `NULL`)  
  object with all available choices and preselected option for variable
  names that can be used as `arm_var`. This defines the grouping
  variable(s) in the results table. If there are two elements selected
  for `arm_var`, the second variable will be nested under the first
  variable. If `NULL`, no arm/treatment variable is included in the
  logistic model.

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

- cov_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  covariates variables.

- avalc_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  analysis variable (categorical).

- conf_level:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  confidence level, each within range of (0, 1).

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

    tm_t_logistic(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEEr8UKRQ+kZc1AD6waHWQSFhhjoA7rSkABa0EOyxULg6IEo6OgCCACIAygAy4TqkMARRcAAeUVD8otTFZeVYlXUNTa3t-IyiOgCkAHyTPSXGtNTkjOwACqVYpQCyAMLlU9mTOgTsCmAAQk6V-WsAkucF53cAcgBqTrbncgEAvgEAKyI2SiAGs4KxRLlEjZkvw4MYoMJSFECPxaKImkCQeDIcBoPAoXk5ABdVwQCo1Op5YDAc6U6rnElkil9AZwxK0+lspkspQsGBRRgI1FEGCoOp0USkM4QEqlXa2TZ7A4AXh0Upl8x0wuMOnV9Kw2x0F0e2uI4v1JzOYGVZUeOkNxt23x6clwPTt6s1spKJV1VvOF0QOjW1CgBDg9CIZrlfotEvVp3pIfKjEEAHMdAANB3nXYh3Zi+jZEKWQJgAIlAIBfnoOrZdK+nR5K15D1xvhCEQTdVd4RiZslBpRFHUIgZjEaZNxv0aqD0ODUQNgaoTqcyHRYOAZ4WiUTl2Nzkp5Am6A22nl4bUlAVRTQsK0EDLAyOiKKiJdwdRwfhD4-Pq+YhWg+jC0AuIioi+MiDhU-QFMmtpGnmSEqt87o3n6n4iD+-Arsq5yYRhs5+neuqipa6pkSKCYdseOioCwsBok+0Fvh+X64f+c6ATBvY6A+1CCHAUFAVCcGVE8YAbFsaF4I60mbDs3x0fROjYd+5B4ReVw3JU9yESR1aqTxRCaPej5JmxYgcThWncfG1n8YhlROLm8n0gA4i4HmXNsWAANJYAAjChFz+UFABMKmYSUGm4SurnuUR2pVjoNZKLQersNkKwRho2jWDYRRxqIWQQKwpToOwaCoAAJIItAFLVdWfow2j+EovxKGAvwkkAA)

## Examples

``` r
library(dplyr)

data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADRS <- tmc_ex_adrs %>%
    filter(PARAMCD %in% c("BESRSPI", "INVET"))
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADRS <- data[["ADRS"]]

arm_ref_comp <- list(
  ACTARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  ),
  ARM = list(
    ref = "B: Placebo",
    comp = c("A: Drug X", "C: Combination")
  )
)

app <- init(
  data = data,
  modules = modules(
    tm_t_logistic(
      label = "Logistic Regression",
      dataname = "ADRS",
      arm_var = choices_selected(
        choices = variable_choices(ADRS, c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        choices = value_choices(ADRS, "PARAMCD", "PARAM"),
        selected = "BESRSPI"
      ),
      cov_var = choices_selected(
        choices = c("SEX", "AGE", "BMRKR1", "BMRKR2"),
        selected = "SEX"
      )
    )
  )
)
#> Initializing tm_t_logistic
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
