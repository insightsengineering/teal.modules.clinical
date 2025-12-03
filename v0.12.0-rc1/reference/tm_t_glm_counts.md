# Teal Module: Regression Counts Summary

Summarize results of a Poisson negative binomial regression that is
result of a generalized linear model of one (e.g. arm) or more
covariates.

## Usage

``` r
tm_t_glm_counts(
  label = "Counts Module",
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  aval_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVAL"), "AVAL", fixed = TRUE),
  arm_var,
  strata_var,
  rate_mean_method = c("emmeans", "ppmeans"),
  distribution = c("negbin", "quasipoisson", "poisson"),
  offset_var,
  cov_var,
  arm_ref_comp = NULL,
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

- aval_var:

  (`character`)  
  name of the analysis value variable.

- arm_var:

  (`character`)  
  variable names that can be used as `arm_var`.

- strata_var:

  (`character`)  
  names of the variables for stratified analysis.

- rate_mean_method:

  (`character`) method used to estimate the mean odds ratio. Either
  "emmeans" or "ppmeans" (as in
  [`summarize_glm_count()`](https://insightsengineering.github.io/tern/latest-tag/reference/summarize_glm_count.html)).

- distribution:

  (`character`) value specifying the distribution used in the regression
  model (Poisson: `"poisson"`, Quasi-Poisson: `"quasipoisson"`, negative
  binomial: `"negbin"`).

- offset_var:

  (`character`) a name of the numeric variable to be used as an offset?

- cov_var:

  (`character`)  
  names of the covariates variables.

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

- conf_level:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for
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

## Details

- Teal module for
  [`tern::summarize_glm_count()`](https://insightsengineering.github.io/tern/latest-tag/reference/summarize_glm_count.html)
  analysis, that summarizes results of a Poisson negative binomial
  regression.

- The arm and stratification variables are taken from the `parentname`
  data.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_glm_counts(
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

[`summarize_glm_count()`](https://insightsengineering.github.io/tern/latest-tag/reference/summarize_glm_count.html)

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+owHdapAAtaCE44bgB9d09rXB0QJR0dAEEAEQBlABlvHXJGCEREXIhwuAAPcKh+UWoE5JTbWydsooKikvLK0nIlAF9FCCUAKyJg8IBrOFZRdiioG0MdfjhjKGFScIJ+WlECcOHRiangaHhp2bkAXVdoRhhwxmWNohhUbLpRUnZapIBhWySsABZH4pHQAXh070+tUSD2M4J0CjAAMBOgAQkjcDCdMQXgiCF9kUDkpjEUTUT8kf1EnIsRBEiiEVCvvTEjo4QikWjEDoAArUKAEOD0IiY7G414QglIpI8lKMQQAcx0AA1SUifjyfs96MEPJYBmBqTp+v0lKlMtlZsBgDL0hkkRcrhBUg0mgtrbbkfVGo7nQBiHQASWkGm4tAAXrpAtGwtQdGhUEpE9lgv4WYlZgjZnTEnwhCJRAj88IxBm2aQ7utFdQ7sQzKRptjMx4oCddBC7W6xay2QnbuFNCx8QERkLROFRHAROo4Pxy33EkPGLQoPQRBtRzIy67GrFpeT1eSQUffv8gSejbTm2yZReUkib9fe2yWHc4U88RC3-dHhLc4uUBDtQg7DlKW7jpO05wLO843kuLCruucCbmOO4+k4sQygAakkDpGgBi5kkkuH4U+hFsh8zCeKBjAjmhE5TjO5BwS+fbLkhG4EBB6GZFhYBpE4aoEfBOgAHIOBkDpsSaFGJEQxjGFO6zLvR26MdBsELouHFrlxPHTBaGT8UkADiLgiTJiQSVJ5E3sQmi0WpkFMTBLHaexiF6Sh3EMewu6YWSaRBo0QYPpZRHWZJ0lEcaNK1Ka1y0PC7DBLkgoaNo1g2PErKiEEECsEk6DsImAAkgi0LE5VTow2iMP0PRgD0FxAA)

## Examples

``` r
data <- within(teal_data(), {
  ADSL <- tern::tern_ex_adsl
  ADTTE <- tern::tern_ex_adtte
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

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

ADSL <- data[["ADSL"]]
ADTTE <- data[["ADTTE"]]
# Initialize the teal app
app <- init(
  data = data,
  modules = modules(
    tm_t_glm_counts(
      dataname = "ADTTE",
      arm_var = choices_selected(
        variable_choices(ADTTE, c("ARM", "ARMCD", "ACTARMCD")),
        "ARMCD"
      ),
      arm_ref_comp = arm_ref_comp,
      aval_var = choices_selected(
        variable_choices(ADTTE, "AVAL"),
        "AVAL"
      ),
      strata_var = choices_selected(
        variable_choices(ADSL, "SEX"),
        NULL
      ),
      offset_var = choices_selected(
        variable_choices(ADSL, "AGE"),
        NULL
      ),
      cov_var = choices_selected(
        variable_choices(ADTTE, "SITEID"),
        NULL
      )
    )
  )
)
#> Initializing tm_t_glm_counts

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
