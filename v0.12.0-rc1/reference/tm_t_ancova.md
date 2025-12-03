# teal Module: ANCOVA Summary

This module produces a table to summarize analysis of variance,
consistent with the TLG Catalog template for `AOVT01` available
[here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/aovt01.html)
when multiple endpoints are selected.

## Usage

``` r
tm_t_ancova(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  arm_ref_comp = NULL,
  aval_var,
  cov_var,
  include_interact = FALSE,
  interact_var = NULL,
  interact_y = FALSE,
  avisit,
  paramcd,
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

- aval_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and pre-selected option for the
  analysis variable.

- cov_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  covariates variables.

- include_interact:

  (`logical`)  
  whether an interaction term should be included in the model.

- interact_var:

  (`character`)  
  name of the variable that should have interactions with arm. If the
  interaction is not needed, the default option is `NULL`.

- interact_y:

  (`character`)  
  a selected item from the interact_var column which will be used to
  select the specific `ANCOVA` results when interact_var is discrete. If
  the interaction is not needed, the default option is `FALSE`.

- avisit:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  value of analysis visit `AVISIT` of interest.

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

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

## Details

When a single endpoint is selected, both unadjusted and adjusted
comparison are provided. This modules expects that the analysis data has
the following variables:

- `AVISIT`: variable used to filter for analysis visits.

- `PARAMCD`: variable used to filter for endpoints, after filtering for
  `paramcd` and `avisit`, one observation per patient is expected for
  the analysis to be meaningful.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` - output of
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_ancova(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOyARVyikrLK6v4AR1ElAF9FCAArIni-AGs4VlFE0Jtw-jhjKGFSPwJ+WlEy0fGpmeBoeFmkuQBdVwgcgqKk4GAFMBv8p-PL6+bWxdCHp5yWm8PkoWDA-IxlhsiDBUEU6KJSOwGpksABZHQAXh08MRDQyEOMmJ0TwAQogdAAFahQAhwehEJ64PE6YgwokEJHPcnZRiCADmOgAGoziWAAMLksXQ+jxDyWCBPIYZORMiAZTJi2wo1Fi7JEnFItUZHQEon-NE6EmM5ms2FYjnm9GZEWOnRixUNIZDEHoIrxaKGjJJIlJVUZPhCESiIkR4RiQPGkp+dZQcxETRQBPG7FQehwahm54AOTFAHkAGqZOy5kTWo3ZpLHXRY-5fOvZjJQTTbaLsmJjWmiPyifNwdRwfhZjsEfsyMREjPUQRwDazwfsAG5FL-csASVyu9sirDHYyI5E4-4hYA6k4nABpHQARh02UyAE0dAAOJ7M5Un7NQT8DNGD7AcxGHUdLynbMZ3A6MsRA2gaxXOC51mF4UgdZ40RdZ5NW1XU8MI7JFRVP9jXPMdyCvFscJ1UiwAo8j62NICCShNksXYyFbQAtjF2AlgwPQyCLxomDjTQwcFxYZD6BEVd4I3L4sM5MUAAkAHFiMrV4wDkFjTzPKCaMLTSdKY1j-wo4hNCE0D7TXCCqOgiiMmk+dELklClPQlSWjU0lMlyFw8FFXJ7EyLUnzw0LhQMozjNcsy6MirBosyWKrNPJLs1QFhYE2ETBzE6iJ0kjznIQnRF2XPz103bcwApFFMgYvDWoy1Fj3cnQUonQsADF7wPAwhuvK0co7PLjXicg1HWECSpc0yKr6zyaqQ3zNoCrcWU5EkQrC5r0sy7LTqcBLDP47MBtoiKopi39rJ0JVjXer0lFoQl2Hm1QaQ0bRrBsNIjVEOIIFYTJ0HYNBUAAEkEWgUnhhGR0YbRGCGfolDAfpziAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADQS <- tmc_ex_adqs
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADQS <- data[["ADQS"]]

arm_ref_comp <- list(
  ARM = list(
    ref = "B: Placebo",
    comp = c("A: Drug X", "C: Combination")
  ),
  ACTARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  )
)

app <- init(
  data = data,
  modules = modules(
    tm_t_ancova(
      label = "ANCOVA Table",
      dataname = "ADQS",
      avisit = choices_selected(
        choices = value_choices(ADQS, "AVISIT"),
        selected = "WEEK 1 DAY 8"
      ),
      arm_var = choices_selected(
        choices = variable_choices(ADSL, c("ARM", "ACTARMCD", "ARMCD")),
        selected = "ARMCD"
      ),
      arm_ref_comp = arm_ref_comp,
      aval_var = choices_selected(
        choices = variable_choices(ADQS, c("CHG", "AVAL")),
        selected = "CHG"
      ),
      cov_var = choices_selected(
        choices = variable_choices(ADQS, c("BASE", "STRATA1", "SEX")),
        selected = "STRATA1"
      ),
      paramcd = choices_selected(
        choices = value_choices(ADQS, "PARAMCD", "PARAM"),
        selected = "FKSI-FWB"
      ),
      interact_var = choices_selected(
        choices = variable_choices(ADQS, c("BASE", "STRATA1", "SEX")),
        selected = "STRATA1"
      )
    )
  )
)
#> Initializing tm_t_ancova
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
