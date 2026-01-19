# teal Module: Cox Regression Model

This module fits Cox univariable or multi-variable models, consistent
with the TLG Catalog templates for Cox regression tables `COXT01` and
`COXT02`, respectively. See the TLG Catalog entries for `COXT01`
[here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/coxt01.html)
and `COXT02`
[here](https://insightsengineering.github.io/tlg-catalog/stable/tables/efficacy/coxt02.html).

## Usage

``` r
tm_t_coxreg(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  arm_ref_comp = NULL,
  paramcd,
  cov_var,
  strata_var,
  aval_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "AVAL"), "AVAL", fixed = TRUE),
  cnsr_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    "CNSR"), "CNSR", fixed = TRUE),
  multivariate = TRUE,
  na_level = tern::default_na_str(),
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

- paramcd:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  parameter code variable from `dataname`.

- cov_var:

  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))  
  object with all available choices and preselected option for the
  covariates variables.

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

- multivariate:

  (`logical`)  
  if `FALSE`, the univariable approach is used instead of the
  multi-variable model.

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

The Cox Proportional Hazards (PH) model is the most commonly used method
to estimate the magnitude of the effect in survival analysis. It assumes
proportional hazards: the ratio of the hazards between groups (e.g., two
arms) is constant over time. This ratio is referred to as the "hazard
ratio" (HR) and is one of the most commonly reported metrics to describe
the effect size in survival analysis.

This modules expects that the analysis data has the following variables:

- `AVAL`: time to event

- `CNSR`: 1 if record in `AVAL` is censored, 0 otherwise

- `PARAMCD`: variable used to filter for endpoint (e.g. OS). After
  filtering for `PARAMCD` one observation per patient is expected

The arm variables and stratification/covariate variables are taken from
the `ADSL` data.

## Note

- The likelihood ratio test is not supported for models that include
  strata - the Wald test will be substituted in these cases.

- Multi-variable is the default choice for backward compatibility.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`TableTree` as created from
  [`rtables::build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_coxreg(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpQGI3OgGK1Go0jpwAB6wqCLungC80TGxMRF2ABa6waEiOvRw1EQA7jq0ohlQonD8OiQ6pMk6gqKC3DpoqIxEUASJ+RCaRNSatBAA5joEjHBQGhVExglQOswQ-EQwOgDCACIAkgDKKzr848VwAVALlckQc2bKQ1W6TXSyExAYrtCMMAD6o8YfxDCo+iMdH87CUOh0AEEVrYIVgALLrHSRHTA0igi7guZwYxInQKMCwuE6ABC+NwYMxfwByIIoIJ8MhZLx9KJK3xigxcnJGMJuNR6MxWJxyPxxMQOgACtQ2nB6EQyRTwVTcbT8RDxWtGIIhgANJn4lbilZLej9caWCDsikcjlKfakWaGSpjagfe1Qax2g6AnQ5WhVfrsd24HQgCkQtZbAAyPtIMAIH2CHyg-FE1HDa1sticsfjiaCyf4pHISgAvhyAFZEfofADWcFYoiDBxsTv42KgwlIv34BQTVZr9cbwGg8Cb7rkAF1XhHoz73cBgGrI1H8ZPpxAI1mc22Dovl9u1xulE0ff1-QK9t7kcGKXwhCJCsj78IxJfwXGPt3iEFRgN35i0qZNQuIGkQQQ6FgcADC8eCKpi7qjroIoEpm2YKhigosJ8mgsCqiTVgQYgfCUIjqKU7Cqiy+osusNFQjC8J0WAXLMoS7LcoK4LYV82K-Es1KNO8vE-FSnFcagLCwAQZQ0gRMjEaRcDkfwAGCrh1CCHAvzyURTZbtmIb4hKsIQgiaw0SZWBmRxzIAPJbPi8Hglyzk6P4zAOh8uGMPhhGKVkynkKpblKnSKx2Q4ABy9gAJo0Vs9gQjCACMCVJTCABMtn4ol1mpU5mGYq5RVKkQmjeXhcn+aIJGBSpamUnSEIAOIuHgzLEnCWAANJYGlHWit1fXZYNYBYE4LUbHZUUDaxaptYVXE6CVy0wF2WgsLQ4zIXYjguEVHIuUoHK0Di7D9OQagaNo1g2GGGKiIk-SsBC6DsE0AAkgi0CGX0lIw2iMBypZKGApaTkAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpQGI3OgMpxiEfjpwAB6wqCLungC80TGxsRF2ABa0ojoa8Lg6UDqirGSJcBoEOqhQtIw6RMY6AIIAIra2TgD09V4AMjr8UKTZKSZQTDI9cAHGRBUAwkRBOoxwAObzoqKWEAkA7gXzabRiWf4lmtwA+vCkiQI6MD3kjBhKdEwsHPxhrIyK6-492YZpcFO3V61iUwL+Rg2tAutAg7HBmRASh0tQaTX0RnBGGMzHg7GRKO8tgcdQAmgBJOo6SI6BRgdoOAByAHE6bgCSiagA1GqdGkEdgAFkyAGZMgBGCWZABMMtFUp00rk7IghJ0k0ZXiw1J0AslOn1+oADArjZkTTojcqObUsABZSZUmnGKDqCb41Vq3XsC2G03mgOW83Wz1q6iDODUVL8-FgGr22ps2lxhMAITpXzVIbVXicAA0dS63YwPV7vRaK4G-QbTdmveH6JHo966QAxJN0u0Zm11lHkzW2Quu0jugV0moduOT9N4ZMzzLj6eTiezunz5MruS9nKkRiwhaiGqiVvDibN1u83MEzOoxpODFzeiw-jsep3zJvpoftFOb93m+fve-xQKIJwaPQ9AiK+P43qIhQYHBozsOKAE-gAJP2XiDv8oihFBgEYQOqF3mhNTMkBRi4TAYRwOwcEAI7sAArJkADsLE6Exyo5LQABeug0hAjBEBs0H-pk8xhK6Al2I4TjEU0aEOF4DipgAUpSD6lKI5CxqIgj0Em4qIEJIliU03EEZhtiZHBqA6nSBjdp6BEAArxjUdoPm5HkOlS-x0gA8l4dIEm0nQ4QZcGkKWhKASqapwSI6g6mOYDKapGl1JOWEkhS2Wrimfk5fmk7Wcu5HOSiXwAL5fEoABWRCwicADWcCsKI8K-DY-z8HALrCKQJwEPwKQECcTUte1nXANA8BdeCcgALquBA4UPuCwDAOOdQdHSy2retP6bb8227XeB1HQkAAGXDUDdWToHQsgaCQCQGJ9X3fT9TkQB4OiMkQ5BpIkPSg7o2rEP1gRBKgEzkAE9CsDoN1eJcGw6Nq0z9Y9-BEPsEDAzosKSIIMMXLo4IJKg8wGLTRAEGIqwQAsGA6KSRCCDoULUNQOgQHAoxpEQurzCMENdL80VZKkgxENoOiNuM8wJIwZjKAskvBPDjCI1juoCHADzfCwMAnPMxgjUQ1EPnQOmvvajo6vbMWWw5RWJrOxC2zG45phmW5rWg9n-LC0KxeCOoIgSfBCCIzZx8IYixSipDm8NxBBPMCyp4SDaRh70yzFgiwPHgNoouC80yRdTRspXWSMObxwVPylwyGIJxJb4iPsGl8bFWA3H+0P24ombFsDdbvtN+blsz6gCVeqUuKjalHdM6BPfqEhjcosc1CCHAI2byn8XJu5WCeY6k5X55GYLmAwWhaGhLjzuzC9Ccrcb81W-d0jL3JCaVyrD0yIyBw7R2gf2IJoH+LA-6d23kA3eL40q5jzBVFw4DkyYMfo3GAQ0tAsFoBLGk9hnA9mvEoL4tBqjsFhHcYcWhaJbh0EiT0ohkgQFYDUdA7AQ5oUELQTIQi4KMG0J8JQNUlBgBqstIAA)

## Examples

``` r
## First example
## =============
## The example below is based on the usual approach involving creation of
## a random CDISC dataset and then running the application.

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

data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADTTE <- tmc_ex_adtte
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADTTE <- data[["ADTTE"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_coxreg(
      label = "Cox Reg.",
      dataname = "ADTTE",
      arm_var = choices_selected(c("ARM", "ARMCD", "ACTARMCD"), "ARM"),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"
      ),
      strata_var = choices_selected(
        c("COUNTRY", "STRATA1", "STRATA2"), "STRATA1"
      ),
      cov_var = choices_selected(
        c("AGE", "BMRKR1", "BMRKR2", "REGION1"), "AGE"
      ),
      multivariate = TRUE
    )
  )
)
#> Initializing tm_t_coxreg
if (interactive()) {
  shinyApp(app$ui, app$server)
}

## Second example
## ==============
## This time, a synthetic pair of ADTTE/ADSL data is fabricated for Cox regression
## where ties and pval_method matter.
library(dplyr)

data <- teal_data()
data <- within(data, {
  ADTTE <- data.frame(
    STUDYID = "LUNG",
    AVAL = c(4, 3, 1, 1, 2, 2, 3, 1, 2),
    CNSR = c(1, 1, 1, 0, 1, 1, 0, 0, 0),
    ARMCD = factor(
      c(0, 1, 1, 1, 1, 0, 0, 0, 0),
      labels = c("ARM A", "ARM B")
    ),
    SEX = factor(
      c(0, 0, 0, 0, 1, 1, 1, 1, 1),
      labels = c("F", "M")
    ),
    INST = factor(c("A", "A", "B", "B", "A", "B", "A", "B", "A")),
    stringsAsFactors = FALSE
  )
  ADTTE <- rbind(ADTTE, ADTTE, ADTTE, ADTTE)
  ADTTE <- as_tibble(ADTTE)
  set.seed(1)
  ADTTE$INST <- sample(ADTTE$INST)
  ADTTE$AGE <- sample(seq(5, 75, 5), size = nrow(ADTTE), replace = TRUE)
  ADTTE$USUBJID <- paste("sub", 1:nrow(ADTTE), ADTTE$INST, sep = "-")
  ADTTE$PARAM <- ADTTE$PARAMCD <- "OS"
  ADSL <- subset(
    ADTTE,
    select = c("USUBJID", "STUDYID", "ARMCD", "SEX", "INST", "AGE")
  )
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADTTE <- data[["ADTTE"]]

## `teal` application
## ----------------
## Note that the R code exported by `Show R Code` does not include the data
## pre-processing. You will need to create the dataset as above before
## running the exported R code.

arm_ref_comp <- list(ARMCD = list(ref = "ARM A", comp = c("ARM B")))

app <- init(
  data = data,
  modules = modules(
    tm_t_coxreg(
      label = "Cox Reg.",
      dataname = "ADTTE",
      arm_var = choices_selected(c("ARMCD"), "ARMCD"),
      arm_ref_comp = arm_ref_comp,
      paramcd = choices_selected(
        value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"
      ),
      strata_var = choices_selected(c("INST"), NULL),
      cov_var = choices_selected(c("SEX", "AGE"), "SEX"),
      multivariate = TRUE
    )
  )
)
#> Initializing tm_t_coxreg
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
